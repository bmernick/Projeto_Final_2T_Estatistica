################## Limpeza e Definicao dos Dados ##########################

#1) Importando bibliotecas

library(tidymodels)
library(tidyverse)
library(skimr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(reshape2)
library(car)
library(corrplot)
library(ranger)
library(MASS)
library(keras)
library(rsample)
library(yardstick)
library(FactoMineR)

library(dplyr)

#2)Import da base
dados_originais <- read.csv("coffee_ratings_1.csv")

#3) Verificando os dados missing
resultado <- dados_originais %>%
  lapply(type_sum) %>%
  as_tibble() %>%
  pivot_longer(cols = 1:ncol(dados_originais),
               names_to = "Coluna",
               values_to = "Tipo") %>%
  mutate(Contagem_NA = colSums(is.na(dados_originais))) %>%
  arrange(desc(Contagem_NA))

resultado_df <- as.data.frame(resultado)

print(resultado_df)

#4) Dropando preditoras desinteressantes 
dados_tratados_1 <- dados_originais %>% 
  dplyr::select(-owner, -region, -farm_name, -lot_number, -mill, -ico_number, -company, -altitude, -producer, -number_of_bags, -bag_weight,
                -in_country_partner, -harvest_year, -grading_date, -owner_1, -variety, -processing_method, -color,
                -expiration, -certification_body, -certification_address, -certification_contact, -altitude_low_meters, -altitude_high_meters)


#4.1) Dropando linhas missing da coluna region
dados_tratados_2 <- dados_tratados_1 %>% 
  dplyr::filter(!is.na(altitude_mean_meters) & !is.na(quakers)) %>% 
  dplyr::filter(country_of_origin != "Cote d?Ivoire")

#6) Convertendo ft em m 
# Função para converter pés para metros de forma vetorial
converter_ft_para_m <- function(altitude, unit) {
  ifelse(unit == "ft", altitude * 0.3048, 
         ifelse(unit == "m", altitude, NA))
}

# Aplicar a função no dataframe
dados_tratados_3 <- dados_tratados_2 %>%
  mutate(
    altitude_mean_meters_ajustada = converter_ft_para_m(altitude_mean_meters, unit_of_measurement)
  )

#7) Dropando preditoras que foram utilizadas para o calculo
dados_tratados_4 <- dados_tratados_3 %>% 
  dplyr::select(-unit_of_measurement,-altitude_mean_meters)

################## Analise descritiva ##########################

# 1) Analise Estastistica******************************************************

#1.1) Estatistica
glimpse(dados_tratados_4)
skim(dados_tratados_4)

#1.2) frequencia
table(dados_tratados_4$country_of_origin)
table(dados_tratados_4$species)


#Grande desbalanceamento de dados entre robusta e Arabica causando efeito de multicolinearidade
dados_tratados_4 <- dplyr::select(dados_tratados_4, -species)

#1.3) Proporcao
prop.table(table(dados_tratados_4$country_of_origin))


# 2) Analise Grafica***********************************************************

# 2.1) Balanceamento paises

# Tiblbe com a frequencia dos dados Paises 
tabela_regioes <- dados_tratados_4 %>%
  count(country_of_origin) %>%
  rename(Contagem = n) %>%
  group_by(country_of_origin) %>%
  mutate(Percentual_Participacao = (Contagem / nrow(dados_tratados_4)) * 100) %>%
  ungroup() %>%
  arrange(desc(Contagem))

# Adicionar uma linha para somar os totais de contagem e participação
tabela_regioes <- tabela_regioes %>%
  bind_rows(
    tibble(
      country_of_origin = "Total",
      Contagem = sum(tabela_regioes$Contagem),
      Percentual_Participacao = sum(tabela_regioes$Percentual_Participacao)
    )
  )

print(tabela_regioes, n = Inf)

# Removendo a linha do 'Total' para o gráfico (se não quiser incluir o total)
tabela_regioes <- tabela_regioes %>% filter(country_of_origin != "Total")

# Reordenando os países com base na Contagem para exibição decrescente
tabela_regioes <- tabela_regioes %>% 
  mutate(country_of_origin = reorder(country_of_origin, -Contagem))

# Calculando o máximo da Contagem e Percentual_Participacao para a escala
max_contagem <- max(tabela_regioes$Contagem)
max_percentual <- max(tabela_regioes$Percentual_Participacao)




# Criando o gráfico
grafico <- ggplot(tabela_regioes, aes(x = country_of_origin, y = Contagem)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_line(aes(y = Percentual_Participacao * max_contagem / max_percentual, group = 1), 
            color = "red", linewidth = 0.5) +
  geom_point(aes(y = Percentual_Participacao * max_contagem / max_percentual), 
             color = "red", size = 2) +
  geom_text(aes(label = Contagem, y = Contagem), vjust = -0.5, size = 3.3) +
  geom_text(aes(y = Percentual_Participacao * max_contagem / max_percentual, label = ifelse(Percentual_Participacao > 1, paste0(round(Percentual_Participacao, 0), "%"), "")), 
            color = "white", vjust = 2.5, size = 2.3) +
  scale_y_continuous(sec.axis = sec_axis(~ . * max_percentual / max_contagem, name = "Percentual de Participação (%)")) +
  labs(x = "Países", y = "Contagem") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exibindo o gráfico
print(grafico)

#########################################################################

# Identificar os países com pelo menos 2% de participação
paises_mais_2_por_cento <- tabela_regioes %>%
  filter(Percentual_Participacao >= 2) %>%
  pull(country_of_origin)

# Filtrar `dados_tratados_4` para manter apenas as observações desses países
dados_tratados_4 <- dados_tratados_4 %>%
  filter(country_of_origin %in% paises_mais_2_por_cento)

# Tiblbe com a frequencia dos dados Paises 
tabela_regioes_ajustado <- dados_tratados_4 %>%
  count(country_of_origin) %>%
  rename(Contagem = n) %>%
  group_by(country_of_origin) %>%
  mutate(Percentual_Participacao = (Contagem / nrow(dados_tratados_4)) * 100) %>%
  ungroup() %>%
  arrange(desc(Contagem))

# Adicionar uma linha para somar os totais de contagem e participação
tabela_regioes_ajustado <- tabela_regioes_ajustado %>%
  bind_rows(
    tibble(
      country_of_origin = "Total",
      Contagem = sum(tabela_regioes_ajustado$Contagem),
      Percentual_Participacao = sum(tabela_regioes_ajustado$Percentual_Participacao)
    )
  )

print(tabela_regioes_ajustado, n = Inf)

# Ordenar os países por Contagem em ordem decrescente
tabela_regioes_ajustado$country_of_origin <- factor(tabela_regioes_ajustado$country_of_origin, levels = tabela_regioes_ajustado$country_of_origin)
tabela_regioes_ajustado <- tabela_regioes_ajustado %>% filter(country_of_origin != "Total")


# Calcular o máximo para escala secundária
max_contagem <- max(tabela_regioes_ajustado$Contagem)
max_percentual <- max(tabela_regioes_ajustado$Percentual_Participacao)

# Plotando o gráfico
grafico_ajustado <- ggplot(tabela_regioes_ajustado, aes(x = country_of_origin, y = Contagem)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_line(aes(y = Percentual_Participacao * max_contagem / max_percentual, group = 1), 
            color = "red", linewidth = 0.5) +
  geom_point(aes(y = Percentual_Participacao * max_contagem / max_percentual), 
             color = "red", size = 2) +
  geom_text(aes(label = Contagem, y = Contagem), vjust = -0.5, size = 3.3) +
  geom_text(aes(y = Percentual_Participacao * max_contagem / max_percentual, label = ifelse(Percentual_Participacao > 1, paste0(round(Percentual_Participacao, 0), "%"), "")), 
            color = "white", vjust = 2.5, size = 2.3) +
  scale_y_continuous(sec.axis = sec_axis(~ . * max_percentual / max_contagem, name = "Percentual de Participação (%)")) +
  labs(x = "Países", y = "Contagem") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(grafico_ajustado)


# 2.2) Nota media por pais

# Calculando a média de total_cup_points por país
media_pontuacao_por_pais <- dados_tratados_4 %>%
  group_by(country_of_origin) %>%
  summarise(media_pontuacao = mean(total_cup_points, na.rm = TRUE)) %>%
  arrange(desc(media_pontuacao))

# Criando o gráfico com ggplot2
plot <- ggplot(media_pontuacao_por_pais, aes(x = reorder(country_of_origin, -media_pontuacao), y = media_pontuacao)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", round(media_pontuacao, 2))),
            position = position_dodge(width = 0.9), hjust = 1.1, size = 3.5, color = "white") +
  theme_minimal() +
  labs(title = "Média de Pontuação Total do Café por País", x = "País", y = "Média de Pontuação") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 16)) +
  coord_flip() # Usado para inverter as coordenadas e facilitar a leitura

# Plotando o gráfico
print(plot)

# 2.3) Dispersao Total pontos x notas por atributos

# Criar seis gráficos de dispersão
g1 <- ggplot(dados_tratados_4, aes(x = aroma, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Aroma vs Total Cup Points")

g2 <- ggplot(dados_tratados_4, aes(x = flavor, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Flavor vs Total Cup Points")

g3 <- ggplot(dados_tratados_4, aes(x = aftertaste, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Aftertaste vs Total Cup Points")

g4 <- ggplot(dados_tratados_4, aes(x = acidity, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Acidity vs Total Cup Points")

g5 <- ggplot(dados_tratados_4, aes(x = body, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Body vs Total Cup Points")

g6 <- ggplot(dados_tratados_4, aes(x = balance, y = total_cup_points)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Balance vs Total Cup Points")

# Combinar os gráficos
combinado <- (g1 | g2 | g3) / (g4 | g5 | g6)

# Exibir o gráfico combinado
combinado


################## Analise de Colinearidade ##########################
# Calculando a matriz de correlação
dados <- dados_tratados_4
colunas_nao_numericas <- sapply(dados, class) != "numeric"
dados_numericos <- dados[, !colunas_nao_numericas]
matrix_de_correlacao <- cor(dados_numericos)
print(corrplot(matrix_de_correlacao, method = "color", type = "upper", tl.col = "black", tl.srt = 45))


# Usando o Vif
modelo <- lm(total_cup_points ~ ., data = dados_tratados_4)

# Calculando o VIF
vif_resultados <- vif(modelo)
print(vif_resultados)

################## Modelagem ##########################

#5) Checando as limpezas de dados antes e depois

dim_antes <- dim(dados_originais)
dim_depois <- dim(dados_tratados_4)
# Criar uma tibble para exibir as dimensões
dimensoes <- tibble(
  "Checagem" = c("Antes", "Depois"),
  "Linhas" = c(dim_antes[1], dim_depois[1]),
  "Colunas" = c(dim_antes[2], dim_depois[2])
)
# Exibindo a tibble
print(dimensoes)


# Definindo a semente para reprodutibilidade***********************************
set.seed(15)

# Criando one-hot encoding para a coluna 'country_of_origin'*******************
dados_one_hot_country <- model.matrix(~ country_of_origin - 1, data = dados_tratados_4)

# Convertendo para dataframes e nomeando as colunas adequadamente
dados_one_hot_country <- as.data.frame(dados_one_hot_country)

# Removendo o prefixo 'country_of_origin' e pontos (.) dos nomes das colunas
colnames(dados_one_hot_country) <- gsub("country_of_origin", "", colnames(dados_one_hot_country))
colnames(dados_one_hot_country) <- gsub(" ", "", colnames(dados_one_hot_country))
colnames(dados_one_hot_country) <- gsub("\\.", "", colnames(dados_one_hot_country))

# Juntando com o dataframe original, excluindo a coluna 'country_of_origin' original
dados_tratados_4_one_hot <- cbind(dados_tratados_4, dados_one_hot_country)
dados_final <- dados_tratados_4_one_hot[, !colnames(dados_tratados_4_one_hot) %in% "country_of_origin"]
View(dados_final)

# Criando os frames vazios para comparacaoe***********************************

# Inicializar os dataframes como vazios
avaliacao_limitada <- data.frame(.metric = character(),
                                 .estimator = character(),
                                 .estimate = double(),
                                 Modelo = character())

comparacao <- data.frame(Observado = double(),
                         Predito = double(),
                         Modelo = character())

# Treino e Teste definicao****************************************************
split <- initial_split(dados_final, prop = 0.7)
treinamento <- training(split)
teste <- testing(split)

# preparando a receita********************************************************
receita <- recipe(total_cup_points ~ ., data = treinamento) %>%
  step_normalize(all_numeric(), -all_outcomes())
receita_prep <- prep(receita)
treinamento_proc <- bake(receita_prep, new_data = NULL)
teste_proc <- bake(receita_prep, new_data = teste)

#INICIO DOS MODELOS **********************************************************

# Modelo Ridge Simples********************************************
ridge_reg_simples <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Ajuste do modelo ao conjunto de treinamento
modelo_ridge_simples <- fit(ridge_reg_simples, total_cup_points ~ ., data = treinamento_proc)

# Previsões no conjunto de teste
previsoes_ridge_simples <- predict(modelo_ridge_simples, teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_ridge_limitada = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_ridge_simples %>%
                      dplyr::select(total_cup_points, predicao_ridge_limitada) %>%
                      mutate(Modelo = "Ridge Simples") %>%
                      rename(Observado = total_cup_points, Predito = predicao_ridge_limitada)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))

avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_ridge_simples %>%
                              metrics(truth = total_cup_points, estimate = predicao_ridge_limitada) %>%
                              mutate(Modelo = "Ridge Simples"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

# Exibindo o resultado
print(comparacao_por_modelo)
print(avaliacao_limitada)

# Modelo Ridge tunado *********************************************
ridge_reg <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
ridge_workflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(ridge_reg)

cv_folds <- vfold_cv(treinamento_proc, v = 10, strata = total_cup_points)

lambda_values_ridge_tun <- grid_regular(penalty(range = c(-5, -1)), levels = 20) %>%
  as_tibble() %>%
  mutate(penalty = 10^penalty)
ridge_resultados <- tune_grid(
  ridge_workflow,
  resamples = cv_folds,
  grid = lambda_values_ridge_tun,
  metrics = metric_set(rmse, rsq)
)

# Seleção do melhor modelo e ajuste final
melhor_lambda <- ridge_resultados %>% select_best("rmse")
modelo_final <- finalize_workflow(ridge_workflow, melhor_lambda)
modelo_ridge_tun_ajustado <- fit(modelo_final, data = treinamento_proc)

# Previsões e avaliação do Modelo Ridge Tunado
previsoes_ridge_tunadas <- predict(modelo_ridge_tun_ajustado, teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_ridge_tun_limitada = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_ridge_tunadas %>%
                      dplyr::select(total_cup_points, predicao_ridge_tun_limitada) %>%
                      mutate(Modelo = "Ridge Tunado") %>%
                      rename(Observado = total_cup_points, Predito = predicao_ridge_tun_limitada)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))

avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_ridge_tunadas %>%
                              metrics(truth = total_cup_points, estimate = predicao_ridge_tun_limitada) %>%
                              mutate(Modelo = "Ridge Tunado"))
# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

# Exibindo o resultado
print(comparacao_por_modelo)
print(avaliacao_limitada)

# Modelo Lasso Simples********************************************
lasso_reg_simples <- linear_reg(penalty = 0.2, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Ajuste do modelo ao conjunto de treinamento
modelo_lasso_simples <- fit(lasso_reg_simples, total_cup_points ~ ., data = treinamento_proc)

# Previsões no conjunto de teste
previsoes_simples_lasso <- predict(modelo_lasso_simples, teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_lasso_simples = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_simples_lasso %>%
                      dplyr::select(total_cup_points, predicao_lasso_simples) %>%
                      mutate(Modelo = "Lasso Simples") %>%
                      rename(Observado = total_cup_points, Predito = predicao_lasso_simples)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))

avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_simples_lasso %>%
                              metrics(truth = total_cup_points, estimate = predicao_lasso_simples) %>%
                              mutate(Modelo = "Lasso Simples"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

# Exibindo o resultado
print(comparacao_por_modelo)
print(avaliacao_limitada)

# Modelo Lasso tunado *********************************************
lasso_tun <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
lasso_workflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(lasso_tun)

cv_folds <- vfold_cv(treinamento_proc, v = 10, strata = total_cup_points)

lambda_values <- grid_regular(penalty(range = c(-5, -1)), levels = 20) %>%
  as_tibble() %>%
  mutate(penalty = 10^penalty)
lasso_resultados <- tune_grid(
  lasso_workflow,
  resamples = cv_folds,
  grid = lambda_values,
  metrics = metric_set(rmse, rsq)
)

# Seleção do melhor modelo e ajuste final
melhor_lambda <- lasso_resultados %>% select_best("rmse")
modelo_final <- finalize_workflow(lasso_workflow, melhor_lambda)
modelo_lasso_final_ajustado <- fit(modelo_final, data = treinamento_proc)

# Previsões e avaliação do Modelo Ridge Tunado
previsoes_lasso_tun <- predict(modelo_lasso_final_ajustado, teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_limitada = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_lasso_tun %>%
                      dplyr::select(total_cup_points, predicao_limitada) %>%
                      mutate(Modelo = "Lasso Tunado") %>%
                      rename(Observado = total_cup_points, Predito = predicao_limitada)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))



avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_lasso_tun %>%
                              metrics(truth = total_cup_points, estimate = predicao_limitada) %>%
                              mutate(Modelo = "lasso Tunado"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

# Exibindo o resultado
print(comparacao_por_modelo)
print(avaliacao_limitada)

# Modelo de Floresta Aleatória (Random Forest)********************************************

rf_s_reg <- rand_forest(mode = "regression") %>%
  set_engine("ranger", importance = "permutation")

# Ajuste do modelo ao conjunto de treinamento
modelo_rf_simples <- rf_s_reg %>% 
  fit(total_cup_points ~ ., data = treinamento_proc)

# Previsões no conjunto de teste
previsoes_rf_simples <- predict(modelo_rf_simples, teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_rf_simples = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_rf_simples %>%
                      dplyr::select(total_cup_points, predicao_rf_simples) %>%
                      mutate(Modelo = "Random Forest") %>%
                      rename(Observado = total_cup_points, Predito = predicao_rf_simples)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))

avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_rf_simples %>%
                              metrics(truth = total_cup_points, estimate = predicao_rf_simples) %>%
                              mutate(Modelo = "Random Forest"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

# Exibindo o resultado
print(comparacao_por_modelo)
print(avaliacao_limitada)

# Modelo de Floresta Aleatória Tunada ********************************************
rf_tunado <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

# Workflow incluindo a receita e a especificação do modelo
rf_workflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(rf_tunado)

# Criação da divisão de validação cruzada
cv_split <- vfold_cv(treinamento_proc, v = 10, strata = total_cup_points)

# Tunando os parâmetros
rf_resultados <- tune_grid(
  rf_workflow, 
  resamples = cv_split, 
  grid = 10, 
  metrics = metric_set(rmse, mae)
)

# Seleção do melhor conjunto de hiperparâmetros
best_params <- rf_resultados %>% 
  select_best("rmse")

# Atualizando a especificação do modelo com os melhores hiperparâmetros
rf_tunado_final <- finalize_model(rf_tunado, best_params)

# Criando um novo workflow com o modelo finalizado
final_workflow <- workflow() %>%
  add_recipe(receita) %>%
  add_model(rf_tunado_final)

# Ajustando o modelo finalizado ao conjunto de treinamento
final_fit <- final_workflow %>%
  fit(data = treinamento_proc)

# Previsões no conjunto de teste com o modelo tunado
previsoes_rf_tunado <- predict(final_fit, new_data = teste_proc) %>%
  bind_cols(teste_proc) %>%
  mutate(predicao_rf_tunado = pmax(pmin(.pred, 100), 0))

# Adicionando ao dataframe de comparação e avaliação
comparacao <- rbind(comparacao, previsoes_rf_tunado %>%
                      dplyr::select(total_cup_points, predicao_rf_tunado) %>%
                      mutate(Modelo = "Random Forest Tunado") %>%
                      rename(Observado = total_cup_points, Predito = predicao_rf_tunado)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))

avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_rf_tunado %>%
                              metrics(truth = total_cup_points, estimate = predicao_rf_tunado) %>%
                              mutate(Modelo = "Random Forest Tunado"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

print(comparacao_por_modelo)
print(avaliacao_limitada)


# Rede Neural ***************************************************************
X_trn <- treinamento_proc %>% 
  dplyr::select(-total_cup_points) %>% 
  as.matrix()

X_tst <- teste_proc %>% 
  dplyr::select(-total_cup_points) %>% 
  as.matrix()

# Normalização dos dados
X_trn <- scale(X_trn)
X_tst <- scale(X_tst, center = attr(X_trn, "scaled:center"), scale = attr(X_trn, "scaled:scale"))

# Construção da Rede Neural
net <- keras_model_sequential() %>% 
  layer_dense(units = 32, activation = "relu", input_shape = ncol(X_trn)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 8, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear")

net <- compile(net, loss = "mse", optimizer = "adam", metrics = "mse")
summary(net)

# Treinamento da Rede Neural
history <- fit(net, X_trn, treinamento_proc$total_cup_points, batch_size = 16, epochs = 20, validation_split = 0.2)

# Previsões e Avaliação
y_hat_net <- predict(net, X_tst)
y_hat_net <- pmax(pmin(y_hat_net, 100), 0) # Limitando as previsões a 0-100

# Cálculo do RMSE
RMSE_net <- sqrt(mean((y_hat_net - teste_proc$total_cup_points)^2))

# Criando um dataframe temporário para as previsões da Rede Neural
previsoes_rede_neural <- data.frame(total_cup_points = teste_proc$total_cup_points, 
                                    predicao_rede_neural = as.numeric(y_hat_net))

# Adicionando ao dataframe de comparação
comparacao <- rbind(comparacao, previsoes_rede_neural %>%
                      dplyr::select(total_cup_points, predicao_rede_neural) %>%
                      mutate(Modelo = "Rede Neural") %>%
                      rename(Observado = total_cup_points, Predito = predicao_rede_neural)%>%
                      mutate(PercentualAcerto = (1 - abs(Observado - Predito) / Observado) * 100,
                             PercentualAcerto = sprintf("%.2f%%", PercentualAcerto)))


# Adicionando ao dataframe de avaliação
avaliacao_limitada <- rbind(avaliacao_limitada, previsoes_rede_neural %>%
                              metrics(truth = total_cup_points, estimate = predicao_rede_neural) %>%
                              mutate(Modelo = "Rede Neural"))

# Exibindo resultados
comparacao_por_modelo <- comparacao %>%
  group_by(Modelo) %>%
  arrange(desc(PercentualAcerto))%>%
  slice(1:3)

View(comparacao_por_modelo)
View(avaliacao_limitada)









