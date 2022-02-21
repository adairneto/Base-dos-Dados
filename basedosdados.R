# install.packages("basedosdados")
library("basedosdados")
library("tidyverse")
# Defina o seu projeto no Google Cloud
set_billing_id("<YOUR_PROJECT_ID>")
# Para carregar o dado direto no R
query <- bdplyr("br_ibge_ipca.mes_brasil")
df <- bd_collect(query)

# Visualização

df %>%
  group_by(ano,mes) %>%
  filter(!is.na(variacao_mensal)) %>%
  ggplot() +
    geom_point(mapping = aes(x = mes, y = variacao_mensal)) +
    facet_wrap(~ ano, nrow = 3) +
    ggtitle("Inflação: Variação Mensal (1980-2021)")

df %>%
  group_by(ano) %>%
  filter(!is.na(variacao_mensal)) %>%
  mutate(
    max = max(variacao_mensal)
  ) %>%
  ggplot() +
    geom_point(mapping = aes(x = ano, y = max), colour = "#D55E00") +
    xlab("Ano") +
    ylab("Variação Mensal Máxima") +
    ggtitle("Inflação: Maior Inflação Mensal por Ano (1980-2021)")

df %>%
  group_by(ano) %>%
  filter(!is.na(variacao_mensal)) %>%
  mutate(
    min = min(variacao_mensal)
  ) %>%
  ggplot() +
    geom_point(mapping = aes(x = ano, y = min), colour = "#56B4E9") +
    xlab("Ano") +
    ylab("Variação Mensal Mínima") +
    ggtitle("Inflação: Menor Inflação Mensal por Ano (1980-2021)")

df %>%
  group_by(ano) %>%
  filter(!is.na(variacao_mensal)) %>%
  summarize(count = n(),
            inflacao_mensal_media = mean(variacao_mensal, na.rm = TRUE),
            inflacao_mensal_desv = sd(variacao_mensal, na.rm = TRUE)) %>%
  ggplot() +
    geom_point(mapping = aes(x = ano, y = inflacao_mensal_media), color = "#009E73") +
    xlab("Ano") +
    ylab("Média da Inflação Mensal") +
    ggtitle("Inflação: Média da Inflação Mensal por Ano (1980-2021)")

df %>%
  group_by(ano) %>%
  filter(!is.na(variacao_mensal)) %>%
  summarize(count = n(),
            inflacao_mensal_media = mean(variacao_mensal, na.rm = TRUE),
            inflacao_mensal_desv = sd(variacao_mensal, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(mapping = aes(x = ano, y = inflacao_mensal_desv), color = "#CC79A7") +
    xlab("Ano") +
    ylab("Média da Inflação Mensal") +
    ggtitle("Inflação: Desvio Padrão da Inflação Mensal por Ano (1980-2021)")

# Alguns dados importantes

df %>%
  group_by(ano) %>%
  filter(!is.na(variacao_mensal)) %>%
  summarize(
    mean = mean(variacao_mensal),
    median = median(variacao_mensal),
    sd = sd(variacao_mensal),
    min = min(variacao_mensal),
    max = max(variacao_mensal),
    count = n(),
  ) %>%
  View()
