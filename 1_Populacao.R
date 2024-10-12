library(tidyverse)
library(readxl)

df <- read_excel('C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Retornometro\\DADOS\\1_POPULACAO_IBGE_Tabela-6579.xlsx',
                      skip = 3)
colnames(df) <- c('municipio', 'populacao')

df <- df[-c(1), ]
df

df <- df %>%
  # Criar a coluna 'estado' extraindo o conteúdo entre parênteses
  mutate(estado = str_extract(municipio, '\\(([^)]+)\\)')) %>%
  # Remover os parênteses da coluna 'municipio'
  mutate(municipio = str_replace(municipio, '\\s*\\([^\\)]+\\)', '')) %>%
  # Remover os parênteses da coluna 'estado'
  mutate(estado = str_replace_all(estado, '[\\(\\)]', ''))
df <- df %>% select(municipio, estado, populacao)
df

## O brasil possui 5571 municipio
summary(df) # aqui, 5571
