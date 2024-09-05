library(tidyverse)
library(readxl)

df <- read_excel('C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Novo Retornometro\\github\\retornometro\\DADOS\\Tabela 6579 POPULACAO.xlsx',
                      skip = 3)
colnames(df) <- c('municipio', 'populacao')

df <- df[-c(1), ]
df

summary(df)
