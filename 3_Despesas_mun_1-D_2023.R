### DESPESAS ###
# - Fonte(s):Finbra/STN (Anexo 1-D) / Despesas Orçamentárias (contabil)
# - Descrição: Arrecadação municipal Direta e Indireta
# * Exercicio: 2023
# O anexo 1-E (finalistico) tambem foi objete de analise

#### 0. Carregar ambiente ####
library(tidyverse)
library(readr)
df <- read_csv2('C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Retornometro\\DADOS\\3_DESPESA_Finbra_1-d_2021.csv',
                locale = locale(encoding = 'windows-1252'),
                skip = 3)
df

#### 0.1 Visão geral ####
str(df)
length(df)
nrow(df)
names(df)
head(df)

#### 0.2 Organizar colunas ####
colnames(df) <- c('cidade', 
                  'cod_ibge', 
                  'uf', 
                  'populacao', 
                  'coluna', 
                  'conta', 
                  'id_conta', 
                  'valor')

head(df)
tail(df)

# Deixar apenas o nome da cidade
df$cidade <- gsub('Prefeitura Municipal de ', '', df$cidade)
df$cidade <- gsub(' - [A-Z]{2}$', '', df$cidade)
df

# Mudar tipo coluna valor
df$valor <- as.integer(round(df$valor, 0)) 

str(df)
head(df)
tail(df)
df <- as_tibble(df)
df

#### 0.3 Conhecer cada coluna ####
table(df$coluna)
df[df$cidade=='Ibiraiaras',] %>% select(conta) %>% table()

#### 0.4 Reorganizar df ####

# Separar a coluna 'conta' em 'cod_conta' e 'conta', tratando casos com mais ou menos de dois pedaços
df <- df %>%
  separate(conta, into = c('cod_conta', 'conta'), sep = ' - ', extra = 'merge', fill = 'right')
df
