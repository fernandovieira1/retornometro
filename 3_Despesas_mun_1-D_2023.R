### DESPESAS ###
# - Fonte(s):Finbra/STN (Anexo 1-D) / Despesas Orçamentárias (mais contabil)
# - Descrição: Arrecadação municipal Direta e Indireta
# * Exercicio: 2023
# Baixei duas versoes do arquivo para confirmas os problemas com valores negativos ou NA (seção 0.6)

#### 0. Carregar ambiente ####
library(tidyverse)
library(readr)
df <- read_csv2('C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Retornometro\\DADOS\\2_RECEITA_Finbra_1-c_2023.csv',
                locale = locale(encoding = 'windows-1252'),
                skip = 3)
df

#### 0.1 Visão geral ####
str(df)
length(df)
nrow(df)
names(df)
head(df)