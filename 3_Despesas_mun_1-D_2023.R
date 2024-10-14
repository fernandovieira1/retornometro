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
                  'tipo_despesa', 
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
# df[df$cidade=='Ibiraiaras',] %>% select(conta) %>% table()

#### 0.4 Reorganizar df ####

# Separar a coluna 'conta' em 'cod_conta' e 'conta', tratando casos com mais ou menos de dois pedaços
df <- df %>%
  separate(conta, into = c('cod_conta', 'conta'), sep = ' - ', extra = 'merge', fill = 'right')
df

# Substituir 'Total Geral da Despesa' na coluna 'cod_conta' e preencher 'NA' na coluna 'conta'
df <- df %>%
  mutate(
    cod_conta = ifelse(cod_conta == 'Total Geral da Despesa', '9.9.99.99.99', cod_conta),
    conta = ifelse(is.na(conta) & cod_conta == '9.9.99.99.99', 'TOTAL DA DESPESA', conta)
  )

# Exibir o dataframe modificado
df

## Conhecer valores
# View(df[df$cidade=='Ibiraiaras',])
# library(openxlsx)
# write.xlsx(df[df$cidade=='Ibiraiaras',], 'cidade_teste.xlsx')

## Deixar apenas valores de Despesas Empenhadas
df <- df %>%
  filter(tipo_despesa == 'Despesas Empenhadas')
df

## Remover colunas 'tipo_despesa' e 'id_conta'
df <- df %>% select(c('cidade', 'cod_ibge', 'uf', 'populacao', 'cod_conta', 'conta', 'valor'))
df

#### 0.5 Descritiva ####
summary(df)

#### 0.6 Valores com problemas ####
## Testando (cidades, valores etc)
df

# Valores negativos
df %>% filter(valor < 0) #%>% View() # Aqui apareceu 52 linhas negativas. POR QUE???

# Valores 0
df %>% filter(valor == 0) # Aqui apareceu 30 linhas com valor 0. POR QUE???

# Valores NA
df %>% filter(is.na(valor) & conta != 'TOTAL DA DESPESA') # Aqui apareceu 109 linhas com valor NA. POR QUE???
df %>% filter(is.na(valor) & conta != 'TOTAL DA DESPESA') %>% summarise(total_cidades = n_distinct(cidade)) # 36 cidades com NA
# View(df2 %>% filter(is.na(valor)))

# (!!!) Investigar motivos dessas cidades com NA e valores negativos ou 0

## O brasil possui 5571 municipio
df %>% summarise(total_cidades = n_distinct(cidade)) # aqui, 5284
df %>% summarise(total_cidades = n_distinct(cidade)) # aqui, 5284

## - Falta definir os criterios das contas que virarao variaveis: despesas correntes, investimentos,
# se havera deducao de algo, se sera quebrado em mais contas etc.
# - Falta remover valores negativos, NA e 0
summary(df)
df[df$cidade=='Ibiraiaras',] %>% View()
