library(tidyverse)
library(readr)

#### 0. Carregar df ####
df <- read_csv2('C:\\Users\\ferna\\OneDrive\\5. Trabalho\\Expediente\\Ativos\\Consultoria\\Novo Retornometro\\DADOS\\finbra.csv',
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
                  'tipo_receita', 
                  'conta', 
                  'id_conta', 
                  'valor')

head(df)
tail(df)

# Deixar apenas o nome da cidade
df$cidade <- gsub("Prefeitura Municipal de ", "", df$cidade)
df$cidade <- gsub(" - [A-Z]{2}$", "", df$cidade)

# Mudar tipo coluna valor
df$valor <- as.integer(round(df$valor, 0)) 

str(df)
head(df)
tail(df)
df <- as_tibble(df)

#### 0.3 Conhecer cada coluna ####
table(df$tipo_receita)

# Remover coluna 'id_conta' 
# (verificar em um segundo momento se pode impactar -- acho que não!)
df <- df %>% select(c('cidade', 'cod_ibge', 'uf', 'populacao', 'tipo_receita', 'conta', 'valor'))
df

# Filtrar apenas as linhas onde a 'uf' é igual a 'SP'
df <- df[df$uf == 'SP', ]


str(df)
head(df)
tail(df)

#### 0.4 Descritiva ####
summary(df)

# Verificando se as deduções estão negativas
df[df$tipo_receita == 'Deduções - FUNDEB', ] # está positivo
df[df$tipo_receita == 'Deduções - Transferências Constitucionais', ] # está negativo
df[df$tipo_receita == 'Outras Deduções da Receita', ] # está positivo

View(df[df$cidade == 'Borá', ])

# Falta definir
#   * Se deve-se filtrar pelo código mais alto da conta (receitas correntes e de capital)
#   * Se as deduções -- tanto negativas como positivas -- devem ser sutraídas das receitas totais
