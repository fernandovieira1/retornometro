### RECEITAS ###
# - Fonte(s):Finbra/STN (NEXO 1-C)
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
df$cidade <- gsub('Prefeitura Municipal de ', '', df$cidade)
df$cidade <- gsub(' - [A-Z]{2}$', '', df$cidade)

# Mudar tipo coluna valor
df$valor <- as.integer(round(df$valor, 0)) 

str(df)
head(df)
tail(df)
df <- as_tibble(df)
df

#### 0.3 Conhecer cada coluna ####
table(df$tipo_receita) # esta sera a coluna base para criar a variavel
# table(df$conta)

#### 0.4 Reorganizar df

# Remover coluna 'id_conta' 
# (verificar em um segundo momento se pode impactar -- acho que não!)
df <- df %>% select(c('cidade', 'cod_ibge', 'uf', 'populacao', 'tipo_receita', 'conta', 'valor'))
df

# Supondo que seu tibble seja chamado 'df'
df %>%
  separate(conta, into = c('codigo', 'descricao'), sep = ' - ', extra = 'merge', fill = 'right') %>%
  separate(codigo, into = c('nivel_1', 'nivel_2', 'nivel_3', 'nivel_4', 'nivel_5', 'nivel_6', 'nivel_7'), sep = '\\.', fill = 'right') -> df2

# Verificar a nova estrutura
head(df2)

# View(table(df2$descricao))
# library(openxlsx)
# write.xlsx(df2, "dados_estruturados.xlsx")

#### 0.4 Descritiva ####
summary(df)

# Verificando se as deduções estão negativas
df[df$tipo_receita == 'Deduções - FUNDEB', ] # está positivo
df[df$tipo_receita == 'Deduções - Transferências Constitucionais', ] # está negativo
df[df$tipo_receita == 'Outras Deduções da Receita', ] # está positivo

# View(df[df$cidade == 'Borá', ])

#### 0.5 Filtrar Receitas ####
## Apenas as receitas disponíveis para o município
# Critérios especificados abaixo

head(df2)
# View(table(df2$descricao))

## Retirar valores negativos de 'valor'
df2$valor <- abs(df2$valor)
any(df2$valor < 0) # Aqui não apareceu mais nenhum negativo

## Versão sintética
# * Considerando apenas a receita líquida do município liquida (a que sobra para ele de fato gastar em 
# custeio e investimentos), devem-se considerar apenas as receitas orçamentárias "externas" (exceto 
# intra-orçamentárias) e subtrair as deduções obrigatórias (como o FUNDEB, Transferências Constitucionais 
# e outras deduções)
# - Deduções - FUNDEB: São deduções obrigatórias que representam uma parcela das receitas de impostos
# arrecadados pelos estados e municípios destinada ao Fundo de Manutenção e Desenvolvimento da
# Educação Básica e de Valorização dos Profissionais da Educação (FUNDEB). Esses recursos são
# redistribuídos para financiar a educação básica.
# - Deduções - Transferências Constitucionais: Representam a parcela da receita arrecadada que,
# por determinação da Constituição Federal, deve ser transferida de um ente federativo para outro,
# como, por exemplo, a cota-parte do ICMS repassada pelos estados aos municípios.
# - Outras Deduções da Receita: Incluem outras deduções legais e específicas, como contribuições para
# regimes previdenciários e outras obrigações que, por lei, devem ser descontadas da receita bruta
# arrecadada antes de ser calculada a receita líquida disponível para o ente público.
df2 <- df2 %>% filter(nivel_1 == 'RECEITAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)')
df2

## Remover colunas sobressalentes
df2 <- df2 %>% select(c('cidade', 'cod_ibge', 'uf', 'populacao', 'tipo_receita', 'valor'))
df2

## Inserir 'Receita Liquida do Municipio' na coluna 'tipo_receita'
# Calcular a Receita Líquida por município e adicionar como nova linha
df2 <- df2 %>%
  group_by(cidade, cod_ibge, uf, populacao) %>%  # Incluímos a coluna 'populacao' no agrupamento
  summarise(
    receita_liquida = sum(valor[tipo_receita == 'Receitas Brutas Realizadas']) - 
      sum(valor[tipo_receita %in% c('Deduções - FUNDEB', 'Deduções - Transferências Constitucionais', 'Outras Deduções da Receita')]),
    .groups = 'drop'
  ) %>%
  # Adicionar a Receita Líquida como uma nova linha no dataframe original
  mutate(tipo_receita = 'Receita Liquida do Municipio', valor = receita_liquida) %>%
  select(-receita_liquida) %>%
  bind_rows(df2, .) %>%
  arrange(cidade, `tipo_receita`)  # Corrigido também para manter a consistência no nome da coluna 'tipo_receita'
df2

#### 0.6 Valores com problemas ####
## Testando (cidades, valores etc)
summary(df2)

# Valores negativos
df2 %>% filter(valor < 0) # Aqui apareceu negativo (Dois Riachos-AL, e Ipu-CE)

# Valores 0
df2 %>% filter(valor == 0) # deduções de receita -- ok; RL do mun: Minas Novas-MG e Silvianópolis-MG

# Valores NA
df2 %>% filter(is.na(valor))
df2 %>% filter(is.na(valor)) %>% summarise(total_cidades = n_distinct(cidade)) # 68 cidades com NA
# View(df2 %>% filter(is.na(valor)))

# (!!!) Investigar motivos dessas cidades com NA e valores negativos ou 0

## O brasil possui 5571 municipio
df %>% summarise(total_cidades = n_distinct(cidade)) # aqui, 5215
df2 %>% summarise(total_cidades = n_distinct(cidade)) # aqui, 5215

