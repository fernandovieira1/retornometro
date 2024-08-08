#### 0. CONFIGURAR O AMBIENTE ####
# 0.1 Carregar pacotes necessários ####
library(readxl)
library(tidyverse)
library(googledrive)
library(kableExtra)
library(ggcorrplot)
library(hrbrthemes)

library(sidrar)
library(geobr)

## 0.2 Notas ####
# Talvez usar dados espaciais depois: https://github.com/ipeaGIT/geobr

## 1. Dados IBGE - demografia, saúde e educação  ####

## 1.1 SIDRAR ####
# https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html
# search_sidra(c("gini"))
# https://sidra.ibge.gov.br/home/pimpfbr/brasil

# (=) Apenas dados econômicos

## 1.2 GEOBR ####
# https://github.com/ipeaGIT/geobr

## Dados Geográficos - Municípios SP ####
# Período: 2022
geo_sp <- read_municipality(code_muni='SP', year=2022)
geo_sp

str(geo_sp)
summary(geo_sp)
# - Informações geográficas básicas.
# - COLUNAS úteis: \geom (coordenadas dos municípios)

## Dados Saúde - Municípios SP ####
# Período: Mar/2023
saude_sp <- read_health_facilities() %>% 
  filter(abbrev_state=='SP')
saude_sp

str(saude_sp)
summary(saude_sp)
# - Dados geograficos e estrutura (em centros de especialidades - cirugia, exames) 
# de estabelecimentos de saúde.
# - COLUNAS úteis: as que começam com \'st' e \'co' aparentemente indicam se há ou não (são binárias)
#algum tipo de estrutura física ou humana no estabelecimento de saúde.

## Dados Educação - Municípios SP ####
# Período: 2020
educ_sp <- read_schools() %>% 
  filter(abbrev_state=='SP')
educ_sp

str(educ_sp)
summary(educ_sp)

# - Dados geograficos e estrutura das escolas.
# - COLUNAS úteis: \education_level (tipo da escola: infantil, adultos, médio, 
# elementar, mista etc), \government_level (estadual, federal, municipal 
# ou privada)


## 1.3 MUNIC ####
#https://sidra.ibge.gov.br/pesquisa/munic/tabelas


