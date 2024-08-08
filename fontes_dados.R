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
geo_sp <- read_municipality(code_muni='SP', year=2022)
geo_sp

str(geo_sp)
summary(geo_sp)
#informações geográficas básicas.

## Dados Saúde - Municípios SP ####
saude_sp <- read_health_facilities() %>% 
  filter(abbrev_state=='SP')
saude_sp

str(saude_sp)
summary(saude_sp)
#dados geograficos e estrutura (em centros de especialidades - cirugia, exames) 
# de estabelecimentos de saúde.

## 1.3 MUNIC ####
#https://sidra.ibge.gov.br/pesquisa/munic/tabelas


