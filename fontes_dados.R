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

## Dados do estado de SP
sp <- read_municipality(code_muni='SP', year=2022)
str(sp)
summary(sp)
# Apenas informações geográficas básicas

read_health_facilities()

## MUNIC ####
#https://sidra.ibge.gov.br/pesquisa/munic/tabelas


