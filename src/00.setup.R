
# Criaçao da relação de Codigos de Municipios TSE e IBGE
# Autor: Yuri de Macedo Passuelo
# Data : 04/06/2022

# Setup

rm( list = ls() )

packages <- c( "tidyverse", "data.table", "fuzzyjoin", "RCurl", "rvest", 
               "archive", "readxl", "jsonlite", "writexl", "geobr" )

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply( new.packages, install.packages )

# Carrega Bibliotecas

library( tidyverse )
library( data.table )
library( fuzzyjoin )
library( RCurl )
library( rvest )
library( archive )
library( readxl )
#library( rjson )
library( jsonlite )
library( writexl )
library( geobr )
