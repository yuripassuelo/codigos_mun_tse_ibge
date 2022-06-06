# Relação de Codigos de Municípios TSE e Municípios IBGE

Processo de construção de uma relação de códigos de municípios disponibilizados nos dados do TSE, com os códigos de municípios utilizados nas tabelas do IBGE;

A relação de códigos de municípios permite o cruzamento de informações relacionados a resultados eleitorais, com diversos dados socioeconômicos disponibilizados pelo IBGE e diversas outras bases, a relação está disponível em:
1. csv ;
2. json;
3. xlsx.

O exemplo abaixo mostra alguns exemplos de acesso a tabela de relação dos códigos.

```R
> # Lendo por meio do arquivo csv
>
> url_csv     <- 
+   "https://raw.githubusercontent.com/yuripassuelo/codigos_mun_tse_ibge/master/relacao_mun_tse_ibge.csv"
> 
> depara_csv  <- read.csv( url_csv, sep = ",", header = TRUE )
>
> head( depara_csv )
  X UF SG_UF SG_UE cd_mun_ibge             nm_mun_ibge cd_mun_tse              nm_mun_tse
1 1 11    RO    BR     1100015   ALTA FLORESTA D OESTE        310   Alta Floresta D'Oeste
2 2 11    RO    BR     1100379 ALTO ALEGRE DOS PARECIS        736 Alto Alegre dos Parecis
3 3 11    RO    BR     1100403            ALTO PARAÍSO        639            Alto Paraíso
4 4 11    RO    BR     1100346       ALVORADA DO OESTE        337        Alvorada D'Oeste
5 5 11    RO    BR     1100023               ARIQUEMES         78               Ariquemes
6 6 11    RO    BR     1100452                 BURITIS        779                 Buritis

```

```R
> # Lendo por meio do arquivo json
> library( jsonlite )
>
> temp_ <- tempfile()
> 
> url_json <-
+   "https://raw.githubusercontent.com/yuripassuelo/codigos_mun_tse_ibge/master/relacao_mun_tse_ibge.json"
>
> download.file( url = url_json,
+                destfile = temp_,
+                mode = "wb")
>
> depara_json <- fromJSON( readLines( temp_  ))
>
> head( depara_json )
  UF SG_UF SG_UE cd_mun_ibge             nm_mun_ibge cd_mun_tse              nm_mun_tse
1 11    RO    BR     1100015   ALTA FLORESTA D OESTE        310   Alta Floresta D'Oeste
2 11    RO    BR     1100379 ALTO ALEGRE DOS PARECIS        736 Alto Alegre dos Parecis
3 11    RO    BR     1100403            ALTO PARAÍSO        639            Alto Paraíso
4 11    RO    BR     1100346       ALVORADA DO OESTE        337        Alvorada D'Oeste
5 11    RO    BR     1100023               ARIQUEMES         78               Ariquemes
6 11    RO    BR     1100452                 BURITIS        779                 Buritis
```

Os scripts da pasta `src` mostram o passo a passo para a geração da relação de códigos com, sendo os scripts principais:
- 00.setup.R;
- 01.baixa_dados.R;
- 02.gera_depara.R.
  
O script `03.cria_mapas.R` mostra um exemplo com o cruzamento de dados georeferênciados com os dados de TSE para a construção de mapas.

## Instruções para replicação

1. Assegurar que os pacotes do script `./src/00.setup.R` estão instalados;
2. Ao rodar o script `01.baixa_dados.R` e `./src/02.gera_depara.R` se assegurar que esteja utilizando um computador com memória RAM suficiente para carregar mesmo que de forma temporaria os dados de votação do TSE, que contém em torno de 1GB;

## Informações extras de versão do R e dos pacotes

```R

R version 4.1.0 (2021-05-18)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252    LC_MONETARY=Portuguese_Brazil.1252
[4] LC_NUMERIC=C                       LC_TIME=Portuguese_Brazil.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] geobr_1.6.4       writexl_1.4.0     jsonlite_1.7.2    readxl_1.3.1      archive_1.1.5     rvest_1.0.2       RCurl_1.98-1.5   
 [8] fuzzyjoin_0.1.6   data.table_1.14.2 forcats_0.5.1     stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4       readr_2.1.0      
[15] tidyr_1.1.4       tibble_3.1.6      ggplot2_3.3.5     tidyverse_1.3.1  

...

```





