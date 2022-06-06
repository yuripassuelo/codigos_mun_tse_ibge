# Relação de Codigos de Municípios TSE e Municípios IBGE

Processo de construção de uma relação de códigos de municípios disponibilizados nos dados do TSE, com os códigos de municípios utilizados nas tabelas do IBGE;

A relação de códigos de municípios permite o cruzamento de informações relacionados a resultados eleitorais, com diversos dados socioeconômicos disponibilizados pelo IBGE e diversas outras bases, a relação está disponível em:
1. csv ;
2. json;
3. xlsx.

O exemplo abaixo mostra alguns exemplos de acesso a tabela de relação dos códigos.

```R

> library( data.table )
> library( readxl )
> library( jsonlite )
>
> url    <- ""
> 
> # Lendo arquivo `csv`
> depara_csv  <- fread( url )
>
> # Lendo arquivo `xlsx`
> depara_xlsx <- read_xlsx( url )
>
> # Lendo arquivo `xlsx`
> depara_json <-
>
> head( depara_csv )

        V1 UF SG_UF SG_UE cd_mun_ibge             nm_mun_ibge cd_mun_tse              nm_mun_tse
   1:    1 11    RO    BR     1100015   ALTA FLORESTA D OESTE        310   Alta Floresta D'Oeste
   2:    2 11    RO    BR     1100379 ALTO ALEGRE DOS PARECIS        736 Alto Alegre dos Parecis
   3:    3 11    RO    BR     1100403            ALTO PARAÍSO        639            Alto Paraíso
   4:    4 11    RO    BR     1100346       ALVORADA DO OESTE        337        Alvorada D'Oeste
   5:    5 11    RO    BR     1100023               ARIQUEMES         78               Ariquemes
  ---                                                                                           
5566: 5566 52    GO    BR     5222005              VIANÓPOLIS      96415              Vianópolis
5567: 5567 52    GO    BR     5222054          VICENTINÓPOLIS      96571          Vicentinópolis
5568: 5568 52    GO    BR     5222203                VILA BOA      93289                Vila Boa
5569: 5569 52    GO    BR     5222302           VILA PROPÍCIO      93505           Vila Propício
5570: 5570 53    DF    BR     5300108                BRASÍLIA      97012                Brasília

```

Os scripts da pasta `src` mostram o passo a passo para a geração da relação de códigos com, sendo os scripts principais:
- 00.setup.R;
- 01.baixa_dados.R;
- 02.gera_depara.R.
  
O script `03.cria_mapas.R` mostra um exemplo com o cruzamento de dados georeferênciados com os dados de TSE para a construção de mapas.




