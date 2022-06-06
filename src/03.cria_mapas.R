
# Criaçao da relação de Codigos de Municipios TSE e IBGE
# Autor: Yuri de Macedo Passuelo
# Data : 04/06/2022

# Load Setup

source( "./src/00.setup.R")

# Le Arquivo TSE

arquivo_2018 <-
  fread( "./data/TSE/votacao_secao_2018_BR.csv" ) 

# Le De para fito

depara_tse_ibge <- 
  fread( "./relacao_mun_tse_ibge.csv" )

# Le dados geo referenciados

base_muni <-
  read_municipality( code_mun = "all", year = 2018 )

# Processa dados Municipio

base_agg_mun <-
  arquivo_2018 %>%
  group_by( NR_TURNO, CD_MUNICIPIO, SG_UF, NM_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL )%>%
  summarise( QT_VOTOS = sum( QT_VOTOS, na.rm = TRUE ))


base_2_turno <-
  filter( base_agg_mun, NR_TURNO == 2 & SG_UF != "ZZ" ) %>%
  select( -c( NM_VOTAVEL )) %>%
  left_join( ., 
             select( depara_tse_ibge, cd_mun_tse, cd_mun_ibge ),
             by = c( "CD_MUNICIPIO" = "cd_mun_tse" ))%>%
  group_by( CD_MUNICIPIO, NM_MUNICIPIO )%>%
  mutate( PC_VOTOS = QT_VOTOS / sum( QT_VOTOS, na.rm = TRUE ) )


eleicoes_2018_13 <-
  left_join( base_muni,
           base_2_turno,
           by = c( "code_muni" = "cd_mun_ibge" ) )%>%
  filter( NR_VOTAVEL == 13 )%>%
  ggplot( )+
  geom_sf( mapping = aes( fill = PC_VOTOS ), lwd = 0, color = NA )+
  scale_fill_gradient( low = "white", high = "red")+
  labs( title = "Votação Haddad 2o Turno" )
  
eleicoes_2018_17 <-
  left_join( base_muni,
           base_2_turno,
           by = c( "code_muni" = "cd_mun_ibge" ) )%>%
  filter( NR_VOTAVEL == 17 )%>%
  ggplot( )+
  geom_sf( mapping = aes( fill = PC_VOTOS ), lwd = 0, color = NA )+
  scale_fill_gradient( low = "white", high = "blue")+
  labs( title = "Votação Bolsonaro 2o Turno" )

# Salvando

ggsave( "./png/exemplo_1.png", 
        plot   = eleicoes_2018_13,
        width  = 15,
        height = 15,
        units  = "cm")

ggsave( "./png/exemplo_2.png", 
        plot   = eleicoes_2018_17,
        width  = 15,
        height = 15,
        units  = "cm" )
  