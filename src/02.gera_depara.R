
# Criaçao da relação de Codigos de Municipios TSE e IBGE
# Autor: Yuri de Macedo Passuelo
# Data : 04/06/2022

# Load Setup

source( "./src/00.setup.R")

# Funcao Sigla UF

cria_sigla_uf <- function( .uf ){
  
  str_splited <- str_split( .uf, " ")
  
  if( length( str_splited[[1]] ) == 1 ){
    if( .uf == "RORAIMA" ){
      return( "RR" )
    }
    if( .uf == "PARAÍBA" ){
      return( "PB" )
    }
    if( .uf == "PARANÁ" ){
      return( "PR" )
    }
    if( .uf == "AMAPÁ" ){
      return( "AP" )
    }
    else{
      return( str_sub( str_splited[[1]], 1, 2 ) )
    }
  } else{
    
    if( .uf == "MATO GROSSO" ){
      return( "MT" )
    }
    else{
      return( paste0( str_sub( str_splited[[1]][ 1],1,1), 
                      str_sub( str_splited[[1]][ length(str_splited[[1]]) ],1,1) ) )
    }
  } } 


# Carrega dados IBGE

mun_cods_ibge <-
  read_excel( "./data/IBGE/RELATORIO_DTB_BRASIL_MUNICIPIO.xls" )


mun_cods_ibge <-
  mutate( mun_cods_ibge,
          SG_UF = map_chr( str_to_upper( Nome_UF ) ,cria_sigla_uf ) ) %>%
  select( UF, Nome_UF, SG_UF, `Código Município Completo`, `Nome_Município` )%>%
  rename( nome_mun     = `Nome_Município`,
          cod_mun_comp = `Código Município Completo` )%>%
  mutate( nome_mun_corr = str_to_upper( nome_mun ),
          
          nome_mun_corr = str_replace_all( nome_mun_corr, "D'", "D "),
          
          nome_mun_corr = case_when(
            nome_mun_corr == "AÇU"                    & SG_UF == "RN" ~ "ASSÚ",
            nome_mun_corr == "ALVORADA D OESTE"       & SG_UF == "RO" ~ "ALVORADA DO OESTE",
            nome_mun_corr == "ANHANGUERA"             & SG_UF == "GO" ~ "ANHANGÜERA",
            nome_mun_corr == "ANTÔNIO OLINTO"         & SG_UF == "PR" ~ "ANTONIO OLINTO",
            nome_mun_corr == "ATÍLIO VIVACQUA"        & SG_UF == "ES" ~ "ATÍLIO VIVÁCQUA",
            nome_mun_corr == "CAÉM"                   & SG_UF == "BA" ~ "CAEM",
            nome_mun_corr == "CAMACAN"                & SG_UF == "BA" ~ "CAMACÃ",
            nome_mun_corr == "ELDORADO DO CARAJÁS"    & SG_UF == "PA" ~ "ELDORADO DOS CARAJÁS",
            nome_mun_corr == "ESPIGÃO D OESTE"        & SG_UF == "RO" ~ "ESPIGÃO DO OESTE",
            nome_mun_corr == "GRACHO CARDOSO"         & SG_UF == "SE" ~ "GRACCHO CARDOSO",
            nome_mun_corr == "GRÃO PARÁ"              & SG_UF == "SC" ~ "GRÃO-PARÁ",
            nome_mun_corr == "LUÍS CORREIA"           & SG_UF == "PI" ~ "LUIS CORREIA",
            nome_mun_corr == "OLHO D ÁGUA"            & SG_UF == "PB" ~ "OLHO D ÁGUA",
            nome_mun_corr == "QUIJINGUE"              & SG_UF == "BA" ~ "QUINJINGUE",
            nome_mun_corr == "SANT'ANA DO LIVRAMENTO" & SG_UF == "RS" ~ "SANT ANA DO LIVRAMENTO",
            nome_mun_corr == "SANTA IZABEL DO PARÁ"   & SG_UF == "PA" ~ "SANTA ISABEL DO PARÁ",
            nome_mun_corr == "SANTO ANTÔNIO DO CAIUÁ" & SG_UF == "PR" ~ "SANTO ANTONIO DO CAIUÁ",
            nome_mun_corr == "SANTO ESTÊVÃO"          & SG_UF == "BA" ~ "SANTO ESTEVÃO",
            nome_mun_corr == "SÃO LUIZ DO PARAITINGA" & SG_UF == "SP" ~ "SÃO LUÍS DO PARAITINGA",
            nome_mun_corr == "SEM-PEIXE"              & SG_UF == "MG" ~ "SEM PEIXE",
            nome_mun_corr == "WESTFÁLIA"              & SG_UF == "RS" ~ "WESTFALIA",
            
            nome_mun_corr == "AMPARO DO SÃO FRANCISCO" & SG_UF == "SE" ~ "AMPARO DE SÃO FRANCISCO",
            nome_mun_corr == "DONA EUZÉBIA"            & SG_UF == "MG" ~ "DONA EUSÉBIA",
            nome_mun_corr == "ERERÉ"                   & SG_UF == "CE" ~ "ERERÊ",
            nome_mun_corr == "OLHOS-D ÁGUA"            & SG_UF == "MG" ~ "OLHOS D ÁGUA",
            nome_mun_corr == "PINGO-D ÁGUA"            & SG_UF == "MG" ~ "PINGO D ÁGUA",
            nome_mun_corr == "SÃO TOMÉ DAS LETRAS"     & SG_UF == "MG" ~ "SÃO THOMÉ DAS LETRAS",
            
            nome_mun_corr == "JANUÁRIO CICCO"          & SG_UF == "RN" ~ "BOA SAÚDE",
            nome_mun_corr == "TABOCÃO"                 & SG_UF == "TO" ~ "FORTALEZA DO TABOCÃO",
            
            TRUE ~ nome_mun_corr)
  )

# Carrega dados TSE

mun_cods_tse <-
  fread( "./data/TSE/votacao_secao_2018_BR.csv", 
         select = c( "SG_UF", "SG_UE", "CD_MUNICIPIO", "NM_MUNICIPIO") )

mun_cods_tse <-
  mun_cods_tse %>%
    group_by( SG_UE, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO )%>%
    summarise( )%>%
    filter( SG_UF != "ZZ" )

gc()

# Cruzamento dos dados

juncao_fontes <-
  map( .x = unique( mun_cods_ibge$SG_UF ),
       function( .uf ){
         fuzzyjoin::stringdist_inner_join(
           filter( mun_cods_ibge, SG_UF == .uf ),
           filter( ungroup( mun_cods_tse), SG_UF == .uf ),
           by = c( "nome_mun_corr" = "NM_MUNICIPIO" ),
           distance_col = "dist" )
       })%>%bind_rows()

base_final <-
  filter( juncao_fontes, dist == 0 ) %>%
  select( - c( dist, SG_UF.y ) ) %>%
  rename( SG_UF       = SG_UF.x,
          cd_mun_tse  = CD_MUNICIPIO,
          cd_mun_ibge = cod_mun_comp,
          nm_mun_ibge = NM_MUNICIPIO,
          nm_mun_tse  = nome_mun ) %>%
  select( UF, SG_UF, SG_UE, cd_mun_ibge, nm_mun_ibge, cd_mun_tse, nm_mun_tse )
  
# Exportando

library(jsonlite)

write.csv( base_final,
           file  = "./relacao_mun_tse_ibge.csv" )

write_xlsx( base_final,
            path = "./relacao_mun_tse_ibge.xlsx" )

base_json <- toJSON( base_final, pretty = TRUE )

write( base_json, "./relacao_mun_tse_ibge.json")
