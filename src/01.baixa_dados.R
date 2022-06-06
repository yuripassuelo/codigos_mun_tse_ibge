
# Criaçao da relação de Codigos de Municipios TSE e IBGE
# Autor: Yuri de Macedo Passuelo
# Data : 04/06/2022

# Load Setup

source( "./src/00.setup.R")

# Funçao Auxiliar ( Listagemd e arquivos no diretorio FTP )

list_geoftp_files <- function( .url ){
  
  
  files <- rvest::read_html( .url ) %>%
            html_nodes( "a" ) %>%
            html_text( trim = T )
  
  return(
    files[ grepl( ".zip", files )]
  )
}

# Baixando Relaçao de Municipios IBGE

download_ibge_terr <- function( .exc_zip = TRUE, .ano = "2021" ){
  
  # Diretorio de Saida dos dados
  
  raw_ibge_dta <- 
    "./data/IBGE/"
  
  # URL Principal
  
  url_ibge <-
    "https://geoftp.ibge.gov.br/"
  
  # URL 
  
  abr_territorio <-
    "/organizacao_do_territorio/estrutura_territorial/divisao_territorial/"
  
  url_mun_terr   <- paste0( url_ibge, abr_territorio, .ano, "/" )
    
  arquivos       <- list_geoftp_files( url_mun_terr )
  
  # Download dos Arquivos
  
  download.file( url      = paste0( url_mun_terr, arquivos ), 
                 destfile = paste0( raw_ibge_dta, arquivos ), 
                 mode     = "wb" )
  
  # Extraindo
  
  archive_extract( paste0( raw_ibge_dta, arquivos ), dir = raw_ibge_dta )
  
  # Exclusão do Arquivo `.zip`
  
  if( .exc_zip ){
    
    unlink( raw_ibge_dta, recursive = TRUE )
    
    closeAllConnections()
    
    file.remove( paste0( raw_ibge_dta, arquivos ) )
  }
  
}

download_ibge_terr( .exc_zip = TRUE, .ano = "2021" )

# Download - Dados do TSE


baixa_dados_2018 <-
  function( .file = "votacao_secao_2018_BR.zip", .exc_zip = TRUE ){
    
    tse_path <- "./data/TSE/"
    
    url_tse  <- "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/"
    
    download.file( url      = paste0( url_tse,  .file ),
                   destfile = paste0( tse_path, .file ),
                   mode     = "wb" )
    
    # Extraindo
    
    archive_extract( paste0( tse_path, .file ), dir = tse_path )
    
    # Exclusão do Arquivo `.zip`
    
    if( .exc_zip ){
      
      unlink( tse_path, recursive = TRUE )
      
      closeAllConnections()
      
      file.remove( paste0( tse_path, .file ) )
    }
  }


baixa_dados_2018( )
# Carregando informaçoes de Nome e Codigo Municípios TSE




