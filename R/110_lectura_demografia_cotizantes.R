message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\tLectura demografía de los cementeros a marzo de 2022' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data,
                'IESS_demografia_cotizantes_cementeros.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R',
        encoding = 'UTF-8',
        echo = FALSE )

#Masa salarial--------------------------------------------------------------------------------------
masa_salarial <- read_excel( 
  file,
  sheet = '1. Masa Salarial Anual',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names( ) %>%
  mutate( hombre = if_else( ano == '2022',
                            4 * hombre,
                            hombre ) ) %>%
  mutate( mujer = if_else( ano == '2022',
                           4 * mujer,
                           mujer ) ) %>%
  mutate( total = hombre + mujer )


#Número de afiliados--------------------------------------------------------------------------------

afi_edad_sexo <- read_excel( 
  file,
  sheet = '2. Número Afiliados',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names( ) %>%
  mutate_if( is.numeric , replace_na, replace = 0 ) %>%
  mutate( fdp = afi / sum( afi, na.rm = TRUE ) )

#Salario promedio-----------------------------------------------------------------------------------

sal_edad_sexo <- read_excel( 
  file,
  sheet = '3. Salario Promedio',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names( ) %>%
  mutate_if( is.numeric , replace_na, replace = 0 )

#Imp promedio---------------------------------------------------------------------------------------

imposiciones_edad <- read_excel( 
  file,
  sheet = '4. Número Imposiciones',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names( )

#Salario promedio por imposiciones------------------------------------------------------------------

sal_imposiciones_edad <- read_excel( 
  file,
  sheet = '5. Salario prom - Número Impo',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names( )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando demografia CE' )

save( 
  masa_salarial,
  afi_edad_sexo,
  sal_edad_sexo,
  imposiciones_edad,
  sal_imposiciones_edad,
  file = paste0( parametros$RData, 'IESS_demografia_ce.RData' )
 )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[!( ls( ) %in% 'parametros' )] )
gc( )