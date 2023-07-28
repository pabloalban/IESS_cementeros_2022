message( paste( rep( '-', 100  ), collapse = '' ) )

message( '\tLectura de las reservas matemáticas pagadas a los cementeros' )

#Cargando información financiera--------------------------------------------------------------------
file <- paste0( parametros$Data, 'IESS_reservas_pagadas.xlsx' )
file2 <- paste0( parametros$Data, 'IESS_beneficiarios.xlsx' )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R',
       encoding = 'UTF-8',
       echo = FALSE )

#Listado de beneficiarios---------------------------------------------------------------------------
reserva_mat_pag <- read_excel( 
  file,
  #sheet = 'CBF',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names(  ) %>%
  mutate( fecha_derecho_ivm   = as.Date( fecha_derecho_ivm, "%Y-%m-%d", tz = Sys.timezone(  ) ) )


pensionistas <- read_excel( 
  file2,
  sheet = 'pensionistas',
  col_names = TRUE,
  col_types = NULL,
  na = "NA",
  skip = 0
 ) %>% clean_names(  ) %>%
  mutate( fecha_de_nacimiento   = as.Date( fecha_de_nacimiento, "%Y-%m-%d", tz = Sys.timezone(  ) ) ) %>%
  mutate( fecha_derecho_ivm   = as.Date( fecha_derecho_ivm, "%Y-%m-%d", tz = Sys.timezone(  ) ) ) %>%
  mutate( edad = round( 
    age_calc( 
      fecha_de_nacimiento,
      enddate = as.Date( "2022-12-31" ),
      units = "years"
     ),
    0
   ) )

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando beneficiarios CE' )

save( 
  reserva_mat_pag,
  pensionistas,
  file = paste0( parametros$RData, 'IESS_reserva_mat_pag.RData' )
 )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% 'parametros' )] )
gc(  )