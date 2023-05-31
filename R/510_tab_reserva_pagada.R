message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura de tabla de resultados' )

# Carga de datos -----------------------------------------------------------------------------------
load(paste0(parametros$RData, "IESS_reserva_mat_pag.RData"))


#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas de resultados de las reservas matemáticas' )

aux <- reserva_mat_pag %>% 
  dplyr::select( cedula,
                 edad,
                 fecha_derecho_ivm,
                 edad_derecho_ivm ,
                 reserva_matematica,
                 montepio,
                 liquidacion_pagada,
                 interes_liquidacion,
                 nomina,
                 interes_nomina,
                 gastos_adm,
                 total ) %>%
  mutate( fecha_derecho_ivm = as.character( fecha_derecho_ivm ) )

n <- nrow(aux)

aux <- rbind((aux), c("Total", NA, NA, NA, NA, as.character(colSums(aux[,6:ncol(aux)]) ) ) )
aux[c(5:ncol(aux))] <- lapply(aux[c(5:ncol(aux))], function(x) as.numeric(x))

aux_xtab <- xtable( aux, digits = c( 0, rep(0, 5), rep(2,7) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'reserva_mat_pag', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( n ),
       sanitize.text.function = identity)

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()