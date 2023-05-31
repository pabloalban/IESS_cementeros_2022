message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
file_comercializacion <- paste0( parametros$RData, 'IESS_proy_comercializacion.RData' )
load( file = file_comercializacion )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas de la comercialización' )

#1. Tabla modelo precios holcim---------------------------------------------------------------------
aux <- modelos_comercializacion
aux_xtab <- xtable( aux, digits = c( 0, 0, 5, 7, 7) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_mod_comercializacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

#2. Test del modelo HOLCIM--------------------------------------------------------------------------
aux <- test_mod_holcim
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 3, 3, 3 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_mod_holcim', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#3. Test del modelo UNACEM--------------------------------------------------------------------------
aux <- test_mod_unacem
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 3, 3, 3 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_mod_unacem', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#4. Test del modelo UCEM----------------------------------------------------------------------------
aux <- test_mod_ucem
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 3, 3, 3 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_test_mod_ucem', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)


#5. Tabla prediciones de comercializacion-----------------------------------------------------------
aux <- proy_ventas %>%
        mutate( porc_holcim = 100 * holcim / total,
                porc_unacem = 100 * unacem / total,
                porc_ucem = 100 * ucem / total,
                porc_total = 100 )

aux$ano <- as.character( aux$ano )
aux_xtab <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 2, 2, 2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_proy_ventas', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
