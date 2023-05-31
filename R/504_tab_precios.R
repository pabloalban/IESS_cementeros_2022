message( '\tLectura de la proyección de precios' )

# Carga de datos -----------------------------------------------------------------------------------
file_precios <- paste0( parametros$RData, 'IESS_proy_precios.RData' )
load( file = file_precios )

#Cargar función tíldes a latex----------------------------------------------------------------------
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

message( '\tGenerando tablas de las proyecciones de los precios' )

#1. Tabla modelo precios holcim---------------------------------------------------------------------
aux <- mod_precios_holcim
aux_xtab <- aux %>% tildes_a_latex()
aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, 5, 5, 5, 0, 6 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_mod_precios_holcim', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#2. Tabla modelo precios unacem---------------------------------------------------------------------
aux <- mod_precios_unacem
aux_xtab <- aux %>% tildes_a_latex()
aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, 5, 5, 5, 0, 6 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_mod_precios_unacem', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#3. Tabla modelo precios ucem-----------------------------------------------------------------------
aux <- mod_precios_ucem
aux_xtab <- aux %>% tildes_a_latex()
aux_xtab <- xtable( aux_xtab, digits = c( 0, 0, 5, 5, 5, 0, 6 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_mod_precios_ucem', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#4. Tabla modelo precios ucem-----------------------------------------------------------------------
aux <- proy_precios
aux$ano <- as.character(aux$ano)
aux_xtab <- xtable( aux, digits = c( 0, 0, 5, 5, 5 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_proy_precios', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#5. Tabla test_mod_holcim---------------------------------------------------------------------------
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

#6. Tabla test_mod_ucem---------------------------------------------------------------------------
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

#7. Tabla test_mod_unacem---------------------------------------------------------------------------
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
#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
