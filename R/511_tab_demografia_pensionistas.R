message( paste( rep('-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de los pensionistas' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_demografia_pensionistas.RData' ) ) 

#Tabla pensionistas por empresa y expuestos al riesgos----------------------------------------------

aux <- pensionistas_riesgo

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_empresa_riesgo_pen', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( 2, nrow(aux) ),
       sanitize.text.function = identity )


#Tabla por rangos de edad--------------------------------------------------------------------------- 

aux <- edad_sexo_total

aux_xtable <- xtable( aux, digits = c( 0, rep(0, 10) ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pensionistas_rango_edad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = c( nrow(aux) - 1, nrow(aux) ),
       sanitize.text.function = identity )

#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()