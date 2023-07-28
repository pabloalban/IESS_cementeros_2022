message( paste( rep( '-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de la población cubierta' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_demografia.RData' ) )
load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( file = paste0( parametros$RData, 'IESS_demografia_ce.RData' ) )
load( file = paste0( parametros$RData, 'IESS_demografia_pensionistas.RData' ) )

#Operativos por estado------------------------------------------------------------------------------
aux <- operativos_estado
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_operativos_estado',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( 3, nrow( aux ) ),
  sanitize.text.function = identity
 )


#administrativos por estado-------------------------------------------------------------------------
aux <- administrativos_estado
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_administrativos_estado',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( 3, nrow( aux ) ),
  sanitize.text.function = identity
 )


#expuestos al riesgo--------------------------------------------------------------------------------
aux <- exposicion_riesgo
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_exposicion_riesgo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( 3, nrow( aux ) ),
  sanitize.text.function = identity
 )

#riesgo por empresa---------------------------------------------------------------------------------
aux <- riesgo_empresa_sexo
aux_xtable <-
  xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'iess_riesgo_empresa_sexo',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = c( 3, nrow( aux ) ),
  sanitize.text.function = identity
 )

# Tabla resumen de hipótesis macro -----------------------------------------------------------------
# La tasa de rendimiento del BIESS se sustituye por la tasa actuarial (  causa mal entendidos  )
aux <- as.data.table( Hipotesis )
aux[Hipotesis == 'Rendimiento_BIESS', Proyeccion := 0.0625]
aux[, print_names := c( 
  'Tasa activa referencial',
  'Tasa pasiva referencial',
  'Tasa  de inter\\\'{e}s actuarial',
  'Tasa variaci\\\'{o}n salarial',
  'Tasa variaci\\\'{o}n SBU',
  'Tasa variaci\\\'{o}n PIB',
  'Tasa inflaci\\\'{o}n'
 )]

aux[, Proyeccion := 100 * Proyeccion]
aux_xtab <-
  xtable( aux[, list( print_names, Proyeccion )], digits = c( 0, 0, 2 ) )

print( 
  aux_xtab,
  file = paste0( parametros$resultado_tablas, 'iess_hip_macro', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#Masa salarial--------------------------------------------------------------------------------------
aux <- masa_salarial %>%
  mutate( ano = as.character( ano ) )

aux_xtable <- xtable( aux, digits = c( 0, 0, rep( 2, 3 ) ) )

print( 
  aux_xtable,
  file = paste0( parametros$resultado_tablas, 'iess_masa_salarial', '.tex' ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

# Pensionistas jubilados administrativos------------------------------------------------------------

aux <- tab_pensionistas_administrativos

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0 ) )

print( 
  aux_xtable,
  file = paste0( 
    parametros$resultado_tablas,
    'tab_pensionistas_administrativos',
    '.tex'
   ),
  type = 'latex',
  include.colnames = FALSE,
  include.rownames = FALSE,
  format.args = list( decimal.mark = ',', big.mark = '.' ),
  only.contents = TRUE,
  hline.after = nrow( aux ),
  sanitize.text.function = identity
 )

#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls(  )[!( ls(  ) %in% c( 'parametros' ) )] )
gc(  )