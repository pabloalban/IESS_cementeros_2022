message(paste(rep("-", 100), collapse = ""))
message( '\tLectura' )

# Carga de datos -----------------------------------------------------------------------------------
file_reserva <- paste0( parametros$RData, 'IESS_presupuesto.RData' )
file_precios <- paste0( parametros$RData, 'IESS_proy_precios.RData' )
file_comercializacion <- paste0( parametros$RData, 'IESS_proy_comercializacion.RData' )
load( file = file_comercializacion )
load( file = file_precios )
load( file = file_reserva )

message( '\tGenerando Balance corriente' )

#1. Balance Corriente-------------------------------------------------------------------------------


balance_anual <- data.frame( t = c(2022:2043) ) %>%
  mutate( reserva_inicial = parametros$reserva ) %>%
  left_join(., proy_precios, by = c('t'='ano') ) %>%
  left_join(., proy_ventas, by = c('t'='ano') ) %>%
  left_join(., reserva_anual, by = c('t'='anio') ) %>%
  mutate( holcim = holcim.x * holcim.y * 0.02/16.58,
          unacem = unacem.x * unacem.y * 0.02/19.20,
          ucem = ucem.x * ucem.y * 0.02/17.98  ) %>%
  mutate( recaudacion = holcim + unacem + ucem ) %>%
  mutate( comercializacion = holcim.x * holcim.y + unacem.x * unacem.y + ucem.x * ucem.y ) %>%
  mutate( recaudacion =  recaudacion * 650702.3/786046.6) %>%
  mutate( activo =  if_else( t == '2023',
                             recaudacion  + parametros$reserva,
                             recaudacion ) ) %>%
  mutate( beneficios = reserva_matematica_pr + montepio_pr  + liquidacion_pr ) %>%
  mutate( gastos_adm = 0.03 * recaudacion ) %>%
  mutate( pasivo = beneficios + gastos_adm) %>%
  mutate( balance_cor = recaudacion - pasivo ) %>%
  mutate( r = 1 + parametros$i_a ) %>%
  mutate( r = if_else( t == '2022',
                       1, 
                       r ) ) %>%
  mutate( r = cumprod( r ) ) %>%
  mutate( v = 1 / r ) %>%
  mutate( balance_cap = if_else( t == 2022,
                                 reserva_inicial,
                                 balance_cor ) ) %>%
  mutate( balance_cap =  r * cumsum( v * balance_cap )  ) %>%
  na.omit(.) %>%
  mutate( holcim_vap = cumsum( v * holcim ),
          unacem_vap = cumsum( v * unacem ),
          ucem_vap = cumsum( v * ucem ),
          activo_vap = cumsum( v * activo ),
          recaudacion_vap = cumsum( v * recaudacion ),
          comercializacion_vap = cumsum( v * comercializacion ),
          reserva_matematica_vap = cumsum( v * reserva_matematica_pr ),
          montepio_pr_vap = cumsum( v * montepio_pr  ),
          liquidacion_pr_vap = cumsum( v * liquidacion_pr ),
          total_pr_vap = cumsum( v * total_pr ),
          gastos_adm_vap = cumsum( v * gastos_adm ),
          pasivo_vap = cumsum( v * pasivo ),
          V = v *  balance_cap ) %>%
  filter( t <= 2042)

aux <- balance_anual %>%
  mutate( prima = (reserva_matematica_vap + montepio_pr_vap + liquidacion_pr_vap) / comercializacion_vap ) %>%
  filter( t == '2042')

tasa_prima <- aux$prima
  
#Guardar en Rdata-----------------------------------------------------------------------------------
message( '\tGuardando balance' )

save( balance_anual,
      tasa_prima,
      file = paste0( parametros$RData, 'IESS_balances_escenario_1.Rdata' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

