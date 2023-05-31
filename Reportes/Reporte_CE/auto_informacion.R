message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

REP$cap_ini <- '1.801.849,43'

# # Escenario 1 --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData, 'IESS_balances_', 'escenario_1', '.RData' ) )

balance_anual <- as.data.table( balance_anual )

REP$bal_act_esc_1 <- format( abs(balance_anual[ t == parametros$horizonte + 2022 ]$V),
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_1 <- format( abs(balance_anual[ t == parametros$horizonte + 2022 ]$balance_cor),
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


REP$I_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2022 ]$recaudacion_vap,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )


REP$Act_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$activo_vap  ,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

REP$Pas_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte + 2020 ]$pasivo_vap,
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$pri_med_niv_esc_1 <- format( tasa_prima * 100,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )
