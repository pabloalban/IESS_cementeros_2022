message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )
source( 'R/503_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )
escenarios_lista <- paste0( 'escenario_', 1: 1 )

for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  #escenario <- escenarios_lista[1]
  load( paste0( parametros$RData, 'IESS_balances_', escenario, '.RData' ) )
  # Balance corriente ------------------------------------------------------------------------
  balance_anual <- as.data.table( balance_anual )
  aux <- balance_anual[ , list( t, nuevos_pensionistas, recaudacion, reserva_matematica_pr, montepio_pr, liquidacion_pr, gastos_adm, reserva_inicial, balance_cor, balance_cap ) ]
  aux<-aux[t>=2023]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, rep( 2, 8) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux), 
         sanitize.text.function = identity )
  
  # Balance dinámico  ------------------------------------------------------------------------
  # Balance dinámico (actuarial) -------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t, recaudacion_vap, reserva_matematica_vap, montepio_pr_vap, liquidacion_pr_vap, gastos_adm_vap, reserva_inicial, V ) ]
  aux<-aux[anio>=2023]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep(2, 7) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow( aux ),
         sanitize.text.function = identity )
  
  
  # # Balance dinámico (beneficios) ------------------------------------------------------------
  # 
  # aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t,B1_vap,B2_vap,B3_vap,B4_vap,B5_vap,B_vap ) ]
  # aux<-aux[anio>2018]
  # aux[, anio := as.character( anio )]
  # xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  # print( xtb_aux,
  #        file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '.tex' ),
  #        type = 'latex', 
  #        include.colnames = FALSE, include.rownames = FALSE, 
  #        format.args = list( decimal.mark = ',', big.mark = '.' ), 
  #        only.contents = TRUE, 
  #        hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------
  aux <- balance_anual[ t == max(t), 
                        list( reserva_inicial,
                              recaudacion_vap ,
                              activo = reserva_inicial + recaudacion_vap,
                              reserva_matematica_vap,
                              montepio_pr_vap,
                              liquidacion_pr_vap,
                              gastos_adm_vap,
                              pasivo = pasivo_vap, 
                              V = abs(V) ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( reserva_inicial = 'Reserva inicial', 
                      recaudacion_vap = 'Recaudación futura', 
                      activo = 'Total activo actuarial', 
                      reserva_matematica_vap = 'Reserva matemática', 
                      montepio_pr_vap = 'Beneficios de montepío',
                      liquidacion_pr_vap = 'Liquidaciones a extrabajadores',
                      gastos_adm_vap = 'Gastos administrativos',
                      pasivo = 'Total pasivo actuarial',
                      V = 'Déficit actuarial' )
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames(aux, c('item', 'descripcion', 'valor'))
  xtb_aux <- xtable( aux[ , list(descripcion, valor)], digits = c( 0, 0, 2 ) )
  xtb_aux <- tildes_a_latex(xtb_aux)
  
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c( 2, 3, 7, 8 ) ,
         sanitize.text.function = identity,
         add.to.row = 
           list( pos = list(0, 3, 8),
                 command = c(paste(" \n \\multicolumn{2}{c}{\\textbf{Activo actuarial}} \\\\ \n \\hline \n"), 
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Pasivo actuarial}} \\\\ \n"),
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Balance actuarial}} \\\\ \n") ) ) )
  
  rm( balance, balance_anual )
}


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

