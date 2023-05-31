message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tProyección de beneficiarios y prestaciones' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_cementeros.RData' ) )
load( paste0( parametros$RData, 'IESS_tasa_salida_int.RData' ) )

#Tasa salida acumulada a fecha de jubilación--------------------------------------------------------

tasa_permanencia_al_derecho <- cementeros %>%
  filter( tipo %in% c('SD', 'CD') ) %>%
  filter( cedula!=('1709682924')) %>% #Prestación 2022
  filter( !(cedula%in%c('1001729506', '0907567986'))) %>%#Fallecidos antes 2023
  filter( !(cedula %in% c('0300596830',
                          '0300658853',
                          '0300710860',
                          '0400504247',
                          '0601028780',
                          '0601131535',
                          '0601381056',
                          '0601451057',
                          '0905950721',
                          '0906355912',
                          '0906474291',
                          '0906581483',
                          '0908312267',
                          '1001167871',
                          '1001189321',
                          '1001225489',
                          '1001307824',
                          '1001529534',
                          '1703828093',
                          '0300706108',
                          '0400630620',
                          '0600500714',
                          '0601053804',
                          '0601195340',
                          '1000951242',
                          '1001127164',
                          '1001225752') ) ) %>% #Jubilados
  mutate( ano_jub = year( fecha_jubilacion ) ) %>%
  mutate( ano_jub = ifelse( ano_jub < 2023,
                            2023,
                            ano_jub ) ) %>%
  mutate( i = ano_jub - 2023 + 1 ) %>%
  mutate( edad = round( age_calc( fecha_nacimiento,
                                  enddate = as.Date("31/12/2022","%d/%m/%Y"),
                                  units = "years"), 0 ) ) %>%
  slice( rep( 1:n(), i ) ) %>%
  group_by( cedula ) %>%
  mutate( j = 1:n() ) %>%
  ungroup() %>%
  mutate( x = edad + j ) %>%
  mutate( anio = 2022 + j) %>%
  dplyr::select( anio,
                 x,
                 i,
                 j,
                 edad,
                 ano_jub,
                 cedula ) %>%
  left_join(., Pr, by =c('x'='Edad') ) %>%
  mutate( px_int = if_else( anio == 2023,
                            1,
                            px_int ) ) %>%
  group_by( cedula ) %>%
  mutate( pr_acu = cumprod( px_int ) )  %>%
  filter(  ano_jub == anio ) %>%
  ungroup( ) %>%
  dplyr::select( cedula, ano_jub, pr_acu )

#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando tasa de permanencia' )

save( tasa_permanencia_al_derecho,
      file = paste0( parametros$RData, 'IESS_tasa_permanencia_al_derecho.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()

