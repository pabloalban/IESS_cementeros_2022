message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( parametros$graf_modelo_1, encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura proyecciones de precios' )
load( file = paste0( parametros$RData, 'IESS_proy_precios.RData' ) )

message( '\tGraficando las proyecciones de precios' )

# Predicciones de precio de HOLCIM------------------------------------------------------------------
aux <- proy_precios %>% 
  select( ano, y:= holcim ) %>%
  mutate( li = ifelse( ano >= '2019', y - 1.96 * 0.002, NA ),
          ls = ifelse( ano >= '2019', y + 1.96 * 0.002, NA ),
          yh = ifelse( ano <= '2018', y, NA ),
          yp = ifelse( ano >= '2019', y, NA ) )
aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0.05, 0.22 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_precio_holcim <-  ggplot( aux, aes( ano ) ) + 
                  geom_line( aes( y = yp ), 
                             color = parametros$iess_blue, 
                             size = graf_line_size, 
                             alpha = 0.8, 
                             linetype = 2 ) + 
                  geom_line( aes( y = yh ), 
                             color = 'black', 
                             size = graf_line_size, 
                             alpha = 0.8, 
                             linetype = 1 ) + 
                  geom_line( aes( y = li ), color = 'red', 
                             size = graf_line_size, alpha = 0.9, linetype = 2 ) +
                  geom_line( aes( y = ls ), color = 'red', 
                             size = graf_line_size, alpha = 0.8, linetype = 2 ) +
                  labs( x = 'Año', y = 'Precio por kilogramo (USD)' ) +
                  scale_y_continuous( labels = y_lbl,
                                      breaks = y_brk) +
                  #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
                  theme_bw( ) +
                  plt_theme +
                  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
                  geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_precio_holcim, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_precio_holcim', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones de precio de UNACEM------------------------------------------------------------------
aux <-  proy_precios %>% 
        select( ano, y:= unacem ) %>%
        mutate( li = ifelse( ano >= '2019', y - 1.96 * 0.004, NA ),
                ls = ifelse( ano >= '2019', y + 1.96 * 0.004, NA ),
                yh = ifelse( ano <= '2018', y, NA ),
                yp = ifelse( ano >= '2019', y, NA ) )

aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0.05, 0.22 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_precio_unacem <-  ggplot( aux, aes( ano ) ) + 
                            geom_line( aes( y = yp ), 
                                       color = parametros$iess_blue, 
                                       size = graf_line_size, 
                                       alpha = 0.8, 
                                       linetype = 2 ) + 
                            geom_line( aes( y = yh ), 
                                       color = 'black', 
                                       size = graf_line_size, 
                                       alpha = 0.8, 
                                       linetype = 1 ) + 
                            geom_line( aes( y = li ), color = 'red', 
                                       size = graf_line_size, alpha = 0.9, linetype = 2 ) +
                            geom_line( aes( y = ls ), color = 'red', 
                                       size = graf_line_size, alpha = 0.8, linetype = 2 ) +
                            labs( x = 'Año', y = 'Precio por kilogramo (USD)' ) +
                            scale_y_continuous( labels = y_lbl,
                                                breaks = y_brk) +
                            #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
                            theme_bw( ) +
                            plt_theme +
                            theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
                            geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_precio_unacem, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_precio_unacem', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Predicciones de precio de UCEM------------------------------------------------------------------
aux <-  proy_precios %>% 
  select( ano, y:= ucem ) %>%
  mutate( li = ifelse( ano >= '2019', y - 1.96 * 0.004, NA ),
          ls = ifelse( ano >= '2019', y + 1.96 * 0.004, NA ),
          yh = ifelse( ano <= '2018', y, NA ),
          yp = ifelse( ano >= '2019', y, NA ) )

aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0.05, 0.22 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_precio_ucem <-  ggplot( aux, aes( ano ) ) + 
  geom_line( aes( y = yp ), 
             color = parametros$iess_blue, 
             size = graf_line_size, 
             alpha = 0.8, 
             linetype = 2 ) + 
  geom_line( aes( y = yh ), 
             color = 'black', 
             size = graf_line_size, 
             alpha = 0.8, 
             linetype = 1 ) + 
  geom_line( aes( y = li ), color = 'red', 
             size = graf_line_size, alpha = 0.9, linetype = 2 ) +
  geom_line( aes( y = ls ), color = 'red', 
             size = graf_line_size, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Precio por kilogramo (USD)' ) +
  scale_y_continuous( labels = y_lbl,
                      breaks = y_brk) +
  #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_precio_ucem, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_precio_ucem', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )
