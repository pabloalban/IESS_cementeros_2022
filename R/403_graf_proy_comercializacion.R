message( paste( rep( '-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( parametros$graf_modelo_1, encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura proyecciones de precios' )
load( file = paste0( parametros$RData, 'IESS_proy_comercializacion.RData' ) )

message( '\tGraficando las proyecciones de la comercialización' )

# Predicciones de precio de HOLCIM------------------------------------------------------------------
aux <- proy_ventas %>% 
  select( ano, y:= holcim ) %>%
  mutate( li = ifelse( ano >= '2019', y - 1.96 * 300000000, NA ),
          ls = ifelse( ano >= '2019', y + 1.96 * 300000000, NA ),
          yh = ifelse( ano <= '2018', y, NA ),
          yp = ifelse( ano >= '2019', y, NA ) )
aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0, 4000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_ventas_holcim <-  ggplot( aux, aes( ano ) ) + 
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
  labs( x = 'Año', y = 'Kilogramos' ) +
  scale_y_continuous( labels = y_lbl,
                      breaks = y_brk) +
  #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_ventas_holcim, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_ventas_holcim', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones de precio de UNACEM------------------------------------------------------------------
aux <- proy_ventas %>% 
  select( ano, y:= unacem ) %>%
  mutate( li = ifelse( ano >= '2019', y - 1.96 * 120000000, NA ),
          ls = ifelse( ano >= '2019', y + 1.96 * 120000000, NA ),
          yh = ifelse( ano <= '2018', y, NA ),
          yp = ifelse( ano >= '2019', y, NA ) )
aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0, 1500000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_ventas_unacem <-  ggplot( aux, aes( ano ) ) + 
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
  labs( x = 'Año', y = 'Kilogramos' ) +
  scale_y_continuous( labels = y_lbl,
                      breaks = y_brk) +
  #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_ventas_unacem, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_ventas_unacem', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones de precio de UCEM------------------------------------------------------------------
aux <- proy_ventas %>% 
  select( ano, y:= ucem ) %>%
  mutate( li = ifelse( ano >= '2019', y - 1.96 * 60000000, NA ),
          ls = ifelse( ano >= '2019', y + 1.96 * 60000000, NA ),
          yh = ifelse( ano <= '2018', y, NA ),
          yp = ifelse( ano >= '2019', y, NA ) )
aux[which(aux$ano==2019),]$yh <- aux[which(aux$ano==2019),]$yp


y_lim <- c( 0, 2000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )

iess_proy_ventas_ucem <-  ggplot( aux, aes( ano ) ) + 
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
  labs( x = 'Año', y = 'Kilogramos' ) +
  scale_y_continuous( labels = y_lbl,
                      breaks = y_brk) +
  #scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = 2019, color = "black", linetype = 2 )

ggsave( plot = iess_proy_ventas_ucem, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_ventas_ucem', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )