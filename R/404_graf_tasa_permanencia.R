# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
#Cargar Tasa de Salida------------------------------------------------------------------------------
load(paste0( parametros$RData, 'IESS_tasa_salida.RData'))
#Ajuste tasa de salida------------------------------------------------------------------------------
Pr<-Pr %>% dplyr::select(Edad,px) %>% mutate(Edad=as.numeric(Edad))

Pr<-na.omit(Pr)
mod <- smooth.spline(Pr$Edad, Pr$px, df = 7) # 16 degrees of freedom
pred <- data.frame(Edad = c(15:115),px_int = predict(mod, c(15:115), deriv = 0)[["y"]])
Pr<-left_join(pred,Pr,by='Edad') 

plot(Pr$Edad,Pr$px)
lines(Pr$Edad,Pr$px_int)

Pr<-Pr %>% dplyr::select(Edad,px,px_int) %>% mutate(px_int=ifelse(px_int>1,1,px_int))

Pr$px <- 1 - Pr$px
Pr$px_int <- 1 - Pr$px_int

# # Guardar en un Rdata ------------------------------------------------------------------------------
# message("\tGuardando en Rdata")
# 
# save(Pr,
#      file = paste0(parametros$RData, "IESS_tasa_salida_int.RData")
# )
# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

#Gráfico del alisado de tasa de permanencia----------- ---------------------------------------------

message( '\tGraficando alisado de la tasa de permanencia' )

aux <- Pr

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 10 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_tasa_permanencia <-  ggplot( data = aux ) + 
  geom_point( aes( x = Edad, y = px, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = Edad, y = px_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("p^{per}_{x-1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = iess_tasa_permanencia, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_permanencia', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# ------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()