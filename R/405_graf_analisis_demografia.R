message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCargando Rdatas' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_demografia.RData' ) ) 
load( file = paste0( parametros$RData, 'IESS_demografia_pensionistas.RData' ) ) 


# 2. Piramide poblacional en 2020-------------------------------------------------------------------
message( '\tGraficando piramides poblacionales de los beneficiarios de cesantía' )
# 2. 1. Sin derecho---------------------------------------------------------------------------------
aux<- as.data.table(edad_sexo_SD)
max_edad<-70
min_edad<-20

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-5
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,85,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_SD<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_SD, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_SD', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 2. Con derecho---------------------------------------------------------------------------------
aux<- as.data.table(edad_sexo_CD)
max_edad<-70
min_edad<-40

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-5
salto_x<-0.05
lim_y<- c(-0.15,0.35)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,85,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_CD<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_CD, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_CD', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# 2. 3. Cesantes---------------------------------------------------------------------------------
aux<- as.data.table(edad_sexo_CS)
max_edad<-70
min_edad<-45

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-5
salto_x<-0.05
lim_y<- c(-0.15,0.35)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,85,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_CS<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_CS, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_CS', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 4. Jubilados---------------------------------------------------------------------------------
aux<- as.data.table(edad_sexo_JB)
max_edad<-100
min_edad<-45

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-5
salto_x<-0.05
lim_y<- c(-0.15,0.35)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_JB<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_JB, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_JB', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


## Pirámide de pensiones ---------------------------------------------------------------------------

aux <- tab_pensiones_edad_sexo  %>%
  mutate( fdp = if_else( sexo == 'M',
                         -pen_prom,
                         pen_prom ) ) %>%
  filter( edad >= 15,
          edad <= 105 ) %>%
  arrange( sexo, edad )

salto_y <- 5
brks_y <- round( c( seq( min(aux$fdp), 0, length.out = 5), seq( 0, max(aux$fdp), length.out = 5 )[-1] ) )
lbls_y <- paste0( "$", formatC( abs( brks_y ), digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ) )
brks_x <- seq( 15, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensiones <- ggplot( aux, aes( x = edad, y = fdp, fill=sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ) ) +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE)) +
  theme( legend.position="bottom" ) +
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))


ggsave( plot = iess_pir_pensiones, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensiones', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


## Pirámide de pensionistas-------------------------------------------------------------------------
aux <- tab_pensionistas_edad_sexo %>%
  mutate( fdp = freq / sum( freq ) ) %>% 
  mutate( fdp = if_else( sexo == 'M',
                         -fdp,
                         fdp ) ) %>%
  filter( edad >= 15,
          edad <= 90 ) %>%
  arrange( sexo, edad )

salto_y <- 2
salto_x <- 0.01
brks_y <- seq( -0.10, 0.07, salto_x )
lbls_y <- paste0( as.character( abs(brks_y)*100) , "%")
brks_x <- seq( 15, 100, salto_y )
lbls_x <- paste0( as.character( brks_x ) )

iess_pir_pensionistas <- ggplot( aux, aes( x = edad, y = fdp, fill=sexo ) ) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux %>% filter( sexo == 'F' ), stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux %>% filter( sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y ) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x ) +
  coord_flip( ) +
  theme_bw( ) +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE)) +
  theme( legend.position="bottom" ) +
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_pensionistas, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pensionistas', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
