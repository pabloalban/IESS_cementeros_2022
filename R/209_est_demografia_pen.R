message( paste( rep('-', 100 ), collapse = '' ) )

message( '\t Tablas del Análisis demográfico de los pensionistas' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_reserva_mat_pag.RData' ) ) 
load(paste0(parametros$RData, "IESS_cementeros.RData"))


#Pensionistas---------------------------------------------------------------------------------------
cementeros <- cementeros %>% dplyr::select( -edad, -sexo )

pensiones <- pensionistas


pensionistas <- pensionistas %>%
  mutate( edad = round(age_calc(fecha_de_nacimiento,
                                enddate = as.Date("2022-12-31"),
                                units = "years"),0) ) %>%
  dplyr::select( cedula,
                 edad, 
                 sexo, 
                 razon_social:= empresa) %>%
  left_join( ., cementeros %>% dplyr::select( cedula, cargo_homologado ), by = 'cedula') %>%
  mutate( cargo_homologado = ifelse( is.na( cargo_homologado ),
                                     'Operativo',
                                     cargo_homologado ) )


#Tabla pensionistas por empresa y expuestos al riesgos----------------------------------------------

pensionistas_riesgo <- pensionistas %>%
  group_by( razon_social, cargo_homologado ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  distinct(., razon_social, cargo_homologado, .keep_all = TRUE ) %>%
  dplyr::select( razon_social, cargo_homologado, freq ) %>%
  na.omit(.) %>%
  mutate( freq = if_else( freq %in% c('21','18'),
                          freq + 1,
                          freq ) ) %>%
  spread(  ., cargo_homologado, value = c(freq ),  sep = "ben" ) %>%
  mutate( total = rowSums(.[2:ncol(.)], na.rm = TRUE ) ) %>%
  mutate_if( is.numeric , replace_na, replace = 0) %>%
  rbind( ., c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))  %>%
  mutate_at( c(2:ncol(.)), as.numeric)


#1.Tabla por rangos de edad----------------------------------------------------- 
a <- pensionistas

cortes_edad <- c( quantile(a$edad, probs = seq( 0, 1, length.out = 9 ) ) )

etiquetas_edad<-c(paste0("(",formatC( cortes_edad[1:length(cortes_edad)-1], 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         " - ",formatC( cortes_edad[2:length(cortes_edad)], 
                                        digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))


aux <- pensionistas %>% 
  filter( cargo_homologado == 'Operativo' ) %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  select(sexo,rango_edad,afiliados,dist) %>%
  arrange(sexo,rango_edad)#ordenar por sexo y edad

aux_h <- aux %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux_m <- aux %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux_t <- left_join(aux_h,aux_m,by="rango_edad") %>% select(rango_edad,hombres:=afiliados.x,mujeres:=afiliados.y)
aux_t <- aux_t%>% mutate(Total= rowSums(aux_t[,2:3], na.rm = TRUE)) 
aux_total_edad <- aux_t%>%  
  add_row(rango_edad = "Total", hombres = sum(aux_t$hombres, na.rm = TRUE), mujeres = sum(aux_t$mujeres, na.rm = TRUE), Total=sum(aux_t$Total, na.rm = TRUE))  

edad_sexo_total_operativos <- aux_total_edad  %>%
  replace(is.na(.), 0)



aux <- pensionistas %>% 
  filter( cargo_homologado == 'Administrativo' ) %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  select(sexo,rango_edad,afiliados,dist) %>%
  arrange(sexo,rango_edad) %>%
  mutate( rango_edad = as.character( rango_edad ))#ordenar por sexo y edad

aux_h <- aux %>% mutate(sexo="M",
                        afiliados = 0) %>% select(-dist,-sexo)
aux_m <- aux %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux_t <- left_join(aux_h,aux_m,by='rango_edad') %>% select(rango_edad,hombres:=afiliados.x,mujeres:=afiliados.y)
aux_t <- aux_t%>% mutate(Total= rowSums(aux_t[,2:3], na.rm = TRUE)) 
aux_total_edad <- aux_t%>%  
  add_row(rango_edad = "Total", hombres = sum(aux_t$hombres, na.rm = TRUE), mujeres = sum(aux_t$mujeres, na.rm = TRUE), Total=sum(aux_t$Total, na.rm = TRUE))  

edad_sexo_total_administrativo <- aux_total_edad  %>%
  replace(is.na(.), 0)


aux <- pensionistas %>% 
  #filter( cargo_homologado == 'Administrativo' ) %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                        labels = etiquetas_edad,
                        include.lowest = TRUE,
                        right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(afiliados=n()) %>%
  ungroup() %>%
  mutate(dist=afiliados/n()) %>%
  distinct(sexo,rango_edad,.keep_all = TRUE) %>%
  select(sexo,rango_edad,afiliados,dist) %>%
  arrange(sexo,rango_edad)#ordenar por sexo y edad

aux_h <- aux %>% filter(sexo=="M") %>% select(-dist,-sexo)
aux_m <- aux %>% filter(sexo=="F") %>% select(-dist,-sexo)
aux_t <- left_join(aux_h,aux_m,by="rango_edad") %>% select(rango_edad,hombres:=afiliados.x,mujeres:=afiliados.y)
aux_t <- aux_t%>% mutate(Total= rowSums(aux_t[,2:3], na.rm = TRUE)) 
aux_total_edad <- aux_t%>%  
  add_row(rango_edad = "Total", hombres = sum(aux_t$hombres, na.rm = TRUE), mujeres = sum(aux_t$mujeres, na.rm = TRUE), Total=sum(aux_t$Total, na.rm = TRUE))  

edad_sexo_total <- aux_total_edad  %>%
  replace(is.na(.), 0)


edad_sexo_total <- left_join( edad_sexo_total_operativos, 
                              edad_sexo_total_administrativo,
                              by = 'rango_edad') %>%
  left_join(., edad_sexo_total, by = 'rango_edad' ) %>%
  replace(is.na(.), 0)


#Tabla pensiones promedio por edad y sexo-----------------------------------------------------------

tab_pensiones_edad_sexo <- pensiones %>%
  group_by( edad, sexo ) %>%
  mutate( pen_prom = mean( ric, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( .,  edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( edad, sexo, pen_prom ) %>%
  arrange( edad, sexo )

#Tabla de pensionistas por edad y sexo--------------------------------------------------------------

tab_pensionistas_edad_sexo <- pensiones %>%
  group_by( edad, sexo ) %>%
  mutate( freq = n() ) %>%
  ungroup( ) %>%
  distinct( .,  edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( edad, sexo, freq ) %>%
  arrange( edad, sexo )

#Estadísticas para la redacción---------------------------------------------------------------------
aux <- pensiones %>%
  group_by( sexo ) %>%
  mutate( prom = mean( ric, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., sexo, .keep_all = TRUE ) %>%
  dplyr::select( sexo, prom )



aux <- pensiones %>%
  group_by( sexo ) %>%
  mutate( edad_prom = mean( edad, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., sexo, .keep_all = TRUE ) %>%
  dplyr::select( sexo, edad_prom )


#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando en Rdata' )

save( pensionistas_riesgo,
      edad_sexo_total,
      tab_pensiones_edad_sexo,
      tab_pensionistas_edad_sexo,
      file = paste0( parametros$RData, 'IESS_demografia_pensionistas.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()