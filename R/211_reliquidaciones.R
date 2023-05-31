message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tProyección de beneficiarios y prestaciones' )

#Carga de datos-------------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_reliquidaciones_cs.RData' ) )
load( file = paste0( parametros$RData, 'IESS_reliquidaciones_jv.RData' ) )
load( file = paste0( parametros$RData, 'IESS_reliquidaciones_fa.RData' ) )
load( file = paste0( parametros$RData, 'IESS_presupuesto.RData' ) )


#Liquidaciones--------------------------------------------------------------------------------------

retroactivos_cs <- retroactivos_cs %>%
  mutate(estado='cesante') %>%
  select(cedula, estado, R_cem, R_cem_max, R_cem_mg, R_cem_mg_max, cargo_homologado)


retroactivos_jv <- retroactivos_jv %>%
  mutate(estado='jubilados') %>%
  select(cedula, estado, R_cem, R_cem_max, R_cem_mg, R_cem_mg_max, cargo_homologado)

retroactivos_jv[which(is.na(retroactivos_jv$cargo_homologado)),]$cargo_homologado<-'Expuesto al riesgo'


retroactivos_fa <- retroactivos_fa %>%
  mutate(estado='fallecidos') %>%
  select(cedula, estado, R_cem, R_cem_max, R_cem_mg, R_cem_mg_max)
retroactivos_fa['cargo_homologado'] <- 'Expuesto al riesgo'



retroactivos <- rbind(retroactivos_cs,
             retroactivos_jv,
             retroactivos_fa) %>%
  dplyr::select( cedula, R_cem_mg_max)


aux <- reserva_matematica %>% left_join(., retroactivos, by = 'cedula' ) %>%
  na.omit(.)
sum(aux$R_cem_mg_max, na.rm = TRUE)
#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando tasa de permanencia' )

save( tasa_permanencia_al_derecho,
      file = paste0( parametros$RData, 'IESS_tasa_permanencia_al_derecho.RData' ) )

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
