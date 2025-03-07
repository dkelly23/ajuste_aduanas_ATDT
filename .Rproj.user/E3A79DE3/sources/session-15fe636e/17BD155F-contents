# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         3-final.R
# Objetivo:       Generar outputs para el tablero de aduanas.
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          24-Febrero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

# rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, zoo, jsonlite)


# CÓDIGO _______________________________________________________________________

# Función
source("scripts/0-procesamiento.R")

# Cargar panel
load("input/panel_aduanas.RData")

# NOMINAL ----------------------------------------------------------------------
tabla_aduanas <- procesar_bases(tipo="aduanas")
tabla_impuestos <- procesar_bases(tipo="impuestos")
tabla_institucion <- procesar_bases(tipo="institucion")

## Tabla aduanas ordenada segunda pestaña --------------------------------------
tabla_aduanas_2p <- tabla_aduanas %>% filter(impuesto == 'TOTAL') %>% 
  ungroup() %>% 
  select(aduana, 
         acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22,
         pd_tw, pd_sem_24, pd_sem_22, dif_s24, dif_s22, var_24, var_22, 
         prom_mes_25_uf, prom_mes_24_uf, prom_mes_22_uf, dif_mes24, dif_mes22, var_mes_24, var_mes_22) %>% 
  arrange(var_acum_24)
tabla_aduanas_2p %>% glimpse()
save(tabla_aduanas_2p, file = 'output/nominal/base_segunda_parte.Rdata')


## Orden para Guardar ----------------------------------------------------------
tabla_impuestos <- tabla_impuestos %>% select(impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                              var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                              p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                              lista_dia, lista_sem, lista_mes, lista_ano)

tabla_institucion <- tabla_institucion %>% select(institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                  var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                  p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                  lista_dia, lista_sem, lista_mes, lista_ano)

tabla_aduanas <- tabla_aduanas %>% select(aduana, institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                          var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                          p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                          lista_dia, lista_sem, lista_mes, lista_ano)

fecha_corte = tabla_impuestos$lista_dia[3] %>% fromJSON() %>% slice_max(order_by = fecha) %>% pull(fecha)

save(tabla_impuestos, tabla_institucion, tabla_aduanas, fecha_corte,
     file = 'output/nominal/bases_primera_parte.Rdata')
 

# REAL -------------------------------------------------------------------------
tabla_aduanas_real <- procesar_bases(tipo="aduanas", ajuste="real")
tabla_impuestos_real <- procesar_bases(tipo="impuestos", ajuste="real")
tabla_institucion_real <- procesar_bases(tipo="institucion", ajuste="real")

## Tabla aduanas ordenada segunda pestaña --------------------------------------
tabla_aduanas_2p_real <- tabla_aduanas_real %>% filter(impuesto == 'TOTAL') %>% 
  ungroup() %>% 
  select(aduana, 
         acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22,
         pd_tw, pd_sem_24, pd_sem_22, dif_s24, dif_s22, var_24, var_22, 
         prom_mes_25_uf, prom_mes_24_uf, prom_mes_22_uf, dif_mes24, dif_mes22, var_mes_24, var_mes_22) %>% 
  arrange(var_acum_24)
tabla_aduanas_2p_real %>% glimpse()
save(tabla_aduanas_2p_real, file = 'output/real/base_segunda_parte_real.Rdata')


## Orden para Guardar ----------------------------------------------------------
tabla_impuestos_real <- tabla_impuestos_real %>% select(impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                        var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                        p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                        lista_dia, lista_sem, lista_mes, lista_ano)

tabla_institucion_real <- tabla_institucion_real %>% select(institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                            var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                            p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                            lista_dia, lista_sem, lista_mes, lista_ano)

tabla_aduanas_real <- tabla_aduanas_real %>% select(aduana, institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                    var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                    p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                    lista_dia, lista_sem, lista_mes, lista_ano)

fecha_corte = tabla_impuestos_real$lista_dia[3] %>% fromJSON() %>% slice_max(order_by = fecha) %>% pull(fecha)

save(tabla_impuestos_real, tabla_institucion_real, tabla_aduanas_real, fecha_corte,
     file = 'output/real/bases_primera_parte_real.Rdata')


# TDC --------------------------------------------------------------------------
tabla_aduanas_tdc <- procesar_bases(tipo="aduanas", ajuste="tdc")
tabla_impuestos_tdc <- procesar_bases(tipo="impuestos", ajuste="tdc")
tabla_institucion_tdc <- procesar_bases(tipo="institucion", ajuste="tdc")

## Tabla aduanas ordenada segunda pestaña --------------------------------------
tabla_aduanas_2p_tdc <- tabla_aduanas_tdc %>% filter(impuesto == 'TOTAL') %>% 
  ungroup() %>% 
  select(aduana, 
         acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22,
         pd_tw, pd_sem_24, pd_sem_22, dif_s24, dif_s22, var_24, var_22, 
         prom_mes_25_uf, prom_mes_24_uf, prom_mes_22_uf, dif_mes24, dif_mes22, var_mes_24, var_mes_22) %>% 
  arrange(var_acum_24)
tabla_aduanas_2p_tdc %>% glimpse()
save(tabla_aduanas_2p_tdc, file = 'output/tdc/base_segunda_parte_tdc.Rdata')


## Orden para Guardar ----------------------------------------------------------
tabla_impuestos_tdc <- tabla_impuestos_tdc %>% select(impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                      var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                      p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                      lista_dia, lista_sem, lista_mes, lista_ano)

tabla_institucion_tdc <- tabla_institucion_tdc %>% select(institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                          var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                          p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                          lista_dia, lista_sem, lista_mes, lista_ano)

tabla_aduanas_tdc <- tabla_aduanas_tdc %>% select(aduana, institucion, impuesto, rec_mes_25, rec_m24, pd_lw, pd_sem_22, dif_lw, dif_tm, dif_s22, var_tm, 
                                                  var_24, var_22, acum_25, acum_24, acum_22, dif_24, dif_22, var_acum_24, var_acum_22, 
                                                  p_sugerido, prom_25_uf, prom_24_uf, max_p_22, lista_acum_dia, lista_acum_sem, lista_acum_mes, 
                                                  lista_dia, lista_sem, lista_mes, lista_ano)

fecha_corte = tabla_impuestos_tdc$lista_dia[3] %>% fromJSON() %>% slice_max(order_by = fecha) %>% pull(fecha)

save(tabla_impuestos_tdc, tabla_institucion_tdc, tabla_aduanas_tdc, fecha_corte,
     file = 'output/tdc/bases_primera_parte_tdc.Rdata')