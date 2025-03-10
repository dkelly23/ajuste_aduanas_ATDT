# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         0-procesamiento.R
# Objetivo:       Función que procesa toda la información de aduanas.
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          24-Febrero-2025
#
# ______________________________________________________________________________

# Función de variación
variacion <- function(val_1, val_2) {
  dplyr::case_when(
    val_2 != 0  ~ -100 * (1 - (val_1 / val_2)),
    val_1 == 0 & val_2 == 0 ~ 0,
    val_2 == 0 ~ 100,
    val_1 == 0 ~ -100
  )
}

procesar_bases <- function(tipo="aduanas", ajuste=NA, last.date=last.date) {

  clase <- tipo
  
  # ==============================================================================
  
  if (is.na(ajuste)) {
    panel_rec_aduanas <- panel_rec_aduanas
    
  } else if (ajuste=="real") {
    # Ajustamos por INPC
    inpc_base <- inpc_df$inpc |> tail(1)
    
    panel_def <- panel_rec_aduanas |> 
      mutate(
        mes=month(fecha),
        year=year(fecha)
      ) |> left_join(inpc_df |> mutate(mes=month(fecha), year=year(fecha)) |> select(-fecha)) 
    
    panel_def[["inpc"]] <- na.locf(panel_def[["inpc"]], na.rm = FALSE)
    panel_def[["inpc"]] <- na.locf(panel_def[["inpc"]], na.rm = FALSE)
    
    panel_def <- panel_def |> 
      mutate(inpc=(inpc/inpc_base)) |> 
      mutate(
        recaudacion=recaudacion*(1/inpc)
      ) |> 
      select(-c(inpc, mes, year))
    
    # Renombrar
    panel_rec_aduanas <- panel_def
    
  } else if (ajuste=="tdc") {
    
    load("input/panel_def.RData")
    
    # Renombrar
    panel_rec_aduanas <- panel_def
    
  }
  
  # ==============================================================================
  
  # TABLA ADUANAS ---------------------------------------------------------------
  # Otros impuestos
  temp <- panel_rec_aduanas |> 
    filter(!impuesto %in% c('TOTAL', 'IVA', 'IEPS')) |> 
    group_by(fecha, cta, aduana, institucion) |> 
    summarise(
      impuesto="OTROS",
      recaudacion=sum(recaudacion, na.rm=TRUE)
    )
  
  # Filtro de impuestos
  panel_rec_aduanas <- panel_rec_aduanas %>% filter(impuesto %in% c('IVA', 'IEPS')) |> bind_rows(temp)
  
  temp <- panel_rec_aduanas |> 
    group_by(fecha, cta, aduana, institucion) |> 
    summarise(
      impuesto="TOTAL",
      recaudacion=sum(recaudacion, na.rm=TRUE)
    )
  
  panel_rec_aduanas <- panel_rec_aduanas |> bind_rows(temp) |> arrange(aduana, fecha, impuesto) |> 
    mutate(recaudacion=ifelse(is.na(recaudacion), 0, recaudacion))
  
  # Suma de recaudación por aduana 
  panel_rec_aduanas <- panel_rec_aduanas  %>% group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T))
  
  # última fecha
  uf <- max(panel_rec_aduanas$fecha)
  
  
  ## Acumulados ------------------------------------------------------------------
  
  # Acumulado 2022
  acum_22 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2022, fecha <= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(acum_22 = sum(recaudacion, na.rm=TRUE))
  
  # Acumulado 2023
  acum_23 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2023, fecha <= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(acum_23 = sum(recaudacion, na.rm=TRUE))
  
  # Acumulado 2024
  acum_24 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2024, fecha <= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(acum_24 = sum(recaudacion, na.rm=TRUE))
  
  # Acumulado 2025
  acum_25 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2025, fecha <= uf) |> 
    group_by(aduana, impuesto) %>% 
    summarise(acum_25 = sum(recaudacion, na.rm=TRUE))
  
  
  # Tabla general
  tabla_aduanas <- acum_25 %>% left_join(acum_25) |> left_join(acum_24) %>% left_join(acum_23) |> left_join(acum_22) %>% 
    # Diferecias en recaudación
    mutate(dif_24 = acum_25-acum_24, 
           dif_23 = acum_25-acum_23,
           dif_22 = acum_25-acum_22)
  
  
  # Generar cambios porcentuales
  tabla_aduanas <- tabla_aduanas %>% group_by(aduana, impuesto) %>% 
    mutate(var_acum_24 = variacion(acum_25, acum_24), 
           var_acum_23 = variacion(acum_25, acum_23),
           var_acum_22 = variacion(acum_25, acum_22))
  
  
  ## Último periodo --------------------------------------------------------------
  
  ### Semanal --------------------------------------------------------------------
  # Ultima semana
  ultima_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(aduana, institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tw = recaudacion) %>% 
    mutate(pd_tw = rec_tw/round(as.numeric(difftime(uf,fecha)+1))) %>% select(-fecha)
  
  # Penultima semana
  pu_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(aduana, institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by(aduana, institucion, impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lw = recaudacion) %>% 
    mutate(pd_lw = rec_lw/7)  %>% 
    ungroup() %>% 
    select(aduana, institucion, impuesto, pd_lw)
  
  ### Mensual --------------------------------------------------------------------
  # Último mes
  ultimo_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) |> 
    group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() |> 
    group_by(aduana, institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tm = recaudacion) %>% 
    mutate(pd_tm = rec_tm/round(as.numeric(difftime(uf,fecha)+1))) %>% 
    select(-fecha)
  
  # Penúltimo mes
  pu_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(aduana, institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by(aduana, institucion, impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lm = recaudacion) %>% 
    mutate(pd_lm = rec_lm)  %>% 
    ungroup() %>% 
    select(aduana, institucion, impuesto, pd_lm)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_aduanas <- tabla_aduanas |> 
    left_join(ultima_semana) |> 
    left_join(pu_semana) |> 
    left_join(ultimo_mes) |> 
    left_join(pu_mes)
  
  
  ## Mismo periodo del año anterior ----------------------------------------------
  ### Semanal --------------------------------------------------------------------
  # 2022
  sem_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(3))) %>% 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_s22 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_22 = rec_s22/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(aduana, institucion, impuesto, rec_s22, pd_sem_22)
  
  # 2023
  sem_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(2))) %>% 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_s23 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_23 = rec_s23/round(as.numeric(difftime(uf-years(2),fecha)+1))) %>% 
    select(aduana, institucion, impuesto, rec_s23, pd_sem_23)
  
  # 2024
  sem_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(1))) %>% 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_s24 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_24 = rec_s24/7) %>%
    # mutate(
    #   pd_sem_24 = rec_s24 / round(as.numeric(difftime(fecha, as.Date(paste(year(uf-years(1)), month(uf), "01", sep="-")), unit = "days") + 1))
    # ) %>% 
    select(aduana, institucion, impuesto, rec_s24, pd_sem_24)
  
  
  ### Mensual --------------------------------------------------------------------
  # 2022
  mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    mutate(mes = month(fecha), dia=day(fecha)) %>% 
    filter(mes == month(uf), dia <= day(uf)) %>% 
    mutate(fecha=floor_date(fecha, unit = 'month')) |> 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_m22 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_22 = rec_m22/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(aduana, institucion, impuesto, rec_m22, pd_mes_22)
  
  # 2023
  mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    mutate(mes = month(fecha), dia=day(fecha)) %>% 
    filter(mes == month(uf), dia <= day(uf)) %>% 
    mutate(fecha=floor_date(fecha, unit = 'month')) |> 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_m23 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_23 = rec_m23/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(aduana, institucion, impuesto, rec_m23, pd_mes_23)
  
  # 2024
  mes_24 <-  panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    mutate(mes = month(fecha), dia=day(fecha)) %>% 
    filter(mes == month(uf)) %>%
    filter(dia <= day(uf)) |> 
    mutate(fecha=floor_date(fecha, unit = 'month')) |> 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(rec_m24 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_24 = rec_m24/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(aduana, institucion, impuesto, rec_m24, pd_mes_24)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_aduanas <- tabla_aduanas |> 
    left_join(sem_22) |> left_join(sem_23) |> left_join(sem_24) |> 
    left_join(mes_22) |> left_join(mes_23) |> left_join(mes_24)
  
  tabla_aduanas <- tabla_aduanas |> 
    group_by(aduana, impuesto) %>%
    mutate(var_tw = variacion(pd_tw, pd_sem_24), 
           var_24 = variacion(pd_tw, pd_sem_24), 
           var_23 = variacion(pd_tw, pd_sem_23),
           var_22 = variacion(pd_tw, pd_sem_22)) 
  
  tabla_aduanas <- tabla_aduanas |> 
    group_by(aduana, impuesto) %>%
    mutate(var_tm = variacion(pd_tm, pd_lw), 
           varm_24 = variacion(pd_tm, pd_mes_24), 
           varm_23 = variacion(pd_tm, pd_mes_23),
           varm_22 = variacion(pd_tm, pd_mes_22)) 
  
  ## Promedio diario -------------------------------------------------------------
  ### Lo que va del año ----------------------------------------------------------
  # 2024
  prom_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    filter(fecha<= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_24_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2023
  prom_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    filter(fecha<= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_23_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2022
  prom_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    filter(fecha<= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_22_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  
  ### Mes en curso ---------------------------------------------------------------
  # 2025
  prom_mes_25 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2025) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2025, month(uf), 1, sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_25_uf = recaudacion/round(as.numeric(difftime(uf,as.Date(paste(year(uf),month(uf),"01",sep="-")),units='days')+1))) %>% 
    mutate(rec_mes_25=recaudacion) |> 
    select(-recaudacion)
  
  # 2024
  prom_mes_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2024, month(uf), 1, sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_24_uf = recaudacion/days_in_month(month(uf-years(1)))) %>% 
    select(-recaudacion)
  
  # 2023
  prom_mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2023, month(uf), 1, sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_23_uf = recaudacion/days_in_month(month(uf-years(2)))) |> 
    select(-recaudacion)
  
  # 2022
  prom_mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2022, month(uf), 1, sep = '-'))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_22_uf = recaudacion/days_in_month(month(uf-years(2)))) %>% 
    select(-recaudacion)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_aduanas <- tabla_aduanas |>
    left_join(prom_24) |> left_join(prom_23) |> left_join(prom_22) |> 
    left_join(prom_mes_25) |> left_join(prom_mes_24) |> left_join(prom_mes_23) |> 
    left_join(prom_mes_22)
  
  
  ## Maximos y Mínimos -----------------------------------------------------------
  max_22 <- panel_rec_aduanas %>% 
    filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(aduana, impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max_22 = recaudacion)%>% mutate(max_p_22 = max_22/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_aduanas <- tabla_aduanas %>% left_join(max_22 %>% select(-max_22))
  
  max <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(aduana, impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max = recaudacion)%>% mutate(p_max = max/days_in_month(fecha)) %>%
    select(-fecha)
  
  min <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(aduana, impuesto) %>% 
    slice_min(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(min = recaudacion)%>% mutate(p_min = min/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_aduanas <- tabla_aduanas %>% left_join(max %>% select(-max)) %>% left_join(min %>% select(-min))
  
  tabla_aduanas <- tabla_aduanas %>% select(aduana, institucion, impuesto, everything())
  
  
  ## Estimaciones Fidel ----------------------------------------------------------
  
  recaudacion_sugerida <- read_excel("input/recaudacion_sugerida.xlsx") %>% clean_names()
  
  recaudacion_sugerida <- recaudacion_sugerida %>% select(aduana, cve_aduana, institucion, recaudacion_sugerida)
  
  # Read cat_aduana
  cat_aduanas <- read_csv("input/cat_aduanas.csv") %>% 
    mutate(cve_aduana = as.numeric(str_extract(cta, '..$')))
  
  
  recaudacion_sugerida <- recaudacion_sugerida %>% 
    select(-aduana, -institucion) %>% 
    left_join(cat_aduanas ) %>% 
    select(cve_aduana, cta, aduana, institucion, recaudacion_sugerida)
  tot_mensual_sug <- recaudacion_sugerida %>% mutate(p_sugerido = recaudacion_sugerida/30)
  
  tabla_aduanas <- tabla_aduanas %>% left_join(tot_mensual_sug %>% select(aduana, p_sugerido))
  
  tabla_aduanas %>% mutate(p_sugerido = ifelse(impuesto == 'TOTAL', p_sugerido, 0))
  
  
  ## Estimar Diferencias ---------------------------------------------------------
  
  tabla_aduanas %>% glimpse()
  
  tabla_aduanas <- tabla_aduanas %>% mutate(#dif_lw = pd_tw - pd_lw, 
    dif_lw = pd_tw - pd_sem_24, 
    dif_s24 = pd_tw - pd_sem_24, 
    dif_s23 = pd_tw - pd_sem_23,
    dif_s22 = pd_tw - pd_sem_22)
  
  tabla_aduanas <- tabla_aduanas %>% mutate(dif_lm = pd_tm - pd_lm, 
                                            dif_m24 = pd_tm - pd_mes_24, 
                                            dif_m23 = pd_tm - pd_mes_23,
                                            dif_m22 = pd_tm - pd_mes_22)
  
  
  tabla_aduanas <- tabla_aduanas |> 
    mutate(
      dif_tm=rec_mes_25-rec_m24,
      var_tm=variacion(rec_mes_25, rec_m24)
    )
  
  
  ## Diferencias en Acumulado ----------------------------------------------------
  panel_rec_aduanas <- panel_rec_aduanas %>% ungroup()
  
  panel_24_uf <- panel_rec_aduanas %>% filter(year(fecha) == 2024, fecha <= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    rename(recaudacion_24 = recaudacion) %>% mutate(mes = month(fecha), dia = day(fecha)) %>% select(-fecha)
  panel_25_uf <- panel_rec_aduanas %>% filter(year(fecha) == 2025) %>% mutate(mes = month(fecha), dia = day(fecha))
  
  
  ### Diario ---------------------------------------------------------------------
  acum_dia <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    select(-mes, -dia) %>% 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(lista_acum_dia = jsonlite::toJSON(tibble(fecha, dif_rec, dif_acum = cumsum(dif_rec))))
  
  
  ### Semanal --------------------------------------------------------------------
  
  temp <- panel_24_uf |> 
    filter(dia==29 & mes==2)
  
  panel_24_ajuste_uf <- panel_24_uf |> 
    filter(as.Date(paste("2024", mes, dia, sep="-"))!=as.Date("2024-02-29"))
  
  for (imp in c("IEPS", "IVA", "OTROS", "TOTAL")) {
    for (adu in unique(temp$aduana)) {
      panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp, panel_24_ajuste_uf$aduana==adu),]$recaudacion_24 <- panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp, panel_24_ajuste_uf$aduana==adu),]$recaudacion_24 + temp[which(temp$impuesto==imp, temp$aduana==adu),]$recaudacion_24
    }
  }
  
  acum_sem <- panel_25_uf %>% left_join(panel_24_uf) %>% 
    mutate(fecha = floor_date(fecha, unit = 'week')) %>%
    group_by(fecha, aduana, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion), recaudacion_24 = sum(recaudacion_24)) %>%
    group_by(aduana, institucion, impuesto) %>% 
    summarise(lista_acum_sem = jsonlite::toJSON(tibble(fecha, acum_rec=cumsum(recaudacion), acum_rec_24=cumsum(recaudacion_24), dif_rec=(acum_rec-acum_rec_24))))
  
  
  ### Mensual --------------------------------------------------------------------
  acum_mes <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, aduana,institucion, impuesto) %>% summarise(recaudacion = sum(recaudacion), 
                                                                recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    group_by(aduana, institucion, impuesto) %>% 
    summarise(lista_acum_mes = jsonlite::toJSON(tibble(fecha, dif_rec,dif_acum = cumsum(dif_rec))))
  
  ## Variaciones -----------------------------------------------------------------
  # Ultima fecha 2024
  panel_24_uf <- panel_24_uf %>% group_by(aduana, impuesto) %>% summarise(recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(prom_24_uf = recaudacion_24/round(as.numeric(difftime(uf, '2025-01-01')+1))) %>% 
    select(aduana, impuesto, prom_24_uf)
  
  # Ultima fecha 2025
  panel_25_uf <- panel_25_uf %>% group_by(aduana, impuesto) %>% summarise(recaudacion_25 = sum(recaudacion)) %>% 
    mutate(prom_25_uf = recaudacion_25/round(as.numeric(difftime(uf, '2025-01-01')+1)) ) %>% 
    select(aduana, impuesto, prom_25_uf)
  
  
  # Diferencia y variación por mes
  tabla_aduanas <- tabla_aduanas %>% 
    mutate(dif_mes24 = prom_mes_25_uf - prom_mes_24_uf, 
           dif_mes23 = prom_mes_25_uf - prom_mes_23_uf, 
           dif_mes22 = prom_mes_25_uf - prom_mes_22_uf) %>% 
    mutate(var_mes_24 = variacion(prom_mes_25_uf, prom_mes_24_uf), 
           var_mes_23 = variacion(prom_mes_25_uf, prom_mes_23_uf),
           var_mes_22 = variacion(prom_mes_25_uf, prom_mes_22_uf))
  tabla_aduanas %>% glimpse()
  
  
  # Unir páneles y listas acumuladas
  tabla_aduanas <- tabla_aduanas %>%
    left_join(panel_24_uf) %>% 
    left_join(panel_25_uf)
  
  tabla_aduanas <- tabla_aduanas %>% 
    left_join(acum_dia) %>% 
    left_join(acum_sem) %>% 
    left_join(acum_mes) %>% 
    select(aduana, impuesto, institucion, everything())
  
  
  ## Listas para gráficas --------------------------------------------------------
  
  ### Diario ---------------------------------------------------------------------
  lista_dia <- panel_rec_aduanas  %>% group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% mutate(pd = recaudacion) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(lista_dia = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_dia$lista_dia[[5]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Semana ---------------------------------------------------------------------
  inicio_semana_actual <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% ungroup() %>% 
    slice_max(fecha, n = 1, with_ties = F) %>% pull(fecha)
  
  lista_sem <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week'))  %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    filter(year(fecha)>=2022) %>%  # Quitar primera línea rara
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, inicio_semana_actual)+1), recaudacion/7)) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(lista_sem = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_sem$lista_sem[[10]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Mes ------------------------------------------------------------------------
  lista_mes <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'month'))  %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, max(fecha))+1), recaudacion/days_in_month(fecha))) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(lista_mes = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_mes$lista_mes[[5]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Anual ----------------------------------------------------------------------
  lista_ano <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'year'))  %>% 
    group_by(fecha, aduana, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = case_when(fecha == max(fecha) ~ recaudacion/as.numeric(difftime(uf, max(fecha))),
                          fecha == '2024-01-01' ~ recaudacion/366,
                          T ~ recaudacion/365)) %>% 
    group_by(aduana, impuesto) %>% 
    summarise(lista_ano = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_ano$lista_ano[[49]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  tabla_aduanas <- tabla_aduanas %>% left_join(lista_dia) %>% left_join(lista_sem) %>%
    left_join(lista_mes) %>% left_join(lista_ano)
  
  
  ## Guardar ---------------------------------------------------------------------
  # Fix aduana SEMAR
  tabla_aduanas %>% ungroup() %>% count(institucion)
  tabla_aduanas <- tabla_aduanas %>% mutate(institucion = ifelse(aduana == 'CD. ACUÑA, COAH.', 'SEDENA', institucion))
  tabla_aduanas %>% ungroup() %>% count(institucion)
  # tabla_aduanas %>% glimpse()
  
  # save(tabla_aduanas, file = 'output/nominal/recaudacion_aduana.Rdata')
  
  
  
  # TABLA IMPUESTOS --------------------------------------------------------------
  
  # rm(list=setdiff(ls(), c('panel_rec_aduanas', 'variacion', 'uf')))
  
  ## Acumulados ------------------------------------------------------------------
  # Acumulado 2022
  acum_22 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2022) %>% 
    filter(fecha<= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) |> 
    group_by(impuesto) %>% 
    summarise(acum_22 = sum(recaudacion))
  
  # Acumulado 2023
  acum_23 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2023) %>% 
    filter(fecha<= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) |> 
    group_by(impuesto) %>% 
    summarise(acum_23 = sum(recaudacion))
  
  # Acumulado 2024
  acum_24 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2024) %>%
    filter(fecha <= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) |> 
    group_by(impuesto) %>% 
    summarise(acum_24 = sum(recaudacion))
  
  # Acumulado 2025
  acum_25 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2025) %>% 
    filter(fecha<= uf) |> 
    group_by(impuesto) %>% 
    summarise(acum_25 = sum(recaudacion))
  
  # Tabla general
  tabla_impuestos <- acum_25 %>% left_join(acum_25) |> left_join(acum_24) %>% left_join(acum_23) |> left_join(acum_22) %>% 
    # Diferecias en recaudación
    mutate(dif_24 = acum_25 - acum_24, 
           dif_23 = acum_25 - acum_23,
           dif_22 = acum_25 - acum_22)
  
  
  # Generar cambios porcentuales
  tabla_impuestos <- tabla_impuestos %>% group_by(impuesto) %>% 
    mutate(var_acum_24 = variacion(acum_25, acum_24), 
           var_acum_23 = variacion(acum_25, acum_23),
           var_acum_22 = variacion(acum_25, acum_22))
  
  
  ## Último periodo --------------------------------------------------------------
  
  ### Semanal --------------------------------------------------------------------
  # Ultima semana
  ultima_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tw = recaudacion) %>% 
    mutate(pd_tw = rec_tw/round(as.numeric(difftime(uf,fecha)+1))) %>% select(-fecha)
  
  # Penultima semana
  pu_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by( impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lw = recaudacion) %>% 
    mutate(pd_lw = rec_lw/7)  %>% 
    ungroup() %>% 
    select(impuesto, pd_lw)
  
  ### Mensual --------------------------------------------------------------------
  # Último mes
  ultimo_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) |> 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() |> 
    group_by(impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tm = recaudacion) %>% 
    mutate(pd_tm = rec_tm/round(as.numeric(difftime(uf,fecha)+1))) %>% select(-fecha)
  
  # Penúltimo mes
  pu_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by(impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lm = recaudacion) %>% 
    mutate(pd_lm = rec_lm/30)  %>% 
    ungroup() %>% 
    select(impuesto, pd_lm)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_impuestos <- tabla_impuestos |> 
    left_join(ultima_semana) |> 
    left_join(pu_semana) |> 
    left_join(ultimo_mes) |> 
    left_join(pu_mes)
  
  
  ## Mismo periodo del año anterior ----------------------------------------------
  ### Semanal --------------------------------------------------------------------
  # 2022
  sem_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(3))) %>% 
    group_by(impuesto) %>% 
    summarise(rec_s22 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_22 = rec_s22/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(impuesto, rec_s22, pd_sem_22)
  
  # 2023
  sem_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(2))) %>% 
    group_by(impuesto) %>% 
    summarise(rec_s23 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_23 = rec_s23/round(as.numeric(difftime(uf-years(2),fecha)+1))) %>% 
    select(impuesto, rec_s23, pd_sem_23)
  
  # 2024
  sem_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf-years(1))) %>% 
    group_by(impuesto) %>% 
    summarise(rec_s24 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_sem_24 = rec_s24/7) %>%
    # mutate(
    #   pd_sem_24 = rec_s24 / round(as.numeric(difftime(fecha, as.Date(paste(year(uf-years(1)), month(uf), "01", sep="-")), unit = "days") + 1))
    # ) %>% 
    select(impuesto, rec_s24, pd_sem_24)
  
  
  ### Mensual --------------------------------------------------------------------
  # 2022
  mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% mutate(fecha = floor_date(fecha, unit = 'month'), 
                                                                                       mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    group_by(impuesto) %>% 
    summarise(rec_m22 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_22 = rec_m22/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(impuesto, rec_m22, pd_mes_22)
  
  # 2023
  mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% mutate(fecha = floor_date(fecha, unit = 'month'), 
                                                                                       mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    group_by(impuesto) %>% 
    summarise(rec_m23 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_23 = rec_m23/round(as.numeric(difftime(uf-years(2),fecha)+1))) %>% 
    select(impuesto, rec_m23, pd_mes_23)
  
  # 2024
  mes_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    mutate(dia = day(fecha), mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    filter(dia <= day(uf)) |> 
    mutate(fecha = floor_date(fecha, unit = 'month')) |> 
    group_by(impuesto) %>% 
    summarise(rec_m24 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_24 = rec_m24/round(as.numeric(difftime(uf-years(1),fecha)+1))) %>% 
    select(impuesto, rec_m24, pd_mes_24)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_impuestos <- tabla_impuestos |> 
    left_join(sem_22) |> left_join(sem_23) |> left_join(sem_24) |> 
    left_join(mes_22) |> left_join(mes_23) |> left_join(mes_24)
  
  tabla_impuestos <- tabla_impuestos |> 
    group_by(impuesto) %>%
    mutate(var_tw = variacion(pd_tw, pd_sem_24), 
           var_24 = variacion(pd_tw, pd_sem_24), 
           var_23 = variacion(pd_tw, pd_sem_23),
           var_22 = variacion(pd_tw, pd_sem_22)) 
  
  tabla_impuestos <- tabla_impuestos |> 
    group_by(impuesto) %>%
    mutate(var_tm = variacion(pd_tm, pd_lw), 
           varm_24 = variacion(pd_tm, pd_mes_24), 
           varm_23 = variacion(pd_tm, pd_mes_23),
           varm_22 = variacion(pd_tm, pd_mes_22)) 
  
  
  ## Promedio diario -------------------------------------------------------------
  ### Lo que va del año ----------------------------------------------------------
  # 2024
  prom_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    filter(fecha<= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>%
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_24_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2023
  prom_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    filter(fecha<= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) %>%
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_23_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2022
  prom_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    filter(fecha<= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) %>%
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_22_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  
  ### Mes en curso ---------------------------------------------------------------
  # 2025
  prom_mes_25 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2025) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2025, month(uf), 1, sep = '-'))) %>% 
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_25_uf = recaudacion/round(as.numeric(difftime(uf,as.Date(paste(year(uf),month(uf),"01",sep="-")),units='days')+1))) %>% 
    mutate(rec_mes_25=recaudacion) |> 
    select(-recaudacion)
  
  # 2024
  prom_mes_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2024, month(uf), 1, sep = '-'))) %>% 
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_24_uf = recaudacion/days_in_month(month(uf-years(1)))) %>% 
    select(-recaudacion)
  
  # 2023
  prom_mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2023, month(uf), 1, sep = '-'))) %>% 
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_23_uf = recaudacion/days_in_month(month(uf-years(2)))) |> 
    select(-recaudacion)
  
  # 2022
  prom_mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2022, month(uf), 1, sep = '-'))) %>% 
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_22_uf = recaudacion/days_in_month(month(uf-years(2)))) %>% 
    select(-recaudacion)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_impuestos <- tabla_impuestos |>
    left_join(prom_24) |> left_join(prom_23) |> left_join(prom_22) |> 
    left_join(prom_mes_25) |> left_join(prom_mes_24) |> left_join(prom_mes_23) |> 
    left_join(prom_mes_22)
  
  
  ## Maximos y Mínimos -----------------------------------------------------------
  max_22 <- panel_rec_aduanas %>% 
    filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max_22 = recaudacion)%>% mutate(max_p_22 = max_22/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_impuestos <- tabla_impuestos %>% left_join(max_22 %>% select(-max_22) )
  
  max <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max = recaudacion)%>% mutate(p_max = max/days_in_month(fecha)) %>%
    select(-fecha)
  
  min <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(impuesto) %>% 
    slice_min(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(min = recaudacion)%>% mutate(p_min = min/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_impuestos <- tabla_impuestos %>% left_join(max %>% select(-max)) %>% left_join(min %>% select(-min))
  
  
  ## Estimaciones Fidel ----------------------------------------------------------
  
  recaudacion_sugerida <- read_excel("input/recaudacion_sugerida.xlsx") %>% clean_names()
  
  recaudacion_sugerida <- recaudacion_sugerida %>% select(aduana, cve_aduana, institucion, recaudacion_sugerida)
  
  # Read cat_aduana
  cat_aduanas <- read_csv("input/cat_aduanas.csv") %>% 
    mutate(cve_aduana = as.numeric(str_extract(cta, '..$')))
  
  
  recaudacion_sugerida <- recaudacion_sugerida %>% summarise(recaudacion_sugerida = sum(recaudacion_sugerida), 
                                                             p_sugerido = recaudacion_sugerida/30)
  
  tabla_impuestos$p_sugerido <- recaudacion_sugerida$p_sugerido
  tabla_impuestos <- tabla_impuestos %>% mutate(p_sugerido = ifelse(impuesto == 'TOTAL', p_sugerido, 0))
  
  
  
  ## Estimar Diferencias ---------------------------------------------------------
  
  tabla_impuestos %>% glimpse()
  
  tabla_impuestos <- tabla_impuestos %>% mutate(#dif_lw = pd_tw - pd_lw, 
    dif_lw = pd_tw - pd_sem_24, 
    dif_s24 = pd_tw - pd_sem_24, 
    dif_s23 = pd_tw - pd_sem_23,
    dif_s22 = pd_tw - pd_sem_22)
  
  tabla_impuestos <- tabla_impuestos %>% mutate(dif_lm = pd_tm - pd_lm, 
                                                dif_m24 = pd_tm - pd_mes_24, 
                                                dif_m23 = pd_tm - pd_mes_23,
                                                dif_m22 = pd_tm - pd_mes_22)
  
  tabla_impuestos <- tabla_impuestos |> 
    mutate(
      dif_tm=rec_mes_25-rec_m24,
      var_tm=variacion(rec_mes_25, rec_m24)
    )
  
  ## Diferencias en Acumulado ----------------------------------------------------
  
  panel_24_uf <- panel_rec_aduanas %>% filter(year(fecha) == 2024) %>% 
    filter(fecha<=uf-years(1)) |> 
    group_by(fecha, impuesto) %>% summarise(recaudacion_24 = sum(recaudacion, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(mes = month(fecha), dia = day(fecha)) %>% select(-fecha)
  panel_25_uf <- panel_rec_aduanas %>% filter(year(fecha) == 2025) %>% group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(mes = month(fecha), dia = day(fecha)) 
  
  
  ### Diario ---------------------------------------------------------------------
  acum_dia <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    select(-mes, -dia) %>% 
    group_by(impuesto) %>% 
    summarise(lista_acum_dia = jsonlite::toJSON(tibble(fecha, dif_rec,dif_acum = cumsum(dif_rec))))
  
  
  ### Semanal --------------------------------------------------------------------
  
  temp <- panel_24_uf |> 
    filter(dia==29 & mes==2)
  
  panel_24_ajuste_uf <- panel_24_uf |> 
    filter(as.Date(paste("2024", mes, dia, sep="-"))!=as.Date("2024-02-29"))

  for (imp in c("IEPS", "IVA", "OTROS", "TOTAL")) {
    panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp),]$recaudacion_24 <- panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp),]$recaudacion_24 + temp[which(temp$impuesto==imp),]$recaudacion_24
  }
  
  acum_sem <- panel_25_uf %>% right_join(panel_24_ajuste_uf) %>% 
    mutate(fecha = floor_date(fecha, unit = 'week')) %>%
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion), recaudacion_24 = sum(recaudacion_24)) %>%
    mutate(recaudacion=ifelse(is.na(recaudacion), 0, recaudacion)) |> 
    group_by(impuesto) %>% 
    summarise(lista_acum_sem = jsonlite::toJSON(tibble(fecha, acum_rec=cumsum(recaudacion), acum_rec_24=cumsum(recaudacion_24), dif_rec=(acum_rec-acum_rec_24))))
  
  
  ### Mensual --------------------------------------------------------------------
  acum_mes <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, impuesto) %>% summarise(recaudacion = sum(recaudacion), 
                                            recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    group_by(impuesto) %>% 
    summarise(lista_acum_mes = jsonlite::toJSON(tibble(fecha, dif_rec,dif_acum = cumsum(dif_rec))))
  
  
  ## Variaciones -----------------------------------------------------------------
  # Ultima fecha 2024
  panel_24_uf <- panel_24_uf %>% group_by(impuesto) %>% summarise(recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(prom_24_uf = recaudacion_24/round(as.numeric(difftime(uf, '2025-01-01')+1)) ) %>% 
    select(impuesto, prom_24_uf)
  
  # Ultima fecha 2025
  panel_25_uf <- panel_25_uf %>% group_by(impuesto) %>% summarise(recaudacion_25 = sum(recaudacion)) %>% 
    mutate(prom_25_uf = recaudacion_25/round(as.numeric(difftime(uf, '2025-01-01')+1)) ) %>% 
    select(impuesto, prom_25_uf)
  
  
  # Diferencia y variación por mes
  tabla_impuestos <- tabla_impuestos %>% 
    mutate(dif_mes24 = prom_mes_25_uf - prom_mes_24_uf, 
           dif_mes23 = prom_mes_25_uf - prom_mes_23_uf, 
           dif_mes22 = prom_mes_25_uf - prom_mes_22_uf) %>% 
    mutate(var_mes_24 = variacion(prom_mes_25_uf, prom_mes_24_uf), 
           var_mes_23 = variacion(prom_mes_25_uf, prom_mes_23_uf),
           var_mes_22 = variacion(prom_mes_25_uf, prom_mes_22_uf))
  tabla_impuestos %>% glimpse()
  
  
  # Unir páneles y listas acumuladas
  tabla_impuestos <- tabla_impuestos %>%
    left_join(panel_24_uf) %>% 
    left_join(panel_25_uf)
  
  tabla_impuestos <- tabla_impuestos %>% 
    left_join(acum_dia) %>% 
    left_join(acum_sem) %>% 
    left_join(acum_mes) %>% 
    select(impuesto, everything())
  
  
  ## Listas para gráficas --------------------------------------------------------
  
  ### Diario ---------------------------------------------------------------------
  lista_dia <- panel_rec_aduanas  %>% group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% mutate(pd = recaudacion) %>% 
    group_by(impuesto) %>% 
    summarise(lista_dia = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_dia$lista_dia[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Semana ---------------------------------------------------------------------
  inicio_semana_actual <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% ungroup() %>% 
    slice_max(fecha, n = 1, with_ties = F) %>% pull(fecha)
  
  lista_sem <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week'))  %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, inicio_semana_actual)+1), recaudacion/7)) %>% 
    filter(year(fecha)>=2022) %>% 
    group_by(impuesto) %>% 
    summarise(lista_sem = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_sem$lista_sem[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Mes ------------------------------------------------------------------------
  lista_mes <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'month'))  %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, max(fecha))+1), recaudacion/days_in_month(fecha))) %>% 
    group_by(impuesto) %>% 
    summarise(lista_mes = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_mes$lista_mes[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Anual ----------------------------------------------------------------------
  lista_ano <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'year'))  %>% 
    group_by(fecha, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = case_when(fecha == max(fecha) ~ recaudacion/as.numeric(difftime(uf, max(fecha))),
                          fecha == '2024-01-01' ~ recaudacion/366,
                          T ~ recaudacion/365)) %>% 
    group_by(impuesto) %>% 
    summarise(lista_ano = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_ano$lista_ano[3] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  tabla_impuestos <- tabla_impuestos %>% left_join(lista_dia) %>% left_join(lista_sem) %>% left_join(lista_mes) %>% left_join(lista_ano)
  
  
  ## Guardar ---------------------------------------------------------------------
  
  # save(tabla_impuestos, file = 'output/nominal/recaudacion_impuesto.Rdata')
  
  
  # TABLA INSTITUCION - IMPUESTO -------------------------------------------------
  
  # rm(list=setdiff(ls(), c('panel_rec_aduanas', 'variacion', 'uf')))
  # panel_rec_aduanas <- panel_rec_aduanas %>% mutate(institucion = ifelse(aduana == 'CD. ACUÑA, COAH.', 'SEDENA', institucion))
  panel_rec_aduanas %>% filter(is.na(institucion))
  
  
  ## Acumulados ------------------------------------------------------------------
  # Acumulado 2022
  acum_22 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2022, fecha<= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(acum_22 = sum(recaudacion))
  
  # Acumulado 2023
  acum_23 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2023, fecha<= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(acum_23 = sum(recaudacion))
  
  # Acumulado 2024
  acum_24 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2024, fecha<= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(acum_24 = sum(recaudacion))
  
  # Acumulado 2025
  acum_25 <- panel_rec_aduanas %>% ungroup() %>% 
    filter(year(fecha) == 2025, fecha<= uf) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(acum_25 = sum(recaudacion))
  
  # Tabla general
  tabla_institucion <- acum_25 %>% left_join(acum_25) |> left_join(acum_24) %>% left_join(acum_23) |> left_join(acum_22) %>% 
    # Diferecias en recaudación
    mutate(dif_24 = acum_25 - acum_24, 
           dif_23 = acum_25 - acum_23,
           dif_22 = acum_25 - acum_22)
  
  
  # Generar cambios porcentuales
  tabla_institucion <- tabla_institucion %>% group_by(impuesto) %>% 
    mutate(var_acum_24 = variacion(acum_25, acum_24), 
           var_acum_23 = variacion(acum_25, acum_23),
           var_acum_22 = variacion(acum_25, acum_22))
  
  
  ## Último periodo --------------------------------------------------------------
  
  ### Semanal --------------------------------------------------------------------
  # Ultima semana
  ultima_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tw = recaudacion) %>% 
    mutate(pd_tw = rec_tw/round(as.numeric(difftime(uf,fecha)+1))) %>% select(-fecha)
  
  # Penultima semana
  pu_semana <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% 
    group_by(fecha,institucion,  impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by(impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lw = recaudacion) %>% 
    mutate(pd_lw = rec_lw/7)  %>% 
    ungroup() %>% 
    select(institucion, impuesto, pd_lw)
  
  ### Mensual --------------------------------------------------------------------
  # Último mes
  ultimo_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) |> 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() |> 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 1) %>% ungroup() %>% 
    rename(rec_tm = recaudacion) %>% 
    mutate(pd_tm = rec_tm/round(as.numeric(difftime(uf,fecha)+1))) %>% select(-fecha)
  
  # Penúltimo mes
  pu_mes <- panel_rec_aduanas %>% ungroup() %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion, na.rm = T)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = fecha, n = 2) %>% ungroup() %>%
    group_by(impuesto) %>% 
    slice_min(order_by = fecha, n =1) %>% 
    rename(rec_lm = recaudacion) %>% 
    mutate(pd_lm = rec_lm/30)  %>% 
    ungroup() %>% 
    select(institucion, impuesto, pd_lm)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_institucion <- tabla_institucion |> 
    left_join(ultima_semana) |> 
    left_join(pu_semana) |> 
    left_join(ultimo_mes) |> 
    left_join(pu_mes)
  
  
  ## Mismo periodo del año anterior ----------------------------------------------
  ### Semanal --------------------------------------------------------------------
  # 2022
  sem_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf)) %>% 
    group_by(institucion, impuesto) %>% summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(pd_sem_22 = recaudacion/7) %>% 
    select(impuesto, pd_sem_22)
  
  # 2023
  sem_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf)) %>% 
    group_by(institucion, impuesto) %>% summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(pd_sem_23 = recaudacion/7) %>% select(-recaudacion)
  
  
  # 2024
  sem_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% mutate(fecha = floor_date(fecha, unit = 'week'), 
                                                                                       semana = week(fecha)) %>% 
    filter(semana == week(uf)) %>% 
    group_by(institucion, impuesto) %>% summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(pd_sem_24 = recaudacion/7) %>% select(-recaudacion)
  
  
  ### Mensual --------------------------------------------------------------------
  # 2022
  mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% mutate(fecha = floor_date(fecha, unit = 'month'), 
                                                                                       mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(rec_m22 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_22 = rec_m22/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(impuesto, rec_m22, pd_mes_22)
  
  # 2023
  mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% mutate(fecha = floor_date(fecha, unit = 'month'), 
                                                                                       mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(rec_m23 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_23 = rec_m23/round(as.numeric(difftime(uf-years(2),fecha)+1))) %>% 
    select(impuesto, rec_m23, pd_mes_23)
  
  # 2024
  mes_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) |> 
    mutate(dia = day(fecha), mes = month(fecha)) %>% 
    filter(mes == month(uf)) %>% 
    filter(dia <= day(uf)) |> 
    mutate(fecha = floor_date(fecha, unit = 'month')) |> 
    group_by(institucion, impuesto) %>% 
    summarise(rec_m24 = sum(recaudacion), fecha=min(fecha)) %>% 
    mutate(pd_mes_24 = rec_m24/round(as.numeric(difftime(uf-years(3),fecha)+1))) %>% 
    select(impuesto, rec_m24, pd_mes_24)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_institucion <- tabla_institucion |> 
    left_join(sem_22) |> left_join(sem_23) |> left_join(sem_24) |> 
    left_join(mes_22) |> left_join(mes_23) |> left_join(mes_24)
  
  tabla_institucion <- tabla_institucion |> 
    group_by(institucion, impuesto) %>%
    mutate(var_tw = variacion(pd_tw, pd_sem_24), 
           var_24 = variacion(pd_tw, pd_sem_24), 
           var_23 = variacion(pd_tw, pd_sem_23),
           var_22 = variacion(pd_tw, pd_sem_22)) 
  
  tabla_institucion <- tabla_institucion |> 
    group_by(institucion, impuesto) %>%
    mutate(var_tm = variacion(pd_tm, pd_lw), 
           varm_24 = variacion(pd_tm, pd_mes_24), 
           varm_23 = variacion(pd_tm, pd_mes_23),
           varm_22 = variacion(pd_tm, pd_mes_22)) 
  
  
  ## Promedio diario -------------------------------------------------------------
  ### Lo que va del año ----------------------------------------------------------
  # 2025
  prom_25 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2025) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_25_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2024
  prom_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    filter(fecha<= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_24_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2023
  prom_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    filter(fecha<= as.Date(paste(2023, month(uf), day(uf), sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_23_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  # 2022
  prom_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    filter(fecha<= as.Date(paste(2022, month(uf), day(uf), sep = '-'))) %>%
    group_by(impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_22_uf = recaudacion/round(as.numeric(difftime(uf, '2025-01-01',units = 'days')+1))
    ) %>% select(-recaudacion)
  
  
  ### Mes en curso ---------------------------------------------------------------
  # 2025
  prom_mes_25 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2025) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2025, month(uf), 1, sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_25_uf = recaudacion/round(as.numeric(difftime(uf,as.Date(paste(year(uf),month(uf),"01",sep="-")),units='days')+1))) %>% 
    mutate(rec_mes_25=recaudacion) |> 
    select(-recaudacion)
  
  # 2024
  prom_mes_24 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2024) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2024, month(uf), 1, sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_24_uf = recaudacion/days_in_month(month(uf-years(1)))) %>% 
    select(-recaudacion)
  
  # 2023
  prom_mes_23 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2023) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2023, month(uf), 1, sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_23_uf = recaudacion/days_in_month(month(uf-years(2)))) |> 
    select(-recaudacion)
  
  # 2022
  prom_mes_22 <- panel_rec_aduanas %>% ungroup() %>% filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    filter(fecha== as.Date(paste(2022, month(uf), 1, sep = '-'))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(prom_mes_22_uf = recaudacion/days_in_month(month(uf-years(2)))) %>% 
    select(-recaudacion)
  
  
  ### Tabla ----------------------------------------------------------------------
  tabla_institucion <- tabla_institucion |>
    left_join(prom_24) |> left_join(prom_23) |> left_join(prom_22) |> 
    left_join(prom_mes_25) |> left_join(prom_mes_24) |> left_join(prom_mes_23) |> 
    left_join(prom_mes_22)
  
  
  ## Maximos y Mínimos -----------------------------------------------------------
  max_22 <- panel_rec_aduanas %>% 
    filter(year(fecha) == 2022) %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max_22 = recaudacion)%>% mutate(max_p_22 = max_22/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_institucion <- tabla_institucion %>% left_join(max_22 %>% select(-max_22) )
  
  max <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_max(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(max = recaudacion)%>% mutate(p_max = max/days_in_month(fecha)) %>%
    select(-fecha)
  
  min <- panel_rec_aduanas %>% 
    mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    group_by(institucion, impuesto) %>% 
    slice_min(order_by = recaudacion, n = 1, with_ties = F) %>% 
    rename(min = recaudacion)%>% mutate(p_min = min/days_in_month(fecha)) %>%
    select(-fecha)
  
  tabla_institucion <- tabla_institucion %>% left_join(max %>% select(-max)) %>% left_join(min %>% select(-min))
  
  
  ## Estimaciones Fidel ----------------------------------------------------------
  
  recaudacion_sugerida <- read_excel("input/recaudacion_sugerida.xlsx") %>% clean_names()
  
  recaudacion_sugerida <- recaudacion_sugerida %>% select(aduana, cve_aduana, institucion, recaudacion_sugerida)
  
  # Read cat_aduana
  cat_aduanas <- read_csv("input/cat_aduanas.csv") %>% 
    mutate(cve_aduana = as.numeric(str_extract(cta, '..$')))
  
  
  recaudacion_sugerida <- recaudacion_sugerida %>% summarise(recaudacion_sugerida = sum(recaudacion_sugerida), 
                                                             p_sugerido = recaudacion_sugerida/30)
  
  tabla_institucion$p_sugerido <- recaudacion_sugerida$p_sugerido
  tabla_institucion <- tabla_institucion %>% mutate(p_sugerido = ifelse(impuesto == 'TOTAL', p_sugerido, 0))
  
  
  
  ## Estimar Diferencias ---------------------------------------------------------
  
  tabla_institucion %>% glimpse()
  
  tabla_institucion <- tabla_institucion %>% mutate(#dif_lw = pd_tw - pd_lw, 
    dif_lw = pd_tw - pd_sem_24, 
    dif_s24 = pd_tw - pd_sem_24, 
    dif_s23 = pd_tw - pd_sem_23,
    dif_s22 = pd_tw - pd_sem_22)
  
  tabla_institucion <- tabla_institucion %>% mutate(dif_lm = pd_tm - pd_lm, 
                                                    dif_m24 = pd_tm - pd_mes_24, 
                                                    dif_m23 = pd_tm - pd_mes_23,
                                                    dif_m22 = pd_tm - pd_mes_22)
  
  
    tabla_institucion <- tabla_institucion |> 
    mutate(
      dif_tm=rec_mes_25-rec_m24,
      var_tm=variacion(rec_mes_25, rec_m24)
    )
  
  
  ## Diferencias en Acumulado ----------------------------------------------------
  
  panel_24_uf <- panel_rec_aduanas %>%  filter(year(fecha) == 2024, fecha <= as.Date(paste(2024, month(uf), day(uf), sep = '-'))) %>% 
    group_by(fecha, institucion, impuesto) %>% summarise(recaudacion_24 = sum(recaudacion, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(mes = month(fecha), dia = day(fecha)) %>% select(-fecha)
  panel_25_uf <- panel_rec_aduanas %>% filter(year(fecha) == 2025) %>% group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% 
    mutate(mes = month(fecha), dia = day(fecha)) 
  
  
  ### Diario ---------------------------------------------------------------------
  acum_dia <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    select(-mes, -dia) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_acum_dia = jsonlite::toJSON(tibble(fecha, dif_rec,dif_acum = cumsum(dif_rec))))
  
  
  ### Semanal --------------------------------------------------------------------
  temp <- panel_24_uf |> 
    filter(dia==29 & mes==2)
  
  panel_24_ajuste_uf <- panel_24_uf |> 
    filter(as.Date(paste("2024", mes, dia, sep="-"))!=as.Date("2024-02-29"))
  
  for (imp in c("IEPS", "IVA", "OTROS", "TOTAL")) {
    for (inst in c("SEDENA", "SEMAR")) {
      panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp, panel_24_ajuste_uf$institucion==inst),]$recaudacion_24 <- panel_24_ajuste_uf[which(panel_24_ajuste_uf$dia==28 & panel_24_ajuste_uf$mes==2 & panel_24_ajuste_uf$impuesto==imp, panel_24_ajuste_uf$institucion==inst),]$recaudacion_24 + temp[which(temp$impuesto==imp, temp$institucion==inst),]$recaudacion_24
    }
  }
  
  acum_sem <- panel_25_uf %>% left_join(panel_24_uf) %>% 
    mutate(fecha = floor_date(fecha, unit = 'week')) %>%
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion), recaudacion_24 = sum(recaudacion_24)) %>%
    group_by(institucion, impuesto) %>% 
    summarise(lista_acum_sem = jsonlite::toJSON(tibble(fecha, acum_rec=cumsum(recaudacion), acum_rec_24=cumsum(recaudacion_24), dif_rec=(acum_rec-acum_rec_24))))
  
  
  ### Mensual --------------------------------------------------------------------
  acum_mes <- panel_25_uf %>% left_join(panel_24_uf) %>% mutate(fecha = floor_date(fecha, unit = 'month')) %>% 
    group_by(fecha, institucion, impuesto) %>% summarise(recaudacion = sum(recaudacion), 
                                                         recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(dif_rec = recaudacion-recaudacion_24) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_acum_mes = jsonlite::toJSON(tibble(fecha, dif_rec,dif_acum = cumsum(dif_rec))))
  
  
  ## Variaciones -----------------------------------------------------------------
  # Ultima fecha 2024
  panel_24_uf <- panel_24_uf %>% group_by(institucion, impuesto) %>% summarise(recaudacion_24 = sum(recaudacion_24)) %>% 
    mutate(prom_24_uf = recaudacion_24/round(as.numeric(difftime(uf, '2025-01-01')+1)) ) %>% 
    select(impuesto, prom_24_uf)
  
  # Ultima fecha 2025
  panel_25_uf <- panel_25_uf %>% group_by(institucion, impuesto) %>% summarise(recaudacion_25 = sum(recaudacion)) %>% 
    mutate(prom_25_uf = recaudacion_25/round(as.numeric(difftime(uf, '2025-01-01')+1)) ) %>% 
    select(impuesto, prom_25_uf)
  
  
  # Diferencia y variación por mes
  tabla_institucion <- tabla_institucion %>% 
    mutate(dif_mes24 = prom_mes_25_uf - prom_mes_24_uf, 
           dif_mes23 = prom_mes_25_uf - prom_mes_23_uf, 
           dif_mes22 = prom_mes_25_uf - prom_mes_22_uf) %>% 
    mutate(var_mes_24 = variacion(prom_mes_25_uf, prom_mes_24_uf), 
           var_mes_23 = variacion(prom_mes_25_uf, prom_mes_23_uf),
           var_mes_22 = variacion(prom_mes_25_uf, prom_mes_22_uf))
  tabla_institucion %>% glimpse()
  
  
  # Unir páneles y listas acumuladas
  tabla_institucion <- tabla_institucion %>%
    left_join(panel_24_uf) %>% 
    left_join(panel_25_uf)
  
  tabla_institucion <- tabla_institucion %>% 
    left_join(acum_dia) %>% 
    left_join(acum_sem) %>% 
    left_join(acum_mes)
  
  
  ## Listas para gráficas --------------------------------------------------------
  
  ### Diario ---------------------------------------------------------------------
  lista_dia <- panel_rec_aduanas  %>% group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% mutate(pd = recaudacion) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_dia = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_dia$lista_dia[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Semana ---------------------------------------------------------------------
  inicio_semana_actual <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week')) %>% ungroup() %>% 
    slice_max(fecha, n = 1, with_ties = F) %>% pull(fecha)
  
  lista_sem <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'week'))  %>% 
    group_by(fecha,institucion,  impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, inicio_semana_actual)+1), recaudacion/7)) %>% 
    filter(year(fecha)>=2022) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_sem = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_sem$lista_sem[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Mes ------------------------------------------------------------------------
  lista_mes <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'month'))  %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = ifelse(fecha == max(fecha), recaudacion/as.numeric(difftime(uf, max(fecha))+1), recaudacion/days_in_month(fecha))) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_mes = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_mes$lista_mes[[3]] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  ### Anual ----------------------------------------------------------------------
  lista_ano <- panel_rec_aduanas %>% mutate(fecha = floor_date(fecha, unit = 'year'))  %>% 
    group_by(fecha, institucion, impuesto) %>% 
    summarise(recaudacion = sum(recaudacion)) %>% ungroup() %>% 
    mutate(pd = case_when(fecha == max(fecha) ~ recaudacion/as.numeric(difftime(uf, max(fecha))),
                          fecha == '2024-01-01' ~ recaudacion/366,
                          T ~ recaudacion/365)) %>% 
    group_by(institucion, impuesto) %>% 
    summarise(lista_ano = jsonlite::toJSON(tibble(fecha, recaudacion, pd)))
  
  lista_ano$lista_ano[3] %>% fromJSON() %>% 
    ggplot(aes(as.Date(fecha), pd))+
    geom_line()
  
  
  tabla_institucion <- tabla_institucion %>% left_join(lista_dia) %>% left_join(lista_sem) %>% left_join(lista_mes) %>% left_join(lista_ano)
  
  # ME QUIERO MATAR
  
  ## Guardar ---------------------------------------------------------------------
  
  # save(tabla_institucion, file = 'output/nominal/recaudacion_institucion.Rdata')
  
  if (clase=="aduanas") {
    return(tabla_aduanas)
  } else if (clase=="impuestos") {
    return(tabla_impuestos)
  } else if (clase=="institucion") {
    return(tabla_institucion)
  }

}