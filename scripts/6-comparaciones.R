# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         6-comparaciones.R
# Objetivo:       Generar outputs para el tablero de aduanas.
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          24-Febrero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

rm(list=ls())

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor)

# opciones
options(scipen = 999)

# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

load("input/panel_real.RData")
load("input/panel_def.RData")
load("input/panel_aduanas.RData")

# Script -----------------------------------------------------------------------

# Usamos funciones para generalizar facilmente a cualquier aduana e impuesto

## REMOVER FINES DE SEMANA -----------------------------------------------------

sin_fines <- function(impuesto="TOTAL",aduana=NA,dataset="panel_rec_aduanas") {
  
  df <- get(dataset)
  
  if (is.na(aduana)) {
    df <- df |> 
      filter(impuesto==impuesto) |> 
      group_by(fecha) |> 
      summarise(recaudacion=sum(recaudacion, na.rm=TRUE))
  } else {
    df <- df |> 
      filter(aduana==aduana, impuesto==impuesto) |> 
      group_by(fecha) |> 
      sumarise(recaudacion=sum(recaudacion, na.rm=TRUE))
  }

  # Filtrar para este momento del año
  out.fechas <- seq((as.Date(max(df$fecha))-years(1)), as.Date("2024-12-31"), by="day")
  df <- df |> 
    filter(year(fecha)>=2024) |> 
    filter(!fecha %in% out.fechas)
  
  # Eliminar fines de semana
  df <- df |> 
    ungroup() |> 
    mutate(weekday=format(fecha |> as.Date(), "%u")) |> 
    filter(!weekday %in% 6:7) |> 
    mutate(fecha_sem=ceiling_date(fecha, unit="week")) |> 
    group_by(fecha_sem) |> 
    summarise(recaudacion=sum(recaudacion, na.rm=TRUE)) |> 
    ungroup() |> 
    group_by(year(fecha_sem)) |> 
    mutate(n=1:n()) |> 
    filter(n!=1 & n!=10)
  
  df_24 <- df |> ungroup() |> filter(year(fecha_sem)==2024) |> select(-c(fecha_sem, `year(fecha_sem)`)) |> rename(recaudacion_24=recaudacion)
  df_25 <- df |> ungroup() |> filter(year(fecha_sem)==2025) |> select(-c(fecha_sem, `year(fecha_sem)`)) |> rename(recaudacion_25=recaudacion)
  
  # Final 
  df <- left_join(df_25, df_24) |> select(n, recaudacion_24, recaudacion_25)
  
  # Return
  return(df)
}

sin_fines(dataset="panel_def")


## SEMANAS ARTIFICIALES --------------------------------------------------------

semanas_locas <- function(impuesto="TOTAL",aduana=NA,dataset="panel_rec_aduanas") {
  
  df <- get(dataset)
  
  if (is.na(aduana)) {
    df <- df |> 
      filter(impuesto==impuesto) |> 
      group_by(fecha) |> 
      summarise(recaudacion=sum(recaudacion, na.rm=TRUE))
  } else {
    df <- df |> 
      filter(aduana==aduana, impuesto==impuesto) |> 
      group_by(fecha) |> 
      sumarise(recaudacion=sum(recaudacion, na.rm=TRUE))
  }
  
  # Filtrar para este momento del año
  out.fechas <- seq((as.Date(max(df$fecha))-years(1)), as.Date("2024-12-31"), by="day")
  df <- df |> 
    filter(year(fecha)>=2024) |> 
    filter(!fecha %in% out.fechas)
  
  # Máxima fecha
  last.date <- max(df$fecha)
  
  # Separar por año
  df_24 <- df |> ungroup() |> filter(year(fecha)==2024)
  df_25 <- df |> ungroup() |> filter(year(fecha)==2025)
  
  # Crear variable que asigna dia de la semana
  for (d in c("df_24", "df_25")) {
    temp <- get(d)  
    temp[["weekday"]] <- format(as.Date(temp$fecha), "%u") |> as.numeric()
    assign(d, temp)
  }
  
  # Dia de la semana de la máxima fecha
  last.date.week <- format(last.date, "%u") |> as.numeric()
  
  # Indice de cual día coincide
  index_24 <- which(df_24$weekday==4) |> max()
  df_24 <- df_24[1:index_24, ]
  
  # Crear variable que asigna número de semana
  for (d in c("df_24", "df_25")) {
    temp <- get(d)  
    temp <- temp |> 
      mutate(fecha=ceiling_date(fecha, unit="week")) |> 
      group_by(fecha) |> 
      summarise(recaudacion=sum(recaudacion, na.rm=TRUE)) |> 
      ungroup() |> 
      mutate(n=1:n())
    assign(d, temp)
  }
  
  # Cuantas semanas hay que sumarle a df_24
  ajuste <- tail(df_25$n, 1) - tail(df_24$n, 1)
  
  # Reescale of df_24
  df_24 <- df_24 |> mutate(n=n+ajuste)
  
  # Cambios de nombre
  df_25 <- df_25 |> select(-fecha) |> rename(recaudacion_25=recaudacion)
  df_24 <- df_24 |> select(-fecha) |> rename(recaudacion_24=recaudacion)
  
  # Final
  df <- right_join(df_25, df_24) |> mutate(n=1:n()) |> select(n, recaudacion_25, recaudacion_24)
  
  # Regresar
  return(df)
}

semanas_locas(dataset="panel_def")


## GRAFICAS --------------------------------------------------------------------

df <- semanas_locas(dataset="panel_def")

grafica <- function(df, dir, t="niveles",colores=c("#235b4e", "#9f2241", "#bc955c")){
  
  par(bg=NA, mar=c(3,4,2,1), ann=F, family="MontserratRoman-Bold")
  
  # Objetos a graficar
  y1=df$recaudacion_25
  y2=df$recaudacion_24
  x=df$n
  
  if (t=="niveles") {
    
    # Canvas
    plot(c(x[1]-0.5,x[length(x)]+0.5), c(min(c(y1, y2))*0.8, max(c(y1, y2)))*1.2, axes=F, t="n")
    # Puntos
    lines(x, y1, t="p", pch=16, col=colores[1], cex=1.4)
    lines(x, y2, t="p", pch=16, col=colores[2], cex=1.4)
    # Lineas
    lines(seq((x[1]-0.5),(tail(x, 1)+0.5),1),c(y1, y1[length(y1)]), t="s", lwd=4, col=colores[1])
    lines(seq((x[1]-0.5),(tail(x, 1)+0.5),1),c(y2, y2[length(y2)]), t="s", lwd=4, col=colores[2])
    # Ejes
    axis(1, at=x, labels=paste0("Sem. ", x), lwd=3, col="#777777")
    axis(1, at=c(-10000, 10000), lwd=3, col="#777777")
    axis(2,lwd=3, col="#777777")
    axis(2, at=c(-100000000, 100000000), lwd=3, col="#777777")
    # Texto
    mtext("Millones de Pesos", side=2, line=2.5, cex=1.2)
    # Leyenda
    legend(
      "top",
      legend = c("2025", "2024"),
      col = c(colores[1], colores[2]),
      bty = "n",
      lwd = 4,
      cex = 1,
      text.col = "black"
    )
    
  } else if (t=="diferencia") {
    
    y3=y1-y2
    
    # Canvas
    plot(c(x[1]-0.5,x[length(x)]+0.5), c(min(y3)*0.8, max(y3))*1.2, axes=F, t="n")
    # Puntos
    lines(x, y3, t="p", pch=16, col=colores[3], cex=1.4)
    # Lineas
    lines(seq((x[1]-0.5),(tail(x, 1)+0.5),1),c(y3, y3[length(y3)]), t="s", lwd=4, col=colores[3])
    # Linea en 0
    abline(h=0, lty=2, lwd=2, col=colores[2])
    # Ejes
    axis(1, at=x, labels=paste0("Sem. ", x), lwd=3, col="#777777")
    axis(1, at=c(-10000, 10000), lwd=3, col="#777777")
    axis(2,lwd=3, col="#777777")
    axis(2, at=c(-100000000, 100000000), lwd=3, col="#777777")
    # Texto
    mtext("Millones de Pesos", side=2, line=2.5, cex=1.2)
    
  }
  
  dev.print(png, file=paste("output/graficas/", dir, ".png"), width = 3024, height = 1964, res = 400)
  dev.off()
  
}


## GENERAR GRAFICAS ------------------------------------------------------------

# Sin Fines
grafica(sin_fines(dataset="panel_def"), "sin_fines_tdc")
grafica(sin_fines(dataset="panel_def"), "sin_fines_tdc_dif", t="diferencia")

# Semanas locas
grafica(semanas_locas(dataset="panel_def"), "semanas_locas_tdc")
grafica(semanas_locas(dataset="panel_def"), "semanas_locas_tdc_dif", t="diferencia")