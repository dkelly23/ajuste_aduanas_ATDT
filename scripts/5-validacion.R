# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         4-validación.R
# Objetivo:       Validar RDatas generados para el tablero antes de montarse.
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
pacman::p_load(tidyverse, readxl, showtext, janitor, jsonlite)


# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

load('output/tdc/bases_primera_parte_tdc.Rdata')
load('output/nominal/bases_primera_parte.Rdata')
load('output/real/bases_primera_parte_real.Rdata')

# Script -----------------------------------------------------------------------

df_real <- tabla_impuestos_real[4,] |> pull(lista_acum_sem) |> fromJSON()
df_tdc <- tabla_impuestos_tdc[4,] |> pull(lista_acum_sem) |> fromJSON()
df_nominal <- tabla_impuestos[4,] |> pull(lista_acum_sem) |> fromJSON()

x <- 1:8
y1 <- df_nominal$dif_rec
y2 <- df_real$dif_rec
y3 <- df_tdc$dif_rec


"nominal"
y1

"real"
y2

"tdc"
y3

par(mar=c(2,2,1,1))
plot(y3, t="l", lwd=2, col="#235b4e")


# Comparar recaudacion por semana
df_real <- tabla_impuestos_real[4,] |> pull(lista_sem) |> fromJSON()
df_tdc <- tabla_impuestos_tdc[4,] |> pull(lista_sem) |> fromJSON()
df_nominal <- tabla_impuestos[4,] |> pull(lista_sem) |> fromJSON()

plot_24 <- df_real |> 
  filter(year(fecha)==2024) |> 
  mutate(n=1:n())

plot_25 <- df_real |> 
  filter(year(fecha)==2025) |> 
  mutate(n=1:n())

par(mar=c(2,2,1,1))
plot(plot_24$n, plot_24$recaudacion, t="l", col="#235b4e", lwd=2)
lines(plot_25$n, plot_25$recaudacion, t="l", col="#9f2241", lwd=2)


# SEGUNDA PARTE ----------------------------------------------------------------
rm(list=ls())
load('output/real/base_segunda_parte_real.Rdata')
load('output/nominal/base_segunda_parte.Rdata')
load('output/tdc/base_segunda_parte_tdc.Rdata')

tabla_aduanas_2p_tdc[,c(1, 16, 17, 19, 21)] |> view()
tabla_aduanas_2p_real[,c(1, 16, 17, 19, 21)] |> view()
tabla_aduanas_2p[,c(1, 16, 17, 19, 21)] |> view()
