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

df_real <- tabla_impuestos_real[2,] |> pull(lista_acum_sem) |> fromJSON()
df_tdc <- tabla_impuestos_tdc[2,] |> pull(lista_acum_sem) |> fromJSON()
df_nominal <- tabla_impuestos[2,] |> pull(lista_acum_sem) |> fromJSON()

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