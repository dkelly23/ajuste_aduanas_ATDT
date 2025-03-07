# ______________________________________________________________________________
#
# Proyecto:       Proyecto 25 - Esfuerzo Recaudatorio
#                 
# Script:         2-elasticidades.R
# Objetivo:       Modelar recaudación en aduanas para extraer elasticidades
#                 respecto al TDC y el IGAE (crecimiento económico).
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          24-Febrero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, fixest, lubridate)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
options(scipen = 999)

# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

igae <- read_xlsx("input/igae.xlsx") |> 
  mutate(fecha=seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by="month"))

inpc_df <- read_xlsx("input/inpc.xlsx")|> 
  mutate(fecha=seq(as.Date("2021-01-01"), as.Date("2025-02-01"), by="month")) |> 
  filter(year(fecha)>=2022)

# tdc <- read_xlsx("input/tdc.xlsx") |> 
#   mutate(
#     tdc=ifelse(tdc=="N/E", NA, tdc) |> as.numeric(),
#     fecha=as.Date(fecha, format="%d/%m/%Y") |> format.Date("%Y-%m-%d") |> as.Date()
#   ) |> 
#   filter(year(fecha)>=2022)

tdc <- read.csv("input/tdc_real.csv") |> 
  mutate(fecha=as.Date(paste(anio, mes, "01", sep="-"))) |> 
  rename(tdc=idea) |> 
  select(-c(mes, anio))

volumen <- read_xlsx("input/volumen.xlsx") |> 
  mutate(Fecha=as.Date(Fecha)) |> 
  pivot_longer(!Fecha, names_to="aduana", values_to="volumen") |> 
  rename(fecha=Fecha)

last.date <- "2024-09-30" |> as.Date()

load("input/panel_aduanas.RData") 

panel_rec_aduanas <- panel_rec_aduanas |> 
  filter(fecha <= last.date - months(1))

first.date <- min(panel_rec_aduanas$fecha)  

# Read CPI
cpi <- read_csv("input/cpi.csv") |> 
  mutate(
    fecha=as.Date(fecha, format="%d/%m/%y"),
    mes=month(fecha),
    year=year(fecha)
  )

# temp <- tail(cpi, 1)
# temp$fecha <- as.Date(paste(year(last.date), month(last.date), "01", sep="-"))
# temp$mes <- month(last.date)
# temp$year <- year(last.date)
# 
# cpi <- bind_rows(cpi, temp)
# rm(temp)
# 
cpi_base <- tail(cpi$cpi, 1)

cpi <- cpi |>
  mutate(cpi=1/(cpi/cpi_base))


# Script -----------------------------------------------------------------------

# Tipo de cambio promedio por mes
tdc_mes <- tdc |> 
  mutate(fecha=floor_date(fecha, unit="month")) |> 
  group_by(fecha) |> 
  summarise(tdc=mean(tdc, na.rm=TRUE))


# Dataframe para estimaciones
df <- data.frame(fecha=seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by="month")) |> 
  left_join(igae) |> left_join(inpc_df) |> left_join(tdc_mes) |> left_join(cpi) |> 
  mutate(fecha=as.Date(fecha))

# Unir al panel final 
panel <- panel_rec_aduanas |> 
  mutate(fecha=floor_date(fecha, unit="month")) |>
  group_by(fecha, aduana, impuesto) |> 
  summarise(recaudacion=sum(recaudacion, na.rm=TRUE)) |> 
  left_join(df)


# Generar serie de Volumen Promedio Diario
volumen_diario <- data.frame(
  expand.grid(
    fecha = seq(as.Date(first.date), as.Date(last.date), by = "month"),
    aduana = panel_rec_aduanas$aduana |> unique()
  )
) |> 
  mutate(
    mes = month(fecha),
    anio = year(fecha)
  ) |> 
  left_join(
    volumen |> mutate(mes = month(fecha), anio = year(fecha))
  ) |> 
  mutate(
    volumen = ifelse(!is.na(volumen), volumen, 0)
  ) |> 
  select(-c(mes, anio))


# Unir panel_rec_aduanas
panel <- panel |> 
  left_join(volumen_diario)


# Transformación ---------------------------------------------------------------

# Diferencias agrupadas
panel_vf <- panel |>
  group_by(aduana, impuesto) |>
  arrange(fecha) |>
  mutate(
    tdc=ifelse(tdc==0, 0, log(tdc)),
    igae=ifelse(igae==0, 0, log(igae)),
    volumen=ifelse(volumen==0, 0, log(volumen)),
    recaudacion=ifelse(recaudacion==0, 0, log(recaudacion)),
    cpi=ifelse(cpi==0, 0, log(cpi)),
    inpc=ifelse(inpc==0, 0, log(inpc)),
    t=1:n()
  ) |>
  mutate(
    tdc=c(0,diff(tdc)),
    igae=c(0,diff(igae)),
    recaudacion=c(0,diff(recaudacion)),
    volumen=c(0, diff(volumen)),
    cpi=c(0, diff(cpi)),
    inpc=c(0, diff(inpc))
  )


## Estimaciones ----------------------------------------------------------------

# Efectos fijos aduana + slope por impuesto
for (imp in panel_vf$impuesto |> unique()) {
  assign(
    paste0("modelo_", imp),
    feols(
      data=panel_vf |> filter(impuesto==imp),
      fml=as.formula("recaudacion ~ tdc + igae + volumen | aduana"),
      vcov="hetero"
    )
  )
}

modelo_IVA |> summary()


## Extracción de Coeficientes --------------------------------------------------
e_tdc <- modelo_IVA$coefficients[1] |> as.numeric()
e_igae <- modelo_IVA$coefficients[2] |> as.numeric()

coeff <- c(e_tdc, e_igae)

# rm(list=setdiff(ls(), "coeff"))

save(coeff, file="input/coef.RData")