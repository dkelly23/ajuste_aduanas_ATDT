# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         2-estimaciones.R
# Objetivo:       Modelar recaudación en aduanas.
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
pacman::p_load(tidyverse, readxl, showtext, janitor, fixest, zoo)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
# font_add_google("Montserrat")
# showtext_auto() 
options(scipen = 999)

# opciones
#create_project_folders()
#file.edit("~/.Rprofile")
#file.edit("/Users/danielkelly/.config/rstudio/templates/default.R")


# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

igae <- read_xlsx("input/igae.xlsx") |> 
  mutate(fecha=seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by="month"))

inpc <- read_xlsx("input/inpc.xlsx")|> 
  mutate(fecha=seq(as.Date("2021-01-01"), as.Date("2025-02-01"), by="month")) |> 
  filter(year(fecha)>=2022)

tdc <- read_xlsx("input/tdc.xlsx") |> 
  mutate(
    tdc=ifelse(tdc=="N/E", NA, tdc) |> as.numeric(),
    fecha=as.Date(fecha, format="%d/%m/%Y") |> format.Date("%Y-%m-%d") |> as.Date()
  ) |> 
  filter(year(fecha)>=2022)

load("input/panel_aduanas.RData")
load("input/coef.RData")


# Script -----------------------------------------------------------------------

# Tipo de cambio promedio por mes
tdc_mes <- tdc |> 
  mutate(fecha=floor_date(fecha, unit="month")) |> 
  group_by(fecha) |> 
  summarise(tdc=mean(tdc, na.rm=TRUE))

# Dataframe para estimaciones
df <- data.frame(fecha=seq(as.Date("2022-01-01"), as.Date("2025-03-01"), by="month")) |> 
  left_join(igae) |> left_join(inpc) |> left_join(tdc_mes) |> 
  mutate(
    mes=month(fecha),
    year=year(fecha)
  ) 

# Asignar valores vacíos
for (v in c("igae", "inpc", "tdc")) {
  df[[v]] <- na.locf(df[[v]], na.rm = FALSE)
  df[[v]] <- na.locf(df[[v]], na.rm = FALSE)
}

# Diferencias en TDC
tdc_base <- tdc_mes$tdc |> tail(1)
df <- df |> mutate(changes_tdc=log(tdc)-log(tdc_base))

# Diferencias en IGAE
igae_base <- igae$igae |> tail(1)
df <- df |> mutate(changes_igae=(log(igae)-log(igae_base)))

# Diferencias en INPC
inpc_base <- inpc$inpc |> tail(1)
df <- df |>  mutate(changes_inpc=log(inpc)-log(inpc_base))

# Eliminar fecha para evitar conflictos en el join
df <- df |> select(-fecha)


## Deflactados -----------------------------------------------------------------
panel_rec_aduanas <- panel_rec_aduanas |> 
  mutate(
    mes=month(fecha),
    year=year(fecha)
  ) |> left_join(inpc |> mutate(mes=month(fecha), year=year(fecha)) |> select(-fecha)) 

panel_rec_aduanas[["inpc"]] <- na.locf(panel_rec_aduanas[["inpc"]], na.rm = FALSE)
panel_rec_aduanas[["inpc"]] <- na.locf(panel_rec_aduanas[["inpc"]], na.rm = FALSE)

panel_rec_aduanas <- panel_rec_aduanas |> 
  mutate(inpc=(inpc/inpc_base)) |> 
  mutate(
    recaudacion=recaudacion*(1/inpc)
  ) |> 
  select(-c(inpc, mes, year))


## Dataframe final -------------------------------------------------------------
# Unir al panel final 
panel <- panel_rec_aduanas |>
  mutate(mes=month(fecha), year=year(fecha)) |>
  left_join(df) |> 
  select(-c(mes, year))

panel_IVA <- panel |> 
  filter(impuesto=="IVA") |> 
  mutate(ajuste=(changes_tdc*coeff[1]+changes_igae*coeff[2])) |> 
  mutate(rec_aj=recaudacion*(1-ajuste)) 

panel_no_IVA <- panel |> 
  filter(impuesto!="IVA") |> 
  mutate(ajuste=(changes_igae*coeff[2])) |> 
  mutate(rec_aj=recaudacion*(1-ajuste)) 

panel <- bind_rows(panel_IVA, panel_no_IVA) |> 
  arrange(fecha, aduana, impuesto)

# Cambio en nombre
panel_def <- panel |> 
  select(fecha:impuesto, rec_aj) |> 
  rename(recaudacion=rec_aj)


## Validación ------------------------------------------------------------------

plot <- panel |> 
  filter(impuesto=="IVA" & fecha<=as.Date("2025-02-28")) |> 
  group_by(month(fecha), year(fecha)) |>
  summarise(recaudacion=sum(recaudacion, na.rm=TRUE), recaudacion_aj=sum(rec_aj, na.rm=TRUE)) |> 
  arrange(`year(fecha)`, `month(fecha)`) |> 
  mutate(fecha=as.Date(paste(`year(fecha)`, `month(fecha)`, "01", sep="-")))

par(bg="white", mar=c(3.5,5.5,0,0), family="Montserrat", ann=F)

plot(plot$fecha, plot$recaudacion, t="l", lwd=4, col="#611232")
lines(plot$fecha, plot$recaudacion_aj, t="l", lwd=4, col="#235b4e")

abline(v=which(plot$`year(fecha)`==2023 & plot$`month(fecha)`==1), col="#bc955c", lwd=4, lty="dashed")
abline(v=which(plot$`year(fecha)`==2024 & plot$`month(fecha)`==9), col="#bc955c", lwd=4, lty="dashed")

legend(
  "topleft",
  legend=c("Ajustada", "Observada"),
  col=c("#002f2a","#611232"),
  bty="n",
  lwd=4,
  cex=1.2
)

suffix <- (Sys.Date()-days(1)) |> format("%Y%m%d") |> as.character()
dev.print(png, paste0("output/observ_prediccion_", suffix, ".png"), width = 3024, height = 1964, res = 400)
dev.off()


panel_def <- panel |> 
  select(fecha:impuesto, rec_aj) |> 
  rename(recaudacion=rec_aj)


# GUARDAR ----------------------------------------------------------------------
save(panel_def, file="input/panel_def.RData")