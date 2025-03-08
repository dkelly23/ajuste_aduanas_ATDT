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

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, fixest, zoo)

# utilidades
options(scipen = 999)

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
  )

load("input/panel_aduanas.RData")
load("input/coef.RData")


# Script -----------------------------------------------------------------------

# Lag IGAE
igae_lag <- igae |> 
  mutate(fecha=fecha+years(1)) |> 
  filter(year(fecha)>=2025) |> 
  rename(igae_lag=igae)

# Dataframe para estimaciones
df <- data.frame(fecha=seq(as.Date("2022-01-01"), as.Date("2025-03-01"), by="month")) |> 
  left_join(igae) |> left_join(inpc) |> left_join(igae_lag) |> 
  mutate(
    mes=month(fecha),
    year=year(fecha)
  ) 

# Asignar valores vacíos
for (v in c("igae", "inpc")) {
  df[[v]] <- na.locf(df[[v]], na.rm = FALSE)
}

# Diferencias en IGAE
igae_base <- igae$igae |> tail(1)
df <- df |> mutate(changes_igae=(log(igae)-log(igae_base)))

# Diferencias en INPC
inpc_base <- inpc$inpc |> tail(1)
df <- df |>  mutate(changes_inpc=log(inpc)-log(inpc_base))

# Diferencias en IGAE_LAG
df <- df |> mutate(changes_igae_lag=log(igae)-log(igae_lag))

# Eliminar fecha para evitar conflictos en el join
df <- df |> select(-fecha)


## Deflactados -----------------------------------------------------------------
panel_rec_aduanas <- panel_rec_aduanas |> 
  mutate(
    mes=month(fecha),
    year=year(fecha)
  ) |> left_join(inpc |> mutate(mes=month(fecha), year=year(fecha)) |> select(-fecha)) 

panel_rec_aduanas[["inpc"]] <- na.locf(panel_rec_aduanas[["inpc"]], na.rm = FALSE)
panel_rec_aduanas[["inpc"]] <- na.locf(panel_rec_aduanas[["inpc"]], na.rm = FALSE, fromLast = TRUE)

panel_rec_aduanas <- panel_rec_aduanas |> 
  mutate(inpc=(inpc/inpc_base)) |> 
  mutate(
    recaudacion=recaudacion*(1/inpc)
  ) |> 
  select(-c(inpc, mes, year))


# Unir al panel final 
panel <- panel_rec_aduanas |>
  mutate(mes=month(fecha), year=year(fecha)) |>
  left_join(df) |> 
  select(-c(mes, year))


## Lag del Tipo de Cambio ------------------------------------------------------

# Unir tipo de cambio
panel <- panel |> left_join(tdc)
panel[["tdc"]] <- na.locf(panel[["tdc"]], na.rm = FALSE)
panel[["tdc"]] <- na.locf(panel[["tdc"]], na.rm = FALSE, fromLast = TRUE)

# Lag del tipo de cambio
tdc_lag <- tdc |> 
  mutate(fecha=fecha+years(1)) |> 
  rename(tdc_lag=tdc) |> 
  filter(year(fecha)>=2025)

# Unir lag del tipo de cambio
panel <- panel |> left_join(tdc_lag)
panel[["tdc_lag"]] <- na.locf(panel[["tdc_lag"]], na.rm = FALSE)

# Cambios en lag_tipo de cambio
panel <- panel |> mutate(changes_tdc_lag = ifelse(is.na(tdc_lag), 0, log(tdc) - log(tdc_lag)))

## Dataframe final -------------------------------------------------------------
panel_IVA <- panel |> 
  filter(impuesto=="IVA" | impuesto=="ADVALOREM") |> 
  # mutate(ajuste=(changes_tdc*coeff[1]+changes_igae*coeff[2])) |>
  mutate(ajuste=(changes_tdc_lag*coeff[1])+(changes_igae_lag*coeff[2])) |>
  mutate(rec_aj=recaudacion*(1-ajuste)) 

panel_no_IVA <- panel |> 
  filter(impuesto!="IVA" & impuesto!="ADVALOREM") |> 
  # mutate(ajuste=(changes_tdc*coeff[1]+changes_igae*coeff[2])) |>
  mutate(ajuste=(changes_igae_lag*coeff[2])) |>
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

par(bg=NA, mar=c(2,2,0,0), family="Montserrat", ann=F)

plot(as.Date(plot$fecha), plot$recaudacion, type = "l", lwd = 4, col = "#ADD8E6", axes = FALSE)
lines(as.Date(plot$fecha), plot$recaudacion_aj, t="l", lwd=4, col="#FFD580")

axis(1, col.axis = "white", col="white", at=seq(as.Date("2022-01-01"), as.Date("2025-02-01"), by="year"), labels = seq(as.Date("2022-01-01"), as.Date("2025-02-01"), by="year") |> format("%Y"))
axis(2, col.axis = "white", col="white")
box(col = "white")

abline(v="2025-01-01" |> as.Date(), col="#90EE90", lwd=2, lty="dashed")

legend(
  "top",
  legend = c("Ajustada", "Observada"),
  col = c("#FFD580", "#ADD8E6"),
  bty = "n",
  lwd = 4,
  cex = 1.2,
  text.col = "white"
)

suffix <- (Sys.Date()-days(1)) |> format("%Y%m%d") |> as.character()
dev.print(png, paste0("output/validacion_plots/observ_prediccion.png"), width = 3024, height = 1964, res = 400)
dev.off()

panel_def <- panel |> 
  select(fecha:impuesto, rec_aj) |> 
  rename(recaudacion=rec_aj)


# GUARDAR ----------------------------------------------------------------------
save(panel_def, file="input/panel_def.RData")
# temp <- left_join(panel_rec_aduanas, panel_def, by=c("fecha", "cta", "aduana", "institucion", "impuesto"), suffix=c("_nominal", "_tdc"))