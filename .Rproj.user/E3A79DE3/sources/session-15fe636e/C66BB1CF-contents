# ______________________________________________________________________________
#
# Proyecto:       Entrega 25 - Procesamiento Aduanas
#                 
# Script:         1-panel.R
# Objetivo:       Generar el panel de recaudación que el resto de scripts usan
#                 como input.
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          24-Febrero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, zoo)

# CÓDIGO _______________________________________________________________________

# Manually set date
# last.date <- as.Date("2025-02-27")
last.date <- Sys.Date()-days(2)

# Read cat_aduanas
cat_aduanas <- read_csv("input/cat_aduanas.csv")

# Read TdeC
tdc <- read_xlsx("input/tdc.xlsx") |> 
  mutate(
    tdc=ifelse(tdc=="N/E", NA, tdc) |> as.numeric(),
    fecha=as.Date(fecha, format="%d/%m/%Y") |> format.Date("%Y-%m-%d") |> as.Date()
  )

tdc$tdc <- na.locf(tdc$tdc, na.rm = FALSE)
tdc$tdc <- na.locf(tdc$tdc, fromLast = TRUE)

# Read INPC / ene 25
inpc_df <- read_xlsx("input/inpc.xlsx")[,2] |>
  mutate(fecha=seq(as.Date("2021-01-01"), as.Date("2025-02-01"), by="month"))

inpc_df <- bind_rows(inpc_df, data.frame(fecha=c(as.Date("2025-01-01"), as.Date("2025-02-01")), inpc=c(138.343, 138.343))) |>
  mutate(
    mes=month(fecha),
    year=year(fecha)
  ) |>
  select(-fecha) |>
  mutate(
    # inpc=(inpc/138.343)*100
    inpc=(inpc/92.04369)*100
  ) |> 
  bind_rows(
            data.frame(
              inpc=100,mes=3,year=2025
            ))



# Calcular diferencias porcentuales con el tipo de cambio
tdc_mes <- tdc |> 
  mutate(fecha=floor_date(fecha, unit="month")) |> 
  group_by(fecha) |> 
  summarise(tdc=mean(tdc, na.rm=TRUE)) |> 
  filter(year(fecha)>=2024)


# Script -----------------------------------------------------------------------

## Loop para importar datos ----------------------------------------------------

load(file = 'input/aduanas_pre_25.Rdata')

data_2025 <- tibble()

to_import_25 <- list.files("input/aduanas_25/")

for (i in to_import_25){
  # Take sheet names
  sheet_names <- excel_sheets(paste0("input/aduanas_25/", i))
  # Take everything not NA, i.e TOTAL
  sheet_names <- as.numeric(sheet_names)[!is.na(as.numeric(sheet_names))]
  
  for (j in sheet_names){
    # NMMS, no existe 31 de abril pero existe la fila, tenemos que hacer un tryCatch de eso
    tryCatch({
      if(!is.na(as.Date(paste(2024,as.numeric(str_extract(i, "..") ), j, sep = '-' )))){
        data_2025 = rbind(data_2025, read_excel(paste0("input/aduanas_25/",i), sheet = j, skip = 5) %>% 
                            select(1:9) %>% 
                            mutate(fecha = as.Date(paste(2025,as.numeric(str_extract(i, "..") ), j, sep = '-' )) )
        ) # LPTM no todos tienen las mismas columnas!!!! Vamos a forzar tomar las primeras 9
      }
    }, error = function(e) {
      message("Invalid date, skipping execution.")
    })
  }
}

feb_25 <- tibble()

for(i in 1:28){
  feb_25 = rbind(feb_25, read_excel(paste0("input/aduanas_25/02_25.xlsx"), sheet = i, skip = 5) %>%
                   select(1:9) %>%
                   mutate(fecha = as.Date(paste(2025,2, i, sep = '-' )) )
  )
}

data_2025 <- rbind(data_2025, feb_25 %>% rename(ADVALOREM = IGI))


mar_25 <- tibble()

for(i in 1:31){
  mar_25 = rbind(mar_25, read_excel(paste0("input/aduanas_25/03_25.xlsx"), sheet = i, skip = 5) %>%
                   select(1:9) %>%
                   mutate(fecha = as.Date(paste(2025,3, i, sep = '-' )) )
  )
}

data_2025 <- rbind(data_2025, mar_25 %>% rename(ADVALOREM = IGI))


data_2025 %>% count(fecha) %>% tail()


# Date cutoff
data_2025 <- data_2025 %>% filter(fecha<=last.date)


## Limpieza de la base de datos ------------------------------------------------
# Hacemos una única bdd
panel_rec_aduanas <- rbind(data_2022, data_2023, data_2024, data_2025)
uf <- max(panel_rec_aduanas$fecha)

# limpiamos nombres
panel_rec_aduanas <- panel_rec_aduanas %>% clean_names() %>% left_join(cat_aduanas %>% select(-cta)) %>% 
  select(fecha, cta, aduana, institucion, everything())

# hacemos numéricos los valores
panel_rec_aduanas <- panel_rec_aduanas %>% mutate(total = str_remove_all(total, ',')) %>%  # stupid catch
  mutate(iva = as.numeric(iva), advalorem = as.numeric(advalorem), dta = as.numeric(dta), 
         ieps = as.numeric(ieps), isan = as.numeric(isan), otros = as.numeric(otros), 
         total = as.numeric(total))

# selecionamos y hacemos panel para agrupar
panel_rec_aduanas <- panel_rec_aduanas  %>% 
  pivot_longer(cols = c(iva,advalorem, dta, ieps, isan, otros,total), names_to = 'impuesto', values_to = 'recaudacion') %>% 
  mutate(impuesto = str_to_upper(impuesto))

# Quitamos ruido
panel_rec_aduanas <- panel_rec_aduanas  %>% filter(!is.na(aduana), aduana!= 'T O T A L.') 

# Hacemos millones de pesos
panel_rec_aduanas <- panel_rec_aduanas %>% mutate(recaudacion = ifelse(is.na(recaudacion), 0, recaudacion/1000000))


# GUARDAR ----------------------------------------------------------------------
save(panel_rec_aduanas, file="input/panel_aduanas.RData")