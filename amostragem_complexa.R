library(sf); library(capm); library(tidyverse); library(ggsn)
rm(list = ls())

## Dados do IBGE
basico <- read_csv2("Basico_PR.csv", locale = locale(encoding = "latin1"))
psu_ssu <- basico %>%
  filter(Nome_do_municipio == "PINHAIS") %>%
  select(Cod_setor)
doms <- read_csv2("Domicilio01_PR.csv", locale = locale(encoding = "latin1"))
psu_ssu <- merge(psu_ssu, doms[ , c(1, 3)], by = "Cod_setor")
write_csv(psu_ssu, "psu_ssu.csv")

mapa <- read_sf("./mapa_estado/41SEE250GC_SIR.shp")
mapa <- mapa %>%
  filter(NM_MUNICIP == "PINHAIS") %>%
  transmute(Cod_setor = as.numeric(CD_GEOCODI)) %>%
  left_join(psu_ssu, by = "Cod_setor")

## Unidades amostrais
psu <- SamplePPS(psu.ssu = psu_ssu, psu = 45)
ssu <- SampleSystematic(psu.ssu = psu, su = 30, write = TRUE)

MapkmlPSU(shape = mapa, psu = psu[, 1], id = 1)
