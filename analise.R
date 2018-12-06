library(gridExtra); library(tidyverse); library(jtools); library(weights);
library(capm); library(sf); library(ggsn); library(circlize)

rm(list = ls())
cat("\014")

## Exemplo com dados reais de Pinhais, Paraná, descritos em
# Baquero, Oswaldo Santos, Solange Marconcin, Adriel Rocha, and Rita de Cassia
# Maria Garcia. "Companion animal demography and population management in
# Pinhais, Brazil." Preventive veterinary medicine 158 (2018): 169-177.

## Dados das entrevistas -------------------------------------------------------
banco1 <- read_csv("banco1.csv", locale = locale(encoding = "latin1"))
psu_ssu <- read_csv("psu_ssu.csv")
banco2 <- read_csv("banco2.csv", locale = locale(encoding = "latin1"))
dogs <- filter(banco2, especie == "cao" | especie3 == "cao")
cats <- filter(banco2, especie == "gato" | especie3 == "gato")

## Status da entrevista
FreqTab(banco1$entrevista)

## Estimativas -----------------------------------------------------------------

# Populacao humana em 2017
pop_municipio2017 <- 129445

# Proporcao de domicilios com caes e gatos
names(banco1)
banco1$tem_caes <- ifelse(banco1$quantos_caes > 0, 1, 0)
banco1$tem_gatos <- ifelse(banco1$quantos_gatos > 0, 1, 0)

# Selecao das variaveis de interesse e remocao de observacoes com
# informacoes faltantes
banco1_comp <- banco1 %>%
  select(ID, codigo_do_setor_censitario, quantos_caes,
         quantos_gatos, quantas_pessoas, tem_caes, tem_gatos) %>%
  filter(complete.cases(.))

# Desenhos amostrais
design <- DesignSurvey(banco1_comp, psu.ssu = psu_ssu,
                       psu.col = "codigo_do_setor_censitario",
                       ssu.col = "ID")
design_cal <- DesignSurvey(banco1_comp, psu.ssu = psu_ssu,
                           psu.col = "codigo_do_setor_censitario",
                           ssu.col = "ID",
                           cal.col = "quantas_pessoas",
                           cal.N = pop_municipio2017)
summary(design)
summary(design_cal)

# Resultados
(est <- SummarySurvey(design, c(rep("total", 3), "prop", "prop")))
(est_cal <- SummarySurvey(design_cal, c(rep("total", 3), "prop", "prop")))

## Correlacoes

# Caes
cor.test(banco1_comp$quantas_pessoas, banco1_comp$quantos_caes)
h_d_cor <- svycor(~quantas_pessoas + quantos_caes, design_cal, sig.stats = TRUE)
h_d_cor$cors[1, 2]
h_d_cor$cors[1, 2] - h_d_cor$std.err[1, 2] * 1.96
h_d_cor$cors[1, 2] + h_d_cor$std.err[1, 2] * 1.96

# Gatos
cor.test(banco1_comp$quantas_pessoas, banco1_comp$quantos_gatos)
h_c_cor <- svycor(~quantas_pessoas + quantos_gatos, design_cal, sig.stats = TRUE)
h_c_cor$cors[1, 2]
h_c_cor$cors[1, 2] - h_c_cor$std.err[1, 2] * 1.96
h_c_cor$cors[1, 2] + h_c_cor$std.err[1, 2] * 1.96

## Caes, gatos e pessoas por domicilio
summary(banco1$quantos_caes)
summary(banco1$quantos_gatos)
summary(banco1$quantas_pessoas)

#tiff("./fig2.tiff", width = 2e3, height = 2e3, res = 300)
PlotHHxSpecies(banco1_comp, c("quantos_caes", "quantos_gatos", "quantas_pessoas")) +
  scale_fill_manual(values = c("black", "orange", "skyblue"),
                    labels = c("Humans", "Dogs", "Cats")) +
  scale_y_continuous(breaks = seq(0, .9, by = .1)) +
  xlim(as.character(0:9))
#dev.off()

## Razao de sexos

# Caes
FreqTab(c(dogs$sexo, dogs$sexo3))
prop_sex <-table(c(dogs$sexo, dogs$sexo3))
prop.test(prop_sex["femea"], sum(prop_sex), p = .5)

# Gatos
FreqTab(c(cats$sexo, cats$sexo3))
prop_sex_c <- table(c(cats$sexo, cats$sexo3))
prop.test(prop_sex_c["femea"], sum(prop_sex_c), p = .5)

## Esterilizacao e sexo

# Caes
(prop_sex_ster <- xtabs(~ c(dogs$castrado, dogs$castrado3) +
                          c(dogs$sexo, dogs$sexo3)))
prop.test(prop_sex_ster["sim", ], apply(prop_sex_ster, 2, sum))

# Gatos
(prop_sex_ster_c <- xtabs(~ c(cats$castrado, cats$castrado3) +
                            c(cats$sexo, cats$sexo3)))
prop.test(prop_sex_ster_c["sim", ], apply(prop_sex_ster_c, 2, sum))

## Status semi-domiciliado e sexo

# Caes
(prop_sex_fr <- xtabs(~ dogs$sai_sozinho_a_rua + dogs$sexo))
prop.test(prop_sex_fr["sim", ], apply(prop_sex_fr, 2, sum))

# Gatos
(prop_sex_fr_c <- xtabs(~ cats$sai_sozinho_a_rua + cats$sexo))
prop.test(prop_sex_fr_c["sim", ], apply(prop_sex_fr_c, 2, sum))

## Status semi-domiciliado e esterilizacao

# Caes
(prop_sex_ster <- xtabs(~ dogs$sai_sozinho_a_rua + dogs$castrado))
prop.test(prop_sex_ster["sim", ], apply(prop_sex_ster, 2, sum))

# Gatos
(prop_sex_ster_c <- xtabs(~ cats$sai_sozinho_a_rua + cats$castrado))
prop.test(prop_sex_ster_c["sim", ], apply(prop_sex_ster_c, 2, sum))


## Idade
summary(c(dogs$idade, dogs$idade3))
summary(c(cats$idade, cats$idade3))

## Piramides populacionais

# Caes
pp_age <- c(dogs$idade, dogs$idade3)
levels(as.factor(dogs$sexo))
pp_sex <- factor(c(dogs$sexo, dogs$sexo3),
                 levels = c("femea", "macho"),
                 labels = c("Female", "Male"))
levels(as.factor(dogs$castrado))
pp_ster <- factor(c(dogs$castrado, dogs$castrado3),
                  levels = c("nao", "sim"),
                  labels = c("No", "Yes"))
pop_pir <- data.frame(pp_age, pp_sex, pp_ster)
PlotPopPyramid(pop_pir, "pp_age", "pp_sex", "pp_ster")

pp_age2 <- cut(c(dogs$idade, dogs$idade3),
               breaks = c(0, 1, 3, 5, 7, 9, 11, 13, 15,
                          max(pp_age, na.rm = TRUE)),
               labels = c("<1", "1-3", "3-5", "5-7", "7-9",
                          "9-11", "11-13", "13-15", ">15"),
               include.lowest = TRUE)
pop_pir2 <- data.frame(pp_age2, pp_sex, pp_ster)

#tiff("./fig3a.tiff", width = 2e3, height = 2e3, res = 300)
PlotPopPyramid(pop_pir2, "pp_age2", "pp_sex", "pp_ster",
               inner.color = "LightBlue",
               outer.color = "DarkRed")
#dev.off()

# Gatos
pp_age_c <- c(cats$idade, cats$idade3)
levels(as.factor(cats$sexo))
pp_sex_c <- factor(c(cats$sexo, cats$sexo3),
                   levels = c("femea", "macho"),
                   labels = c("Female", "Male"))
levels(as.factor(cats$castrado))
pp_ster_c <- factor(c(cats$castrado, cats$castrado3),
                    levels = c("nao", "sim"),
                    labels = c("No", "Yes"))
pop_pir_c <- data.frame(pp_age_c, pp_sex_c, pp_ster_c)

PlotPopPyramid(pop_pir_c, "pp_age_c", "pp_sex_c", "pp_ster_c")

pp_age2_c <- cut(c(cats$idade, cats$idade3),
                 breaks = c(0, 1, 3, 5, 7, max(pp_age_c, na.rm = TRUE)),
                 labels = c("<1", "1-3", "3-5", "5-7", ">7"),
                 include.lowest = TRUE)
pop_pir2_c <- data.frame(pp_age2_c, pp_sex_c, pp_ster_c)

#tiff("./fig3b.tiff", width = 2e3, height = 2e3, res = 300)
PlotPopPyramid(pop_pir2_c, "pp_age2_c", "pp_sex_c", "pp_ster_c",
               inner.color = "LightBlue",
               outer.color = "DarkRed")
#dev.off()

## Tamanho das nihadas
summary(dogs$filhotes_u12[dogs$filhotes_u12 > 0])
summary(cats$filhotes_u12[cats$filhotes_u12 > 0])

## Proporcao de femeas que tiveram cria
sum(dogs$filhotes_u12 > 0, na.rm = T) / sum(dogs$sexo == "femea", na.rm = T)
sum(cats$filhotes_u12 > 0, na.rm = T) / sum(cats$sexo == "femea", na.rm = T)

## Proporcao de filhotes em relacao ao total da populacao
sum(dogs$filhotes_u12, na.rm = TRUE) / nrow(dogs)
sum(cats$filhotes_u12, na.rm = TRUE) / nrow(cats)

## Aquisicao
FreqTab(dogs$aquisicao)
FreqTab(cats$aquisicao)

# Cidade de aquisicao

# Caes

# Frequencias por estado
FreqTab(dogs$estado_de_aquisicao)

# Numero de imigrantes por cidade de origem
(cities <- cities2 <- FreqTab(dogs$municipio_de_aquisicao))

# Numero de imigrantes por cidade de origem (versao para graficar)
cities <- cities[, c(1, 3)]
names(cities)[1] <- "NM_MUNICIP"

# Mapa com as cidades de Parana
pr <- read_sf("./mapa_estado/41MUE250GC_SIR.shp", options = "ENCODING=windows-1252")
names(pr)
pr$NM_MUNICIP <- str_to_title(iconv(pr$NM_MUNICIP, to='ASCII//TRANSLIT'))
pr$NM_MUNICIP <- gsub("Dos", "dos", pr$NM_MUNICIP)
pr <- left_join(pr, cities)
pr[pr$NM_MUNICIP == "Pinhais", "Proportion"] <- NA

(map1 <- ggplot(data = pr, aes(fill = Proportion)) +
    geom_sf(size = .08) +
    geom_sf(data = pr[pr$NM_MUNICIP == "Pinhais", ],
            aes(color = ""), fill = "black") +
    scale_fill_continuous(low = c("lightblue", "yellow"),
                          high = c("yellow", "red"), 
                          na.value = "white", breaks = c(.001, .01, .02, .09)) +
    scale_color_manual(name = "Pinhais", values = "black") +
    blank())

# Gatos

# Frequencias por estado de aquisicao
FreqTab(cats$estado_de_aquisicao)

# Numero de imigrantes por cidade de origem
(cities_c <- cities2_c <- FreqTab(cats$municipio_de_aquisicao))

# Numero de imigrantes por cidade de origem (versao para graficar)
cities_c <- cities_c[, c(1, 3)]
names(cities_c)[1] <- "NM_MUNICIP"

# Mapa do Brasil
brazil <- read_sf("./mapa_brasil/BRUFE250GC_SIR.shp") %>%
  mutate(NM_ESTADO = ifelse(NM_ESTADO == "PARANÁ", "Paraná", "Brazil"))

#tiff("./fig4a.tiff", width = 4e3, height = 1e3, res = 300)
ggplot() +
  geom_sf(data = brazil, aes(fill = NM_ESTADO), color = "gray") +
  scale_fill_manual(name = "",
                    values = c("Brazil" = "gray", "Paraná" = "orange")) +
  blank() +
  scalebar(brazil, dist = 500, dd2km = TRUE, model = "WGS84", st.size = 3,
           anchor = c(x = -35, y = -32))
#dev.off()

# Mapa das cidades de Parana
pr_c <- read_sf("./mapa_estado/41MUE250GC_SIR.shp", options = "ENCODING=windows-1252")
pr_c$NM_MUNICIP <- str_to_title(iconv(pr_c$NM_MUNICIP, to='ASCII//TRANSLIT'))
pr_c$NM_MUNICIP <- gsub("Dos", "dos", pr_c$NM_MUNICIP)
pr_c <- left_join(pr_c, cities_c)
pr_c[pr_c$NM_MUNICIP == "Pinhais", "Proportion"] <- NA

(map2 <- ggplot(data = pr_c, aes(fill = Proportion)) +
    geom_sf(size = .08) +
    geom_sf(data = pr_c[pr_c$NM_MUNICIP == "Pinhais", ],
            aes(color = ""), fill = "black") +
    guides(color = guide_legend(order = 2),
           fill = guide_colorbar(order = 1)) +
    scale_fill_continuous(low = c("lightblue", "yellow"),
                          high = c("yellow", "red"), 
                          na.value = "white", breaks = c(.001, .01, .02, .08)) +
    scale_color_manual(name = "Pinhais", values = "black") +
    blank() +
    scalebar(pr, dist = 50, dd2km = TRUE, model = "WGS84", st.size = 3) +
    north(pr, symbol = 15))

## Diagramas de cordas

# Caes
cls <- c("blue3", "orange", "skyblue", "darkgreen", "yellow3", "black")
#tiff("./fig5.tiff", width = 1.5e3, height = 3e3, res = 250)
#par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
PlotImmigrationFlow(dogs,
                    source = "municipio_de_aquisicao",
                    destination = "Pinhais",
                    agg.sources.prefix = "Outras ",
                    agg.sources.suffix = " Cidades",
                    cls = cls)
#dev.off()

# Gatos
cls_c <- c("blue3", "orange", "black")
#tiff("./fig6.tiff", width = 1.5e3, height = 3e3, res = 250)
#par(mfrow = c(2, 1), mar = c(0, 0, 0, 0))
PlotImmigrationFlow(cats,
                    n.sources = 2,
                    source = "municipio_de_aquisicao",
                    destination = "Pinhais",
                    agg.sources.prefix = "Outras ",
                    agg.sources.suffix = " Cidades",
                    cls = cls_c)
#dev.off()

## Aquisicao no ultimo ano
FreqTab(dogs$aquisicao_u12)
FreqTab(cats$aquisicao_u12)

## Destinos
sum(!is.na(dogs$destino)) / nrow(dogs)
sum(!is.na(cats$destino)) / nrow(cats)
FreqTab(dogs$destino)
FreqTab(cats$destino)

## Reposicao
FreqTab(dogs$perdeu_animais)
FreqTab(cats$perdeu_animais)

## Modelagem -------------------------------------------------------------------

# Condicoes iniciais e parametros
iasa_data <- GetDataIASA(dogs,
                         sex.col = "sexo",
                         female.label = "femea",
                         male.label = "macho",
                         sterilized.col = "castrado",
                         sterilized.label = "sim",
                         sterilized.ly.col = "castrado_u12",
                         sterilized.ly.label = "sim",
                         births.ly.col = "filhotes_u12",
                         species3.col = "especie3",
                         species.label = "cao",
                         sex3.col = "sexo3",
                         fate.col = "destino",
                         died.label = "morreu",
                         lost.label = "perdido",
                         acquisition.col = "aquisicao",
                         acquired.ly.col = "aquisicao_u12",
                         acquired.ly.label = "sim",
                         adopted.label = "adotou",
                         bought.label = "comprou",
                         acquisition.source.col = "municipio_de_aquisicao",
                         acquired.sterilized.col = "aquisicao_castrado",
                         destination.label = "pinhais",
                         total.estimate = 50443.79)

## Estimativas pontoais
iasa <- SolveIASA(pars = iasa_data$pars, init = iasa_data$init, time = 0:10)

## Analise de sensibilidade

# Global
# Os resultados podem mudar um pouco devido ah estocasticidade. Quando
# comparados com os resultados do artigo, ha uma variacao maior se a versao do
# capm é 0.13.6 ou superior. As mudanca decorrentes da versao nao alteram as
# conclusoes do artigo e recomenda-se usar a mais recente.
ranges <- SetRanges(pars = iasa_data$pars, .1)
glob_N1 <- CalculateGlobalSens(iasa, ranges, sensv = "N1", all = TRUE)
glob_ns1 <- CalculateGlobalSens(iasa, ranges, sensv = "ns1", all = TRUE)
gg_glob_N1 <- PlotGlobalSens(glob_N1,
                             x.label = "Years",
                             y.label = "Total")
gg_glob_ns1 <- PlotGlobalSens(glob_ns1,
                              x.label = "Years",
                              y.label = "Sterilized")

gg_glob_N1
gg_glob_ns1

# Local
loc_N1 <- CalculateLocalSens(iasa, "N1")
loc_ns1 <- CalculateLocalSens(iasa, "ns1")

gg_loc_N1 <- PlotLocalSens(loc_N1)
gg_loc_ns1 <- PlotLocalSens(loc_ns1)

prop_L1_N1 <- round(summary(loc_N1)[, "L1"] /
                      max(summary(loc_N1)[, "L1"]) * 100, 2)
names(prop_L1_N1) <- rownames(summary(loc_N1))
sort(prop_L1_N1, decreasing = TRUE)

prop_L1_ns1 <- round(summary(loc_ns1)[, "L1"] /
                       max(summary(loc_ns1)[, "L1"]) * 100, 2)
names(prop_L1_ns1) <- rownames(summary(loc_ns1))
sort(prop_L1_ns1, decreasing = TRUE)

#tiff("./fig7.tiff", width = 4e3, height = 2e3, res = 300)
grid.arrange(gg_loc_N1, gg_loc_ns1, ncol = 2)
#dev.off()

## Mudanca entre 2017 e 2027
# Os resultados podem mudar um pouco devido ah estocasticidade. Quando
# comparados com os resultados do artigo, ha uma variacao maior se a versao do
# capm é 0.13.6 ou superior. As mudancas decorrentes da versao nao alteram as
# conclusoes do artigo e recomenda-se usar a mais recente.
CalculatePopChange(glob_N1, variable = "N1", t2 = 10, t1 = 0)
CalculatePopChange(glob_ns1, variable = "ns1", t2 = 10, t1 = 0)

## Populacao humana projetada (IBGE)
pop_estado2017 <- 11320892
pop_estado2027 <- 11929009

# Pinhais 2027, pressupondo uma razao constante entre as populacoes de
# Pinhais e Parana ao longo da decada
pop_municipio2027 <- pop_estado2027 * (pop_municipio2017 / pop_estado2017)

# Mudanca
pop_municipio2027 / pop_municipio2017
pop_municipio2027 - pop_municipio2017

## Taxas de esterilizacao propostas
pars2 <- iasa_data$pars
pars2["sf1"] <- pars2["sf1"] * 1.3
pars2["sm1"] <- pars2["sm1"] * 1.3
iasa2 <- SolveIASA(pars = pars2, init = iasa_data$init, time = 0:10)
ranges2 <- SetRanges(pars = pars2, .1)
glob2_ns1 <- CalculateGlobalSens(iasa2, ranges2, sensv = "ns1", all = TRUE)

# Os resultados podem mudar um pouco devido ah estocasticidade. Quando
# comparados com os resultados do artigo, ha uma variacao maior se a versao do
# capm é 0.13.6 ou superior. As mudancas decorrentes da versao nao alteram as
# conclusoes do artigo e recomenda-se usar a mais recente.
CalculatePopChange(glob2_ns1, variable = "ns1", t2 = 10, t1 = 0)

