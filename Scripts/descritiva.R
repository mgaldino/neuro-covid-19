## Análise descritiva

library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyr)
library(readr)
library(rstanarm)
library(forcats)
library(papeR)
library(sjPlot)
library(sjmisc)
here()
icsmp <- readRDS("Dados/icsmp.rds")

icsmp_br <- icsmp %>%
  clean_names() %>%
  dplyr::filter(iso3 == "BRA")

# perfil sócio demográfico
icsmp_br_socio <- icsmp_br %>%
  select(age, employ_status1, slf_ladder, marital1, happy, political_ideology, sex1, sex2, urban ) %>%
  mutate(sex1 = factor(sex1, levels = 1:3, labels = c("Male", "Female", "Other")),
         marital1 = factor(marital1, levels = 1:3, labels =c("Single", "in a Relationsuop", "Maried")),
         employ_status1 = factor(employ_status1, levels = 1:6, labels = c("Employed full-time", "Employed part-time", 
                                                          "Unemployed/looking for work", "Student", "Retired", "Other")))

labels(icsmp_br_socio) <- c("idade (anos)", "tipo de emprego", "percepção de status (escada)", "statu marital", "felicidade", "ideologia",
                            "gênero", "outro gênero", "urbano")

labels(icsmp_br_socio)

summary(icsmp_br_socio)  

icsmp_br_socio_Redux <- icsmp_br_socio %>%
  select(age, employ_status1, slf_ladder, marital1, political_ideology, sex1) 

par(mfrow = c(3, 2))
plot(icsmp_br_socio_Redux)

descr(icsmp_br_socio_Redux,
      out= "viewer",
      file= "descrip_test.doc")
