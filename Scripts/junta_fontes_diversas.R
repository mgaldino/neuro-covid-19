### Entendendo os dados

allDuplicated <- function(vec){
  front <- duplicated(vec)
  back <- duplicated(vec, fromLast = TRUE)
  all_dup <- front + back > 0
  return(all_dup)
}

library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyr)
library(readr)
here()
icsmp <- readRDS("Dados/icsmp.rds")

#apenas sample A (representative)
icsmp_a <- icsmp %>%
  dplyr::filter(sample_coding == "A")


icsmp_a_ideo_country <- icsmp_a %>%
  group_by(ISO3) %>%
  summarise(ideologia_media = mean(political_ideology, na.rm=T)) %>%
  ungroup() %>%
  rename(country = ISO3)


# importando dados policies covid-19

# fonte "https://github.com/OxCGRT/covid-policy-tracker/tree/master/data/timeseries"
# Thomas Hale, Noam Angrist, Rafael Goldszmidt, Beatriz Kira, Anna Petherick, Toby Phillips, Samuel Webster, 
# Emily Cameron-Blake, Laura Hallas, Saptarshi Majumdar, and Helen Tatlow. (2021).
# “A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker).” 
# Nature Human Behaviour. https://doi.org/10.1038/s41562-021-01079-8

OxCGRT_timeseries_all <- read_excel("Dados/OxCGRT_timeseries_all.xlsx", 
                                    sheet = "stringency_index") %>% clean_names() %>%
  pivot_longer(names_to = "date", cols = starts_with("x"),
               names_prefix = "x", values_to = "stringency_index") %>%
  mutate(date = as.Date(date, "%d%b%Y"))

glimpse(OxCGRT_timeseries_all)

OxCGRT_timeseries_march <- OxCGRT_timeseries_all %>% 
  dplyr::filter(date < "2020-04-01")

covid_policies <- OxCGRT_timeseries_all %>%
  group_by(country_code) %>%
  summarise(country_name = max(country_name),
            stringency_index = mean(stringency_index, na.rm=T)) %>%
  ungroup()

# juntando dados
# junt_dados <- icsmp_a_ideo_country %>%
#   inner_join(covid_policies, by = c("country" = "country_code"))
# 
# X11()
# junt_dados %>%
#   ggplot(aes(ideologia_media, stringency_index)) + geom_point() + geom_smooth(method = "lm")

# importando dados ideologia política partidos
# source https://manifestoproject.wzb.eu/

# citation
# Volkens, Andrea / Burst, Tobias / Krause, Werner / Lehmann, Pola / Matthieß, Theres /
#   Merz, Nicolas / Regel, Sven / Weßels, Bernhard / Zehnter, Lisa (2020): The Manifesto Data
# Collection. Manifesto Project (MRG / CMP / MARPOR). Version 2020b. Berlin: Wissenschaftszentrum Berlin für Sozialforschung (WZB). https://doi.org/10.25522/manifesto.
# mpds.2020b


ideo_parties_north <- read.csv("Dados/MPDataset_MPDS2020b.csv")
ideo_parties_sa <- read.csv("Dados/MPDataset_MPDSSA2020b.csv")
ideology_parties <- ideo_parties_north %>%
  bind_rows(ideo_parties_sa)
  

# limpando os dados
ideology_parties_last <- ideology_parties %>%
  mutate(date = as.Date(edate, "%d/%m/%Y")) %>%
  group_by(country) %>%
  mutate(last_year_info = max(date)) %>%
  dplyr::filter(date >= last_year_info)

# adicionando identificador de país para join futuro

ideology_parties_last <- ideology_parties_last %>%
  inner_join(dplyr::select(covid_policies, country_code,country_name),  by = c("countryname" = "country_name"))

# países excluídos do join
# x <- sort(unique(ideology_parties_last$countryname))
# x[is.na(match(x , sort(unique(ideology_parties_last$countryname))))]

# "Armenia"                    "Bosnia-Herzegovina"         "German Democratic Republic" "Montenegro"                
# [5] "North Macedonia"            "Northern Ireland"           "Slovakia"   


glimpse(ideology_parties_last)

## selecionando variávies

ideology_parties_last <- ideology_parties_last %>%
  dplyr::select(countryname, edate, date, partyname, candidatename, parfam, pervote, presvote, rile, id_perm, country, country_code)

# precisa achar um jeito de fazer match por país
data_pol_covid <- icsmp_a %>%
  inner_join(ideology_parties_last, by = c("ISO3"= "country_code"))
  

## importando lista de líderes
lideres <- read_csv("Dados/leader_list_8_21.csv")

lideres <- lideres %>%
  mutate(stateabb = gsub("GMY", "DEU", stateabb),
         stateabb = gsub("FRN", "FRA", stateabb),
         stateabb = gsub("SWZ", "CHE", stateabb),
         stateabb = gsub("UKG", "GBR", stateabb),
         stateabb = gsub("ROK", "KOR", stateabb),
         stateabb = gsub("SWZ", "CHE", stateabb),
         stateabb = gsub("SWZ", "CHE", stateabb),)

UKG

lideres1 <- lideres %>%
  group_by(ccode) %>%
  mutate(paises_multiplos_lider = allDuplicated(ccode),
         teste = is.na(eyear)) %>%
  dplyr::filter(paises_multiplos_lider) %>%
  # dplyr::filter(eyear < 2021) %>%
  mutate(last_end_year = max(eyear, na.rm=T),
         new_eyear = if_else(teste, 2021, eyear)) %>%
  dplyr::filter(new_eyear >= last_end_year) %>%
  dplyr::filter(syear <= 2020) %>%
  inner_join(icsmp_a_ideo_country, by = c("stateabb" = "country"))
 
x <- sort(unique(icsmp_a_ideo_country$country ))
y <- sort(unique(lideres$stateabb))
x[is.na(match(x, y))]

# ideology_parties_last %>%


## others sources
# https://www.globalpartysurvey.org/
# https://forsbase.unil.ch/project/study-public-overview/17220/2/ http://www.comparativecandidates.org/data-access


## revenues, para cruzar com políticas econômicas adotadas
## https://www.wider.unu.edu/database/data-and-resources-grd

## lista de dados https://github.com/erikgahner/PolData
