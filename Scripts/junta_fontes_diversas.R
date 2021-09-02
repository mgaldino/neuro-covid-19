### Entendendo os dados


library(here)
library(dplyr)
library(ggplot2)
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

# ideoologia média por país
icsmp_a_ideo_country %>%
  ggplot(aes(x=country, y=ideologia_media)) + geom_bar(stat="identity") +
  coord_flip()

# correlação agregada entre ideologia e generosidade
icsmp_a_ideo_country %>%
  ggplot(aes(x=country, y=ideologia_media)) + geom_bar(stat="identity") +
  coord_flip()

