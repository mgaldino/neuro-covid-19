### Análise 1

## ideology -> bloco de conspiracy theory , ideology leader -> conspiracy theory, policy -> conspiracy


## Simple Bayes

library(here)
library(dplyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyr)
library(readr)
library(rstanarm)
library(forcats)
here()
icsmp <- readRDS("Dados/icsmp.rds")

#apenas sample A (representative)
icsmp_a <- icsmp %>%
  dplyr::filter(sample_coding == "A") %>%
  clean_names()

## start with ideology -> conspiracy
# ctheory1 bioweapon
icsmp_a_m1 <- icsmp_a %>%
  rename(id = x1) %>%
  dplyr::select(id, political_ideology, ctheory1, ctheory2, ctheory3, age, country, sex1, iso3, mor_cop_avg) %>%
  mutate(gender = if_else(sex1 == 1, "male", 
                          if_else(sex1 == 2, "female", "other")),
         gender = as.factor(gender),
         gender = relevel(gender, "female"), # Make female the reference category and rename variable
         country_factor = as.factor(country),
         iso_factor = as.factor(iso3))


icsmp_a_m1 %>%
  dplyr::filter(!is.na(ctheory1)) %>%
  dplyr::filter(!is.na(ctheory2)) %>%
  summarise(cor(ctheory1, ctheory2 )) #  0.724

icsmp_a_m1 %>%
  dplyr::filter(!is.na(ctheory1)) %>%
  dplyr::filter(!is.na(ctheory3)) %>%
  summarise(cor(ctheory1, ctheory3 )) #  0.629

icsmp_a_m1 %>%
  dplyr::filter(!is.na(ctheory3)) %>%
  dplyr::filter(!is.na(ctheory2)) %>%
  summarise(cor(ctheory3, ctheory2 )) #   0.753

## descriptive statistics
# variaton among countries of conspiracy theory
conspiracy_country <- icsmp_a_m1 %>%
  group_by(iso3) %>%
  summarise(ctheory1_mean = mean(ctheory1, na.rm=T),
            ctheory1_sd = sd(ctheory1, na.rm=T),
            n = length(ctheory1)) %>%
  mutate(ctheory1_se = ctheory1_sd / sqrt(n),
         sd_between = sd(ctheory1_mean))


p <- ggplot() +
  geom_boxplot(data = icsmp_a_m1,
               mapping = aes(y = ctheory1,
                             x = fct_reorder(iso3, ctheory1, mean,  na.rm = TRUE)),
               colour = "gray") +
  # geom_point(data = icsmp_a_m1,
  #            mapping = aes(y = ctheory1,
  #                          x = fct_reorder(iso3, ctheory1, mean, na.rm = TRUE)),
  #            colour = "gray") +
  geom_point(data = conspiracy_country,
             mapping = aes(x = fct_reorder(iso3, ctheory1_mean,  na.rm = TRUE),
                           y = ctheory1_mean),
             colour = "black") +
  coord_flip() +
  labs(y = "ctheory1 - bioweapon", x = "")


p <- p + ggtitle ("Variation in conspiracy theory 1 score among countries")
x11()
ggsave(p, file= "Graficos/variation among countries.png")
# observations size varies per country
x11()
ggplot(conspiracy_country, aes(x = log2(n))) +
  geom_density() +
  geom_rug()

# Count unique individuals and countries
J <-  length(unique(icsmp_a_m1$iso3))
N <- nrow(icsmp_a_m1)


## Bayesian model

#JAGS https://github.com/johnmyleswhite/JAGSExamples/blob/master/bugs/hierarchical/hierarchical_linear.bugs

## likert scale
## https://medium.com/vertex-intelligence/bayesian-modeling-of-likert-scale-survey-responses-809bbd00c2ca

# vayring intercept, no predictor  yij ∼ N(αj,σ2y), and αj ∼ N(μα,σ2α). 
M1_stanlmer <- stan_lmer(formula = ctheory1 ~ 1 + political_ideology + (1 | iso3), 
                         data = icsmp_a_m1, iter = 4000,
                         seed = 349)

# Obtain a summary of priors used
prior_summary(object = M1_stanlmer)

# Obtain SD of outcome
sd(icsmp_a_m1$ctheory1, na.rm = TRUE)


print(M1_stanlmer, digits = 2)
summary(M1_stanlmer, 
        pars = c("(Intercept)", "political_ideology", "sigma", "Sigma[iso3:(Intercept),(Intercept)]"),
        probs = c(0.025, 0.975),
        digits = 2)
fixef(M1_stanlmer)

NROW(ranef(M1_stanlmer)$iso3)
# 25

head(cbind(coef(M1_stanlmer)$iso3[,1], 
           fixef(M1_stanlmer)[1] + ranef(M1_stanlmer)$iso3))

x11()
ppp_check_plot <- pp_check(M1_stanlmer, plotfun = "dens_overlay")
x11() 
ppp_check_plot 

ggsave(ppp_check_plot, file = "Graficos/ppp_check_plot_normal_varying_intercept.png")
# PPD <- posterior_linpred(post, newdata = nd,
#                          transform = TRUE)

# http://mjskay.github.io/tidybayes/articles/tidy-rstanarm.html