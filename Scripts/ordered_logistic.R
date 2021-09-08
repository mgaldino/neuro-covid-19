## ordered probit
library(brms)

## ref
# https://bookdown.org/content/3686/ordinal-predicted-variable.html
my_data_1 %>% 
  mutate(Y = factor(Y)) %>% 
  
  ggplot(aes(x = Y)) +
  geom_bar(fill = sl[5]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# call this once to distribute chains across cpu cores:
#options(mc.cores=parallel::detectCores())

cumulativemodelfit <- function(formula, data, links=c("logit", "probit", "cloglog", "cauchit"),
                               thresholds=c("flexible", "equidistant"), verbose=TRUE) {
  names(links) <- links
  names(thresholds) <- thresholds
  llks <- outer(links, thresholds,
                Vectorize(function(link, threshold)
                  # catch error for responses with 2 levels
                  tryCatch(ordinal::clm(formula, data=data, link=link, threshold=threshold)$logLik,
                           error = function(e) NA)))
  print(llks)
  if (verbose) {
    bestfit <- which.max(llks)
    cat("\nThe best link function is ", links[bestfit %% length(links)], " with a ",
        thresholds[1 + bestfit %/% length(thresholds)], " threshold (logLik ", llks[bestfit],
        ")\n", sep="")
  }
  invisible(llks)
}

icsmp_a_m1 <- icsmp_a_m1 %>%
  mutate(ctheory1_1 = factor(ctheory1, ordered=T))

x11()
icsmp_a_m1 %>% 
  ggplot(aes(x = ctheory1)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# credit_factor_ordered <- factor(credit_rating, ordered = TRUE, 
#                                 levels = c("AAA", "AA", "A", "BBB"))
cumulativemodelfit(ctheory1_1 ~ 1 + political_ideology, data=x)
# logit
       

# this will take some time
null_mdl <- brm(ctheory1_1 ~ (1|iso_factor), data=x, family=cumulative("logit"), iter = 4000)
ideology_mdl <- brm(ctheory1_1 ~ political_ideology + (1|iso_factor), data=x, family=cumulative("logit"),
                    iter = 4000)

LOO(null_mdl, ideology_mdl)


