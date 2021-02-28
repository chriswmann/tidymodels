# https://embed.tidymodels.org/articles/Applications/GLM.html
library(embed)
library(glue)
library(modeldata)
library(rstan)
library(rstanarm)
library(tidymodels)
library(tidyverse)

theme_set(theme_light())

data(okc)

props <- okc %>%
  group_by(location) %>%
  summarise(
    prop = mean(Class == "stem"),
    log_odds  = log(prop / (1 - prop)),
    n = length(Class)
  ) %>% 
  mutate(label = paste0(gsub("_", " ", location), " (n=", n, ")"))


props %>%
  select(-label)


# later, for plotting
rng <- extendrange(props$log_odds[is.finite(props$log_odds)], f = 0.1)

okc_glm <- 
  recipe(Class ~ ., data = okc) %>%
  # specify the variable being encoded and the outcome
  step_lencode_glm(location, outcome = vars(Class)) %>%
  # estimate the effects
  prep(training = okc)

okc_glm

glm_estimates <- 
  tidy(okc_glm, number = 1)

glm_estimates <- 
  tidy(okc_glm, number = 1) %>% 
  dplyr::select(-terms, -id)
glm_estimates

glm_estimates <- 
  glm_estimates%>%
  set_names(c("location", "glm")) %>%
  inner_join(props, by = "location")

glm_estimates %>%
  dplyr::filter(is.finite(log_odds)) %>%
  mutate(difference = log_odds-glm) %>%
  dplyr::select(difference) %>%
  summary()

tidy(okc_glm, number = 1) %>%
  dplyr::filter(level == "..new") %>%
  select(-id)

opts <- 
  list(
    ## the number of chains
    chains = 4,
    ## how many cores to use 
    cores = 4,
    ## the total number of iterations per chain (low here for time)
    iter = 500,
    ## set the random number seed
    seed = 8779
  )

okc_glmer <- 
  recipe(Class ~ ., data = okc) %>%
  step_lencode_bayes(
    location,
    outcome = vars(Class),
    options = opts
  ) %>% 
  prep(training = okc)

all_estimates <- 
  tidy(okc_glmer, number = 1) %>% 
  dplyr::select(-terms, -id) %>%
  set_names(c("location", "glmer")) %>%
  inner_join(glm_estimates, by = "location")

all_estimates %>% dplyr::select(location, log_odds, glm, glmer)