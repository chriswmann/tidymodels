library(tidymodels)
library(tidyverse)
theme_set(theme_light())
set.seed(2021)


# Load and Clean Data
fpath <- "https://raw.githubusercontent.com/chriswmann/datasets/master/psds_data/full_train_set.csv"
loans <- read_csv(fpath)

# Remove nugatory columns, convert term to numeric and tidy up the outcome values.
loans <- loans %>% 
  select(!ends_with("_")) %>% 
  mutate(term = parse_number(term)) %>% 
  mutate(outcome = str_to_title(outcome)) %>% 
  select(-status) %>% 
  mutate(outcome = as.factor(outcome))

# Train test split, stratified on purpose, home_ownership and outcome.
train_ind <- loans %>% 
  mutate(index = row_number()) %>% 
  group_by(purpose, home_ownership, outcome) %>% 
  sample_frac(size = 0.75) %>% 
  pull(index)

train <- loans[train_ind, ]
test <- loans[-train_ind, ]

# Create a model specification
glm_spec <- logistic_reg(penalty = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") %>% 
  translate()

