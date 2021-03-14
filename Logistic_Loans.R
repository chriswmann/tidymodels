library(tidymodels)
library(tidyverse)
theme_set(theme_light())
set.seed(42)

# Script version of the rmarkdown notebook of the same name, just to rattle through a bit quicker

# Load and Clean Data
fpath <- "https://raw.githubusercontent.com/chriswmann/datasets/master/psds_data/full_train_set.csv"
loans <- read_csv(fpath)

# Remove nugatory columns, convert term to numeric and tidy up the outcome values.
loans <- loans %>% 
  select(!ends_with("_")) %>% 
  mutate(term = parse_number(term)) %>% 
  mutate(outcome = str_to_title(outcome)) %>% 
  select(-status) %>% 
  mutate(across(where(is.character), as.factor))

# Train test split, stratified on purpose, home_ownership and outcome.
train <- loans %>% 
  group_by(purpose, home_ownership, outcome) %>% 
  sample_frac(size = 0.75) %>% 
  ungroup()

test <- loans %>% 
  anti_join(train)

# Create a model specification
glm_spec <- logistic_reg(penalty = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") %>% 
  translate()

# Create a recipe
loans_rec <- recipe(outcome ~ ., data = train) %>%
  themis::step_downsample(outcome) %>%
  step_other(purpose, other = "the_rest") %>% 
  step_dummy(purpose, home_ownership) %>%
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())


loans_glm_workflow <- workflow() %>% 
  add_recipe(loans_rec) %>% 
  add_model(glm_spec)

# First Fit
loans_wflow_fit <- fit(loans_glm_workflow, data = train)
preds <- predict(loans_wflow_fit, new_data = test)
results <- test %>% 
  bind_cols(preds) %>% 
  rename(preds = .pred_class)
classification_metrics <- metric_set(accuracy, bal_accuracy, kap)

results %>% 
  classification_metrics(truth = outcome, estimate = preds)

# Second Recipe Update
loans_rec$steps[[2]] <- update(loans_rec$steps[[2]], threshold = 4)

## Second Fit
loans_glm_workflow <- update_recipe(loans_glm_workflow, loans_rec)
loans_wflow_fit <- fit(loans_glm_workflow, data = train)
preds <- predict(loans_wflow_fit, new_data = test)
results <- test %>% 
  bind_cols(preds) %>% 
  rename(preds = .pred_class)
results %>% 
  classification_metrics(truth = outcome, estimate = preds)


# Third Recipe Update
train <- train %>% mutate(lti = annual_inc / loan_amnt)
test <- test %>% mutate(lti = annual_inc / loan_amnt)

## Third Fit
# Update the recipe with new data.
loans_rec <- recipe(outcome ~ ., data = train) %>%
  themis::step_downsample(outcome) %>%
  step_other(purpose, other = "the_rest", threshold = 4) %>% 
  step_dummy(purpose, home_ownership) %>%
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())
loans_wflow_fit <- fit(loans_glm_workflow, data = train)
preds <- predict(loans_wflow_fit, new_data = test)
results <- test %>% 
  bind_cols(preds) %>% 
  rename(preds = .pred_class)
results %>% 
  classification_metrics(truth = outcome, estimate = preds)
