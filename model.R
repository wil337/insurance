# In this module, we ask you to define your pricing model, in R.

# TODO: load your packages here.
# Don't forget to list all packages you use to the `install.R` file.
devtools::install_github("stevenpawley/h2oparsnip")

library("tidyverse")
library("h2o")
library("parsnip")
library("recipes")
library("workflows")
library("rsample")
library("h2oparsnip")
library("tidymodels")
h2o.init()

#split data into training and test set
data <- read_csv("training.csv") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))

set.seed(1)
splits = initial_split(data , prop = 0.8, strata="claim_amount")

#initial_time_split(data, prop=0.8, lag=1)
train_data = training(splits)
test_data = testing(splits)

submit = h2o.glm(x=names(train_data %>% select(-claim_amount)),
                 y = "claim_amount",
                 training_frame = as.h2o(train_data),
                 seed = 1,
                 nfolds=10)
h2o.rmse(submit, train=TRUE, xval = TRUE)
testrf = h2o.randomForest(x=names(train_data %>% select(-claim_amount)),
                 y = "claim_amount",
                 training_frame = as.h2o(train_data),
                 seed = 1,
                 nfolds=10)
h2o.rmse(testrf, train=TRUE, xval = TRUE)
testgbm = h2o.gbm(x=names(train_data %>% select(-claim_amount)),
                          y = "claim_amount",
                          training_frame = as.h2o(train_data),
                          seed = 1,
                          nfolds=10)
h2o.rmse(testgbm, train=TRUE, xval = TRUE)

#check splits proportion similar for key variables
train_data %>% 
  group_by(year) %>% 
  summarise(n = n(),
            propn = n / dim(train_data)[[1]])
train_data %>% 
  group_by(pol_coverage) %>% 
  summarise(nrows = n(),
            propn = nrows / dim(train_data)[[1]])
test_data %>% 
  group_by(year) %>% 
  summarise(n = n(),
            propn = n / dim(test_data)[[1]])
test_data %>% 
  group_by(pol_coverage) %>% 
  summarise(n = n(),
            propn = n / dim(test_data)[[1]])

glm <- linear_reg() %>% 
  set_engine("h2o", 
             family="tweedie",
             #link = "log",
             nfolds=10,
             seed=1)
glm_freq <- linear_reg() %>% 
  set_engine("h2o", 
             family="poisson",
             link="log",
             nfolds=10,
             alpha = 0,
             lambda = 0,
             #compute_p_values = TRUE,
             seed=1)
glm_acs <- linear_reg() %>% 
  set_engine("h2o", 
             family="gamma",
             link="log",
             nfolds=10,
             lambda = 0,
             alpha = 0,
             #compute_p_values = TRUE,
             seed=1)

rf <- rand_forest() %>% 
  set_engine("h2o",
             seed=1)
gbm <- boost_tree() %>% 
  set_engine("h2o",
             seed=1)

rec <- recipe(train_data, formula = claim_amount ~ .) %>% 
  update_role(id_policy, new_role = "id") %>% 
  update_role(starts_with("vh"), new_role = "car") %>% 
  update_role(population, town_surface_area, new_role = "geo") %>% 
  step_mutate(year = as.factor(year),
              pol_duration = as.factor(pol_duration)) %>% 
  step_rm(id_policy) %>% 
  step_mutate(rat_driv = ifelse(drv_age2 %>% is.na(),drv_age1,pmin(drv_age1, drv_age2))) %>%  
  step_mutate(rat_lic = ifelse(drv_age_lic2 %>% is.na(),drv_age1,pmin(drv_age_lic1, drv_age_lic2))) %>% 
  step_mutate(rat_sex = ifelse(drv_age2 %>% is.na(), as.character(drv_sex1),
                          ifelse(drv_age2 < drv_age1, as.character(drv_sex2), as.character(drv_sex1))) %>% 
           as.factor()) %>% 
  #step_interact(terms = ~rat_driv:rat_sex) %>% 
  #step_rm(drv_age1, drv_age2, drv_sex1, drv_sex2, drv_drv2,
  #       drv_age_lic1, drv_age_lic2) %>% 
  step_mutate(dens = population / town_surface_area, role = "geo") 
  #step_center(has_role("geo")) %>% 
  #step_scale(has_role("geo")) %>% 
  #step_pca(has_role("geo"), num_comp = 1) %>% 

roles <- summary(rec)
rec
wflow_glm <- workflow() %>% 
  add_model(glm) %>% 
  add_recipe(rec)

wflow_glm_freq <- workflow() %>% 
  add_model(glm_freq) %>% 
  add_recipe(rec %>% 
               step_mutate(clmcount = ifelse(claim_amount >0,1,0), 
                           role = "outcome") %>% 
               step_rm(claim_amount))
wflow_glm_acs <- workflow() %>% 
  add_model(glm_acs) %>% 
  add_recipe(rec %>% 
               step_filter(claim_amount >0))

wflow_rf <- workflow() %>% 
  add_model(rf) %>% 
  add_recipe(rec)

wflow_gbm <- workflow() %>% 
  add_model(gbm) %>% 
  add_recipe(rec)

dataprep <- prep(rec) %>% bake(train_data)

fit_glm <- wflow_glm %>% fit(train_data)
fit_glm_freq <- wflow_glm_freq %>% fit(train_data)
fit_glm_acs <- wflow_glm_acs %>% fit(train_data)

fit_rf = wflow_rf %>% fit(train_data)
fit_gbm = wflow_gbm %>% fit(train_data)

fitdata_glm_freq = fit_glm_freq %>% 
  pull_workflow_fit() # %>% tidy()
fitdata$fit@model_id
fitdata$fit@parameters
h2o.performance(fitdata$fit, xval=TRUE)
h2o.rmse(fitdata$fit, train=TRUE, xval=TRUE)
predfreq=predict(fitdata_glm_freq$fit, as.h2o(dataprep))
fitdata_glm_acs = fit_glm_acs %>% 
  pull_workflow_fit()
predacs = predict(fitdata_glm_acs$fit, as.h2o(dataprep))
pred = h2o.cbind(predfreq, predacs, as.h2o(dataprep %>% 
                                             select(claim_amount))) %>%
  as_tibble() %>% 
  mutate(pred = predict * predict0)
rmse(pred, truth = claim_amount,
     estimate = pred)
fitrf = fit_rf %>% 
  pull_workflow_fit() # %>% tidy()

fitacs <- pull_workflow_fit(fit_glm_acs)
h2o.predict(fitacs$fit, train_data %>% as.h2o())
fitacs$fit

#time folds
set.seed(1)
picksamples = train_data %>%
  select(year, pol_coverage) %>% 
  mutate(ID = row_number()) %>% 
  group_by(year, pol_coverage) 
analysis1 = picksamples %>% 
  filter(year == 1) %>% 
  sample_frac(0.8) %>% 
  pull(ID)
assess1 = picksamples %>% 
  filter(year == 1) %>% 
  slice(-analysis1) %>% 
  pull(ID)
analysis2 = picksamples %>% 
  filter(year == 2) %>% 
  sample_frac(0.5) %>% 
  pull(ID)
assess2 = picksamples %>% 
  filter(year == 2) %>% 
  slice(-analysis2) %>% 
  pull(ID) 
analysis3 = picksamples %>% 
  filter(year == 3) %>% 
  sample_frac(0.5) %>% 
  pull(ID)
assess3 = picksamples %>% 
  filter(year == 3) %>% 
  slice(-analysis3) %>% 
  pull(ID) 
analysis4 = picksamples %>% 
  filter(year == 4) %>% 
  sample_frac(0.5) %>% 
  pull(ID)
assess4 = picksamples %>% 
  filter(year == 4) %>% 
  slice(-analysis2) %>% 
  pull(ID)
year1 = picksamples %>% 
  filter(year==1) %>% 
  pull(ID)
year2 = picksamples %>% 
  filter(year==2) %>% 
  pull(ID)
year3 = picksamples %>% 
  filter(year==3) %>% 
  pull(ID)

indices <- list(
  list(analysis = analysis1, assessment = assess1),
  list(analysis = c(analysis2, year1), assessment = assess2),
  list(analysis = c(analysis3, year1, year2), assessment = assess3),
  list(analysis = c(analysis4, year1, year2, year3), assessment = assess4)
)
splits <- lapply(indices, make_splits, train_data)
folds = manual_rset(splits, c("fold1", "fold2", "fold3", "fold4"))
folds
#folds = vfold_cv(train_data, v=5)
h2o_fit_resamples <- function(model, split, id) {
  
  analysis_set <- split %>% analysis()
  wflow_rf <- workflow() %>% 
    add_model(model) %>% 
    add_recipe(rec)
  fit_rf = wflow_rf %>% fit(analysis_set)
  assessment_set     <- split %>% assessment()
  assessment_baked   <- prep(rec) %>% bake(assessment_set)
  print(id)
  tibble(
    "id" = id,
    "truth" = assessment_baked$claim_amount,
    "prediction" = fit_rf %>%
      pull_workflow_fit() %>%
      .$fit %>% 
      h2o.predict(newdata = assessment_baked %>% 
                    as.h2o()) %>%
        as.vector() 
  )

}
pred_h2o <- map2_df(
  .x = folds$splits,
  .y = folds$id,
  ~ h2o_fit_resamples(model = gbm, split = .x, id = .y)
)
foldrmse = pred_h2o %>% 
  group_by(id) %>% 
  rmse(truth=truth, estimate = prediction)
overallrmse = pred_h2o %>% 
  rmse(truth=truth, estimate = prediction)
print(foldrmse %>% bind_rows(overallrmse))

rf_fit_gbm <- wflow_gbm %>% 
  fit_resamples(folds)
collect_metrics(rf_fit_glm)
xval = rf_fit_rs %>% 
  unnest(cols=.metrics) %>% 
  filter(.metric=="rmse")
rf_fit_glm <- wflow_glm %>% 
  fit_resamples(folds)
collect_metrics(rf_fit_glm)
xval = rf_fit_rs %>% 
  unnest(cols=.metrics) %>% 
  filter(.metric=="rmse")
rf_fit_glm_freq <- wflow_glm_freq %>% 
  fit_resamples(folds, save_pred=TRUE)
rf_fit_glm_freq
collect_predictions(rf_fit_glm_freq)
collect_metrics(rf_fit_glm_freq)
xval = rf_fit_glm_freq %>% 
  unnest(cols=.metrics) %>% 
  filter(.metric=="rmse")


# (optional) data pre-processing function.
preprocess_X_data <- function (x_raw){
  # Data preprocessing function: given X_raw, clean the data for training or prediction.

  # Parameters
  # ----------
  # X_raw : Dataframe, with the columns described in the data dictionary.
  #   Each row is a different contract. This data has not been processed.

  # Returns
  # -------
  # A cleaned / preprocessed version of the dataset

  # YOUR CODE HERE ------------------------------------------------------
  x_processed <- x_raw %>% 
    select(-id_policy, -pol_sit_duration) %>% 
    mutate(rat_driv = min(drv_age1, drv_age2) )
  
  # ---------------------------------------------------------------------
  # The result trained_model is something that you will save in the next section
  return(x_raw %>% as.h2o()) # change this to return the cleaned data
}


# Feel free to add other functions/constants to use in your model.



fit_model <- function (x_raw, y_raw){
	# Model training function: given training data (X_raw, y_raw), train this pricing model.

	# Parameters
	# ----------
	# X_raw : Dataframe, with the columns described in the data dictionary.
	# 	Each row is a different contract. This data has not been processed.
	# y_raw : a array, with the value of the claims, in the same order as contracts in X_raw.
	# 	A one dimensional array, with values either 0 (most entries) or >0.

	# Returns
	# -------
	# self: (optional), this instance of the fitted model.

	
  # This function trains your models and returns the trained model.

  # First, create all new features, if necessary
  
  
  # YOUR CODE HERE ------------------------------------------------------
  #fit_lm = lm(unlist(ydata) ~ 1) # toy linear model
  y_raw <- as.h2o(y_raw)
  x_clean = preprocess_X_data(x_raw)
  train <- h2o.cbind(x_clean, y_raw)
  x = names(x_raw)
  y = names(y_raw)
  glm = h2o.glm(x, y, train, 
                seed = 1)
  rf = h2o.randomForest(x, y, train, seed = 1)
  trained_model = glm
  # ---------------------------------------------------------------------
  # The result trained_model is something that you will save in the next section
  return(glm)  # return(trained_model)
}


predict_expected_claim <- function(model, x_raw){
	# Model prediction function: predicts the average claim based on the pricing model.

	# This functions estimates the expected claim made by a contract (typically, as the product
	# of the probability of having a claim multiplied by the average cost of a claim if it occurs),
	# for each contract in the dataset X_raw.

	# This is the function used in the RMSE leaderboard, and hence the output should be as close
	# as possible to the expected cost of a contract.

	# Parameters
	# ----------
	# X_raw : Dataframe, with the columns described in the data dictionary.
	# 	Each row is a different contract. This data has not been processed.

	# Returns
	# -------
	# avg_claims: a one-dimensional array of the same length as X_raw, with one
	#     average claim per contract (in same order). These average claims must be POSITIVE (>0).
	
  #y_predict = predict(model, newdata = x_raw)  # tweak this to work with your model
  expected_claims = predict(model, newdata = x_raw %>% as.h2o())  # tweak this to work with your model
  
  return(expected_claims)  
}


predict_premium <- function(model, x_raw){
  # Model prediction function: predicts premiums based on the pricing model.

  # This function outputs the prices that will be offered to the contracts in X_raw.
  # premium will typically depend on the average claim predicted in 
  # predict_expected_claim, and will add some pricing strategy on top.

  # This is the function used in the average profit leaderboard. Prices output here will
  # be used in competition with other models, so feel free to use a pricing strategy.

  # Parameters
  # ----------
  # X_raw : Dataframe, with the columns described in the data dictionary.
  # 	Each row is a different contract. This data has not been processed.

  # Returns
  # -------
  # prices: a one-dimensional array of the same length as X_raw, with one
  #     price per contract (in same order). These prices must be POSITIVE (>0).
	
  return(predict_expected_claim(model, x_raw) * 1)  # Default: multiply prices by 2
}


save_model <- function(model, output_path){
  # Saves this trained model to a file.

  # This is used to save the model after training, so that it can be used for prediction later.

  # Do not touch this unless necessary (if you need specific features). If you do, do not
  #  forget to update the load_model method to be compatible.
	
  # Save in `trained_model.RData`.

#  save(model, file='trained_model.RData')
  MODEL_OUTPUT_PATH <<- h2o.saveModel(model, path = output_path, force = TRUE)
  print(MODEL_OUTPUT_PATH)
}


load_model <- function(){ 
 # Load a saved trained model from the file `trained_model.RData`.

 #    This is called by the server to evaluate your submission on hidden data.
 #    Only modify this *if* you modified save_model.

#  load('trained_model.RData')
  model = h2o.loadModel("model/GLM_model_R_1611661053916_2")
  return(model)
}
