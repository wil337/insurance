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
data <- read.csv("training.csv") %>% 
  as_tibble()

set.seed(1)
splits = initial_split(data , prop = 0.8, strata="claim_amount")

#initial_time_split(data, prop=0.8, lag=1)
train_data = training(splits)
test_data = testing(splits)

#check splits proportion similar for key varibles
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
             #family="poisson",
             #nfolds=5,
             seed=1)
rf <- rand_forest() %>% 
  set_engine("ranger",
             seed=1)
rec <- recipe(train_data, formula = claim_amount ~ .) %>% 
  update_role(id_policy, new_role = "id") %>% 
  update_role(starts_with("vh"), new_role = "car") %>% 
  update_role(population, town_surface_area, new_role = "geo") %>% 
  step_mutate(year = as.factor(year),
              pol_duration = as.factor(pol_duration)) %>% 
  step_rm(id_policy, pol_sit_duration) %>% 
  step_mutate(rat_driv = ifelse(drv_age2 %>% is.na(),drv_age1,pmin(drv_age1, drv_age2))) %>%  
  step_mutate(rat_lic = ifelse(drv_age_lic2 %>% is.na(),drv_age1,pmin(drv_age_lic1, drv_age_lic2))) %>% 
  step_mutate(rat_sex = ifelse(drv_age2 %>% is.na(), as.character(drv_sex1),
                          ifelse(drv_age2 < drv_age1, as.character(drv_sex2), as.character(drv_sex1))) %>% 
           as.factor()) %>% 
  step_rm(drv_age1, drv_age2, drv_sex1, drv_sex2, drv_drv2,
         drv_age_lic1, drv_age_lic2) %>% 
  step_mutate(dens = population / town_surface_area, role = "geo") %>% 
  step_center(has_role("geo")) %>% 
  step_scale(has_role("geo")) %>% 
  step_pca(has_role("geo"), num_comp = 1)

roles <- summary(rec)
rec
wflow_glm <- workflow() %>% 
  add_model(glm) %>% 
  add_recipe(rec)

wflow_rf <- workflow() %>% 
  add_model(rf) %>% 
  add_recipe(rec)
dataprep <- prep(rec) %>% bake(train_data)

fit_glm <- wflow_glm %>% fit(train_data)
fit_rf = wflow_rf %>% fit(train_data)

fitdata = fit_glm %>% 
  pull_workflow_fit() # %>% tidy()
fitdata$fit@model_id
fitdata$fit@parameters
h2o.performance(fitdata$fit, xval=TRUE)
h2o.rmse(fitdata$fit, train=TRUE, xval=TRUE)

fitrf = fit_rf %>% 
  pull_workflow_fit() # %>% tidy()


set.seed(345)
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
manual_rset(splits, c("fold1", "fold2", "fold3", "fold4"))


train_data %>% slice(analysis3)
  mutate(rand = map_dbl(.f=runif, .x=1),
         analysis1 = ifelse(rand < 0.8 && year == 1, 1, 0),
         assess1 = ifelse(rand >= 0.8 && year == 1, 1, 0),
         analysis2 = ifelse(year == 1 || year == 2 && rand < 0.5, 1, 0),
         assess2 = ifelse(year == 2 && rand >=0.5, 1, 0), 
         analysis3 = ifelse(year <= 2 || year == 3 && rand < 0.5, 1, 0),
         assess3 = ifelse(year == 3 || rand >= 0.5, 1, 0),
         analysis4 = ifelse(year <= 3 && year == 4 && rand < 0.5, 1, 0),
         assess4 = ifelse(year == 4 && rand >= 0.5, 1, 0)) 
picksamples %>% filter(year == 1) %>% 
  ggplot(aes(x=rand)) + geom_histogram()
analysis1 = picksamples %>% 
  filter(analysis1 == 1) %>% 
  pull(ID)

assess1 = picksamples %>% 
  filter(assess1 == 1) %>% 
  pull(ID)








%>% 
  ungroup() %>% 
  summarise(analysis1 = sum(analysis1)/pull(count(data)),
            assess1 = sum(assess1)/pull(count(data)),
            analysis2 = sum(analysis2)/pull(count(data)),
            assess2 = sum(assess2)/pull(count(data)),
            analysis3 = sum(analysis3)/pull(count(data)),
            assess3 = sum(assess3)/pull(count(data)),
            analysis4 = sum(analysis4)/pull(count(data)),
            assess4 = sum(assess4)/pull(count(data))) %>% 
  mutate(train1 = assess1 / sum(analysis1, assess1))
train_data %>% 
  count(year) %>% 
  mutate(cum = cumsum(as.numeric(n))) %>% 
  mutate(indices = list(1:10)) %>% 
  unnest(.cols=indices)
analysis1 = train_data %>% filter(year<=1)
assess1 = train_data %>% filter(year==1)

analysis2 = train_data %>% filter(year<=2)
assess2 = train_data %>% filter(year==2)

analysis3 = train_data %>% filter(year<=3)
assess3 = train_data %>% filter(year==3)

analysis4 = train_data %>% filter(year<=4)
assess4 = train_data %>% filter(year==4)
timefolds = tibble(splits = list(c(analysis1, assess1), 
                     c(analysis2, assess2),
                     c(analysis3, assess3),
                     c(analysis4, assess4)),
       id = c("fold1", "fold2", "fold3", "fold4"))
#manual create  timefolds rsample rset object
indices <- list(
  list(analysis = 1:dim(analysis1)[1], assessment = dim(analysis1)[1]+1:dim(assess1)[1]),
  list(analysis = 1:dim(analysis2)[1], assessment = dim(analysis2)[1]+1:dim(assess2)[1]),
  list(analysis = 1:dim(analysis3)[1], assessment = dim(analysis3)[1]+1:dim(assess3)[1]),
  list(analysis = 1:dim(analysis4)[1], assessment = dim(analysis4)[1]+1:dim(assess4)[1])
)
splits <- lapply(indices, make_splits, bind_rows(analysis1, assess1, analysis2, assess2, analysis3, assess3, analysis4, assess4))
manual_rset(splits, c("fold1", "fold2", "fold3", "fold4"))
folds
fold1 = initial_split(train_data %>% filter(year<=1), prop=0.8)
fold2 = initial_split(train_data %>% filter(year<=2), prop=0.8)
fold3 = initial_split(train_data %>% filter(year<=3), prop=0.8)
fold4 = initial_split(train_data %>% filter(year<=4), prop=0.8)
testing(fold1)
splits = list(fold1, fold2, fold3, fold4)
folds = manual_rset(splits, c("fold1", "fold2", "fold3", "fold4"))
folds
rf_fit_rs <- wflow_rf %>% 
  fit_resamples(timefolds)
collect_metrics(rf_fit_rs)
xval = rf_fit_rs %>% 
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
