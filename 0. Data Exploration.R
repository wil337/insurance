library(skimr)
library(plotly)
library(tidyverse)
library(h2o)
library(rsample)


h2o.init()
#input data
data <- read.csv("training.csv") %>% as_tibble()

data %>% 
  #filter(claim_amount <= 5000) %>% 
  mutate(ID=row_number()) %>% 
  ggplot(aes(y=claim_amount)) +
  geom_point(aes(x=ID))
  

#preprocess columns
data2 = data %>% 
  mutate(year = as.factor(year), 
         pol_duration = as.factor(pol_duration)) %>% 
  select(-id_policy, -pol_sit_duration) %>% 
  mutate(rat_driv = ifelse(drv_age2 %>% is.na(),drv_age1,pmin(drv_age1, drv_age2))) %>% 
  mutate(rat_lic = ifelse(drv_age_lic2 %>% is.na(),drv_age1,pmin(drv_age_lic1, drv_age_lic2))) %>% 
    mutate(rat_sex = ifelse(drv_age2 %>% is.na(), as.character(drv_sex1),
                           ifelse(drv_age2 < drv_age1, as.character(drv_sex2), as.character(drv_sex1))) %>% 
           as.factor()) %>% 
  select(-drv_age1, -drv_age2, -drv_sex1, -drv_sex2, -drv_drv2,
         -drv_age_lic1, -drv_age_lic2) %>% 
  mutate(dens = population / town_surface_area)
#pca on location variables (alternatively get a density pop / area)
pca <- data2 %>% 
  as.h2o() %>% 
  h2o.prcomp(training_frame = ., 
             x=c("population", "town_surface_area", "dens"),
             transform = "STANDARDIZE",
             seed = 1,
             k = 2)
loc <- h2o.predict(pca, newdata=data2 %>% as.h2o()) %>% 
  as_tibble() %>% 
  rename(loc1=PC1, loc2=PC2)
data3 <- bind_cols(data2, loc) %>% 
  select(-population, -town_surface_area, -dens)

#stratified sample by year and pol coverage
data4 = data3 %>% 
  group_by(year, pol_duration) %>% 
  summarise(rand = floor(r(1)*4))
table(data4$year)
h2odata = data3 %>% as.h2o() %>% h2o.splitFrame(ratios = 0.8, 
                                                seed = 1,
                                                )
features = data3 %>% select(-claim_amount) %>% names()
glm = h2o.glm(x=features,
        y = "claim_amount",
        seed = 1,
        training_frame = h2odata[[1]],
        interaction_pairs = list(c("rat_driv", "rat_sex")),
        nfolds = 5)
glm


# dimension reduction vehicle into 1 column
vh = data3 %>% 
  select(starts_with("vh")) %>% 
  names()
glrm <- data3 %>% 
  select(starts_with("vh")) %>% 
  as.h2o() %>% 
  h2o.glrm(training_frame = ., 
             transform = "STANDARDIZE",
             seed = 1,
           k=7)
veh <- h2o.predict(glrm, newdata=data3 %>% 
                     select(starts_with("vh")) %>% 
                     as.h2o()) %>% 
  as_tibble() %>% 
  rename(loc=PC1)


skim(data2$vh_make_model)
oneWays <- function(data, fac, cuts = 10L, jenks_flag = FALSE){
    #x axis
    ax <- list(title = fac)
    
    fac <- as.name(fac) # convert to symbol 
    
    #formatting for second y axis
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Cost"
    )
    
    
    #natural jenks breaks if selected
    if (jenks_flag==TRUE){
      cuts <- formatTrain %>% 
        select(H_BUILDING_SUMINSURED) %>% 
        pull() %>% 
        getJenksBreaks(cuts+1)  
    }
    
    
    varType <- data[[fac]] %>% class()
    #data %>% select(!!fac) %>% pull() %>% class()
    
    #band data if numeric factor
    if (varType %in% c('numeric', 'Date')){
      procSum <- data %>% 
        mutate(band = cut(!!fac, breaks = cuts))  %>% 
        group_by(band)
      procSumTime <- data %>% 
        mutate(band = cut(!!fac, breaks = cuts))  %>% 
        group_by(band, year)
      xfac <- ~band
      
    } else {
      procSum <- data %>% 
        group_by(!!fac)
      # !! is unquoting the string factor
      procSumTime <- data %>% 
        group_by(!!fac, year)
      xfac <- quo(!!fac)
    }
    
    procSum2 <- procSum %>%   
      summarise(polCount=n(),
                avgCost = mean(claim_amount)) 
    procSumTime2 <- procSumTime %>%   
      summarise(polCount=n(),
                avgCost = mean(claim_amount)) 
    
    plot_ly() %>% 
      add_trace(data = procSum2,
                x=xfac,
                #quo adds a quosure to be in the right format for plotly to read ~variable
                y= ~polCount,
                type = "bar",
                name = "policy count") %>% 
      add_trace(data = procSumTime2, 
                x=xfac,
                #quo adds a quosure to be in the right format for plotly to read ~variable
                y= ~avgCost,
                color = ~year,
                yaxis = "y2",
                type = "scatter",
                mode = "marker") %>% 
      layout(title = "One Way Summary", 
             xaxis = ax,
             yaxis2 = ay)
  }
oneWays(data2, "pol_coverage")
oneWays(data2, "pol_duration")
oneWays(data2, "pol_sit_duration")
oneWays(data2, "pol_pay_freq")
oneWays(data2, "pol_usage")
oneWays(data2, "drv_sex1")
oneWays(data2, "drv_age1", 99)
oneWays(data2, "pol_no_claims_discount")

oneWays(data2, "pol_payd")
oneWays(data2, names(data2)[26])
plot_ly() %>% 
add_trace(data = data2, 
          x=~pol_duration,
          #quo adds a quosure to be in the right format for plotly to read ~variable
          y= ~pol_sit_duration,
          yaxis = "y2",
          type = "scatter",
          mode = "marker")