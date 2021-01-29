library(skimr)
library(plotly)
data <- read.csv(input_dataset) %>% as_tibble()
data2 = data %>% 
  mutate(year = as.factor(year))
         pol_duration = as.factor(pol_duration))
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