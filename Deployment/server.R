shinyServer(function(input, output, session) {
  
  library(shiny)
  library(tidyverse)
  library(caret)
  library(glue)
  library(httr)
  library(jsonlite)
  
  
  
  # read_modellist <- readRDS("F:/Users/hp/Desktop/Lecture/06ML/Deployment/rf_class_model.RDS")
  # model_shiny <- read_modellist[[1]]
  model_class <- readRDS("rf_classModel.RDS")
  model_reg <- readRDS("XGB_reg_model.RDS")
  
  
  recommend_action <- function(input, output){
    
  values <- reactiveValuesToList(input)  

  location_regionAfrica <- if(values$location_region == "Africa"){1} else {0}
  location_regionAsia <- if(values$location_region == "Asia"){1} else {0}
  location_regionEurope <- if(values$location_region == "Europe"){1} else {0}
  location_regionNorth.America <- if(values$location_region == "North America"){1} else {0}
  location_regionSouth.America <- if(values$location_region == "South America"){1} else {0}
  purchase_patternfocused <- if(values$purchase_pattern == "focused"){1} else {0}
  purchase_patternhigh_value <- if(values$purchase_pattern == "high_value"){1} else {0}
  purchase_patternrandom <- if(values$purchase_pattern == "random"){1} else {0}
  age_groupestablished <- if(values$age_group == "established"){1} else {0}
  age_groupnew <- if(values$age_group == "new"){1} else {0}
  age_groupveteran <- if(values$age_group == "veteran"){1} else {0}
  weekend_or_weekdayWeekday <- if(values$weekend_or_weekday == "Weekday"){1} else {0}
  weekend_or_weekdayWeekend <- if(values$weekend_or_weekday == "Weekend"){1} else {0}
  time_of_dayAfternoon <- if(values$time_of_day == "Afternoon"){1} else {0}
  time_of_dayEvening <- if(values$time_of_day == "Evening"){1} else {0}
  time_of_dayLate.Night <- if(values$time_of_day == "Late Night"){1} else {0}
  time_of_dayMorning <- if(values$time_of_day == "Morning"){1} else {0}
  # new_transactionspurchase <- if(values$new_transactions == "purchase"){1} else {0}
  # new_transactionssale <- if(values$new_transactions == "sale"){1} else {0}
  # new_transactionsscam <- if(values$new_transactions == "scam"){1} else {0}
  # new_transactionstransfer <- if(values$new_transactions == "transfer"){1} else {0}
  network_sizeLarge <- if(values$network_size == "Large"){1} else {0}
  network_sizeMedium <- if(values$network_size == "Medium"){1} else {0}
  network_sizeSmall <- if(values$network_size == "Small"){1} else {0}
    
  input_df <- data.frame(location_regionAfrica = location_regionAfrica,
                         location_regionAsia = location_regionAsia ,
                         location_regionEurope = location_regionEurope,
                         location_regionNorth.America = location_regionNorth.America ,
                         location_regionSouth.America = location_regionSouth.America ,
                         login_frequency = as.numeric(values$login_frequency) ,
                         purchase_patternfocused = purchase_patternfocused,
                         purchase_patternhigh_value = purchase_patternhigh_value,
                         purchase_patternrandom = purchase_patternrandom,
                         age_groupestablished = age_groupestablished ,
                         age_groupnew= age_groupnew,
                         age_groupveteran= age_groupveteran,
                         weekend_or_weekdayWeekday= weekend_or_weekdayWeekday,
                         weekend_or_weekdayWeekend= weekend_or_weekdayWeekend,
                         session_duration_normalized= as.numeric(values$session_duration_normalized),
                         time_of_dayAfternoon= time_of_dayAfternoon,
                         time_of_dayEvening= time_of_dayEvening,
                         time_of_dayLate.Night= time_of_dayLate.Night,
                         time_of_dayMorning= time_of_dayMorning,
                         # new_transactionspurchase= new_transactionspurchase,
                         # new_transactionssale= new_transactionssale,
                         # new_transactionsscam = new_transactionsscam,
                         # new_transactionstransfer = new_transactionstransfer,
                         network_sizeLarge = network_sizeLarge,
                         network_sizeMedium = network_sizeMedium,
                         network_sizeSmall = network_sizeSmall,
                         freq= as.numeric(values$freq),
                         amount= as.numeric(values$amount))
    
    #classification predict part
    predicted_value <- predict(model_class, input_df)
    predicted_value <- as.character(predicted_value)
    predicted_value_df <- data.frame(predicted_value)
    colnames(predicted_value_df) <- "predicted_value"
    location_region_ind <- input_df[nrow(predicted_value_df),1]
    predicted_value_df$predicted_value[predicted_value_df$predicted_value == "high_risk"] <- "High risk"
    predicted_value_df$predicted_value[predicted_value_df$predicted_value == "low_risk"] <- "Low risk"
    predicted_value_df$predicted_value[predicted_value_df$predicted_value == "moderate_risk"] <- "Moderate risk"
    output_var <- predicted_value_df[nrow(predicted_value_df),"predicted_value"]
    output_message <- glue("  Attention Please! {output_var}!")
    
    output$output_msg <- renderText(output_message)
    
    output$image <- renderUI({
      if(output_var=="High risk")
      {img(src='highrisk.jpg', width = 300, height = 300)}
      else if(output_var == "Low risk")
      {img(src='lowrisk.jpg', width = 300, height = 300)}
      else
      {img(src='moderaterisk.jpg', width = 300, height = 300)}
    })
  }
  
  observeEvent(input$predict_class, recommend_action(input,output))
  
  
  # regression part
  
  #IZZUL EDIT
  #JUST REPLACE ALL THE REGRESSION CODE TO LAST LINE FROM HERE
  
  
  #load preprocessing objects
  preProcValues <- readRDS("preProcValues.RDS")
  dummies_model <- readRDS("dummies_model.RDS")
  lambda <- preProcValues$yj[['risk_score']]
  center<- preProcValues$mean[['risk_score']]
  scale <- preProcValues$std[['risk_score']]
  
  sample_data <- readRDS("sample_data.RDS")

  #define yeojohnson inverse function
  InvYeoJohnson <- function(x, lambda) {
    if (lambda == 0) {
      return(exp(x) - 1)
    } else {
      pos <- (x >= 0)
      neg <- (x < 0)
      y <- numeric(length(x))
      y[pos] <- (x[pos] * lambda + 1)^(1 / lambda) - 1
      y[neg] <- 1 - (-(x[neg]) * (2 - lambda) + 1)^(1 / (2 - lambda))
      return(y)
    }
  }

  #define reverse center and scale

  reverse_center_scale <- function(x, center, scale) {
    return((x * scale) + center)
  }

  #define recommend action for regression
  recommend_reg_action <- function(inputs, outputs){
    values_reg <- reactiveValuesToList(inputs)

    location_region <- values_reg$location_region_reg
    login_frequency <- as.numeric(values_reg$login_frequency_reg)
    purchase_pattern <- values_reg$purchase_pattern_reg
    age_group <- values_reg$age_group_reg
    weekend_or_weekday <- values_reg$weekend_or_weekday_reg
    session_duration_normalized= as.numeric(values_reg$session_duration_normalized_reg)
    time_of_day <- values_reg$time_of_day_reg
    network_size <- values_reg$network_size_reg
    freq <-  as.numeric(values_reg$freq_reg)
    amount <-  as.numeric(values_reg$amount_reg)
    risk_score <- 1

    input_reg_df <- data.frame(
      location_region = location_region ,
      login_frequency = login_frequency,
      purchase_pattern = purchase_pattern,
      age_group = age_group,
      weekend_or_weekday = weekend_or_weekday,
      session_duration_normalized = session_duration_normalized,
      time_of_day = time_of_day,
      network_size = network_size,
      freq = freq,
      amount =  amount,
      risk_score = risk_score
    )
    
    combine_input_reg_df <- rbind(input_reg_df,sample_data)
    input_reg_df <- combine_input_reg_df 
    

    library(janitor)

    # one hot encoding
    input_reg_df <- predict(dummies_model, newdata = input_reg_df) %>%
      as.data.frame() %>% 
      clean_names()
      
      
    # preprocess
    input_reg_df <- predict(preProcValues, input_reg_df)

    # regression prediction
    first_row <- input_reg_df[1,]
    
    predicted <- predict(model_reg, first_row)

    # inverse predicted score to actual scale
    predicted <- reverse_center_scale(predicted, center, scale)
    predicted_reg_value <- InvYeoJohnson(predicted, lambda)

    # prepare for output
    predicted_reg_value <- as.numeric(predicted_reg_value)
    output_reg_message <- glue("  Attention Please! This transaction risk score is {predicted_reg_value}!")

    outputs$output_reg_msg <- renderText(output_reg_message)

    outputs$image_reg <- renderUI({
      if(predicted_reg_value >= 90)
      {img(src='highrisk.jpg', width = 300, height = 300)}
      else if(predicted_reg_value <= 62)
      {img(src='lowrisk.jpg', width = 300, height = 300)}
      else
      {img(src='moderaterisk.jpg', width = 300, height = 300)}
    })

  }
  observeEvent(input$predict_reg, recommend_reg_action(input,output))
  
  #Scrapper
  
  # Scraper function to get Ethereum transactions
  get_transactions <- function(address) {
    api_token <- "KABTMI6DR81NKQS1I1IVUD6I63BEEWPNTR"
    base_url <- "https://api.etherscan.io/api"
    
    params <- list(
      module = "account",
      action = "txlist",
      address = address,
      startblock = 0,
      endblock = 99999999,
      sort = "asc",
      apikey = api_token
    )
    
    response <- GET(base_url, query = params)
    if (status_code(response) == 200) {
      data <- content(response, "parsed")
      if (data$status == "1") {
        transactions <- data$result
        transactions_df <- do.call(rbind, lapply(transactions, as.data.frame))
        return(transactions_df)
      } else {
        print(paste("Error:", data$message))
        return(NULL)
      }
    } else {
      print(paste("Error:", status_code(response)))
      return(NULL)
    }
  }
  
  values <- reactiveValues(transactions_df = NULL, current_page = 1)
  
  # Observe event for scrape button
  observeEvent(input$scrape, {
    address <- input$eth_address
    if (address != "") {
      transactions_df <- get_transactions(address)
      if (!is.null(transactions_df)) {
        values$transactions_df <- transactions_df
        values$current_page <- 1
      } else {
        output$output_msg <- renderText("No transactions found or an error occurred.")
      }
    } else {
      output$output_msg <- renderText("Please enter a valid Ethereum address.")
    }
  })
  
  
  output$transactions_table <- renderTable({
    if (is.null(values$transactions_df)) return(NULL)
    start <- (values$current_page - 1) * 10 + 1
    end <- min(start + 9, nrow(values$transactions_df))
    values$transactions_df[start:end, ]
  })
  
  
  output$pagination_buttons <- renderUI({
    if (is.null(values$transactions_df)) return(NULL)
    total_pages <- ceiling(nrow(values$transactions_df) / 10)
    fluidRow(
      column(6, offset = 3,
             if (values$current_page > 1) {
               actionButton("prev_page", "Previous")
             },
             if (values$current_page < total_pages) {
               actionButton("next_page", "Next")
             }
      )
    )
  })
  
  observeEvent(input$next_page, {
    values$current_page <- values$current_page + 1
  })
  
  observeEvent(input$prev_page, {
    values$current_page <- values$current_page - 1
  })
  
  
})

