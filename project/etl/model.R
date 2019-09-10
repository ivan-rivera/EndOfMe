# ================================
# Modelling
# ================================


#' Build and evaluate models
#'
#' @param sleep_collection list with processed sleep datasets
#' @param response_vars vector of strings that currently supports the default values only
#'
#' @return a list of datasets
generate_predictions <- function(
  sleep_collection,
  response_vars = c("sleep_rating_next", "time_asleep_next")
){
  # NOTE: right now this function is optimized specifically for 2 response variables
  # in a sense that it is designed to be used to generate graphics
  
  days_for_model_validation <- floor(
    n_distinct(sleep_collection[["modelling"]][["data"]]$sleep_date) * prop_for_model_validation
  )
  
  predictor_vars <- sleep_collection[["modelling"]][["data"]] %>% select(
    -one_of(
      c(
        sleep_collection[["modelling"]][["variables"]]$id,
        sleep_collection[["modelling"]][["variables"]]$exclusions
      )
    )
  ) %>% colnames
  
  model_results <- list(
    "performance" = tibble(),
    "variables" = tibble(),
    "predictions" = tibble()
  )
  
  # generate a model for each response variable
  for(v in response_vars){
    
    print(sprintf("processing response variable %s...", v))
    
    model_fitting_data <- sleep_collection[["modelling"]][["data"]] %>%
      filter(!is.na(!!rlang::parse_expr(v)))
    
    model_data <- list(
      "fitting" = list(
        "predictors" = model_fitting_data %>%
          filter(sleep_date < max(sleep_date) - days_for_model_validation) %>%
          select(one_of(predictor_vars)),
        "response" = model_fitting_data %>%
          filter(sleep_date < max(sleep_date) - days_for_model_validation) %>%
          pull(v)
      ),
      "validation" = list(
        "predictors" = model_fitting_data %>%
          filter(sleep_date >= max(sleep_date) - days_for_model_validation) %>%
          filter(!is.na(!!rlang::parse_expr(v))) %>%
          select(one_of(predictor_vars)),
        "response" = model_fitting_data %>%
          filter(sleep_date >= max(sleep_date) - days_for_model_validation) %>%
          filter(!is.na(!!rlang::parse_expr(v))) %>%
          pull(v)
      ),
      "prediction" = list(
        "predictors" = sleep_collection[["modelling"]][["data"]] %>%
          filter(is.na(!!rlang::parse_expr(v))) %>%
          select(one_of(predictor_vars))
      )
    )
    
    target_model <- caret::train(
      x=as.data.frame(model_data[["fitting"]][["predictors"]]),
      y=model_data[["fitting"]][["response"]],
      method="xgbLinear",
      metric="RMSE",
      tuneLength=20,
      preProcOptions=list(method=c("center", "scale")),
      trControl=caret::trainControl(
        method = "boot",
        number = 10,
        search = "random",
        verboseIter = FALSE
      )
    )
    
    validation_results <- tibble(
      response = v,
      actual = model_data[["validation"]][["response"]],
      predicted = predict(
        target_model,
        model_data[["validation"]][["predictors"]]
      )
    )
    
    var_importance <- varImp(target_model)$importance %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      rename(
        "variable" = rowname, 
        "importance" = Overall
      ) %>%
      filter(importance > 0) %>%
      mutate(
        response = v,
        importance = importance / 100,
        variable = gsub("_", " ", variable),
        variable = ifelse(
          grepl("before", variable), 
          variable, 
          paste0(variable, " yesterday")
        )
      )
    
    prediction_results <- tibble(
      variable = v,
      prediction_date = sleep_collection[["modelling"]][["data"]] %>%
        filter(is.na(!!rlang::parse_expr(v))) %>%
        pull(sleep_collection[["modelling"]][["variables"]]$id),
      prediction = predict(
        target_model,
        model_data[["prediction"]][["predictors"]]
      )
    )
    
    model_results[["performance"]] %<>% rbind(validation_results)
    model_results[["variables"]] %<>% rbind(var_importance)
    model_results[["predictions"]] %<>% rbind(prediction_results)
    
  }
  
  model_results
  
}