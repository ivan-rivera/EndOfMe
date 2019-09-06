# ================================
# Supporting Utilities
# ================================


# SUPPORTING METHODS ----------------------------------

#' Roll over midnight
#' Convert linear time to a circular form revolving around midnight
#'
#' @param time_value a string in the format HH:MM:SS
#'
#' @return
#' @export
#'
#' @examples
roll_midnight <- function(time_value){
  time_value %>%
    na.omit %>%
    hms %>% (function(z){
      current_hour <- ifelse(hour(z) < 12, hour(z)+24, hour(z))
      current_hour * 60^2 + minute(z) * 60 + second(z)
    }) 
}

average_time <- function(time_value){
  time_value %>%
    roll_midnight
  mean %>% 
    ifelse(./60^2 >=24, .-24*60^2, .) %>%
    seconds_to_period
}

reformat_csv <- function(df, var){
  df %>%
    select(one_of(c("sleep_date", var))) %>%
    rename("target" = var) %>%
    na.omit() %>%
    mutate(top_cols = max(stringr::str_count(target, ","))+1) %>%
    separate(target, sep=",", into=paste("w", 1:.$top_cols[1], sep="_")) %>%
    select(-top_cols) %>% 
    gather(variable, value, -sleep_date) %>%
    arrange(sleep_date, variable) %>%
    na.omit()
}

generate_from_to_segments <- function(df, anchor_cols, event_type){
  date_col <- "sleep_date"
  df %>% 
    select(one_of(c(date_col, anchor_cols))) %>%
    na.omit() %>%
    mutate_at(vars(anchor_cols), as.character) %>%
    mutate(event_type = event_type) %>%
    rename(
      "from" = anchor_cols[1],
      "to" = anchor_cols[2]
    ) %>%
    select(sleep_date, from, to, event_type)
}

# dynamically generate averages over N days
generate_average_over_n_days <- function(processed_sleep_data, n){
  
}


time_scaler <- function(z){
  converted_z <- z/60^2
  stringr::str_pad(
    round(converted_z) %>% 
      ifelse(. >= 24, .-24, .), 
    width=2, 
    pad = "0", 
    side = "left"
  ) %>% paste(
    stringr::str_pad(
      round(60 * abs(converted_z - round(converted_z))),
      width=2,
      pad="0",
      side="left"
    ), 
    sep=":")
}


summary_extraction <- function(sleep_data, agg_function){
  
  # fetch date boundaries
  most_recent_date_break <- max(sleep_data$sleep_date) - days_to_average
  previous_date_break <- most_recent_date_break - days_to_average
  
  # create filters (if changes needed to be made, its probably here
  create_filters <- function(df, date_var){
    df %>%
      rename("date_var" = date_var) %>%
      mutate(
        filter_a._all_recorded_history = TRUE,
        filter_d._yesterday = date_var == max(date_var),
        filter_c._most_recent_n_days = date_var > most_recent_date_break,
        filter_d._previous_n_days = (date_var > previous_date_break) & 
          (date_var <= most_recent_date_break)
      )
  }
  
  data_with_filters <- sleep_data %>% create_filters("sleep_date")
  
  apply_to_all_filters <- function(agg_function){
    filter_cols <-data_with_filters %>% 
      select(starts_with("filter")) %>% 
      colnames %>% setNames(.,.)
    col_labels <- filter_cols %>%
      gsub("filter_","",.) %>%
      gsub("_n_",paste0(" ",days_to_average," "),.) %>%
      gsub("_"," ",.)
    lapply(
      filter_cols,
      function(z){
        data_with_filters %>% 
          filter(!!rlang::parse_expr(z)) %>%
          agg_function
      }
    ) %>% (function(z){
      bind_rows(z) %>% 
        mutate(category = col_labels)
    })
  }
  
  apply_to_all_filters(agg_function)
  
}

