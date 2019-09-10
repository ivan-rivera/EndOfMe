# ================================
# Data extraction and processing
# ================================


#' Fetch data from Google Sheets
retrieve_sleep_data <- function(){
  gs_auth() # authenticate
  gs_title(target_spreadsheet) %>% 
    gs_read(ws=target_worksheet) %>%
    filter(!is.na(`sleep rating`))
}


#' Compile datasets for plotting and modelling
#'
#' @param sleep_df data from Google Sheets
process_sleep_data <- function(sleep_df){
  
  processed_sleep_data <- prepare_sleep_data(sleep_df)
  events <- collect_events_log(processed_sleep_data)
  annotations <- collect_annotations(processed_sleep_data)
  nightly_indicators <- compile_nightly_indicators(processed_sleep_data)
  weekend_boundaries <- generate_weekend_boundaries(processed_sleep_data)
  wakings <- compile_awakenings_data(processed_sleep_data)
  sleep_stats <- get_sleep_statistics(wakings)
  summary_data <- get_summary_data(processed_sleep_data, sleep_stats, annotations)
  model_data <- get_model_data(processed_sleep_data, sleep_stats, nightly_indicators, events)
  model_vars <- get_model_vars(model_data)
  
  summary_limits_data <- tribble(
    ~variable, ~limit, ~lower_bound, ~upper_bound, ~shadow_limit,
    "average hours asleep", 8, 3, 6, 9,
    "average longest sleep segment", 6, 1, 3, 8,
    "average pills used", 1, 0.25, 0.75, 1.1,
    "average sleep rating", 7, 3, 6, 7.5, 
    "average mins to fall asleep", 20, 30, 60, 80,
    "percentage of time asleep", 0.9, 0.5, 0.75, 1
  )
  
  list(
    "weekend boundaries" = weekend_boundaries,
    "sleep" = processed_sleep_data,
    "wakings" = wakings,
    "hourly annotations" = annotations,
    "nightly indicators" = nightly_indicators,
    "events" = events,
    "sleep stats" = sleep_stats,
    "modelling" = list(
      "data" = model_data,
      "variables" = model_vars
    ),
    "summary" = list(
      "data" = summary_data,
      "limits" = summary_limits_data
    )
  )
  
}


#' Initial preprocessing steps
prepare_sleep_data <- function(sleep_df){
  sleep_df %>% 
    set_colnames(
      ., colnames(.) %>% gsub("\\s+", "_", .)
    ) %>% mutate(
      sleep_date = lubridate::dmy(sleep_night),
      sleep_rating_average = zoo::rollmeanr(sleep_rating, k=days_to_average, fill=NA)
    )
}


#' Extract event log from Google Sheets
collect_events_log <- function(sleep_df){
  events_df <- sleep_df %>% 
    select(sleep_date, sleep_rating, special) %>%
    na.omit()
}


#' Compile hourly annotation data
collect_annotations <- function(sleep_df){
  
  toilet_visits <- sleep_df %>% 
    reformat_csv("toilet") %>%
    mutate(
      time = paste(gsub("\\s+", "", value), "00", sep=":"),
      annotation = "toilet",
    ) %>% select(-variable, -value)
  
  pills <- sleep_df %>% 
    reformat_csv("pills") %>%
    mutate(
      time = paste(gsub(".* (\\d+:\\d+)$", "\\1", value), "00", sep=":"),
      annotation = "pills",
      message = gsub("(.* )(\\d+:\\d+)$", "\\1", value)
    ) %>% select(-variable, -value)
  
  bind_rows(toilet_visits, pills) %>%
    mutate(time = roll_midnight(time))
}


#' Compile nightly indicators (nose state, panic, etc)
compile_nightly_indicators <- function(sleep_df){
  nightly_indicators <- sleep_df %>%
    transmute(
      sleep_date = sleep_date, 
      bedtime_palpitations = ifelse(bedtime_palpitations == "yes", 2, 0), 
      midnight_palpitations = ifelse(midnight_palpitations == "yes", 2, 0),
      panic = as.numeric(factor(panic, c("no", "nearly", "yes")))-1,
      otrivin = 2 * as.numeric(grepl("otrivin", nose_state)),
      nose_state = as.numeric(factor(
        gsub("\\s*\\(otrivin\\)","",nose_state), 
        c("clear", "partially blocked", "blocked")
      ))-1
    ) %>% gather(variable, value, -sleep_date) %>%
    mutate(
      variable = gsub("_", " ", variable),
      variable = gsub("palpitations", "palp.", variable)
    )
}

#' Create weekend boundary data
generate_weekend_boundaries <- function(sleep_df){
  sleep_df %>%
    select(sleep_date) %>%
    mutate(weekdays = weekdays(sleep_date)) %>%
    filter(ifelse(weekdays %in% c("Saturday", "Sunday"), TRUE, FALSE)) %>%
    mutate(
      boundary = ifelse(weekdays == "Saturday", "from", "to")
    ) %>%
    # we need to make sure that the series begin with a "from" and end with a "to"
    select(-weekdays) %>% (function(df){
      adjusted_df <- df
      if(df %$% boundary %>% tail(1) == "from"){
        adjusted_df <- df %>% bind_rows(
          tibble(
            "sleep_date" = df %$% sleep_date %>% tail(1),
            "boundary" = "to"
          ) 
        )
      }
      if(df %$% boundary %>% head(1) == "to"){
        adjusted_df <- df %>% bind_rows(
          tibble(
            "sleep_date" = df %$% sleep_date %>% head(1),
            "boundary" = "from"
          ) 
        )
      }
      adjusted_df
    }) %>% mutate(id = rep(seq(1:(nrow(.)/2)), each=2)) %>%
    spread(boundary, sleep_date) %>% select(-id)
}


#' Fetch awakening times and statuses
compile_awakenings_data <- function(sleep_df){
  sleep_df %>% 
    reformat_csv("waking") %>%
    mutate(
      event_type = ifelse(grepl("\\(SS\\)", value), "shallow sleep", "awake"),
      value = gsub("\\s+|\\(SS\\)", "", value)
    ) %>%
    select(-variable) %>%
    na.omit() %>%
    separate(value, into=c("from","to"), sep="-") %>%
    mutate_at(vars(from, to), function(z) paste(z, "00", sep=":")) %>%
    # ADD TIME IT TAKES TO FALL ASLEEP
    bind_rows(
      generate_from_to_segments(
        sleep_df, c("bedtime", "asleep"), event_type="falling asleep"
      )
    ) %>%
    # ADD TIME IT TAKES FROM START TO END
    bind_rows(
      generate_from_to_segments(
        sleep_df, c("asleep", "up"), event_type="asleep"
      )
    ) %>% mutate_at(vars(from, to), function(z) roll_midnight(z))
}


#' generate sleep stats
get_sleep_statistics <- function(wakings_df){
  
  main_attributes <- wakings_df %>% 
    mutate(
      time_gap_seconds = to-from,
      event_type = gsub("\\s+","_", event_type)
    ) %>% 
    select(sleep_date, event_type, time_gap_seconds) %>%
    group_by(sleep_date, event_type) %>%
    summarise_all(sum) %>%
    ungroup %>%
    spread(event_type, time_gap_seconds) %>%
    mutate_at(vars(-sleep_date), function(z) replace_na(z, 0) / 60) %>%
    transmute(
      sleep_date = sleep_date,
      time_awake_in_the_middle_of_the_night = awake + shallow_sleep/shallow_sleep_factor,
      time_asleep = asleep - shallow_sleep/shallow_sleep_factor - awake,
      time_to_fall_asleep = falling_asleep
    )
  
  longest_sleep_segment <- wakings_df %>%
    group_by(sleep_date) %>%
    mutate(
      asleep_from = max(ifelse(event_type == "asleep", from, 0)),
      asleep_to = max(ifelse(event_type == "asleep", to, 0))
    ) %>%
    filter(
      from >= asleep_from, 
      to <= asleep_to
    ) %>%
    mutate(awakening_event = max(event_type != "asleep")) %>%
    (function(df){
      no_awakenings <- df %>% 
        filter(awakening_event == 0) %>%
        ungroup %>%
        transmute(
          sleep_date = sleep_date,
          longest_sleep_segment = to-from
        )
      with_awakenings <- df %>%
        filter(event_type != "asleep") %>%
        mutate(
          first_sleep_chunk = min(from) - asleep_from,
          last_sleep_chunk = asleep_to - max(to),
          midnight_chunks = from - lag(to),
          longest_segment = pmax(first_sleep_chunk, last_sleep_chunk, midnight_chunks, na.rm=TRUE)
        ) %>%
        summarise(longest_sleep_segment = max(longest_segment, na.rm=TRUE)) %>%
        ungroup
      bind_rows(no_awakenings, with_awakenings) %>%
        arrange(sleep_date) %>%
        mutate(longest_sleep_segment = longest_sleep_segment/60)
    })
  
  
  count_of_awakenings <- wakings_df %>%
    filter(event_type %in% c("awake", "shallow sleep")) %>%
    group_by(sleep_date) %>%
    summarise(number_of_awakenings = n()) %>%
    ungroup
  
  main_attributes %>% 
    left_join(longest_sleep_segment, by="sleep_date") %>%
    left_join(count_of_awakenings, by="sleep_date") %>%
    gather(variable, value, -sleep_date) %>%
    mutate(variable = gsub("_"," ",variable)) %>%
    group_by(variable) %>%
    arrange(sleep_date) %>%
    mutate(n_day_average = zoo::rollmeanr(value, k=days_to_average, fill=NA)) %>%
    ungroup
  
}

#' Generate summary data for the main plot
get_summary_data <- function(sleep_data, sleep_stats, annotations){
  
  sleep_ratings <- summary_extraction(
    sleep_data,
    function(df){
      df %>% summarise(
        average_sleep_rating = mean(sleep_rating)
      )
    }
  )
  
  sleep_quality_summary <- summary_extraction(
    sleep_stats %>%
      select(-n_day_average) %>%
      mutate(variable = gsub(" ","_",variable)) %>%
      spread(variable, value),
    function(df){
      df %>% summarise(
        average_hours_asleep = mean(time_asleep)/60,
        average_mins_to_fall_asleep = mean(time_to_fall_asleep),
        percentage_of_time_asleep = mean(time_asleep/(time_asleep+time_awake_in_the_middle_of_the_night)),
        average_longest_sleep_segment = mean(longest_sleep_segment)/60
      )
    }
  )
  
  sleeping_pill_usage <- summary_extraction(
    data.frame(
      sleep_date = seq(
        from = min(sleep_data$sleep_date),
        to = max(sleep_data$sleep_date),
        by = "day"
      )
    ) %>% left_join(
      annotations %>% 
        filter(annotation == "pills") %>%
        select(sleep_date, time),
      by = "sleep_date"
    ) %>% group_by(sleep_date) %>%
      summarise(pills_used = sum(!is.na(time))) %>%
      ungroup,
    function(df){
      df %>% summarise(
        average_pills_used = mean(pills_used)
      )
    }
  )
  
  sleep_ratings %>%
    left_join(sleep_quality_summary, by="category") %>%
    left_join(sleeping_pill_usage, by="category") %>%
    gather(variable, value, -category) %>%
    mutate(variable = gsub("_"," ", variable))
  
}

#' create required data for modelling
get_model_data <- function(sleep_data, sleep_stats, indicators, events){
  sleep_data %>%
    transmute(
      # gonna ignore sleeping pill intake for now
      # I take then rarely, so there is little data and
      # there is also a bias involved -- if I take them
      # in the middle of the night, things are going bad
      # and even though they might improve my sleep a little
      # just by looking at their effect on sleep ratings
      # we might assume the opposite only because I take them
      # on bad nights only. Furthermore, I want to be able
      # to predict my rating before going to bed, whereas
      # when I wake up in the middle of the night and I feel
      # like I need a pill, I can already tell that the rating
      # is going to be low. If I start taking them regularly
      # then I will attempt to introduce a break point into the data
      sleep_date = sleep_date,
      sleep_rating = sleep_rating,
      temperature = temperature,
      alcohol_std = alcohol_std
    ) %>%
    left_join(
      sleep_stats %>% 
        select(-n_day_average) %>%
        mutate(variable = gsub("\\s+", "_", variable)) %>% 
        spread(variable, value),
      by="sleep_date"
    ) %>%
    left_join(
      indicators %>% 
        mutate(
          variable = gsub("\\s+", "_", variable),
          variable = gsub("\\.", "", variable)
        ) %>% spread(variable, value),
      by="sleep_date"
    ) %>%
    left_join(
      events %>%
        transmute(
          sleep_date = sleep_date,
          event = 1
        ),
      by="sleep_date"
    ) %>% 
    replace(., is.na(.), 0) %>%
    mutate(
      event = replace_na(event, 0),
      sleep_rating_day_before = lag(sleep_rating),
      time_asleep_day_before = lag(time_asleep),
      sleep_rating_next = lead(sleep_rating),
      time_asleep_next = lead(time_asleep)
    ) %>% filter(
      !is.na(time_asleep_day_before),
      !is.na(sleep_rating_day_before)
    )
}

#' a small function that isolates predictor variables
get_model_vars <- function(df){
  list(
    # everything that isn't in the below list is a predictor
    "id" = c("sleep_date"),
    "exclusions" = df %>% select(ends_with("next")) %>% colnames
  )
}