# Snapshot view: averages for the past N days compared with the previous N days, vs all recorded
# * sleep rating (out of 10 gauge)
# * hours slept (vs 8 hour benchmark gauge)
# * percentage of time in bed asleep
# * time to fall asleep (gauge with markers for 30 mins and hour)
# * bedtime (gauge with markers)
# * up time
# * length of longest sleep segment
# * sleeping pills used



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

sleep_summary <- get_summary_data(
  sleep_collection[["sleep"]],
  sleep_collection[["sleep stats"]],
  sleep_collection[["hourly annotations"]]
)

limits_data <- tribble(
  ~variable, ~limit, ~lower_bound, ~upper_bound,
  "average hours asleep", 8, 3, 6,
  "average longest sleep segment", 6, 1, 3,
  "average pills used", 1, 0.25, 0.75,
  "average sleep rating", 7, 3, 6,
  "average mins to fall asleep", 20, 30, 60,
  "percentage of time asleep", 0.9, 0.5, 0.75
)

sleep_summary %>%
  ggplot(aes(x=category, y=value)) +
  facet_wrap(~variable, nrow=2, scales="free_x") +
  geom_rect(
    aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=0, ymax=lower_bound), 
    data=limits_data, fill="lightblue", alpha=0.9
   ) +
  geom_rect(
    aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=lower_bound, ymax=upper_bound), 
    data=limits_data, fill="skyblue", alpha=0.9
  ) +
  geom_rect(
    aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=upper_bound, ymax=Inf), 
    data=limits_data, fill="skyblue2", alpha=0.9
  ) +
  geom_bar(stat="identity", fill="royalblue", width=0.1) +
  geom_point(size=5, color="navy") +
  geom_hline(aes(yintercept = limit), data=limits_data, linetype="dashed", color="orange2") +
  labs(x="", y="") +
  ggtitle(
    "Summary Statistics", 
    subtitle="dashed lines represent targets or upper thresholds, 
    \nshades areas also represent milestones"
  ) +
  custom_theme +
  coord_flip()


summary_plot <- function(sleep_collection){
  sleep_collection[["summary"]][["data"]] %>%
    ggplot(aes(x=category, y=value)) +
    facet_wrap(~variable, nrow=2, scales="free_x") +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=0, ymax=lower_bound), 
      data=sleep_collection[["summary"]][["limits"]], fill="lightblue", alpha=0.9
    ) +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=lower_bound, ymax=upper_bound), 
      data=sleep_collection[["summary"]][["limits"]], fill="skyblue", alpha=0.9
    ) +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=0, xmax=Inf, ymin=upper_bound, ymax=Inf), 
      data=sleep_collection[["summary"]][["limits"]], fill="skyblue2", alpha=0.9
    ) +
    geom_bar(stat="identity", fill="royalblue", width=0.1) +
    geom_point(size=5, color="navy") +
    geom_hline(aes(yintercept = limit), data=limits_data, linetype="dashed", color="orange2") +
    labs(x="", y="") +
    ggtitle(
      "Summary Statistics", 
      subtitle="dashed lines represent targets or upper thresholds, 
    \nshades areas also represent milestones"
    ) +
    custom_theme +
    coord_flip()
}



wakings_df <- sleep_collection[["wakings"]]
sleep_stats <- sleep_collection[["sleep stats"]]

hours_of_sleep



ratings_summary %>%
  ggplot(aes(x=category, y=average_rating)) +
  geom_bar(stat="identity", fill="royalblue", width=0.1) +
  geom_point(size=10, color="navy") +
  geom_hline(yintercept = 10, linetype="dotted", color="orange2") +
  labs(x="", y="") +
  ggtitle("Average Sleep Ratings") +
  custom_theme +
  coord_flip()






ratings_summary %>% ggplot(aes(x=))

gauge(
  3.71, min=0, max=10, 
  label = "Last 7 days",
  gaugeSectors(success=c(7, 10), warning=c(3,7), danger=c(0,3))
)

ratings_summary %>% mutate(
  performance_group = 
)

cut(ratings_summary$average_rating, breaks=c(0,3,7,10), labels=c("horrible", "bearable", "good"))


ratings_summary %>%
  mutate(performance = cut(
      average_rating, 
      breaks=c(0,3,7,10), 
      labels=c("horrible", "bearable", "good")
    )
  ) %>% 
  ggplot(aes(fill=performance, ymax=average_rating, ymin=0, xmin=1, xmax=2)) +
  geom_rect() +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
  geom_rect() + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = category, colour=performance), size=6.5, family="Poppins SemiBold") +
  #geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=4.2) +
  theme_void() +
  scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE)


sleep_data %>% mutate(
  filter_last_n_days = sleep_date > most_recent_date_break,
  filter_previous_n_days = sleep_date > previous_date_filter, sleep_date <= most_recent_date_filter,
  filter_all_days = TRUE
) %>% filter(!!rlang::parse_expr("filter_last_n_days"))




filter_types <- c("last_n_days", "previous_n_days", "all_days") %>% setNames(.,.)
lapply(filter_types,function(z){
  sleep_data %>% mutate(
    filter_last_n_days = sleep_date > most_recent_date_break,
    filter_previous_n_days = sleep_date > previous_date_filter, sleep_date <= most_recent_date_filter,
    filter_all_days = TRUE
  ) %>% filter(!!rlang::parse_expr(paste0("filter_", z)))
})


create_filters <- function(df, date_var){
  df %>%
    rename("date_var" = date_var) %>%
    mutate(
      filter_last_n_days = date_var > most_recent_date_break,
      filter_previous_n_days = (date_var > previous_date_filter) & 
        (date_var <= most_recent_date_filter),
      filter_all_recorded_history = TRUE
    )
}

data_with_filters <- sleep_data %>% create_filters("sleep_date")


apply_to_all_filters <- function(target_function){
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
        target_function
    }
  ) %>% (function(z){
    bind_rows(z) %>% 
      mutate(category = col_labels)
  })
}


test_func <- function(df) df %>% summarise(x = mean(sleep_rating, na.rm=TRUE))
apply_to_all_filters(test_func)


#sleep_data %>% 
  #filter(sleep_date > most_recent_date_filter)
  #filter(sleep_date > previous_date_filter, sleep_date <= most_recent_date_filter)
