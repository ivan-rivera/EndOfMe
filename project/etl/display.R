# ================================
# Visualisations
# ================================

# todo:
# - review themes of all plots (look into ggthemes and ggtech) and review flexboard css



# SUMMARY ---------------------------------------------

summary_plot <- function(sleep_collection){
  shades_alpha <- 0.4
  category_labels <- sleep_collection[["summary"]][["data"]] %>% 
    select(category) %>% 
    distinct %>% 
    arrange(category) %$%
    category %>% gsub("^\\w.\\s","",.)
  p <- sleep_collection[["summary"]][["data"]] %>%
    mutate(variable = as.character(variable)) %>%
    ggplot(aes(x=category, y=value)) +
    facet_wrap(~variable, nrow=2, scales="free_x") +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=-Inf, xmax=Inf, ymin=0, ymax=lower_bound),
      data=sleep_collection[["summary"]][["limits"]], 
      fill=custom_color_palette[["secondary2"]][2],
      alpha=shades_alpha
    ) +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=-Inf, xmax=Inf, ymin=lower_bound, ymax=upper_bound),
      data=sleep_collection[["summary"]][["limits"]], 
      fill=custom_color_palette[["secondary2"]][3], 
      alpha=shades_alpha
    ) +
    geom_rect(
      aes(x=NULL,y=NULL,xmin=-Inf, xmax=Inf, ymin=upper_bound, ymax=Inf),
      data=sleep_collection[["summary"]][["limits"]], 
      fill=custom_color_palette[["secondary2"]][4], 
      alpha=shades_alpha
    ) +
    geom_bar(
      stat="identity", 
      fill=custom_color_palette[["secondary2"]][1], 
      color=custom_color_palette[["secondary2"]][4], 
      width=0.1
    ) +
    geom_point(
      size=5, 
      color=custom_color_palette[["secondary2"]][1]
    ) +
    geom_hline(
      aes(yintercept=shadow_limit), 
      data=sleep_collection[["summary"]][["limits"]], 
      color="transparent"
    ) +
    geom_hline(
      aes(yintercept = limit), 
      data=sleep_collection[["summary"]][["limits"]], 
      linetype="dashed", 
      color=custom_color_palette[["secondary1"]][1],
      size=1
    ) +
    labs(x="", y="") +
    ggtitle(
      "Summary Statistics", 
      subtitle="dashed lines represent goals"
    ) +
    scale_x_discrete(labels=category_labels) +
    scale_y_continuous(labels=facet_percent_formatter, expand=c(0,0)) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=0),
      panel.spacing = unit(1.5, "lines")
    ) + 
    coord_flip()
  p
}

sleeping_pill_info <- function(sleep_collection){
  latest_sleep_date <- max(sleep_collection[["sleep"]]$sleep_date)
  lastest_sleeping_pill_date <- sleep_collection[["sleep"]] %>% 
    filter(!is.na(pills)) %$% 
    max(sleep_date)
  p <- data.frame(
    label = sprintf(
      "last sleeping pill consumed %s days ago", 
      as.numeric(latest_sleep_date - lastest_sleeping_pill_date)
    )
  ) %>% ggplot(aes(x=1, y=1, label=label)) + 
    geom_tile(
      fill=custom_color_palette[["primary"]][5]
    ) +
    custom_theme +
    theme_void() + 
    theme(
      panel.background = element_rect(fill=custom_color_palette[["primary"]][5]),
      plot.background = element_rect(fill=custom_color_palette[["primary"]][5], color=NA)
    ) +
    geom_fit_text(
      family="Source Sans Pro",
      colour=custom_color_palette[["secondary1"]][1],
      size=16
    )
  p
}


# CHARTS ----------------------------------------------



rating_over_time <- function(sleep_collection){
  # todo:
  # - add model predictions (once they become available)
  average_col <- sprintf("average over %s days", days_to_average)
  col_mappings <- c(
    custom_color_palette[["secondary2"]][2],
    custom_color_palette[["secondary2"]][1]
  ) %>% setNames(c("daily", average_col))
  size_mappings <- c(0.7, 1) %>% setNames(c("daily", average_col))
  linetype_mappings <- c("dotted", "solid") %>% setNames(c("daily", average_col))
  p <- sleep_collection[["sleep"]] %>%
    select(sleep_date, sleep_rating, sleep_rating_average) %>%
    gather(type, rating, -sleep_date) %>%
    #filter(type == "sleep_rating") %>% # removed the average line
    mutate(
      type = ifelse(type == "sleep_rating", "daily", sprintf("average over %s days", n_day_breaks)),
      weekend = ifelse(weekdays(sleep_date) %in% c("Saturday", "Sunday"), TRUE, FALSE)
    ) %>% ggplot() +
    geom_rect(
      aes(xmin=from, xmax=to, ymin=-Inf, ymax=Inf), 
      fill=custom_color_palette[["primary"]][1], 
      alpha=0.25, 
      data=sleep_collection[["weekend boundaries"]]
    ) +
    geom_hline(
      yintercept = positive_rating_threshold, 
      linetype="dashed", 
      size=0.5, 
      color=custom_color_palette[["primary"]][1]
    ) +
    geom_point(
      aes(x=sleep_date, y=predicted_rating), 
      color=custom_color_palette[["secondary1"]][1], 
      size=1, 
      data=sleep_collection[["sleep"]]
    ) +
    geom_line(
      aes(x=sleep_date, y=rating, color=type, size=type, linetype=type), 
      alpha=0.9, 
    ) +
    # geom_label(
    #   aes(x=sleep_date, y=sleep_rating-0.25, label=special), 
    #   data=sleep_collection[["events"]], 
    #   fontface="bold", 
    #   size=3, 
    #   color=custom_color_palette[["secondary1"]][5],
    #   fill=custom_color_palette[["secondary1"]][1]
    # ) + 
    scale_y_continuous(breaks=1:10, expand=expand_settings) +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) +
    scale_color_manual("", values = col_mappings) +
    scale_size_manual("", values = size_mappings) +
    scale_linetype_manual("", values = linetype_mappings) +
    labs(x="", y="") +
    ggtitle(
      "How I Subjectively Rated Each Night", 
      subtitle = sprintf("shaded areas = weekends; dots = my guessed ratings from the night before; dotted line = actuals, solid line = %s day average", days_to_average)
    ) +
    custom_theme +
    theme(
      #legend.position = c(0.1, 0.85),
      legend.position="none",
      legend.margin = margin(t=-1.25, unit="cm")
    ) + guides(color=guide_legend(nrow=2))
  p
}


hourly_plot <- function(sleep_collection){
  
  time_plot_aes <- aes(
    x=sleep_date, 
    xend=sleep_date,
    y=from,
    yend=to,
    color=event_type
  )
  
  p <- ggplot() +
    geom_segment(
      size=main_plot_segment_size, 
      alpha=1, 
      data=sleep_collection[["wakings"]] %>% filter(event_type=="asleep"), 
      time_plot_aes
    ) +
    geom_segment(
      size=main_plot_segment_size, 
      alpha=1, 
      data=sleep_collection[["wakings"]] %>% filter(event_type!="asleep"), 
      time_plot_aes
    ) +
    geom_point(
      aes(x=sleep_date, y=time, shape=annotation), 
      data=sleep_collection[["hourly annotations"]], 
      size=main_plot_segment_size/2,
      color=custom_color_palette[["secondary1"]][1]
    ) +
    labs(x="", y="") +
    ggtitle("Each Night Dissected By Hour", subtitle="all times are approximate") +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) +
    scale_y_continuous(
      labels=time_scaler, 
      expand=expand_settings, 
      breaks=seq(23*60^2, length.out=6, by=2*60^2)
    ) + 
    scale_shape_manual("", values=c("pills"=12, "toilet"=2)) +
    scale_colour_manual("", values=c(
      "asleep"=custom_color_palette[["primary"]][4], 
      "falling asleep"=custom_color_palette[["secondary1"]][4], 
      "awake"=custom_color_palette[["secondary2"]][4], 
      "out of bed"=custom_color_palette[["secondary2"]][1],
      "shallow sleep"=custom_color_palette[["primary"]][2]
    )) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=0),
      legend.position=c(0.7, 1.025),
      legend.margin = margin(t=-1, unit="cm"),
      plot.margin = unit(c(0.5,1,0.1,0.2), "cm")
    ) + guides(
      color=guide_legend(nrow=1), 
      shape=guide_legend(nrow=1)
    )
  p
}




nightly_indicator_plot <- function(sleep_collection){
  p <- sleep_collection[["nightly indicators"]] %>% 
    ggplot(aes(x=sleep_date, y=variable, color=factor(value))) +
    geom_point(shape=15, size=main_plot_segment_size) +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) + 
    labs(x="", y="") +
    scale_color_manual("", values=c(
      "0"=good_bad_colours[["good"]], 
      "1"=good_bad_colours[["average"]], 
      "2"=good_bad_colours[["bad"]]
    )) +
    ggtitle(
      "Nightly Observations",
      subtitle="positive, neutral (if applicable), negative"
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=0)
      ) +
    guides(color=FALSE)
  p
}


hours_of_sleep <- function(sleep_collection){
  p <- sleep_collection[["sleep stats"]] %>% 
    select(-n_day_average) %>%
    filter(variable %in% c("time asleep", "longest sleep segment")) %>%
    spread(variable, value) %>%
    mutate(`time asleep` = `time asleep` - `longest sleep segment`) %>%
    gather(variable, value, -sleep_date) %>%
    mutate(variable = factor(variable, levels = c("time asleep", "longest sleep segment"))) %>% 
    na.omit() %>%
    ggplot(aes(x=sleep_date)) +
    geom_bar(aes(y=value, fill=variable), alpha=1, stat="identity", width=0.5) +
    geom_hline(yintercept = 480, color=custom_color_palette[["secondary1"]][1]) + 
    geom_line(
      aes(y=n_day_average), 
      color=custom_color_palette[["primary"]][2], 
      linetype="dashed",
      size=0.5,
      data=sleep_collection[["sleep stats"]] %>% filter(variable == "time asleep")
    ) + 
    geom_line(
      aes(y=n_day_average), 
      color=custom_color_palette[["secondary2"]][1], 
      linetype="dashed",
      size=0.5,
      data=sleep_collection[["sleep stats"]] %>% 
        filter(variable == "longest sleep segment") %>%
        na.omit()
    ) +
    labs(x="", y="") +
    ggtitle(
      "How Much Sleep I'm Getting", 
      subtitle = sprintf("reported in hours; dashed lines are %s day averages", days_to_average)
    ) +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=c(expand_settings[1], expand_settings[2]-0.25)
    ) +
    scale_y_continuous(
      label = function(z) paste0(round(z/60,1), " "),
      breaks = seq(2,8, by=2) * 60,
      expand = expand_settings
      #sec.axis=sec_axis(~ ./60, name="Hours")
    ) +
    scale_fill_manual("", values=c(
      "time asleep" = custom_color_palette[["primary"]][3], 
      "longest sleep segment" = custom_color_palette[["secondary2"]][3]
    )) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=0),
      axis.text.y = element_text(margin = margin(l=-3)),
      legend.margin = margin(t=-1.25, unit="cm"),
      legend.position = c(0.825, 1.05)
    ) + 
    guides(fill=guide_legend(nrow=1))
  p
}


falling_asleep_and_staying_awake <- function(sleep_collection){
  p <- sleep_collection[["sleep stats"]] %>% 
    filter(!variable %in% c("number of awakenings", "time asleep", "longest sleep segment")) %>%
    ggplot(aes(x=sleep_date)) +
    facet_wrap(~variable, ncol=1, scales="free_y") +
    geom_point(aes(y=value), color=custom_color_palette[["secondary1"]][1], size=1.5, alpha=0.5) +
    geom_line(aes(y=value), color=custom_color_palette[["secondary2"]][1]) +
    geom_line(aes(y=n_day_average), color=custom_color_palette[["secondary2"]][3], alpha=0.5, linetype="dashed") +
    labs(x="", y="") +
    ggtitle(
      "Falling Asleep and Staying Awake",
      subtitle=sprintf("reported in hours; dashed line represents a %s day average", days_to_average)
    ) + 
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=c(expand_settings[1], expand_settings[2]+0.25)
    ) +
    scale_y_continuous(
      #sec.axis=sec_axis(~ ./60, name="Hours"), 
      #breaks = seq(2,8, by=2) * 60,
      label = function(z) paste0(round(z/60,1), " "),
      expand=c(expand_settings[1]+0.1, expand_settings[2]-0.15)
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=0)
    )
  p
}


# todo: if you recover this, call it sleep_debt
damage_plot <- function(sleep_collection){
  p <- sleep_collection[["sleep stats"]] %>% 
    select(-n_day_average) %>%
    filter(variable == "time asleep") %>%
    mutate(
      date_time = as_datetime(sleep_date),
      value = value/60 - min_required_hours,
      value = ifelse(value > 0, value * recovery_multiplier, value),
      damage_ext = pmin(0, Reduce(
        function(x,y){ifelse(x+y > 0, 0, x+y)}, 
        value, 
        accumulate = TRUE
      ))
    ) %>% ggplot(aes(x=date_time)) + 
    geom_hline(
      yintercept = 0, 
      linetype="dotted", 
      size = 0.5,
      color=custom_color_palette[["primary"]][1]
    ) + 
    geom_line(aes(y=damage_ext, color=damage_ext), size=0.5) +
    scale_fill_gradient2(
      low=good_bad_colours[["bad"]], 
      mid=good_bad_colours[["average"]], 
      high=good_bad_colours[["good"]], 
      midpoint=0
    ) +
    scale_color_gradient2(
      low=good_bad_colours[["bad"]], 
      mid=good_bad_colours[["average"]], 
      high=good_bad_colours[["good"]], 
      midpoint=0
    ) +
    scale_y_continuous(expand=expand_settings) +
    scale_x_datetime(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) + 
    labs(x="", y="Hours of Sleep Debt") +
    ggtitle(
      "Accumulated Sleep Debt",
      subtitle=sprintf("Each night when I get less sleep than I need (%s), I accumulated sleep debt.\nEach extra hour that I get above the limit the following night counts for %s hours", min_required_hours, recovery_multiplier)  
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle=0)) +
    guides(fill=FALSE, color=FALSE)
  p
}


sleep_hours <- function(sleep_collection){
  p <- sleep_collection[["sleep"]] %>% 
    select(sleep_date, bedtime, up) %>%
    mutate_at(vars(bedtime, up), roll_midnight) %>%
    ggplot(aes(x=sleep_date)) +
    geom_segment(
      aes(x=sleep_date, xend=sleep_date, y=bedtime, yend=up),
      size=1.5,
      color=custom_color_palette[["primary"]][1],
      alpha=0.5
    ) +
    geom_point(aes(y=bedtime), color=custom_color_palette[["secondary2"]][1], size=3) +
    geom_point(aes(y=up), color=custom_color_palette[["secondary2"]][1], size=3) +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      expand=c(expand_settings[1], expand_settings[2]+0.25)
    ) +
    scale_y_continuous(
      labels=time_scaler, 
      breaks=seq(21*60^2, length.out=7, by=2*60^2),
      expand=c(expand_settings[1]+0.01, expand_settings[2]+30*60)
    ) +
    labs(x="", y="") +
    ggtitle(
      "Times I Go To Bed And Get Up", 
      subtitle = "bed time = time I switch the lights off, up time = time I get out of bed"
    ) +
    custom_theme
  p
}


sleep_efficiency_plot <- function(sleep_collection){
  p <- sleep_collection[["sleep stats"]] %>% 
    select(sleep_date, variable, value) %>%
    mutate(variable = gsub("\\s+","_", variable)) %>%
    spread(variable, value) %>%
    transmute(
      sleep_date = sleep_date,
      sleep_efficiency = time_asleep / (
        time_asleep +
          time_to_fall_asleep + 
          time_awake_in_the_middle_of_the_night
      ),
      sleep_efficiency_n_day_avg = zoo::rollmeanr(sleep_efficiency, k=days_to_average, fill=NA)
    ) %>% ggplot(aes(x=sleep_date)) +
    geom_hline(yintercept = desired_sleep_efficiency, color=custom_color_palette[["secondary1"]][2]) +
    geom_line(
      aes(y=sleep_efficiency), 
      color=custom_color_palette[["secondary2"]][4], 
      size=1
    ) +
    geom_point(
      aes(y=sleep_efficiency), 
      color=custom_color_palette[["secondary2"]][2],
      size=1.5
    ) +
    geom_line(
      aes(y=sleep_efficiency_n_day_avg), 
      color=custom_color_palette[["secondary2"]][1], 
      size=0.5,
      linetype="dotted"
    ) +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) +
    scale_y_continuous(
      labels=scales::percent,
      expand = c(0, 0.05),
      limits = c(0, 1)
    ) +
    labs(x="", y="") +
    ggtitle(
      "Sleep Efficiency: Proportion of time in bed spent asleep", 
      subtitle=sprintf("Actuals and %s day averages (dotted); horizontal line represents healthy minimum", days_to_average)
    ) +
    custom_theme
  p
}

daily_alcohol_plot <- function(sleep_collection){
  p <- sleep_collection[["sleep"]] %>%
    select(sleep_date, alcohol_std)  %>% 
    ggplot(aes(x=sleep_date)) +
    geom_bar(
      aes(y=alcohol_std), 
      stat='identity', 
      width=0.25,
      fill=custom_color_palette[["primary"]][2]
    ) + 
    geom_point(
      aes(y=alcohol_std), 
      size=1,
      color=custom_color_palette[["secondary2"]][2]
    ) +
    labs(x="", y="standard drinks") +
    ggtitle("Daily Alcohol Consumption") +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) + custom_theme
  p
}

alcohol_plot_over_range <- function(sleep_collection){
  p_alc_averaged <- sleep_collection[["sleep"]] %>%
    select(sleep_date, alcohol_std) %>%
    mutate(moving_sum = zoo::rollsumr(alcohol_std, k=days_to_average, fill=0)) %>% 
    ggplot(aes(x=sleep_date, y=moving_sum)) +
    ggtitle(sprintf("Total Alcohol Consumption Over Last %s Days", days_to_average)) +
    geom_line(color=custom_color_palette[["secondary2"]][2]) +
    geom_point(color=custom_color_palette[["secondary2"]][3], alpha=0.5) +
    labs(x="", y="standard drinks") +
    scale_x_date(
      labels = scales::date_format("%d/%m/%y"),
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=expand_settings
    ) + custom_theme
}


# ANALYTICS -------------------------------------------


plot_hours_v_rating_relation <- function(sleep_collection){
  # todo: change this to a 3D plot showing relationships between ratings, hours and energy next day
  # consider using plotly for this
  relation_data <- sleep_collection[["modelling"]][["data"]] %>% 
    mutate( # jitter ratings and sleep times
      sleep_rating = jitter(sleep_rating, sd(sleep_rating)/2),
      time_asleep = jitter(time_asleep, sd(time_asleep)/2)
    )
  p <- relation_data %>%
    ggplot(aes(x=time_asleep, y=sleep_rating)) +
    geom_point(color=custom_color_palette[["secondary2"]][1]) +
    geom_smooth(method="lm", se=FALSE, color=custom_color_palette[["secondary1"]][1]) +
    labs(x="time asleep (mins)", y="sleep rating") +
    ggtitle(
      "Relationship Between Sleep Ratings and Time Asleep",
      subtitle=sprintf(
        "All values are jittered; the correlation is %s",
        relation_data %>% 
          summarise(time_rating_corr = cor(sleep_rating, time_asleep)) %$%
          time_rating_corr %>% scales::percent()
      )
    ) + custom_theme +
    theme(axis.text.x = element_text(angle=0))
  p
}


model_performance_plot <- function(model_results){
  performance_plot_data <- model_results$performance %>% 
    left_join(
      model_results$predictions %>%
        transmute(
          response = variable,
          prediction = ifelse(response == "time_asleep_next", prediction/60, prediction),
          pred_label = sprintf("prediction for tomorrow: %s", round(prediction,2)),
          pred_label = ifelse(response == "time_asleep_next", paste0(pred_label, " hrs"), pred_label)
        ),
      by="response"
    ) %>%
    group_by(response) %>% mutate(
      cor = cor(actual, predicted),
      rmse = Metrics::rmse(actual, predicted)
    ) %>% ungroup %>% mutate(
      response = gsub("_next", "_prediction", response),
      response = gsub("_", " ", response),
      response = paste0(response, sprintf(
        "\nCOR: %s, RMSE: %s\n%s", 
        scales::percent(cor), round(rmse,2), pred_label)
      )
    ) 
  p <- performance_plot_data %>% ggplot(aes(x=actual, y=predicted)) +
    facet_wrap(~response, scales="free", nrow=1) +
    geom_point() +
    geom_point(color=custom_color_palette[["secondary2"]][1]) +
    geom_smooth(method="lm", se=FALSE, color=custom_color_palette[["secondary1"]][1]) +
    geom_abline(
      intercept = 0, 
      slope = 1, 
      linetype="dotted", 
      color=custom_color_palette[["primary"]][1]
    ) + 
    labs(x="Actual Ratings", y="Predicted Ratings") +
    custom_theme +
    theme(axis.text.x = element_text(angle=0)) +
    ggtitle(
      "Model Performance",
      subtitle = sprintf(
        "Predictions are made for %s",
        model_results$predictions$prediction_date[1] + 1 # +1 because we are predicting _next
      )
    )
  p
}


variable_importance_plot <- function(model_results){
  p <- model_results$variables %>% 
    filter(importance > 0.01) %>%
    group_by(response) %>%
    top_n(importance, n=10) %>%
    ungroup %>%
    mutate(
      variable = reorder(variable, importance)
    ) %>%
    mutate(
      importance = ifelse(response == "sleep_rating_next", -importance, importance)
    ) %>% mutate(
      response = gsub("_next", "_model", response),
      response = gsub("_", " ", response)
    ) %>% ggplot(aes(x=variable, y=importance)) +
    facet_wrap(~response, nrow=1, scales="free_x") +
    geom_bar(stat="identity", width=0.1, fill=custom_color_palette[["secondary2"]][2]) +
    geom_point(size=3, color=custom_color_palette[["secondary2"]][2]) +
    ggtitle("Variable Importances", subtitle = "All values are relative to the maximum of 100%") +
    labs(x="", y="") +
    custom_theme +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=8),
      panel.grid.major = element_blank(),
      panel.spacing = unit(0, "cm"),
    ) + coord_flip()
  p
}


# DATA ------------------------------------------------


display_data <- function(sleep_collection){
  
  display_cols <- c(
    "sleep date" = "sleep_night",
    "sleep rating" = "sleep_rating",
    "outdoor temp. (c)" = "temperature",
    "bedtime palpitations" = "bedtime_palpitations",
    "midnight palpitations" = "midnight_palpitations",
    "panic" = "panic",
    "nose state" = "nose_state",
    "alcohol intake (STD)" = "alcohol_std"
    #"notes" = "notes"
  )
  
  sleep_collection[["sleep"]] %>% 
    select(one_of(display_cols)) %>% 
    DT::datatable(
      colnames=display_cols,
      filter="top",
      caption="Manually collected sleep data. Noted that most numbers are approximations"
    )
  
}




