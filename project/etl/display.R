# ================================
# Visualisations
# ================================

# todo:
# - review themes of all plots (look into ggthemes and ggtech) and review flexboard css



# SUMMARY ---------------------------------------------

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
    geom_hline(
      aes(yintercept = limit), 
      data=sleep_collection[["summary"]][["limits"]], 
      linetype="dashed", 
      color="orange2"
    ) +
    labs(x="", y="") +
    ggtitle(
      "Summary Statistics", 
      subtitle="dashed lines represent targets or upper thresholds, shades areas also represent milestones"
    ) +
    custom_theme +
    coord_flip()
}


# CHARTS ----------------------------------------------



rating_over_time <- function(sleep_collection){
  # todo:
  # - add model predictions (once they become available)
  average_col <- sprintf("average over %s days", days_to_average)
  col_mappings <- c("navy", "royalblue") %>% setNames(c("daily", average_col))
  linetype_mappings <- c("solid", "dashed") %>% setNames(c("daily", average_col))
  p <- sleep_collection[["sleep"]] %>%
    select(sleep_date, sleep_rating, sleep_rating_average) %>%
    gather(type, rating, -sleep_date) %>%
    mutate(
      type = ifelse(type == "sleep_rating", "daily", sprintf("average over %s days", n_day_breaks)),
      weekend = ifelse(weekdays(sleep_date) %in% c("Saturday", "Sunday"), TRUE, FALSE)
    ) %>%ggplot() +
    geom_rect(
      aes(xmin=from, xmax=to, ymin=sleep_collection[["rating boundaries"]][["min"]], ymax=sleep_collection[["rating boundaries"]][["max"]]), 
      fill="lightblue", alpha=0.25, data=sleep_collection[["weekend boundaries"]]
    ) +
    geom_hline(yintercept = positive_rating_threshold, linetype="solid", size=2, color="orange") +
    geom_point(aes(x=sleep_date, y=sleep_rating), color="navy", size=1, data=sleep_collection[["sleep"]]) +
    geom_point(aes(x=sleep_date, y=predicted_rating), color="firebrick2", size=1.5, shape=23, data=sleep_collection[["sleep"]]) +
    geom_line(aes(x=sleep_date, y=rating, color=type, linetype=type), alpha=0.75, size=0.75) +
    geom_label(
      aes(x=sleep_date, y=sleep_rating-0.25, label=special), 
      data=sleep_collection[["events"]], fontface="bold", size=3, color="black"
    ) + 
    scale_y_continuous(breaks=1:10, expand=expand_settings) +
    scale_x_date(date_breaks=sprintf("%s days", n_day_breaks), expand=expand_settings) +
    scale_color_manual("", values = col_mappings) +
    scale_linetype_manual("", values = linetype_mappings) +
    labs(x="", y="rating") +
    ggtitle(
      "How I Subjectively Rated Each Night", 
      subtitle = "Shaded areas = weekends; red hollow dots = my guessed ratings"
    ) +
    custom_theme +
    theme(
      legend.position = c(0.1, 0.8),
      axis.text.x = element_text(angle=45),
      legend.margin = margin(t=-1.25, unit="cm")
    ) + guides(color=guide_legend(nrow=2))
  return(p)
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
      alpha=0.5, 
      data=sleep_collection[["wakings"]] %>% filter(event_type=="asleep"), 
      time_plot_aes
    ) +
    geom_segment(
      size=main_plot_segment_size, 
      alpha=0.5, 
      data=sleep_collection[["wakings"]] %>% filter(event_type!="asleep"), 
      time_plot_aes
    ) +
    geom_point(
      aes(x=sleep_date, y=time, shape=annotation), 
      data=sleep_collection[["hourly annotations"]], 
      size=main_plot_segment_size/2
    ) +
    labs(x="", y="") +
    ggtitle("Every Excruciating Hour...", subtitle="All times are approximate") +
    scale_x_date(date_breaks=sprintf("%s days", n_day_breaks), expand=expand_settings) +
    scale_y_continuous(
      labels=time_scaler, 
      expand=expand_settings, 
      breaks=seq(23*60^2, length.out=6, by=2*60^2)
    ) + 
    scale_shape_manual("", values=c("pills"=12, "toilet"=6)) +
    scale_colour_manual("", values=c(
      "asleep"="skyblue", 
      "falling asleep"="grey", 
      "awake"="firebrick2", 
      "shallow sleep"="orange2"
    )) +
    custom_theme +
    theme(
      legend.position=c(0.2, 0.85),
      legend.margin = margin(t=-1, unit="cm"),
      plot.margin = unit(c(0,1,0.1,0.2), "cm")
    ) + guides(
      color=guide_legend(nrow=1), 
      shape=guide_legend(nrow=1)
    )
  return(p)
}




nightly_indicator_plot <- function(sleep_collection){
  p <- sleep_collection[["nightly indicators"]] %>% 
    ggplot(aes(x=sleep_date, y=variable, color=factor(value))) +
    geom_point(shape=15, size=main_plot_segment_size) +
    scale_x_date(date_breaks=sprintf("%s days", n_day_breaks), expand=expand_settings) + 
    labs(x="", y="") +
    scale_color_manual("", values=c(
      "0"=good_bad_colours[["good"]], 
      "1"=good_bad_colours[["average"]], 
      "2"=good_bad_colours[["bad"]]
    )) +
    ggtitle(
      "Nightly Observations",
      subtitle=sprintf(
        "%s = positive, %s = neutral (if applicable), %s = negative",
        gsub("\\d","",good_bad_colours[["good"]]),
        gsub("\\d","",good_bad_colours[["average"]]),
        gsub("\\d","",good_bad_colours[["bad"]])
      )
    ) +
    custom_theme +
    theme(
      axis.text = element_text(angle=45)
      ) +
    guides(FALSE)
  return(p)
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
    geom_bar(aes(y=value, fill=variable), alpha=0.5, stat="identity") +
    geom_hline(yintercept = 480, color="orange2") + 
    geom_line(
      aes(y=n_day_average), 
      color="skyblue", 
      linetype="dashed",
      size=1,
      data=sleep_collection[["sleep stats"]] %>% filter(variable == "time asleep")
    ) + 
    geom_line(
      aes(y=n_day_average), 
      color="firebrick2", 
      linetype="dashed",
      size=1,
      data=sleep_collection[["sleep stats"]] %>% 
        filter(variable == "longest sleep segment") %>%
        na.omit()
    ) +
    labs(x="", y="") +
    ggtitle("How Much Sleep I'm Getting", subtitle = sprintf("%s hours of shallow sleep = 1 hour of regular sleep\ndashed lines are %s day averages", shallow_sleep_factor, days_to_average)) +
    scale_x_date(
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=c(expand_settings[1], expand_settings[2]-0.25)
    ) +
    scale_y_continuous(
      "Minutes",
      expand = expand_settings, 
      sec.axis=sec_axis(~ ./60, name="Hours")
    ) +
    scale_fill_manual("", values=c("time asleep" = "skyblue", "longest sleep segment"="firebrick2")) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=45),
      legend.margin = margin(t=-1.25, unit="cm"),
      legend.position = c(0.9, 1.05)
    ) + guides(fill=guide_legend(ncol=1))
  return(p)
}


falling_asleep_and_staying_awake <- function(sleep_collection){
  p <- sleep_collection[["sleep stats"]] %>% 
    filter(!variable %in% c("number of awakenings", "time asleep", "longest sleep segment")) %>%
    ggplot(aes(x=sleep_date)) +
    facet_wrap(~variable, ncol=1, scales="free_y") +
    geom_point(aes(y=value), color="navy", size=1.5, alpha=0.5) +
    geom_line(aes(y=value), color="navy") +
    geom_line(aes(y=n_day_average), color="skyblue", alpha=0.5, linetype="dashed") +
    labs(x="", y="") +
    scale_x_date(
      date_breaks=sprintf("%s days", n_day_breaks), 
      expand=c(expand_settings[1], expand_settings[2]+0.25)
    ) +
    scale_y_continuous(
      "Minutes", 
      sec.axis=sec_axis(~ ./60, name="Hours"), 
      expand=c(expand_settings[1]+0.1, expand_settings[2]-0.15)
    ) +
    custom_theme +
    theme(
      axis.text.x = element_text(angle=45)
    )
  return(p)
}


damage_plot <- function(sleep_collection){
  p <- sleep_collection[["sleep"]] %>%
    transmute(
      sleep_date = sleep_date,
      date_time = as_datetime(sleep_date),
      damage = cumsum(sleep_rating - positive_rating_threshold)
    ) %>%
    (function(df){
      date_seq <- seq(from = min(df$sleep_date), to = max(df$sleep_date), by = "days")
      expanded_df <- merge(
        seq(from = min(df$sleep_date), to = max(df$sleep_date), by = "days"),
        chron::chron(time = sprintf("%s:00:00", 0:23))
      ) %>% 
        mutate(date_time = as.POSIXct(paste(x, y))) %>% 
        arrange(date_time) %>%
        select(date_time)
      expanded_df %>% left_join(df, by="date_time")
    }) %>% filter(cumsum(!is.na(sleep_date)) > 0) %>%
    mutate(damage_ext = zoo::na.spline(damage, na.rm=TRUE)) %>%
    ggplot(aes(x=date_time)) +
    geom_hline(yintercept = 0, linetype="dotted", color="grey") + 
    #geom_bar(aes(y=damage_ext, fill=damage_ext), stat="identity") +
    geom_line(aes(y=damage_ext, color=damage_ext), size=1.5) +
    #geom_point(aes(y=damage), size=1.5, color="navy", na.rm=TRUE) +
    scale_fill_gradient2(low="firebrick2", mid="grey", high="green", midpoint=0) +
    scale_color_gradient2(low="firebrick2", mid="grey", high="green", midpoint=0) +
    scale_y_continuous(expand=expand_settings) +
    scale_x_datetime(date_breaks=sprintf("%s days", n_day_breaks), expand=expand_settings) + 
    labs(x="", y="") +
    ggtitle(
      "Damage Absorbed",
      subtitle=sprintf("cumulative sum of nightly sleep ratings minus survivable threshold (%s)", positive_rating_threshold)  
    ) +
    custom_theme +
    theme(axis.text.x = element_text(angle=45)) +
    guides(fill=FALSE, color=FALSE)
  return(p)
}


sleep_hours <- function(sleep_collection){
  p <- sleep_collection[["sleep"]] %>% 
    select(sleep_date, bedtime, up) %>%
    mutate_at(vars(bedtime, up), roll_midnight) %>%
    ggplot(aes(x=sleep_date)) +
    geom_segment(
      aes(x=sleep_date, xend=sleep_date, y=bedtime, yend=up),
      size=1.5,
      color="royalblue",
      alpha=0.5
    ) +
    geom_point(aes(y=bedtime), color="navy", size=2) +
    geom_point(aes(y=up), color="navy", size=2) +
    scale_x_date(
      expand=c(expand_settings[1], expand_settings[2]+0.25)
    ) +
    scale_y_continuous(
      labels=time_scaler, 
      breaks=seq(21*60^2, length.out=7, by=2*60^2),
      expand=c(expand_settings[1]+0.01, expand_settings[2])
    ) +
    labs(x="", y="Time") +
    ggtitle("Times I go to bed and get up") +
    custom_theme
  return(p)
}



# DATA ------------------------------------------------


display_data <- function(sleep_collection){
  
  display_cols <- c(
    "sleep date" = "sleep_night",
    "sleep rating" = "sleep_rating",
    "outdoor temp. (c)" = "temperature",
    "bedtime palpatations" = "bedtime_palpatations",
    "midnight palpatations" = "midnight_palpatations",
    "panic" = "panic",
    "nose state" = "nose_state",
    "alcohol intake (STD)" = "alcohol_std",
    "notes" = "notes"
  )
  
  sleep_collection[["sleep"]] %>% 
    select(one_of(display_cols)) %>% 
    DT::datatable(
      colnames=display_cols,
      filter="top",
      caption="Manually collected sleep data. Noted that most numbers are approximations"
    )
  
}

