# ================================
# Data Extraction
# ================================
# Collecting sleep data



# SETUP -----------------------------------------------


# load libraries
library(tidyverse)
library(magrittr)
library(googlesheets)
library(lubridate)


fill_correction <- function(df){
  do.call(
    "rbind",
    sapply(1:(nrow(df)-1), function(i){
      f <- lm(x~y, df[i:(i+1),])
      if (f$qr$rank < 2) return(NULL)
      r <- predict(f, newdata=data.frame(y=0))
      if(df[i,]$x < r & r < df[i+1,]$x)
        return(data.frame(x=r,y=0))
      else return(NULL)
  }))
}

intermediate_damage_df <- sleep_data %>% 
  select(sleep_date, sleep_rating) %>%
  arrange(sleep_date) %>%
  mutate(
    sleep_time = as_datetime(sleep_date),
    x = row_number(),
    y = cumsum(sleep_rating - positive_rating_threshold),
  )

interpolated_df <- fill_correction(intermediate_damage_df)
if(nrow(interpolated_df) > 0){
  intermediate_damage_df %<>% 
    select(sleep_time, damage=y) %>%
    rbind(
    interpolated_df %>%
      mutate(index = floor(x), add_hours = as.integer(1 + 24 * (x - floor(x)))) %>%
      left_join(
        intermediate_damage_df %>% 
          select(index=x, sleep_time), 
        by="index"
      ) %>% mutate(sleep_time = sleep_time + hours(add_hours)) %>%
      select(sleep_time, damage=y)
  )
}


intermediate_damage_df %>%
  ggplot(aes(x=sleep_time, y=damage)) +
  geom_area(data= . %>% filter(damage <= 0), fill="firebrick2") + 
  geom_area(data= . %>% filter(damage >= 0), fill="lightgreen") +
  geom_line(size=1.5, color="black") +
  labs(x="", y="Damage Points") +
  ggtitle(
    "Accumulated Damage", 
    subtitle=sprintf("cumulative sum of nightly sleep ratings vs survivable threshold (%s)", positive_rating_threshold)
  ) +
  scale_y_continuous(expand = expand_settings) +
  scale_x_datetime(date_breaks=sprintf("%s days", n_day_breaks), expand=expand_settings) +
  custom_theme + theme(
    axis.text.x = element_text(angle=45)
  )
  
  
  geom_bar(stat="identity", alpha=0.5, width = 2) +
  geom_line(color="navy", alpha=0.5)
  #
  #geom_ribbon(aes(ymin=0, ymax=positive_damage), fill="blue") +
  #geom_ribbon(aes(ymin=negative_damage, ymax=0), fill="red")



wakings_df %>%
  filter(sleep_date >= "2019-08-15", sleep_date <= "2019-08-16") %>%
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
  filter(event_type != "asleep") %>%
  mutate(
    first_sleep_chunk = min(from) - asleep_from,
    last_sleep_chunk = asleep_to - max(to),
    midnight_chunks = from - lag(to),
    longest_segment = pmax(first_sleep_chunk, last_sleep_chunk, midnight_chunks, na.rm=TRUE)
  ) %>% View()
  summarise(longest_sleep_segment = max(longest_segment, na.rm=TRUE)) %>%
  ungroup



# - sleep hours (done)
# - nightly max hours of uninterrupted sleep
# - time to fall asleep (done)
# - number of times woken up?
# - time awake in bed (done)

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
      
  
  count_of_awakenings <- sleep_collection[["wakings"]] %>%
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

sleep_stats <- sleep_collection[["wakings"]] %>% get_sleep_statistics

p_hours_of_sleep <- sleep_stats %>% 
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
    data=sleep_stats %>% filter(variable == "time asleep")
  ) + 
  geom_line(
    aes(y=n_day_average), 
    color="firebrick2", 
    linetype="dashed",
    size=1,
    data=sleep_stats %>% 
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

p_extra_stats <- sleep_stats %>% 
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


ggarrange(
  p_hours_of_sleep + rremove("x.text"), 
  p_extra_stats,
  ncol=1, 
  heights=c(5,7), 
  label.x = sleep_collection[["sleep"]]$sleep_date, 
  align="v"
)


sleep_collection[["wakings"]] %>%
  filter(event_type %in% c("awake", "shallow sleep")) %>%
  group_by(sleep_date) %>%
  summarise(num_awakenings = n()) %>%
  ungroup

sleep_collection[["wakings"]] %>%
  group_by(sleep_date) %>%
  mutate(
    asleep_from = max(ifelse(event_type == "asleep", from, 0)),
    asleep_to = max(ifelse(event_type == "asleep", to, 0))
    ) %>%
  filter(
    from >= asleep_from, 
    to <= asleep_to,
    event_type != "asleep"
  ) %>%
  select(-event_type) %>%
  mutate(
    first_sleep_chunk = min(from) - asleep_from,
    last_sleep_chunk = asleep_to - max(to),
    midnight_chunks = from - lag(to),
    longest_segment = pmax(first_sleep_chunk, last_sleep_chunk, midnight_chunks, na.rm=TRUE)
  ) %>%
  summarise(longest_segment_min = max(longest_segment, na.rm=TRUE)/60) %>%
  ungroup

# logic:
# part 1: from - asleep_from gives us the time until the first awakening (interesting in itself!)
# part 2: compare lag of "to" until "from" to get subsequent sleep segments
# part 3: asleep_to - max(to) gives us the time asleep after falling asleep from the last awakening until the end of the night




data.frame(
  from = c(1)
)

sleep_data %>% 











# process data

sleep_data <- sleep_collection[["sleep"]]
weekend_boundaries <- sleep_collection[["weekend boundaries"]]
events_data <- sleep_collection[["events"]]
lowest_rating <- sleep_collection[["rating boundaries"]][["min"]]
highest_rating <- sleep_collection[["rating boundaries"]][["max"]]
wakings_df = sleep_collection[["wakings"]]
annotations = sleep_collection[["hourly annotations"]]

p <- sleep_data %>%
  select(sleep_date, sleep_rating, weekly_sleep_rating_average) %>%
  gather(type, rating, -sleep_date) %>%
  mutate(
    type = ifelse(type == "sleep_rating", "daily", "weekly average"),
    weekend = ifelse(weekdays(sleep_date) %in% c("Saturday", "Sunday"), TRUE, FALSE)
  ) %>%ggplot() +
  geom_rect(
    aes(xmin=from, xmax=to, ymin=lowest_rating, ymax=highest_rating), 
    fill="skyblue", alpha=0.25, data=weekend_boundaries
  ) +
  geom_hline(yintercept = positive_rating_threshold, linetype="solid", size=2, color="orange") +
  geom_point(aes(x=sleep_date, y=sleep_rating), color="navy", size=1.5, data=sleep_data) +
  geom_point(aes(x=sleep_date, y=predicted_rating), color="firebrick2", size=1.5, shape=23, data=sleep_data %>% mutate(rn = row_number())) +
  geom_line(aes(x=sleep_date, y=rating, color=type, linetype=type), alpha=0.75, size=0.75) +
  geom_label(
    aes(x=sleep_date, y=sleep_rating-0.25, label=special), 
    data=events_data, fontface="bold", size=3, color="black"
  ) + 
  scale_y_continuous(breaks=1:10, expand=c(0,0.25)) +
  scale_x_date(date_breaks="7 days", expand=c(0,0.25)) +
  scale_color_manual("", values = c("daily" = "navy", "weekly average" = "royalblue")) +
  scale_linetype_manual("", values = c("daily"="solid", "weekly average"="dashed")) +
  labs(x="", y="rating") +
  ggtitle(
    "How I Subjectively Rated Each Night", 
    subtitle = "Shaded areas = weekends; red hollow dots = my guessed ratings"
  ) +
  custom_theme +
  theme(
    legend.position = c(0.1, 0.8),
    legend.margin = margin(t=-1.25, unit="cm")
  ) + guides(color=guide_legend(nrow=2))

time_plot_aes <- aes(
  x=sleep_date, 
  xend=sleep_date,
  y=from,
  yend=to,
  color=event_type
)

p2 <- ggplot() +
  geom_segment(size=3, alpha=0.5, data=wakings_df %>% filter(event_type=="asleep"), time_plot_aes) +
  geom_segment(size=3, alpha=0.5, data=wakings_df %>% filter(event_type!="asleep"), time_plot_aes) +
  geom_point(
    aes(x=sleep_date, y=time, shape=annotation), 
    data=annotations, 
    size=2,
  ) +
  labs(x="", y="") +
  ggtitle("Every Excruciating Hour...", subtitle="All times are approximate") +
  scale_x_date(date_breaks="7 days", expand=c(0,0.25)) +
  scale_y_continuous(labels=time_scaler, expand=c(0, 0.25), breaks=seq(23*60^2, length.out=6, by=2*60^2)) + 
  scale_shape_manual("", values=c("pills"=18, "toilet"=17)) +
  scale_colour_manual("", values=c(
    "asleep"="skyblue", 
    "falling asleep"="grey", 
    "awake"="firebrick2", 
    "shallow sleep"="orange2"
  )) +
  custom_theme +
  theme(
    legend.position=c(0.2, 0.85),
    legend.margin = margin(t=-1, unit="cm")
  ) + guides(color=guide_legend(nrow=1), shape=guide_legend(nrow=1))
p2

x <- qc(
  "bedtime palp." = "bedtime palp. (yes, no)",
  "midnight palp." = "midnight palp. (yes, no)",
  "panic" = "panic (yes, nearly, no)",
  "otrivin" = "otrivin (yes, no)",
  "nose state" = "nose state (clear, part. blocked, blocked)"
)

p3 <- nightly_indicators %>% 
  ggplot(aes(x=sleep_date, y=variable, color=factor(value))) +
  geom_point(shape=15, size=3) +
  scale_x_date(date_breaks="7 days", expand=c(0, 0.25)) + 
  labs(x="", y="") +
  scale_color_manual("", values=c(
    "0"=custom_colours[["good"]], 
    "1"=custom_colours[["average"]], 
    "2"=custom_colours[["bad"]]
  )) +
  ggtitle(
    "Nightly Observations",
    subtitle=sprintf(
      "%s = positive, %s = neutral (if applicable), %s = negative",
      gsub("\\d","",custom_colours[["good"]]),
      gsub("\\d","",custom_colours[["average"]]),
      gsub("\\d","",custom_colours[["bad"]])
    )
  ) +
  custom_theme +
  theme(axis.text = element_text(angle=45)) +
  guides(FALSE)


ggarrange(
  p + rremove("x.text"), 
  p2 + rremove("x.text"),
  p3 + rremove("legend"),
  ncol=1, 
  heights=c(5,4,3), 
  label.x = sleep_collection[["sleep"]]$sleep_date, 
  align="v"
)




x[nightly_indicators$variable]

#library(cowplot)
library(patchwork)
ggiraph::girafe(
  code = print(p + p2 + plot_layout(ncol = 1)),
  width_svg=12,
  height_svg=5
)







sleep_df <- retrieve_sleep_data()

sleep_df %>% 
  select(sleep_date, pills) %>%
  na.omit() %>%
  mutate(top_cols = max(stringr::str_count(pills, ","))+1) %>%
  separate(pills, sep=",", into=paste("w", 1:.$top_cols[1], sep="_")) %>%
  select(-top_cols) %>% 
  gather(variable, value, -sleep_date) %>%
  arrange(sleep_date, variable) %>%
  na.omit()





wakings_df <- sleep_df %>% 
  reformat_csv(var="waking") %>%
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
    sleep_df %>% 
      select(sleep_date, bedtime, asleep) %>%
      na.omit() %>%
      mutate(
        from=as.character(bedtime),
        to=as.character(asleep),
        event_type="falling asleep"
      ) %>%
    select(sleep_date, from, to, event_type)
  ) %>% 
  # ADD START TO END OF NIGHT TIMES
  bind_rows(
    sleep_df %>%
      select(sleep_date, asleep, up) %>%
      na.omit() %>%
      mutate(
        from=as.character(asleep),
        to=as.character(up),
        event_type="asleep"
      ) %>%
      select(sleep_date, from, to, event_type)
  ) %>% 
  mutate_at(vars(from, to), function(z) roll_midnight(z))

toilet_visits <- sleep_df %>% 
  select(sleep_date, toilet) %>%
  na.omit() %>%
  mutate(top_cols = max(stringr::str_count(toilet, ","))+1) %>%
  separate(toilet, sep=",", into=paste("w", 1:.$top_cols[1], sep="_")) %>%
  select(-top_cols) %>%
  gather(variable, value, -sleep_date) %>%
  arrange(sleep_date, variable) %>%
  na.omit() %>%
  mutate(
    time = paste(gsub("\\s+", "", value), "00", sep=":"),
    annotation = "toilet",
  ) %>% select(-variable, -value)


pills <- sleep_df %>% 
  select(sleep_date, pills) %>%
  na.omit() %>%
  mutate(top_cols = max(stringr::str_count(pills, ","))+1) %>%
  separate(pills, sep=",", into=paste("w", 1:.$top_cols[1], sep="_")) %>%
  select(-top_cols) %>% 
  gather(variable, value, -sleep_date) %>%
  arrange(sleep_date, variable) %>%
  na.omit() %>%
  mutate(
    time = paste(gsub(".* (\\d+:\\d+)$", "\\1", value), "00", sep=":"),
    annotation = "pills",
    message = gsub("(.*)( \\d+:\\d+)$", "\\1", value)
  ) %>% select(-variable, -value)

annotations <- bind_rows(toilet_visits, pills) %>%
  mutate(time = roll_midnight(time))

time_plot_aes <- aes(
  x=sleep_date, 
  xend=sleep_date,
  y=from,
  yend=to,
  color=event_type
)

ggplot() +
  geom_segment(size=1.5, alpha=0.5, data=wakings_df %>% filter(event_type=="asleep"), time_plot_aes) +
  geom_segment(size=1.5, alpha=0.5, data=wakings_df %>% filter(event_type!="asleep"), time_plot_aes) +
  geom_point(aes(x=sleep_date, y=time, shape=annotation), data=annotations, size=1.5) +
  #annotate("text", x=as.Date("2019-07-26"), y=90120, label="T", fontface="bold", color="grey50") +
  labs(x="", y="") +
  ggtitle("Sleep State by Date and Time") +
  scale_y_continuous(labels=time_scaler, breaks=seq(23*60^2, length.out=6, by=2*60^2)) + 
  scale_shape_manual("", values=c("pills"=12, "toilet"=6)) +
  scale_colour_manual("", values=c(
    "asleep"="skyblue", 
    "falling asleep"="grey", 
    "awake"="firebrick2", 
    "shallow sleep"="orange2"
  )) +
  theme_light() +
  theme(legend.position="bottom")


x <- wakings_df$from[1] / 60^2



stringr::str_pad(
  round(x) %>% 
    ifelse(. >= 24, .-24, .), 
  width=2, 
  pad = "0", 
  side = "left"
) %>% paste(60 * (x - round(x)), sep=":")

60 * (x - round(x))

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
  
time_scaler(wakings_df$from)


wakings_df %>% 
  ggplot(aes(
    x=sleep_date, 
    xend=sleep_date, 
    y=period_to_seconds(awake_from), 
    yend=period_to_seconds(awake_to),
    color=shallow_sleep
    )
  ) +
  geom_segment(size=1)

# want to annotate bathroom visits, pills taken, start of the night, end of the night, fill asleep times

