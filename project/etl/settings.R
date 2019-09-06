# ================================
# Settings
# ================================



# DATA ACCESS -----------------------------------------


target_spreadsheet <- "mydb"
target_worksheet <- "Sleep Diary (new)"


# PARAMETERS ------------------------------------------


days_to_average <- 7  # number of days to average over (used in a number of plots)
positive_rating_threshold <- 5  # this number or higher defines an okay night
shallow_sleep_factor <- 2  # N hours of shallow sleep count for 1 our of regular sleep

# PLOT CONFIGURATIONS ---------------------------------


n_day_breaks <- 7  # distance between dates on each plot


# THEME SETTINGS --------------------------------------


good_bad_colours <- list(
  "good"="lightblue",
  "average"="orange2",
  "bad"="firebrick2"
)


expand_settings <- c(0,0.25)

main_plot_segment_size <- 3


custom_theme <- 
  ggplot2::theme_light() + 
  #ggtech::theme_tech(theme="airbnb") +
  theme(
    axis.text = element_text(size=8),
    axis.text.x = element_text(angle=45),
    legend.key = element_rect(fill = "transparent", color="transparent"),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.text = element_text(size=7),
    plot.margin = unit(c(0,0.5,-0.1,0.75), "cm"),
    plot.title = element_text(size=12),
    plot.subtitle = element_text(size=10)
)
