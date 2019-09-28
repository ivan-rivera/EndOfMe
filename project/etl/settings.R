# ================================
# Settings
# ================================



# DATA ACCESS -----------------------------------------


target_spreadsheet <- "mydb"
target_worksheet <- "Sleep Diary"


# PARAMETERS ------------------------------------------


days_to_average <- 7  # number of days to average over (used in a number of plots)
min_required_hours <- 6 # min required hours of sleep in order for your body
recovery_multiplier <- 3 # every hour above min_required_hours recovers that many hours of lost sleep
positive_rating_threshold <- 5  # this number or higher defines an okay night
shallow_sleep_factor <- 2  # N hours of shallow sleep count for 1 our of regular sleep
prop_for_model_validation <- 0.2  # proportion of data to retain for model validation
desired_sleep_efficiency <- 0.9  # recommended sleep efficiency

# PLOT CONFIGURATIONS ---------------------------------


n_day_breaks <- 7  # distance between dates on each plot


# THEME SETTINGS --------------------------------------

custom_color_palette <- list(
  # this is based on: paletton.com
  # theme: dark neon triad based off #0D5176 (background colour)
  # don't forget to set these values in the css file separately:
  # *  background-color: primary 5
  # * .nav-tabs-custom > .nav-tabs > li > a: seconday2 1
  # * .navbar-inverse .navbar-nav > li > a AND .navbar-brand:  primary 1
  # use the following regex to isolate exported values: ^.*(?=#\w{6})|(?<=#\w{6}).*
  #Option 1:
  "primary" =       c("#1886C2", "#08699D", "#0D5176", "#083C58", "#021F2E"),
  "secondary1" =    c("#FFBF13", "#F8B400", "#BA8A0B", "#8B6706", "#483500"),
  "secondary2" =    c("#FF4813", "#F83800", "#BA330B", "#8B2406", "#481000"),
  "complementary" = c("#1886C2", "#08699D", "#0D5176", "#083C58", "#021F2E")
  #Option 2:
  # "primary" = c("#17E244", "#00C72C", "#088C25", "#025E16", "#022A0B"),
  # "secondary1" = c("#FFAB1A", "#FFA100", "#BA790B", "#7C4F03", "#372402"),
  # "secondary2" = c("#CF17D5", "#A901AF", "#750779", "#4E0251", "#230224"),
  # "complementary" = c("#FF301A", "#FF1800", "#BA1B0B", "#7C0E03","#370702")
)


good_bad_colours <- list(
  "good"=custom_color_palette[["primary"]][2],
  "average"=custom_color_palette[["secondary1"]][3],
  "bad"=custom_color_palette[["secondary2"]][3]
)


expand_settings <- c(0,0.25)

main_plot_segment_size <- 3

custom_theme <- 
  ggplot2::theme_light() + 
  #ggthemes::theme_fivethirtyeight() +
  #ggtech::theme_tech(theme="etsy") +
  theme(
    text = element_text(family="Source Sans Pro"),
    axis.ticks = element_line(color = custom_color_palette[["primary"]][1]),
    axis.text = element_text(size=11, color=custom_color_palette[["primary"]][1]),
    #axis.text.x = element_text(angle=45),
    axis.title = element_text(size=14, color=custom_color_palette[["primary"]][1]),
    legend.key = element_rect(fill = "transparent", color="transparent"),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.text = element_text(size=12,color=custom_color_palette[["primary"]][1]),
    plot.margin = unit(c(0.25,0.5,0,0.75), "cm"),
    plot.title = element_text(
      #family="Source Sans Pro", 
      face="bold",
      size=16, 
      color=custom_color_palette[["primary"]][1]
    ),
    plot.subtitle = element_text(
      #family="Open Sans", 
      size=13,
      color=custom_color_palette[["secondary2"]][2]
    ),
    strip.text = element_text(color=custom_color_palette[["primary"]][1], size=12),
    strip.background = element_rect(
      fill=custom_color_palette[["primary"]][4], 
      color="transparent",
      size=1.5
    ),
    panel.grid.minor = element_line(color="transparent"),
    panel.grid.major = element_line(color=custom_color_palette[["primary"]][4], size=0.1),
    panel.border = element_rect(color=custom_color_palette[["primary"]][4]),
    panel.background = element_rect(fill=custom_color_palette[["primary"]][5]),
    plot.background = element_rect(fill=custom_color_palette[["primary"]][5], color=NA)
)
