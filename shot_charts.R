#shot charts.
#Very much a work in progress.
#https://www.owenlhjphillips.com/new-blog/2020/6/25/how-to-make-nba-shots-charts-in-r
#https://github.com/toddwschneider/ballr/blob/master/hex_chart.R
library(tidyverse)
library(sportsdataverse)
library(gamezoneR)
library(vroom)

library(hexbin)
library(scales)
library(hoopR)

future::plan("multisession")
progressr::with_progress({
  pbp <- gamezoneR::load_gamezone_pbp(gamezoneR:::available_seasons())
})
df_shots_only <- pbp %>%
  filter(!is.na(shot_desc))

#Add in shot made and shot value and calculate shot distance
#X range -.1-50 width
#Y range0-46.95 height

df_shots_only <- df_shots_only %>%
  mutate(shot_made_numeric = if_else(shot_outcome == "made", 1, 0),
         shot_value = if_else(three_pt == TRUE, 3, 2),
         shot_dist = ((loc_x - 25)^2 + (loc_y - 5.25)^2)^.5)

#Add in groups.
#Group 1. baseline three left
#Group 2. baseline three right
#Group 3. above the break 3
#Group 4. deep three (>29 ft)
#Group 5. midrange baseline left
#Group 6. midrange baseline right
#Group 7. midrange 
#Group 8. in the key
#Group 9. Layup
#Group 10. Dunk
df_shots_only <- df_shots_only %>%
  mutate(shot_zone = case_when(
    loc_x == 25 & loc_y == 5.25 ~ "layup",
    loc_x == 26 & loc_y == 6.25 ~ "dunk",
    shot_dist > 29  | (shot_value == 3 & shot_dist < 19) ~ "deep_three",
    shot_value == 3 & loc_y < 8 & loc_x < 25 ~ "left_three",
    shot_value == 3 & loc_y < 8 & loc_x > 25 ~ "right_three",
    shot_value == 3 ~ "three",
    loc_x >= (25-6) & loc_x <= (25+6) & loc_y < 19 ~ "paint",
    loc_y > 19 ~ "midrange",
    loc_x < (25-6) ~ "left_mid",
    loc_x > (25+6) ~ "left_mid"
  ))


df_tmp <- df_shots_only %>%
  filter(shot_value == 3 &
           shot_dist<19) %>%
  summarize(mean(shot_made))
df_baseline <- df_shots_only1 %>%
  #filter(is.na(shot_zone)) %>%
  filter(shot_zone == "layup") %>%
  #filter(shot_zone == "left side three") %>%
  arrange(shot_dist)

gamezoneR::base_court +
  geom_jitter(data = df_shots_only1,
              aes(loc_x, loc_y, color = shot_zone),
              alpha = 0.8, width=.5, height = .5) +
theme(axis.line = element_blank(),
      axis.text= element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 30/.pt, margin = margin(0, 0, 5, 0)),
      plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 24/.pt),
      plot.caption = element_text(face = "italic", hjust = 1, size = 20/.pt, margin = margin(0, 0, 0, 0)),
      legend.spacing.x = grid::unit(0, 'cm'),
      legend.title = element_text(size = 20/.pt, face = "bold"),
      legend.text = element_text(size = 16/.pt),
      legend.margin = margin(0, 0, 0, 0),
      legend.position = 'bottom',
      legend.box.margin = margin(-35, 0, 0, 0),
      plot.margin = margin(5, 0, 5, 0)) +
  labs(title = "All of Zion Williamson's shots at Duke",
       subtitle = "2018-19 college basketball season",
       color = "Outcome",
       caption = "Chart: @jacklich10 | Data: @gamezoneR")




#Hex shot chart shooting percentage.
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}


calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, shot_zone) %>%
    summarize(attempts = n(), .groups = "drop") %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

shots <- df_shots_only
calculate_hexbins_from_shots = function(shots, binwidths = c(1, 1), 
                                        min_radius_factor = 0.6, 
                                        fg_pct_limits = c(0.2, 0.7)) 
                                         {
  
  if (nrow(shots) == 0) {
    return(list())
  }
  
  #grouped_shots = group_by(shots, shot_zone_range, shot_zone_area)  
  grouped_shots = group_by(shots, shot_zone)  

  zone_stats <- shots %>%
    group_by(shot_zone) %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  league_zone_stats = league_averages %>%
    group_by(shot_zone) %>%
    summarize(league_pct = sum(shot_made_numeric) / n(), .groups = "drop")
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone")
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys) %>%
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}
percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

points_formatter = function(x) {
  scales::comma(x, accuracy = 0.01)
}

generate_hex_chart = function(hex_data, base_court, court_theme = court_themes$dark, metric = sym(bounded_fg_diff), alpha_range = c(0.85, 0.98)) {
  if (length(hex_data) == 0) {
    return(base_court)
  }
  
  if (metric == "bounded_fg_diff") {
    fill_limit = hex_data$fg_diff_limits
    fill_label = "FG% vs. League Avg"
    label_formatter = percent_formatter
  } else if (metric == "bounded_fg_pct") {
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    label_formatter = percent_formatter
  } else if (metric == "bounded_points_per_shot") {
    fill_limit = hex_data$pps_limits
    fill_label = "Points Per Shot"
    label_formatter = points_formatter
  } else {
    stop("invalid metric")
  }
  
  base_court +
    geom_polygon(
      data = hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = !!metric#,
        #alpha = hex_attempts
      ),
      size = court_theme$hex_border_size,
      color = court_theme$hex_border_color
    ) +
    #scale_fill_viridis_c(
    #  paste0(fill_label, "   "),
    #  #limit = fill_limit,
    #  labels = label_formatter,
    #  guide = guide_colorbar(barwidth = 15)
    #) +
    #scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    theme(legend.text = element_text(size = rel(0.6)))
  }


#Hex shot chart pps





#Hex shot cahrt vs league average.
