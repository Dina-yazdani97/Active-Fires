# -----------------------------------------------------------
# 0. LIBRARIES
# -----------------------------------------------------------

libs <- c(
  "tidyverse", "sf", "data.table",
  "classInt", "gganimate", "gifski",
  "transformr", "rio", "showtext","extrafont"
)

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = TRUE))


# -----------------------------------------------------------
# 1. READ IRAN SHAPEFILE
# -----------------------------------------------------------

Iran_sf <- st_read("F:\\30DayMapChallenge\\Iran-Shpfile\\gadm41_IRN_1.shp") %>% 
  st_transform(4326)

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# -----------------------------------------------------------
# 2. MAKE HEXBIN GRID (larger hex cells)
# -----------------------------------------------------------

get_Iran_hex <- function(Iran_sf) {
  
  Iran_transformed <- Iran_sf %>% 
    st_transform(3575)
  
  Iran_hex <- st_make_grid(
    Iran_transformed,
    cellsize = 20000,  
    what = "polygons",
    square = FALSE
  ) %>%
    st_intersection(Iran_transformed) %>%
    st_sf() %>%
    mutate(id = row_number()) %>%
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    st_cast("MULTIPOLYGON")
  
  st_transform(Iran_hex, 4326)
}

Iran_hex <- get_Iran_hex(Iran_sf)



# -----------------------------------------------------------
# 3. READ FIRE DATA
# -----------------------------------------------------------

fire_data <- import("F:\\30DayMapChallenge\\Day15\\Terra.xlsx") %>% 
  rename(
    latitude = LATITUDE,
    longitude = LONGITUDE,
    acq_date = ACQ_DATE,
    confidence = CONFIDENCE
  ) %>% 
  mutate(acq_date = as.Date(acq_date))

# -----------------------------------------------------------
# 4. SPATIAL JOIN
# -----------------------------------------------------------

get_Iran_fires <- function(fire_data, Iran_hex) {
  
  fire_pts <- st_as_sf(
    fire_data,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  st_join(fire_pts, Iran_hex, join = st_within)
}

fire_Iran <- get_Iran_fires(fire_data, Iran_hex)

# -----------------------------------------------------------
# 5. AGGREGATION
# -----------------------------------------------------------

get_aggregated_Iran_fires <- function(fire_joined, Iran_hex) {
  
  fire_joined <- fire_joined %>% 
    mutate(weighted_fire = confidence / 100)
  
  fire_sum <- fire_joined %>%
    st_drop_geometry() %>%
    group_by(id, acq_date) %>%
    summarise(sum_fire = sum(weighted_fire, na.rm = TRUE), .groups = "drop")
  
  fire_sf <- left_join(Iran_hex, fire_sum, by = "id")
  
  fire_sf$sum_fire[is.na(fire_sf$sum_fire)] <- NA
  fire_sf
}

fire_Iran_sf <- get_aggregated_Iran_fires(fire_Iran, Iran_hex)

# -----------------------------------------------------------
# 6. CLASS INTERVALS
# -----------------------------------------------------------

get_intervals <- function(df) {
  
  df2 <- df %>% drop_na(sum_fire)
  
  ni <- classIntervals(df2$sum_fire, n = 5, style = "jenks")$brks
  
  labels <- sapply(
    1:(length(ni)-1),
    function(i) paste0(round(ni[i],2), " – ", round(ni[i+1],2))
  )
  
  df$cat <- cut(
    df$sum_fire,
    breaks = ni,
    labels = labels,
    include.lowest = TRUE
  )
  
  df
}

df <- get_intervals(fire_Iran_sf)



# -----------------------------------------------------------
# 7. MAP FUNCTION
# -----------------------------------------------------------

get_Iran_map <- function(df) {
  
  ggplot(na.omit(df)) +
    
    # HEXES
    geom_sf(aes(fill = cat, group = interaction(cat, acq_date)),
            color = NA, size = 1) +
    
    # BORDER
    geom_sf(data = Iran_sf, fill = NA,
            color = "#e0e0e0", size = 1, alpha = 0.5) +
    
    scale_fill_manual(
      name = "",
      values = rev(c(
        "#2b1055", "#4f2c73", "#87407b",
        "#c75e67", "#f5a15c", "#ffe96f"
      )),
      drop = FALSE
    ) +
    
    coord_sf(crs = crsLONGLAT) +
    
    theme_minimal(base_family = "Times New Roman") +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.background = element_rect(fill = "#0d0d0f"),
      plot.background  = element_rect(fill = "#0d0d0f"),
      panel.background = element_rect(fill = "#0d0d0f"),
      panel.grid = element_blank(),
      
      legend.text = element_text(color = "white", size = 14, family = "Times New Roman"),
      plot.title = element_text(color = "white", size = 25, hjust = 0.5, family = "Times New Roman", face = "bold"),
      plot.subtitle = element_text(color = "#f3b267", size = 20, hjust = 0.5, family = "Times New Roman"),
      plot.caption = element_text(color = "grey80", family = "Times New Roman", hjust = 0.5)
    ) +
    
    labs(
      title = "🔥 Active Fires in Iran",
      subtitle = "{as.Date(frame_time)}",
      caption = "MODIS-Terra"
    )
}

Iran_map <- get_Iran_map(df)

# -----------------------------------------------------------
# 8. ANIMATION
# -----------------------------------------------------------

Iran_map <- Iran_map +
  transition_time(acq_date) +
  enter_fade() +
  exit_shrink()

Iran_anim <- gganimate::animate(
  Iran_map,
  nframes = 80,
  duration = 25,
  start_pause = 3,
  end_pause = 30,
  width = 7,
  height = 6,
  units = "in",
  res = 300
)

anim_save("Terra.gif", Iran_anim)

