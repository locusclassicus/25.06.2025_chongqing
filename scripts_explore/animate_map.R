load("./data/plato_pub_ed.Rdata")
library(tidyverse)

# only after 1950 
plato_pub_ed2 <- plato_pub_ed |> 
  filter(year > 1949)

# country data
world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  select(name, geometry)


# -- Step 1: Prepare your list of years and country names --
all_years <- sort(unique(plato_pub_ed2$year))
all_countries <- world_sf$name

full_grid <- expand_grid(
  name = all_countries,
  year = all_years
)

# -- Step 2: Prepare publication count per country per year --
map_stat_year <- plato_pub_ed2 |>
  filter(!is.na(affiliation_country)) |>
  mutate(
    affiliation_country = case_when(
      affiliation_country == "Czech Republic" ~ "Czechia",
      affiliation_country == "Russian Federation" ~ "Russia",
      affiliation_country == "United States" ~ "United States of America",
      affiliation_country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
      .default = affiliation_country
    )
  ) |>
  count(affiliation_country, year) |>
  rename(name = affiliation_country)

# -- Step 3: Join to full grid, fill missing as zero --
map_stat_year_full <- full_grid |>
  left_join(map_stat_year, by = c("name", "year")) |>
  mutate(n = replace_na(n, 0))

# -- Step 4: Attach to world_sf, keep Antarctica out --
world_sf_year <- world_sf |>
  left_join(map_stat_year_full, by = "name") |>
  filter(name != "Antarctica")

world_sf_year_cum <- world_sf_year  |> 
  arrange(name, year)  |> 
  group_by(name)  |> 
  mutate(cum_n_papers = cumsum(n))  |> 
  #filter(year > 1899) |> 
  ungroup()

library(showtext)
font_add_google("Roboto Condensed", "Roboto")
showtext_auto()

# -- Step 5: Animated plot (as before, but will not jump/zoom!) --
library(gganimate)
p <- ggplot(world_sf_year_cum) +
  geom_sf(aes(fill = cum_n_papers), color = "grey80", lwd = 0.2) +
  coord_sf(crs = "+proj=robin") +
  scale_fill_viridis_c(option = "mako", 
                       direction = -1,
                       trans = "log1p",
                       breaks = c(1, 10, 100, 1000),
                       name     = "") +
  labs(
    title    = "Cumulative Publications on Plato",
    subtitle = "Year: {current_frame} | Data: Scopus",
    caption  = "@RAntiquity"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "bottom",
    plot.caption     = element_text(size = 10, hjust = 0.5, family = "Roboto", color = "#004d40"),
    plot.title       = element_text(size = 20, face = "bold", hjust = 0.5, family = "Roboto", color = "grey20"),
    plot.subtitle    = element_text(size = 14, hjust = 0.5, family = "Roboto", color = "grey20")
  ) +
  transition_manual(year)

# Step5: manual transition
n_years <- length(unique(world_sf_year$year))


anim <- animate(
  p,
  nframes = n_years,   # One frame per year, no fading
  fps = 4,
  width = 900, height = 600,
  renderer = av_renderer()  # To output video (e.g., MP4)
)

anim_save("plato_publications.mp4", animation = anim)


anim <- animate(
  p,
  nframes = n_years,   # Один кадр на год, без затухания
  fps = 4,
  width = 900, height = 600,
  renderer = gifski_renderer()  # GIF-выход вместо видео
)

anim_save("plato_publications_1950_2025.gif", animation = anim)
