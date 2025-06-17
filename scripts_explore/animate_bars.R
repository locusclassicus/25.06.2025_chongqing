load("./data/plato_pub_ed.Rdata")
library(tidyverse)
library(gganimate)
library(scales)

# Filter data after 1970
plato_pub_ed2 <- plato_pub_ed |> 
  filter(year > 1999 & year < 2025) |> 
  mutate(cited_by_count = as.numeric(cited_by_count))

# Aggregate citations by country and year
cited_stat_year <- plato_pub_ed2 |>
  filter(!is.na(affiliation_country)) |>
  mutate(
    affiliation_country = case_when(
      affiliation_country == "Czech Republic" ~ "Czechia",
      affiliation_country == "Russian Federation" ~ "Russia",
      affiliation_country == "United States" ~ "USA",
      affiliation_country == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
      affiliation_country == "United Kingdom" ~ "UK",
      .default = affiliation_country
    )
  ) |>
  group_by(affiliation_country, year) |> 
  summarise(total_cited = sum(cited_by_count, na.rm = TRUE), .groups = 'drop') 

# Get top 10 countries per year with proper ranking
top_cited <- cited_stat_year |> 
  group_by(year) |>
  slice_max(total_cited, n = 10, with_ties = FALSE) |>
  mutate(rank = row_number(-total_cited)) |> 
  ungroup() 

# Create animation - FIXED VERSION
p <- ggplot(top_cited, aes(x = rank, 
                           y = total_cited,
                           group = affiliation_country)) +

  geom_col(aes(fill = affiliation_country), 
           width = 0.8,
           show.legend = FALSE,
           alpha = 0.8) +
  
  # Country names
  geom_text(aes(y = total_cited + max(total_cited) * 0.01, 
                label = paste(affiliation_country, scales::comma(total_cited))), 
            hjust = "left",
            color = "grey40", 
            size = 4,
            fontface = "bold") +
  
  coord_flip(clip = "off") +
  labs(title = "Total Citations of Plato Research",
       subtitle = "Year: {closest_state} | Data: Scopus",
       caption = "@RAntiquity",
       x = NULL,
       y = "Total Citations") +
  
  scale_x_reverse(breaks = 1:10) +
  scale_y_continuous(labels = scales::comma_format(),
                     trans = "log10",
                     expand = expansion(mult = c(0, 0.1))) +
  
  theme_minimal(base_size = 15) +
  
  scale_fill_viridis_d("") +
  
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(size = 20, face = "bold", hjust = 0.5, color = "grey20"),
    plot.subtitle    = element_text(size = 14, hjust = 0.5, color = "grey20")
  ) +
  
  transition_states(year,
                    transition_length = 2,
                    state_length = 1,
                    wrap = FALSE) +
  ease_aes('cubic-in-out')

# Render animation
animate(p,
        fps = 24,
        width = 900,
        height = 600,
        duration = 20,
        renderer = av_renderer("plato_citations.mp4"))
