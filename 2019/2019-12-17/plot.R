
# Library Load ------------------------------------------------------------


library(waffle)
library(ggwaffle)
library(tidyverse)
library(geofacet)
library(statebins)
library(ggtext)
library(extrafont)

# Read in Data ------------------------------------------------------------

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

# Recreate Dog Moves Plot -------------------------------------------------

state_df <- tibble(
  state_abb = state.abb,
  state_name = state.name
)


dog_moves_long <- dog_moves %>%
  pivot_longer(cols = c(exported, imported), names_to = "travel_type") %>% 
  rename(state_name = location) %>% 
  left_join(state_df, by = "state_name") %>% 
  mutate(travel_type = str_to_title(travel_type),
         travel_type = factor(travel_type, levels = c("Imported", "Exported"))) %>% 
  mutate(value = replace_na(value, 0)) %>% 
  filter(state_name %in% state.name) %>% 
  select(state_name, state_abb, travel_type, total, value) %>% 
  mutate(value = if_else(travel_type == "Imported", value, -1 * value))

dog_states <- dog_moves_long %>% 
  ggplot(aes(x = "Dogs", y = value, fill = travel_type)) +
  geom_col(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black", size = 0.2) +
  scale_fill_manual(values = c("#1E88E5", "#FA8072")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Merriweather"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.placement = "inside",
        plot.background = element_rect(fill = "#F8F8F8")
        ) +
  labs(x = "", y= "",
       title = "**<span style='color:#1E88E5'>Imports</span> and <span style='color:#FA8072'>Exports</span> of Dogs by State**",
       subtitle = "Based on PetFinder data for adoptable dogs on September 20, 2019",
       caption = "\n\n#TidyTuesday | Plot: @thomas_mock") +
  facet_geo(~ state_abb, strip.position = "bottom", grid = "us_state_grid2")

# Save Plot ---------------------------------------------------------------

ggsave("dog_states.png", dog_states, height = 8, width = 12, units = "in", dpi = "retina")
