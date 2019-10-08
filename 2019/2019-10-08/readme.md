# Powerlifting data

Wanted to do a riff on the age-related lifting data, but also compare Male vs Female and equipped vs raw.

![](age_plots.png)

```{r}
library(tomtom)
library(tidyverse)
library(ggtext)
library(ggridges)

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

max_data <- ipf_lifts %>%
  filter(equipment %in% c("Raw", "Single-ply")) %>%
  filter(!is.na(date)) %>%
  mutate_at(.vars = vars(age, bodyweight_kg:best3deadlift_kg), as.double) %>%
  mutate(
    age_round = round(age, 0),
    year = lubridate::year(date)
  ) %>%
  filter(year >= 2010) %>%
  mutate(year = factor(year)) %>%
  gather(key = "lift", value = "weight", best3squat_kg:best3deadlift_kg) %>%
  mutate(lift = case_when(
    str_detect(lift, "squat") ~ "Squat",
    str_detect(lift, "bench") ~ "Bench",
    str_detect(lift, "dead") ~ "Deadlift"
  ))

max_plot <- max_data %>%
  mutate(sex = factor(sex,
    levels = c("F", "M"),
    labels = c("Female", "Male")
  )) %>%
  group_by(sex, year, equipment, lift) %>%
  summarize(
    max = max(weight, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = year, y = max, group = lift, color = lift)) +
  geom_path(size = 2) +
  geom_point(size = 5) +
  facet_grid(sex ~ equipment, scales = "free") +
  theme_tomtom() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = tomtom::tomtom_pal("main", 3)) +
  scale_x_discrete(breaks = seq(2012, 2018, 2)) +
  labs(
    x = "",
    y = "Max Lift (kg)\n",
    caption = "\nGraph: @thomas_mock | Data: OpenPowerlifting.org",
    title = "<span style='color:#003399'>**Bench**</span>, <span style='color:#FF2B4F'>**Deadlift**</span>, and <span style='color:#FCAB27'>**Squat**</span> performances have increased over time",
    subtitle = "Single-ply wraps only give a mechanical advantage for bench and squat"
  ) +
  theme(
    plot.title = element_markdown(),
    legend.position = "none",
    axis.title.y = element_text(size = 20, face = "bold")
  )

ggsave("max_plots.png", max_plot,
  height = 20, width = 12, units = "in", dpi = 700
)

#####

equipment_data <- ipf_lifts %>%
  mutate(sex = factor(sex,
    levels = c("F", "M"),
    labels = c("Female", "Male")
  )) %>%
  filter(!is.na(date)) %>%
  mutate_at(.vars = vars(age, bodyweight_kg:best3deadlift_kg), as.double) %>%
  mutate(
    age_round = round(age, 0),
    year = lubridate::year(date)
  ) %>%
  filter(year >= 2010) %>%
  mutate(year = factor(year)) %>%
  gather(key = "lift", value = "weight", best3squat_kg:best3deadlift_kg) %>%
  mutate(lift = case_when(
    str_detect(lift, "squat") ~ "Squat",
    str_detect(lift, "bench") ~ "Bench",
    str_detect(lift, "dead") ~ "Deadlift"
  ))


plot_lifts <- equipment_data %>%
  ggplot(aes(x = weight, y = equipment, group = equipment, fill = lift)) +
  geom_density_ridges(color = "white") +
  facet_grid(sex ~ lift) +
  theme_tomtom() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = tomtom::tomtom_pal("main", 3)) +
  scale_x_discrete(breaks = seq(2012, 2018, 2)) +
  labs(
    x = "Max Lift (kg)\n",
    y = "",
    caption = "\nGraph: @thomas_mock | Data: OpenPowerlifting.org",
    title = "Wraps give a larger mechanical advantage in bench and squat"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 20, face = "bold")
  )

plot_lifts

ggsave("plot_lifts.png", plot_lifts,
       height = 12, width = 20, units = "in", dpi = 700
)

#####

age_data <- ipf_lifts %>%
  filter(equipment %in% c("Raw", "Single-ply")) %>%
  filter(!is.na(date)) %>%
  mutate_at(.vars = vars(age, bodyweight_kg:best3deadlift_kg), as.double) %>%
  mutate(
    age_round = round(age, 0),
    year = lubridate::year(date)
  ) %>%
  filter(year >= 2010) %>%
  mutate(year = factor(year)) %>%
  gather(key = "lift", value = "weight", best3squat_kg:best3deadlift_kg) %>%
  mutate(lift = case_when(
    str_detect(lift, "squat") ~ "Squat",
    str_detect(lift, "bench") ~ "Bench",
    str_detect(lift, "dead") ~ "Deadlift"
  ))

age_plot_data <- max_data %>%
  mutate(sex = factor(sex,
                      levels = c("F", "M"),
                      labels = c("Female", "Male")
  )) %>%
  filter(weight >= 0) %>% 
  group_by(sex, age, equipment, lift) %>%
  summarize(
    max = max(weight, na.rm = TRUE),
    n = n()
  ) 

max_age <- age_plot_data %>% 
  group_by(sex, lift, equipment) %>% 
  top_n(1, max)

age_plot <- age_plot_data %>%
  ggplot(aes(x = age, y = max, group = lift, color = lift, fill = lift)) +
  geom_point(size = 5, alpha = 0.85) +
  geom_smooth(size = 2) +
  geom_segment(data = max_age, aes(x = age, y = max, xend = age, yend = 0),
               size = 2, alpha = 0.7) +
  geom_point(data = max_age, size = 5, color = "black", shape = 21, stroke = 1) +

  facet_grid(sex ~ equipment, scales = "free") +
  theme_tomtom() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_color_manual(values = tomtom::tomtom_pal("main", 3),
                     aesthetics = c("fill", "color")) +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  labs(
    x = "",
    y = "Max Lift (kg)\n",
    caption = "\nGraph: @thomas_mock | Data: OpenPowerlifting.org",
    title = "<span style='color:#003399'>**Bench**</span>, <span style='color:#FF2B4F'>**Deadlift**</span>, and <span style='color:#FCAB27'>**Squat**</span> performances peak around 30",
    subtitle = "Max performance highlighted in black"
  ) +
  theme(
    plot.title = element_markdown(),
    legend.position = "none",
    axis.title.y = element_text(size = 20, face = "bold")
  )

age_plot

ggsave("age_plots.png", age_plot,
       height = 12, width = 20, units = "in", dpi = 700
)

```
