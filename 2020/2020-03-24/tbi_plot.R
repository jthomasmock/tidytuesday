library(tidyverse)
library(ggtext)

# Read in the data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# Get the factor levels for ordering
tbi_levels <- tbi_age %>% distinct(age_group) %>% pull() %>% .[2:10]

tbi_mech <- tbi_age %>% distinct(injury_mechanism) %>% pull()

# past the levels for manual assignment of ordering
datapasta::vector_paste_vertical(tbi_mech)

tbi_mech_levels <- c(
  "Assault", 
  "Unintentional Falls", 
  "Motor Vehicle Crashes", 
  "Unintentionally struck by or against an object",
  "Intentional self-harm", 
  "Other unintentional injury, mechanism unspecified"
  )

# assign names aka colors to the vector above to align categories
names(tbi_mech_levels) <- c("#003399", "#fcab27", "#003399", "#fcab27","#003399", "#fcab27")

# Find percentage within each category of injury mechanism
age_group_tbi <- tbi_age %>% 
  filter(!age_group %in% c("0-17", "Total"),
         injury_mechanism != "Other or no mechanism specified",
         type == "Hospitalizations",
         !is.na(number_est)
         ) %>% 
  mutate(age_group = factor(age_group, levels = tbi_levels),
         injury_mechanism = factor(injury_mechanism, levels = tbi_mech_levels)) %>% 
  group_by(injury_mechanism) %>% 
  mutate(total = sum(number_est)) %>% ungroup() %>% 
  mutate(pct_total = number_est/total)

# Plot the data
plot_tbi <- age_group_tbi %>% 
  ggplot(aes(x = age_group, y = pct_total, color = injury_mechanism)) +
  geom_point() +
  geom_path(aes(group = 1)) +
  facet_wrap(~injury_mechanism, scales = "free_y", ncol = 2) +
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  # Add 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5, min.n = 5),
                     labels = scales::label_percent(accuracy = 1.0)) +
  scale_color_manual(values = names(tbi_mech_levels)) +
  theme(legend.position = "None",
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold"),
        plot.subtitle = element_markdown(),
        plot.title = element_text(face= "bold"),
        axis.text = element_text(face = "bold")) +
  labs(x = "\nAge Groups", y = "Percent of Category Hospitalizations\n", 
       title = "Distinct categories of age-related TBI Hospitalizations",
       subtitle = " <span style='color:#003399'>**Peak in Middle Age:**</span> Assault, Crashes, and Self-harm<br><span style='color:#fcab27'>**Increase with Age:**</span> Unintentionally falling or striking/against an object and other uninentional",
       caption = "\nPlot: @thomas_mock | Data: CDC") +
  # Note the free y-axis, unintentional falls skews the data vertically due to 
  # High percentage in oldest age
  facet_wrap(~injury_mechanism, ncol = 2, scales = "free_y")

plot_tbi  

ggsave("tbi_plot.png", plot_tbi, height = 6, width = 8, units = "in", dpi = 450)
