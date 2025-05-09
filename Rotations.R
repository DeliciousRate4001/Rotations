#### Data ####
# Data file
rotations <- read.csv('https://raw.githubusercontent.com/DeliciousRate4001/Rotations/refs/heads/main/RotationsData.csv')
# Manipulation for age graphing
pt_age <- rotations %>% 
  select(location:X..80) %>% 
  pivot_longer(cols = X0.to.2:X..80,
               names_to = "age",
               values_to = "amount")
pt_age$age <- pt_age$age %>% #setting factors for graph
  factor(levels = c("X0.to.2",
                    "X3.to.5",
                    "X6.to.17",
                    "X18.to.39",
                    "X40.to.65",
                    "X66.to.80",
                    "X..80"))
# Giving age ranges a median value for stats
midpoints <- c("X0.to.2" = 1,
               "X3.to.5" = 4,
               "X6.to.17" = 11.5,
               "X18.to.39" = 28.5,
               "X40.to.65" = 52.5,
               "X66.to.80" = 73,
               "X..80" = 87.5)
pt_age <- pt_age %>%
  mutate(MidAge = midpoints[age])
# Expanding rows inputting median value for each set # for the associated age range
expanded_pt_age <- pt_age %>%
  uncount(weights = amount)
# manipulation for disease graphing
disease <- rotations %>% 
  select(location,modality,Diabetes,Glaucoma.Suspect) %>% 
  pivot_longer(cols = Diabetes:Glaucoma.Suspect,
               names_to = "disease",
               values_to = "amount")

#### Statistics ####
## Does amount of patients seen vary across Modality?
rotations %>% 
  aov(total ~ modality, data=.) %>% 
  summary() # p=0.171

## Does pt age vary across Modality?
expanded_pt_age %>% 
  aov(MidAge ~ modality, data=.) %>% 
# summary() #p<2E-16
  TukeyHSD()

#### Data Visualization ####
## Patients Seen
pt_seen_bar <- rotations %>% 
  ggplot(aes(x = location,
             y = total,
             fill = modality)) +
  geom_col(color = "black") + 
  theme_classic() +
  scale_fill_viridis_d(name = "Modality",
                       labels = c("CH"="Community Health",
                                  "PP"="Private Practice",
                                  "VA"="Vetarans Affairs")) +
  labs(title = "Patients Seen",
       x = "State",
       y = "Patient #")

## Pt spread across Modalities
{
pt_age_bar_CA <- pt_age %>% 
  filter(location == "CA") %>% 
  ggplot(aes(x = modality,
             y = amount,
             fill = age)) +
  geom_col(position = position_dodge(),
           color = "black") +
  theme_classic() +
  scale_fill_viridis_d(name = "Age",
                       labels = c("X0.to.2" = "<3",
                                  "X3.to.5" = "3-5",
                                  "X6.to.17" = "6-17",
                                  "X18.to.39" = "18-39",
                                  "X40.to.65" = "40-65",
                                  "X66.to.80" = "66-80",
                                  "X..80" = ">80"),
                       option = "A") +
  labs(x = NULL,
       y = NULL) +
  scale_y_continuous(breaks = seq(0, 200, by=50),
                     limits = c(0,200))
pt_age_bar_FL <- pt_age %>% 
  filter(location == "FL") %>% 
  ggplot(aes(x = modality,
             y = amount,
             fill = age)) +
  geom_col(position = position_dodge(),
           color = "black") +
  theme_classic() +
  scale_fill_viridis_d(name = "Age",
                       labels = c("X0.to.2" = "<3",
                                  "X3.to.5" = "3-5",
                                  "X6.to.17" = "6-17",
                                  "X18.to.39" = "18-39",
                                  "X40.to.65" = "40-65",
                                  "X66.to.80" = "66-80",
                                  "X..80" = ">80"),
                       option = "A") +
  labs(x = NULL,
       y = NULL) +
  scale_y_continuous(breaks = seq(0, 200, by=50),
                     limits = c(0,200))
pt_age_bar_MA <- pt_age %>% 
  filter(location == "MA") %>% 
  ggplot(aes(x = modality,
             y = amount,
             fill = age)) +
  geom_col(position = position_dodge(),
           color = "black") +
  theme_classic() +
  scale_fill_viridis_d(name = "Age",
                       labels = c("X0.to.2" = "<3",
                                  "X3.to.5" = "3-5",
                                  "X6.to.17" = "6-17",
                                  "X18.to.39" = "18-39",
                                  "X40.to.65" = "40-65",
                                  "X66.to.80" = "66-80",
                                  "X..80" = ">80"),
                       option = "A") +
  labs(x = NULL,
       y = NULL) +
  scale_y_continuous(breaks = seq(0, 200, by=50),
                     limits = c(0,200))
pt_age_bar_WA <- pt_age %>% 
  filter(location == "WA") %>% 
  ggplot(aes(x = modality,
             y = amount,
             fill = age)) +
  geom_col(position = position_dodge(),
           color = "black") +
  theme_classic() +
  scale_fill_viridis_d(name = "Age",
                       labels = c("X0.to.2" = "<3",
                                  "X3.to.5" = "3-5",
                                  "X6.to.17" = "6-17",
                                  "X18.to.39" = "18-39",
                                  "X40.to.65" = "40-65",
                                  "X66.to.80" = "66-80",
                                  "X..80" = ">80"),
                       option = "A") +
  labs(x = NULL,
       y = NULL) +
  scale_y_continuous(breaks = seq(0, 200, by=50),
                     limits = c(0,200))
}
pt_age_bar_MA / pt_age_bar_FL / pt_age_bar_CA / pt_age_bar_WA +
  plot_layout(guides = "collect")

## Pt Age
# Outlining columns
columns_to_summarize <- c("MidAge")
# Create the summary data frame
pt_age_summary <- expanded_pt_age %>%
  group_by(modality) %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ std.error(.x),
                        max = ~ mean(.x, na.rm = TRUE) + std.error(.x),
                        min = ~ mean(.x, na.rm = TRUE) - std.error(.x)),
                   .names = "{col}_{fn}"))
# graph
pt_age_sum_bar <- pt_age_summary %>% 
  ggplot(aes(x = modality,
             y = MidAge_mean,
             fill = modality)) +
  geom_col(color = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = MidAge_min,
                    ymax = MidAge_max),
                width = 0.3) +
  labs(x = NULL,
       y = "Average Patient Age") + 
  theme_classic() +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(0, 75, by=10),
                     limits = c(0,75))
pt_age_sum_bar +
  geom_line(data = tibble(x = c(1,3), y = c(75,75)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2), y = c(75)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(2,3), y = c(68,68)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(2.5), y = c(68)),
            aes(x = x, y = y, label = "***"),
            inherit.aes = FALSE,
            size = 7) +
  geom_line(data = tibble(x = c(1,2), y = c(55,55)),
            aes(x = x, y = y),
            inherit.aes = FALSE) +
  geom_text(data = tibble(x = c(1.5), y = c(55)),
            aes(x = x, y = y, label = "**"),
            inherit.aes = FALSE,
            size = 7)