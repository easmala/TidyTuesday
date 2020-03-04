library(tidyverse)
library(ggthemes)
library(ggrepel)

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# Make DF for Finns only
finns <- data.frame("player" = c("Teemu Selanne", 
                      "Jari Kurri",
                      "Olli Jokinen"),
                    "nationality" = "Finnish")

# Merge DFs and calculate cumulative goals
df <- game_goals %>% 
  left_join(., 
            top_250,
            by = "player") %>% 
  left_join(., 
            finns, 
            by = "player") %>% 
  mutate(nationality = ifelse(is.na(nationality),
                              "other",
                              nationality)) %>% 
  filter(raw_rank <= 20) %>% 
  separate(age, into = c("age_years", "age_days"), sep = "-") %>% 
  mutate(age_years = as.numeric(age_years)) %>% 
  mutate(age_days = as.numeric(age_days)) %>% 
  mutate(age_days = age_days / 365) %>% 
  rename(age_frac = age_days) %>% 
  mutate(age = age_years + age_frac) %>% 
  group_by(player) %>% 
  arrange(age) %>% 
  mutate(goal_sum = cumsum(goals)) %>% 
  ungroup()

# Final goal dataframe
df_final <- df %>% 
  group_by(player) %>% 
  arrange(-age) %>% 
  filter(goal_sum == max(goal_sum)) %>% 
  distinct(player, goal_sum, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(player = case_when(
    player == "Wayne Gretzky" ~ "Wayne Gretzky\n894 goals",
    player == "Jaromir Jagr" ~ "Jaromir Jagr\n766 goals",
    player == "Brett Hull" ~ "Brett Hull\n741 goals",
    player == "Teemu Selanne" ~ "Teemu Selänne\n684 goals",
    player == "Jari Kurri" ~ "Jari Kurri\n601 goals",
    TRUE ~ as.character(player)
  ))

# Plot
ggplot(df, aes(x = age,
               y = goal_sum,
               color = nationality,
               group = player,
               label = player)) +
  geom_hline(aes(yintercept = 200), color = "#E0E4CC") +
  geom_hline(aes(yintercept = 400), color = "#E0E4CC") +
  geom_hline(aes(yintercept = 600), color = "#E0E4CC") +
  geom_hline(aes(yintercept = 800), color = "#E0E4CC") +
  geom_step(size = 0.2) +
  geom_step(data = subset(df, 
                          player %in% c("Teemu Selanne",
                                        "Jari Kurri")),
                          size = 0.4) +
  geom_point(data = df_final,
             aes(x = age,
                 y = goal_sum),
             size = 3) +
  geom_point(data = df_final,
             aes(x = age,
                 y = goal_sum),
             size = 2,
             color = "white") +
  geom_label_repel(data = subset(df_final, 
                                 player %in% c("Wayne Gretzky\n894 goals",
                                               "Jaromir Jagr\n766 goals",
                                               "Brett Hull\n741 goals",
                                               "Teemu Selänne\n684 goals",
                                               "Jari Kurri\n601 goals")),
                   size = 1.8) +
  scale_color_manual(values = c("#0E4EAD", "gray50")) +
  scale_x_continuous(limits = c(18, 46),
                     breaks = c(20, 25, 30, 35, 40, 45),
                     labels = c(20, 25, 30, 35, 40, 45)) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 920),
                     breaks = c(200, 400, 600, 800),
                     labels = c(200, 400, 600, 800)) +
  theme_few() +
  annotate("text", x = -Inf, y = 200, label = "200", vjust = -0.2, hjust = 0, color = "#E86E1C", size = 3) +
  annotate("text", x = -Inf, y = 400, label = "400", vjust = -0.2, hjust = 0, color = "#E86E1C", size = 3) +
  annotate("text", x = -Inf, y = 600, label = "600", vjust = -0.2, hjust = 0, color = "#E86E1C", size = 3) +
  annotate("text", x = -Inf, y = 800, label = "800", vjust = -0.2, hjust = 0, color = "#E86E1C", size = 3) +
  labs(title = "Two Finns in the top 20 NHL goal scorers",
       caption = "Data source: HockeyReference.com\n(only for players who started at or after 1979-80 season are included)",
       tag = "Age") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(color = "#E0E4CC"),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(color = "#E0E4CC"),
        axis.text.x = element_text(color = "#E86E1C",
                                   size = 8),
        plot.tag.position = c(0.02, 0.12),
        plot.tag = element_text(size = 10,
                                color = "#E86E1C"),
        plot.title = element_text(size = 11),
        plot.caption = element_text(size = 7))

# Save
ggsave("nhl top20.jpg",
       height = 8,
       width = 16,
       units = "cm",
       dpi = 300)
