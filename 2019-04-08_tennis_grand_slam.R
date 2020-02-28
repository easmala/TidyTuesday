library(tidyverse)
library(ggthemes)
library(rvest)
library(grid)
library(ggimage)
library(png)
library(ggrepel)

# Read in data
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

#Read in figure files
image_bg <- jpeg::readJPEG("istockphoto-491830606-612x612.jpg")
image_ball <- readPNG("Tennis-Ball-Free-Download.png")


# Remove unnecessary column
grand_slams <- grand_slams %>% 
  select(-grand_slam)

# Merge dataframes
df <- left_join(grand_slams, player_dob)

# Calculate age of each player when winning
df <- df %>%
  mutate(winning_age = difftime(tournament_date, date_of_birth, units = c("days"))) %>% 
  mutate(winning_age = as.numeric(winning_age)) %>% 
  mutate(image_ball = sample(c("Tennis-Ball-Free-Download.png"),
                             size = nrow(df), replace = TRUE)) # Add figure file as column

# Create rank column in dataframe
ranks <- df %>% 
  group_by(gender) %>% 
  arrange(desc(rolling_win_count)) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(rank = dense_rank(desc(rolling_win_count))) %>% 
  ungroup() %>%
  mutate(rank = ifelse(rank <= 3, "top", "low")) %>% 
  select(name, rank)

# Merge dataframes
df <- left_join(df, ranks, by = "name")

# Create separate dataframe for top ranked players
top_players <- df %>% 
  filter(rank == "top") %>% 
  group_by(name) %>% 
  top_n(1, winning_age)

# Plot
ggplot(df, aes(x = year, 
               y = (winning_age/365), 
               label = name)) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "white", 
              fullrange = TRUE, 
              size = 1) +
  geom_image(data = subset(df, rank == "top"), aes(image = image_ball), 
             size = .05, 
             by="height") +
  geom_image(data = subset(df, rank != "top"), aes(image = image_ball), 
             size = .02, 
             by="height") +
  facet_wrap(~ gender) +
  ylab("Age") +
  ggtitle("Age of Grand Slam Winners 1968 - 2019", subtitle = "Top 3 highlighted") + 
  geom_text_repel(data = top_players,
                  color = "white",
                  size = 3,
                  segment.size  = 0.5,
                  segment.colour = "white",
                  box.padding = 2, 
                  nudge_x = -2, 
                  nudge_y = 1,
                  segment.color = "white",
                  direction     = "x") +
  theme_few() +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(colour = "white", size = 12),
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(color = "white", size = 3),
        plot.title = element_text(face="bold", size = 12, color = "white"),
        axis.line = element_line(color = "white"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 10, color = "white"),
        axis.title.y = element_text(face = "bold", size = 12, color = "white"),
        axis.title.x = element_blank(),
        title = element_text(color = "white"),
        plot.margin = unit(c(5, 30, 5, 25), "pt"),
        panel.spacing = unit(c(30), "pt"))

# Save image
jpeg("grand slam winner age.jpg", width = 20, height = 12, units = "cm", quality = 80, res = 600)
grid.draw(gList(rasterGrob(image_bg, width = unit(1,"npc"), height = unit(1,"npc")), 
                ggplotGrob(p1)))
dev.off()
