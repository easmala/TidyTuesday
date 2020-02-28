library(tidyverse)
library(rayshader)
library(av)

df_nc <- read_csv("https://raw.githubusercontent.com/WSJ/measles-data/master/individual-states/northCarolina.csv")

# Get NC counties
nc_map <- tbl_df(map_data("county", 
                          region = "north carolina"))

# Clean dataframe and calculate mean value for each county
county_data <- df_nc %>% 
  group_by(county) %>% 
  summarise(mean_overall = mean(overall, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(subregion = county) %>% 
  mutate(subregion = tolower(subregion),
         unvaccinated = (100 - mean_overall) / 100)

# Join the values to the map
df <- left_join(nc_map, 
                county_data, 
                by = "subregion")

# Plot
gg <- ggplot() +
  geom_polygon(data = df,
               aes(x = long, 
                   y = lat, 
                   group = subregion, 
                   fill = unvaccinated), 
               color = NA) +
  scale_fill_distiller("Unvaccinated school children",
                       palette = "YlOrRd",
                       direction = 1,
                       limits = c(0, 0.15),
                       breaks = c(0, 0.05, 0.1, 0.15),
                       labels = scales::percent_format(accuracy = 1),
                       guide = guide_colourbar(direction = "horizontal")) +
  coord_map("polyconic") +
  theme_map(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = c(0.01, 0.01),
        legend.justification = c(0, 0)) +
  ggtitle("Proportion of students not vaccinated in NC",
          subtitle = "School year 2018-2019") +
  labs(caption = "Data Source: NC Department of Health and Human Services")
gg


# Rayshader plot
plot_gg(gg,
        width = 7,
        height = 4,
        units = "cm",
        scale = 250)

# Save as .mp4 file
render_movie("nc_vaccines_movie.mp4",
             fps = 30,
             phi = 50)
