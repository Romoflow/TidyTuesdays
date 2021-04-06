library(tidyverse)
# Load data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')
brazil_loss <- tuesdata$brazil_loss

# Make the data tidy: each row is the deforestation from a driver in a given year
brazil_loss <- brazil_loss %>%
  select(!(entity:code)) %>%
  gather("driver", "forest_loss", !(year))

# Evolution of the drivers ranking
loss_drivers <- brazil_loss %>%
  group_by(year) %>%
  mutate(ranking = rank(-forest_loss, ties.method = 'first')) %>%
  ungroup()
  
chart_df <- loss_drivers %>%
  filter(year %in% c(2001, 2013)) %>%
  arrange(driver, year) %>%
  group_by(driver) %>%
  mutate(ranking_diff = diff(ranking)) %>%
    ungroup()

# Slope chart
tick_labels_2001 <- chart_df %>%
  filter(year == 2001) %>%
  arrange(ranking) %>%
  unite(label, ranking, driver, sep = ". ") %>%
  mutate(label = str_to_title(str_replace_all(label, "_", " "))) %>%
  pull(label)

tick_labels_2013 <- chart_df %>%
  filter(year == 2013) %>%
  arrange(ranking) %>%
  unite(label, ranking, driver, sep = ". ") %>%
  mutate(label = str_to_title(str_replace_all(label, "_", " "))) %>%
  pull(label)

ggplot(data = chart_df, 
       aes(x = year, y = ranking, group = driver, colour = ranking_diff))+
  geom_line()+
  geom_point()+
  scale_y_reverse("2001", breaks = 1:11, labels = tick_labels_2001, 
                     sec.axis = sec_axis(~.,name = "2013", breaks = 1:11, 
                                         labels = tick_labels_2013))+
  scale_color_gradient(low = "red", high = "green")+
  ggtitle("Main Drivers of Deforestation (2001 vs 2013)")+
  theme_minimal()+
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(hjust=0.5))
ggsave("slope_chart.png", device = "png")
# Area chart
loss_shares <- brazil_loss %>%
  group_by(year) %>%
  mutate(loss_share = forest_loss / sum(forest_loss)) 

ggplot(loss_shares, aes(x = year, y = loss_share, fill = driver))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_brewer("Driver",palette = "Paired")+
  scale_y_continuous("Percentage", labels = scales::percent)+
  scale_x_continuous("Year", breaks = seq(2001, 2013, 3))+
  ggtitle("Drivers share of total deforestation")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))
ggsave("area_chart.png", device = "png")