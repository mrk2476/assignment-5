#load libraries 
library(lintr)
library(styler)
library(dplyr)
library(stringr)
library(tm)
library(tidyr)
library(leaflet)
library(kableExtra)
library(ggplot2)
library(plotly)


#load mass shooting dataset
Mass_shootings <- read.csv("./data/shootings-2018.csv", 
                           stringsAsFactors = FALSE)


#summary info
num_shootings <- nrow(Mass_shootings)

total_lives_lost_all_shootings <- pull(summarise(Mass_shootings, num_killed = 
                                                   sum(num_killed)))
Mass_shootings <- Mass_shootings %>%
  mutate(impact = rowSums(select(., num_killed, num_injured)))
  
most_impacted_city <- pull(filter(Mass_shootings, impact == max(impact)) %>%
  select(city))

city_max_killed_of_shootings <- pull(Mass_shootings %>% 
  filter(num_killed == max(num_killed)) %>%
  select(city))

city_most_injured_of_shootings <- pull(Mass_shootings %>% 
  filter(num_injured == max(num_injured)) %>%
  select(city))


#summary table information filtering
summarized_shootings <- Mass_shootings %>% 
  mutate(Month = (str_trim(removeNumbers(gsub(",", "", date))))) %>%
  group_by(Month) %>%
  summarise(
    Killed = sum(num_killed),
    Injured = sum(num_injured),
  ) %>%
  arrange(match(Month, month.name))


#Summary Table
table <- kable(summarized_shootings, caption = "2018 Shootings by Month") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)
 
 
#Specific Incident
worst_shooting <- Mass_shootings %>% filter(num_killed == max(num_killed))

worst_date <- pull(worst_shooting %>% select(date))

worst_state <- pull(worst_shooting %>% select(state))

worst_city <- pull(worst_shooting %>% select(city))

worst_deaths <- pull(worst_shooting %>% select(num_killed))

worst_address <- pull(worst_shooting %>% select(address))

worst_shooting_injuries <- pull(worst_shooting %>% select(num_injured))


#Interactive Map

Mass_shootings <- Mass_shootings %>% 
  mutate(radius = 
           ((rowSums(Mass_shootings[, c("num_killed", "num_injured")])* 1.3 ))) 

map <- leaflet(data = c(Mass_shootings)) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = "green",
    lat = ~lat,
    lng = ~long,
    popup = ~paste(city, ", ", state, "<br>", "Killed: ", num_killed, 
                   "<br>", "Injured: ", num_injured, "<br>", date, sep = ""),
    stroke = FALSE,
    radius = ~radius,
    fillOpacity = 0.5
  )


#plot

shootings_per_month <- Mass_shootings %>% 
  mutate(Month = (str_trim(removeNumbers(gsub(",", "", date))))) %>%
  group_by(Month) %>%
  summarise(
    killed = sum(num_killed),
    injured = sum(num_injured),
    Number_of_Shootings = sum(n())
  ) %>%
  arrange(match(Month, month.name)) 

shootings_plot <- ggplot(data = shootings_per_month, 
       aes(y = Month, x = Number_of_Shootings, fill = Number_of_Shootings, 
           group = 1, text = paste("Killed: ", killed, "<br>Injured: ",
                                   injured, "<br>Shootings: ", 
                                   Number_of_Shootings))) +
  ggtitle("2018 Shootings by Month") +
  scale_y_discrete(limits = month.name) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low = "#99B2FF", high = "#001F80")

run_plot <- ggplotly(shootings_plot, tooltip = "text") %>%
  config(displayModeBar = FALSE)


