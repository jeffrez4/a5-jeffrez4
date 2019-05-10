# This R file performs the computations for the dynamic report regarding 
# shootings that have occurred in the year of 2018.

library(dplyr)

# Constructing data frame of shootings 2018
shooting_df <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# Basic table information
shooting_row <- nrow(shooting_df) #also the # of shootings
shooting_col <- ncol(shooting_df)

# How many lives were lost?
num_livelost <- sum(shooting_df$num_killed)

# Which city was most impacted by shootings? Impact is measured by combining 
# number of injured and lives lost.
impact_city <- shooting_df %>% 
  mutate(sum_lives = num_killed + num_injured) %>% 
  filter(sum_lives == max(sum_lives)) %>% 
  pull(city)

# Which city had the most recent shooting with most injured with no one killed?
most_injured_0_killed <- shooting_df %>% 
  filter(num_killed == 0) %>% 
  filter(num_injured == max(num_injured)) %>%
  arrange(date) %>% 
  pull(city) %>% 
  tail(1)


# Which month had the most killed?
impact_month <- shooting_df %>%
  mutate(month_only = format(as.Date(date, "%B %d, %Y"), "%B")) %>%
  group_by(month_only) %>% 
  summarize(sum_killed = sum(num_killed)) %>% 
  filter(sum_killed == max(sum_killed)) %>% 
  pull(month_only)

# Constructing Summary Table
summary_table <- shooting_df %>% 
  group_by(state) %>% 
  summarize(total_killed = sum(num_killed), total_injured = sum(num_injured),
            total_impact = total_killed + total_injured, 
            avg_impact_per = round(mean(num_killed + num_injured), 1)) %>% 
  arrange(-total_impact)

# Additional insights from summary table
# Top 3 impacted states
top_impact_state <- summary_table %>% 
  select(state) %>% 
  head(1)

second_impact_state <- summary_table %>% 
  select(state) %>% 
  slice(2)

third_impact_state <- summary_table %>% 
  select(state) %>% 
  slice(3)

# Lowest impacted state
lowest_impact_state <- summary_table %>% 
  select(state) %>% 
  tail(1)

# Amount of Impact for the top three states
top_impact_amt <- summary_table %>% 
  select(total_impact) %>% 
  head(1)

second_impact_amt <- summary_table %>% 
  select(total_impact) %>% 
  slice(2)

third_impact_amt <- summary_table %>% 
  select(total_impact) %>% 
  slice(3)

# Amount of Impact for the lowest state
lowest_impact_amt <- summary_table %>% 
  select(total_impact) %>% 
  tail(1)

# Descripting Seattle Shooting (Particular Incident)

# Filtering for Seattle shooting event in April only
seattle_biker_shooting <- shooting_df %>% 
  filter(state == "Washington", date == "April 29, 2018")

# Extracton of values to be used
shooting_state <- seattle_biker_shooting %>% 
  select(state)

shooting_city <- seattle_biker_shooting %>% 
  select(city)

shooting_address <- seattle_biker_shooting %>% 
  select(address)

shooting_date <- seattle_biker_shooting %>% 
  select(date)

shooting_num_killed <- seattle_biker_shooting %>% 
  select(num_killed)

shooting_num_injured <- seattle_biker_shooting %>% 
  select(num_injured)

# Interactive Map
# Custom label message string for popup feature in interactive map
label_msg <- paste("<strong>Location: </strong>", shooting_df$city, ", ", 
                   shooting_df$state, "</br><strong>Killed: </strong>", 
                   shooting_df$num_killed, " <strong>Injured: </strong>", 
                   shooting_df$num_injured, "</br><strong>Date: </strong>",
                   shooting_df$date, sep = ""
)

# Set up question to be answered by ggplot histogram depiction
shooting_states <- shooting_df %>% 
  group_by(state) %>% 
  summarize(shooting_occurrences = length(state)) %>% 
  arrange(shooting_occurrences) %>% 
  mutate(order = factor(state, state))










  






