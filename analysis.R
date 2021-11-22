library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(reshape2)
library(maps)

main_df <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/assignment-3---incarceration-jessica5m/main/incarceration_trends.csv?token=AV5FGDTEUGNMWUEREF7FRQTBUFQAO")

#1 & 2. which state has the highest jail pop within recent year and what is it?
highest_jail_df <- main_df %>%
  select(state, year, total_jail_pop) %>%
  group_by(state) %>%
  filter(year ==max(year))
highest_jail_df <- highest_jail_df %>%
  group_by(state) %>% 
  summarise(state_sum = sum(total_jail_pop, na.rm = TRUE))
highest_jail_df$state <- state.name[match(highest_jail_df$state,state.abb)]

highest_jail_pop <- max(highest_jail_df$state_sum)

highest_state_pop <- highest_jail_df %>%
  filter(state_sum == max(state_sum, na.rm = TRUE)) %>%
  pull(state)

# 3-6 which race has the highest and lowest incarceration rate withing the past year?
race_df <- main_df %>%
  select(year, aapi_jail_pop, latinx_jail_pop, black_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>%
  filter(year == max(year))
 
race_df <- race_df[, -1]
race_df <- melt(as.matrix(race_df))[-1]
race_df <- race_df %>%
  group_by(Var2) %>% 
  summarise(sum_by_race = sum(value, na.rm = TRUE))

highest_race_pop <- max(race_df$sum_by_race)
highest_race <- race_df %>%
  filter(sum_by_race == max(sum_by_race)) %>%
  pull(Var2)

lowest_race_pop <- min(race_df$sum_by_race)
lowest_race <- race_df %>%
  filter(sum_by_race == min(sum_by_race)) %>%
  pull(Var2)

#Chart 1 Over the past decade (2008-2018) how much has the total jail pop chnaged over time by region?
division_df <- main_df %>%
  select(division, year, total_jail_pop) %>%
  group_by(division)

division_df <- division_df %>%
  group_by(year, division) %>% 
  summarise(division_sum = sum(total_jail_pop, na.rm = TRUE))

ggplot(division_df, aes(x = year, y = division_df$division_sum, colour = division, group = division)) +
  geom_line() +
  ggtitle("Change In Jail Population By Division, From 1970-2018") +
  xlab("Year") +
  ylab("Jail Population")

#Chart 2 ratio between total pop and jail pop over time by race
# gorup by year, mean
# find mean of each race from each year
ratio_race_df <- main_df %>%
  select(year, aapi_jail_pop, latinx_jail_pop, black_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop, total_jail_pop) %>%
  group_by(year)
ratio_race_df <- ratio_race_df %>%
  group_by(year) %>%
  summarise(aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE), 
            latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE), 
            black_jail_pop = sum(black_jail_pop, na.rm = TRUE), 
            native_jail_pop = sum(native_jail_pop, na.rm = TRUE), 
            white_jail_pop = sum(white_jail_pop, na.rm = TRUE), 
            other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE), 
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE))

ratio_race_df <- ratio_race_df %>%
  group_by(year) %>%
   summarise(aapi_ratio = aapi_jail_pop/total_jail_pop *100,
            latinx_ratio = latinx_jail_pop/total_jail_pop *100,
            black_ratio = black_jail_pop/total_jail_pop *100,
            native_ratio = native_jail_pop/total_jail_pop *100,
            white_ratio = white_jail_pop/total_jail_pop *100,
            other_ratio = other_race_jail_pop/total_jail_pop*100)
ratio_race_df <- round(ratio_race_df, digits=2)
ratio_race_df <- melt(ratio_race_df, id=c("year"))

pop_df <- main_df %>%
  select(year, total_jail_pop) %>%
  group_by(year)%>%
  summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
            
ratio_race_df <- left_join(x = ratio_race_df, y = pop_df, by ="year")

ggplot(ratio_race_df, aes(x=total_jail_pop, y=value, colour= variable)) + 
  geom_point() +
  scale_colour_manual("Race", values = c("purple", "orange", "red", "dark green", "navyblue", "pink"), labels = c("Asian American/Pacific Islander", "Latin American", "African American", "Native American", "Caucasian", "Other")) +
  ggtitle("Total Incarceration Population vs. Ethnic Population") +
  xlab("Total Incarceration Population") +
  ylab("Percent of Race That Makes Up Jail Population(%)")

#map
states_map <- map_data("state")

incarceration_rate_df <- main_df %>%
  select(year, state, total_pop, total_jail_pop) %>%
  group_by(state) %>%
  filter(year == max(year))
incarceration_rate_df <- incarceration_rate_df %>%
  group_by(state, year) %>% 
  summarise(total_pop = sum(total_pop, na.rm = TRUE),
            total_jail_pop = sum(total_jail_pop, na.rm = TRUE))

incarceration_rate_df <-mutate(incarceration_rate_df, percent_incarcerated = total_jail_pop/total_pop * 100)
incarceration_rate_df$percent_incarcerated <- round(incarceration_rate_df$percent_incarcerated, digits=2)
colnames(incarceration_rate_df)[1] <- "region"

incarceration_rate_df$region <- state.name[match(incarceration_rate_df$region,state.abb)]
incarceration_rate_df <- incarceration_rate_df[-8,]
incarceration_rate_df$region = tolower(incarceration_rate_df$region)


map_df <- left_join(states_map, incarceration_rate_df, by = "region")
ggplot() + 
  geom_polygon( data=states_map, aes(long,lat, group = group), color="black", fill="white" )
blank_theme <- theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
ggplot() +
  geom_polygon( data= map_df, aes(long,lat, group = group, fill = percent_incarcerated), color="white", size = 0.5) +
  ggtitle("Percentage Of Population That Is Incarcerated") +
  scale_fill_continuous(name="Percentage",low = "lightblue", high = "navyblue") +
  blank_theme
