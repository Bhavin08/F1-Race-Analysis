#install.packages("ggdark")
#install.packages("ggExtra")
#install.packages("circlize")

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggdark)
library(ggthemes)
library(RColorBrewer)
library(viridisLite)
library(viridis)
library(stringr)
library(grid)
library(gridExtra)
library(ggExtra)
library(ggExtra) # for ggmarginal. helpful to show the distribution on the x and y axis on ggplot graph
library(circlize)

results = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/results.csv", header = T, sep = ',', stringsAsFactors = F)
drivers = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/drivers.csv", header = T, sep = ',', stringsAsFactors = F)
races = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/races.csv", header = T, sep = ',', stringsAsFactors = F)
circuits = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/circuits.csv", header = T, sep = ',', stringsAsFactors = F)
drivers_standing = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/driver_standings.csv", header = T, sep = ',', stringsAsFactors = F)
constructors = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/constructors.csv", header = T, sep = ',', stringsAsFactors = F)
constructorStandings = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/constructor_standings.csv", header = T, sep = ',', stringsAsFactors = F)
constructorResults = read.csv("/Users/bhavinghoghari/Desktop/Work/Self Learn/R/F1 Race/formula-1-world-championship-1950-2020/constructor_results.csv", header = T, sep = ',', stringsAsFactors = F)


#__________________#________________#__________________#________________
# Results Data Set
str(results)
head(results$fastestLapSpeed)

# Convert fastLapSpeed to numeric. as.numeric() only will give an inernal representation values which R stores but using as.numeric(as.character()) will give actual values as numeric
results$fastestLapSpeed = as.numeric(as.character(results$fastestLapSpeed))

# Not working
convertFastestLap = function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum<-sapply(results$fastestLapTime, convertFastestLap)


#____________________#____________________#____________________#____________________
str(races)

# Convert date from factor to date format
races$date = ymd(races$date)

# Remove Grand Prix from the name variable
races$name = gsub("Grand Prix", "", races$name)

# Join races data set to results data set using left_join()
results_2 = left_join(results %>% select(-time, -fastestLapTime),
                      races %>% select(-time, -url),
                      by = 'raceId')
str(results)


## Are F1 cars going slower nowadays ?
  ## by each country grand prix
results_2 %>% 
  filter(year > 2003) %>% 
  group_by(name, year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed, na.rm = T)) %>% 
  ggplot(aes(x = factor(year), y = medianFastestLapSpeed, color = medianFastestLapSpeed)) +
  geom_point() +
  theme_fivethirtyeight() +
  scale_color_gradientn(name = "", colors = rev(viridis::viridis(20))) +
  facet_wrap(~name, ncol = 9) +
  labs(title = "Fastest Lap Per Circuit",
       subtitle = "2003 - 2019",
       x = "",
       y = "Speed in km/h") +
  guides(color = F) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        strip.text.x = element_text(size = 10))

  ## Average lap speed grouped by grand prix to see the over all trend
results_2 %>% 
  filter(year > 2003) %>% 
  group_by(name, year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed, na.rm = T)) %>% 
  ggplot(aes(x = factor(year), y = medianFastestLapSpeed, color = medianFastestLapSpeed)) +
  geom_boxplot(alpha = 0.25) +
  theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = "loess", aes(group = 1), color = 'red', lty = 2, size = 0.5) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(title = "Fastest Lap Per Year", subtitle = "in km/h, grouped by grand prix", x = "", y = "") +
  guides(color = F) +
  dark_theme_minimal()

  ## Fastest Lap Distribution Per Circuit
results_2 %>% 
  filter(year > 2003) %>% 
  group_by(name) %>% 
  ggplot(aes(x = fastestLapSpeed)) +
  geom_histogram(bins = 100) +
  theme_fivethirtyeight() +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  facet_wrap(~name, ncol = 9) +
  labs(title = "Fastest Lap Distribution Per Circuit",
       subtitle = "speed in km/h, grouped by year",
       x = "",
       y = "") +
  guides(color = F) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 45),
        strip.text.x = element_text(size = 10))

  ## median fastest lap time number


#_________________#_________________#_________________#_________________
# Drivers Data Set
str(drivers)

# Calculate the drivers age in 2020
  # Function to calculate age from dob
calc_age = function(birthDate, refDate = Sys.Date()) {
  require(lubridate)
  period = as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
}
  # Mutate age column using calc_age function
drivers$age_driver = calc_age(drivers$dob)


#_________________#_________________#_________________#_________________
#Drivers Standing Data Set

str(drivers_standing)
str(results)
str(drivers)

# Left join drivers standing with drivers data set
drivers = left_join(drivers %>% select(-url),
                    drivers_standing,
                    by = 'driverId')

# Left join drivers data set with results data set
results_3 = left_join(results,
                       drivers %>% rename(number_driver = number) %>% 
                                            select(-points, -position, positionText),
                                          by = c('driverId', 'raceId'))

  # Left join further races data set to result 3
results_3 = left_join(results_3,
                      races %>% select(-time),
                      by = 'raceId')
colnames(results_3)

winDis = results_3 %>% 
  filter(position == "1") %>% 
  group_by(driverRef, name) %>% 
  summarize(count = n()) %>% 
  mutate(allWins = sum(count)) %>% 
  ggplot(aes(x = allWins)) + 
  geom_histogram(bins = 60) +
  theme_fivethirtyeight() +
  ggtitle("Distribution of the number of victories")

winBar = results_3 %>% 
  filter(position == "1") %>% 
  group_by(driverRef, name) %>% 
  summarize(count = n()) %>%
  mutate(allWins = sum(count)) %>% 
  filter(allWins > 2) %>% 
  ggplot(aes(x = reorder(driverRef, allWins), y = count)) +
  geom_col(aes(fill = name), color = 'white', size = .1) +
  geom_text(aes(y = allWins, # position the label based on total win
                label = allWins),
            colour = "grey",
            hjust = 0, # left align labesl to total win (default is center aligned)
            nudge_y = 0.4, # offset lables slightly to the right 
            check_overlap = T, # for labels overlapping in the same position
            size = 2.3) + # reduce the label font size
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_manual(name = "", values = rev(viridis::viridis(41))) +
  guides(fill = guide_legend(ncol = 5)) + # change number of columns of guides
  labs(title = "Total Number of Victories Per Driver", 
       subtitle = "by circuit, only drivers with more than 2 wins",
       x = "", y = "") +
  dark_theme_minimal() +
  theme(legend.text = element_text(size = 7.5), # reduce the legends text size in guide
        legend.key.size = unit(.2, "cm"), # change the guides shape from box to smaller 
        legend.position = c(.77,.10)) # move legends around graph to re position

  # Dsiplay win distribution histogram with win bar graph using annotation_custom() function
winBar + annotation_custom(grob = ggplotGrob(winDis), xmin = 22, xmax = 50, ymin = 31, ymax = 90)

# Exploring how many times did Leclerc win
str(results_3)
results_3 %>% 
  filter(position == "1") %>% 
  group_by(surname) %>% 
  summarize(count = n()) %>% 
  filter(stringr::str_detect(surname, 'Lec')) # library(stringr) allows to use str_detect to look for certain specific words


#_______________________#_______________________#_______________________#_______________________
# Constructor Data Set
# Merge all three constructor data set
str(constructors)
str(constructorResults)
str(constructorStandings)


# Left join races to constructorResult. ##This will join race result with constructor result 
str(races)
constructorResults_2 = left_join(constructorResults,
                              races %>% rename(name_races = name),
                              by = 'raceId')

# Left join constructors data set to constructorResults. ## This will join constructors name to constructors result
constructorResults_2 = left_join(constructorResults_2,
                              constructors %>% select(-url) %>% rename(name_constructor = name),
                              by = 'constructorId')

# Left join constructorStanding with constructorResults
str(constructorStandings)
constructorResults_2 = left_join(constructorResults_2,
                              constructorStandings %>% rename(point_constructor = points),
                              by = 'constructorId', 'raceId')

str(constructorResults_2)

# Left join constructor name to results data set and name results to results 4
  # Data set with constructors name
results_4 = left_join(results_3,
                  constructors %>% 
                    rename(name_constructor = name) %>% select(constructorId, name_constructor),
                  by = "constructorId")
str(results_4)

## Which constructor won the most races?
  # Visualize which constructors won the most
winConstructorsBar = results_4 %>% 
  filter(position == "1") %>% 
  group_by(name_constructor) %>% 
  summarise(count = n()) %>% 
  filter(count >= 1) %>% 
  ggplot(aes(x = reorder(name_constructor, count), y = count, fill = count)) +
  geom_col() +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_gradientn(name = "", colors = viridis::viridis(6)) +
  guides(fill = F) + # use fill = F to remove legend from scale_fill_gradientn
  geom_text(aes(y = count, # position the label based on total win
                label = count),
            colour = "grey",
            hjust = 0, # left align labels to total win (default positon is center aligned) 
            nudge_y = 0.5, # offset labels to slightly right
            size = 2.3) + # set the font size for label
  dark_theme_minimal() +
  labs(title = "Which constructor won the most", x = "", y = "")

# Top 5 constructors win per year
top5Constructors = results_4 %>% 
  filter(name_constructor %in% c("Ferrari", "McLaren", "Williams", "Mercedes", "Red Bull")) %>% 
  filter(position == "1") %>% 
  group_by(name_constructor, year) %>% 
  summarize(count = n())
table(top5Constructors$name_constructor)

top5Constructors_graph = ggplot(top5Constructors, aes(x = factor(year), y = count)) +
  geom_col(aes(fill = name_constructor),
           stat = 'identity',
           position = "fill",
           size = 1.5) +
  theme_fivethirtyeight() +
  scale_fill_viridis(discrete = T, option = "D") +
  dark_theme_minimal() +
  labs(title = "Top 5 constructor's win per year", x =  "", y = "") +
  theme(axis.text.x = element_text(angle = 45, size = 5))

winConstructorsBar + annotation_custom(grob = ggplotGrob(top5Constructors_graph),
                                       xmin = 20, xmax = 0, ymin = 20, ymax = 200)

# Scuderia Ferrari
  # Winning of ferrari
results_4 %>% 
  filter(name_constructor == "Ferrari") %>% 
  filter(position == "1") %>% 
  group_by(year) %>% 
  summarize(count = n()) %>% 
  mutate(current = cumsum(count)) %>% 
  ggplot(aes(x = year, y = current, fill = count)) +
  geom_col() +
  geom_text(aes(label = count),
            position = position_dodge(0.9), # position_dodge sets the number right on top of bars
            vjust = -1,
            size = 2.5,
            color = 'grey') +
  scale_fill_viridis() +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  guides(fill = F) +
  labs(title = "Ferrari's total win from 1950 - 2019", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45))
  
# Bit of history
  # Winners from 1950 - 1990 by constructor
constructors_win_1950_1990 = results_4 %>% 
  filter(year <=1990) %>% 
  filter(position == "1") %>% 
  group_by(name_constructor) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

top10_constructors_win_1950_1990 = results_4 %>% 
  filter(year <=1990) %>% 
  filter(position == "1") %>% 
  group_by(name_constructor, year) %>% 
  filter(name_constructor %in% c("Ferrari", "McLaren", "Team Lotus", "Williams", "Brabham", "Tyrrell", 
                                 "Lotus-Climax", "BRM", "Renault", "Cooper-Climax")) %>% 
  summarise(count = n()) 

  # Visualizing the top ten winners from 1950 - 1990
ggplot(top10_constructors_win_1950_1990, aes(x = factor(year), y = count)) +
  geom_col(aes(fill = name_constructor),
           stat = 'identity',
           position = "fill",
           size = 1.5) +
  scale_fill_viridis(discrete = T) +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Top 10 Constructor by Win",
       subtitle = "1950 - 1990", x = "", y = "") +
  theme(legend.position = "right",
        legend.text = element_text(size = 10),
        legend.key.size = unit(.3, "cm"),
        axis.text.x = element_text(angle = 45, size = 8.5))

## Hunt VS Lauda
str(results_3)
results_3 %>% 
  filter(year == 1976) %>% 
  filter(surname == "Hunt" | surname == "Lauda") %>% 
  select(date, surname, points) %>% 
  mutate(win = ifelse(points == 9, 'yes', 'no')) %>% 
  group_by(surname) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x = date, y = current, color = surname)) +
  geom_line(alpha = 0.7, size = 2) +
  geom_point(aes(shape = win), color = 'grey', size = 2.5) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  guides(shape = F) +
  labs(title = "Points accumulated during 1976", 
       subtitle = "Hunt Vs Lauda. Triangle indicates a win, circle otherwise",
       x = "", y = "")

str(results_3)
results_3 %>% 
  filter(year == 1976) %>% 
  filter(surname == "Hunt" | surname == "Lauda") %>% 
  select(date, surname, points, name) %>% 
  mutate(win = ifelse(points == 9, 'yes', 'no')) %>% 
  group_by(surname) %>% 
  mutate(current = cumsum(points))


# Prost and Senna. Analyzing their rivalry
  # Analyzing for year 1989
prost_vs_senna_1988 = results_3 %>% 
  filter(surname == "Prost" | surname == "Senna") %>% 
  filter(year == 1988) %>% 
  select(date, surname, points) %>% 
  mutate(win = ifelse(points == 9, 'yes', 'no')) %>% 
  group_by(surname) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x = date, y = current, color = surname)) +
  geom_line(size = 2, alpha = 0.5) +
  geom_point(aes(shape = win), color = 'grey', size = 2) +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Points Accumulated During 1988", x = "", y = "")

  # Analyzing for year 1989
prost_vs_senna_1989 = results_3 %>% 
  filter(surname == "Prost" | surname == "Senna") %>% 
  filter(year == 1989) %>% 
  select(date, surname, points) %>% 
  mutate(win = ifelse(points == 9, 'yes', 'no')) %>% 
  group_by(surname) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x = date, y = current, color = surname)) +
  geom_line(size = 2, alpha = 0.5) +
  geom_point(aes(shape = win), color = 'grey', size = 2) +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Points Accumulated During 1989", x = "", y = "")

  # Points accumulated in 1988 and 1989
grid.arrange(prost_vs_senna_1988, prost_vs_senna_1989, ncol = 1)

# Age of vettel and sainz
results_3 %>% 
  filter(surname == "Vettel" | surname == "Sainz") %>% 
  filter(year == 2019) %>% 
  select(surname, age_driver) %>% 
  group_by(surname) %>% 
  summarize(age = mean(age_driver)) %>% 
  ggplot(aes(x = surname, y = age, fill = surname)) +
  geom_col(width = 0.3, alpha = 0.7) +
  geom_text(aes(label = age), vjust = -.5) +
  guides(color = F) +
  theme_fivethirtyeight() +
  labs(title = "Age of Driver", x = "", y = "") +
  dark_theme_minimal()

# Data set for points accumulated in 2018 for Vettel and Leclerc
str(results_3)
points_result_2018 = results_3 %>% 
  filter(surname %in% c("Vettel", "Leclerc")) %>% 
  filter(year == 2018) %>% 
  select(date, surname, points, name) %>% 
  group_by(surname) %>%
  summarize(count = n())
  mutate(current = cumsum(points))
  
results_3 %>% 
  filter(surname %in% c("Leclerc", "Vettel"))

  # Visualizing the points earn in 2018
points_result_2018_graph = 
  ggplot(points_result_2018, aes(x = date, y = current, color = surname)) +
  geom_line(size = 2, alpha = 0.47) +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  labs(title = "Points Won in 2018", x = "", y = "") + 
  ylim(0,30) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Data set for points accumulated in 2019 for Hamilton, Vettel and Sainz
points_result_2019 = results_3 %>% 
  filter(year == 2019) %>% 
  select(surname, date, points, name) %>% 
  group_by(surname, date, name) %>% 
  summarize(total_points = sum(points)) %>% 
  filter(surname %in% c("Hamilton", "Vettel", "Sainz"))

  # Visualizing the points earn in 2019
points_result_2019_graph = 
  ggplot(points_result_2019, aes(x = date, y = total_points, color = surname)) +
  geom_line(size = 1.1, alpha = 0.47) +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%y") +
  labs(title = "Points Won in 2019", x = "", y = "") +
  ylim(0,30)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Arrnaging two plots into one to compare between 2018 and 2019
grid.arrange(points_result_2018_graph, points_result_2019_graph, ncol = 1)

# Analyzing the best driver
str(topDrivers)
table(topDrivers$driverRef)

topDrivers = results_4 %>% 
  filter(position == "1") %>% 
  group_by(year, driverRef, name_constructor) %>% 
  summarize(count = n()) %>% 
  mutate(period = if(year <= 1970) {
    print("1950 - 1970")
  } else if (year >= 1971 & year <= 1990) {
    print("1971 - 1990")
  } else if (year >= 1991 & year <= 2010) {
    print("1991 - 2010")
  } else { print("2011 - 2020")
  }) 

  # visualizing drivers winning by period from 1950 - 2020
topDrivers %>% 
  group_by(driverRef, period) %>% 
  summarise(count = sum(count)) %>% 
  ggplot(aes(x = reorder(driverRef, count), y = count, fill = period)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "D") +
  coord_flip() +
  dark_theme_minimal()

  # filtering top 5 drivers by changing the period value in filter
topDrivers %>% 
  filter(period == "2011 - 2020") %>% 
  group_by(driverRef) %>% 
  summarize(count = sum(count)) %>% 
  arrange(desc(count))

  # Visualizing top drivers by period from 1950 - 1970
topDrivers_1950_1970 = topDrivers %>% 
  filter(period == "1950 - 1970") %>% 
  filter(driverRef %in% c("clark", "fangio", "moss", "hill", "jack_brabham")) %>% 
  ggplot(aes(x = factor(year), y = count, fill = driverRef)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "D") +
  labs(title = "Top 5 drivers from 1950 - 1970", subtitle = "Winnings", x= "", y = "") +
  scale_y_continuous(name = "", limits = c(0, 7.5)) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 9), # reduce the legends text size in guide
        legend.key.size = unit(.2, "cm"), # change the guides shape from box to smaller 
        legend.position = "bottom")
    
  # Visualizing top drivers by period from 1971 - 1990
topDrivers_1971_1990 = topDrivers %>% 
  filter(period == "1971 - 1990") %>% 
  filter(driverRef %in% c("prost", "senna", "lauda", "piquet", "mansell")) %>% 
  ggplot(aes(x = factor(year), y = count, fill = driverRef)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "D") +
  labs(title = "Top 5 drivers from 1971 - 1990", subtitle = "Winnings", x = "", y = "") +
  scale_y_continuous(name = "", limits = c(0,15)) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 9),
        legend.key.size = unit(.2, "cm"),
        legend.position = "bottom")

  # Visualizing top drivers by period from 1991 - 2010
topDrivers_1991_2010 = topDrivers %>% 
  filter(period == "1991 - 2010") %>% 
  filter(driverRef %in% c("michael_schumacher", "alonso", "damon_hill", "hakkinen", "raikkonen")) %>% 
  ggplot(aes(x = factor(year), y = count, fill = driverRef)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "D") +
  labs(title = "Top 5 driver from 1990 - 2010", subtitle = "Winnings", x = "", y = "") +
  scale_y_continuous(name = "", limits = c(0, 15)) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 9),
        legend.key.size = unit(.2, "cm"),
        legend.position = "bottom")

# Visualizing top drivers by period from 2011 - 2020
topDrivers_2011_2020 = topDrivers %>% 
  filter(period == "2011 - 2020") %>% 
  filter(driverRef %in% c("hamilton", "vettel", "rosberg", "max_verstappen", "bottas")) %>% 
  ggplot(aes(x = factor(year), y = count, fill = driverRef)) +
  geom_col() +
  scale_fill_viridis(discrete = T, option = "D") +
  labs(title = "Top 5 drivers from 2011 - 2020", subtitle = "Winnings", x = "", y = "") +
  scale_y_continuous(name = "", limits = c(0, 20)) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 9),
        legend.key.size = unit(.2, "cm"),
        legend.position = "bottom")

grid.arrange(topDrivers_1950_1970, topDrivers_1971_1990, topDrivers_1991_2010, topDrivers_2011_2020,
             ncol = 2)


## Analyzing and exploring drivers 
## Leclerc VS Vettel points earned in 2019
vettel_leclerc_2019_points_line = results_4 %>% 
  filter(driverRef %in% c("leclerc", "vettel")) %>% 
  filter(year == 2019) %>% 
  select(date, driverRef, points) %>% 
  mutate(win = ifelse(points == 25, 'yes', 'no')) %>% 
  group_by(driverRef) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x = date, y = current, color = driverRef)) +
  geom_line(size = 1.5) +
  geom_point(aes(shape = win), color = 'grey', size = 2) +
  scale_x_date(breaks = "2 weeks",
               date_labels = "%d-%b") +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Points earned in 2019", subtitle = "Leclerc VS Vettel, Triangle indicates a win, circle otherwise", x = "", y = "") +
  guides(shape = F) +
  theme(axis.text.x = element_text(angle = 45, size = 7.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

## Visualizing points earned in 2019 of top 10 drivers
str(results_4)
results_4 %>% 
  filter(year == 2019) %>% 
  filter(driverRef %in% c("hamilton", "bottas", "max_verstappen", "leclerc", "vettel", "sainz", "gasly", "albon", "ricciardo", "perez")) %>% 
  select(date, driverRef, points) %>% 
  mutate(win = ifelse(points == 25, 'yes', 'no')) %>% 
  group_by(driverRef) %>% 
  mutate(current = cumsum(points)) %>% 
  ggplot(aes(x = date, y = current, color = driverRef)) +
  geom_line(size = 1.5) +
  geom_point(aes(shape = win), color = 'grey', size = 2) +
  scale_color_brewer(palette = "Set3") +
  scale_x_date(breaks = "1 month",
               date_labels = "%b") +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Points earned in 2019", 
       subtitle = "Leclerc VS Vettel, Triangle indicates a win, circle otherwise", x = "", y = "") +
  guides(shape = F) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# Data set for Leclerc and Vettel 2019 race
str(vettel_leclerc_2019)
vettel_leclerc_2019 = results_4 %>% 
  filter(driverRef %in% c("leclerc", "vettel")) %>% 
  filter(year == 2019) %>% 
  select(date, driverRef, points, fastestLapTime) %>% 
  mutate(win = ifelse(points == 25, 'yes', 'no')) %>% 
  group_by(driverRef) %>% 
  mutate(current = cumsum(points))

  # Converting fastest lap time to ms format (minute second) using lubridate
vettel_leclerc_2019$fastestLapTime = ms(vettel_leclerc_2019$fastestLapTime)

  # Convert fastestLapTime to numeric
vettel_leclerc_2019$fastestLapTime = as.numeric(vettel_leclerc_2019$fastestLapTime)

# Visualizing fastest lap speed for vettel and leclerc
vettel_leclerc_2019_fastestLapSpeed = ggplot(vettel_leclerc_2019, aes(x = date, y = fastestLapTime, color = driverRef)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d-%b") +
  theme_fivethirtyeight() +
  dark_theme_minimal() +
  labs(title = "Fastest Lap Time", subtitle = "2019 | Speed in seconds", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, size = 7.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

  # Arrange fastestLapTime and points earned in one graph
grid.arrange(vettel_leclerc_2019_points_line, vettel_leclerc_2019_fastestLapSpeed, ncol = 1)

# result_5 contains data with fastestLapTime being converted to numeric
results_5 = results_4
results_5$fastestLapTime = ms(results_5$fastestLapTime)
results_5$fastestLapTime = as.numeric(results_5$fastestLapTime)

results_5 %>% 
  filter(driverRef %in% c("hamilton", "bottas", "max_verstappen", "leclerc", "vettel", "sainz", "gasly", "albon", "ricciardo", "perez")) %>% 
  select(year, driverRef, fastestLapTime) %>% 
  group_by(driverRef) %>% 
  ggplot(aes(x = factor(year), y = fastestLapTime, fill = driverRef)) +
  geom_boxplot() 

# Visualizing fastes lap time over years for vettel
results_5 %>% 
  filter(driverRef %in% c("vettel")) %>% 
  select(year, driverRef, fastestLapTime, fastestLapSpeed) %>% 
  group_by(driverRef) %>% 
  ggplot(aes(x = factor(year), y = fastestLapTime, color = fastestLapTime)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = "loess", aes(group = 1), color = "red", lty = 2, size = 0.5) +
  scale_color_viridis() +
  dark_theme_minimal() +
  guides(color = F) +
  labs(title = "Sebastian Vettel Average Fastest Lap Time", subtitle = "2007 - 2019 | Speed in Seconds", x = "", y = "")
  
# Visualizing fastes lap speed over years for vettel
results_5 %>% 
  filter(driverRef %in% c("vettel")) %>% 
  select(year, driverRef, fastestLapTime, fastestLapSpeed) %>% 
  group_by(driverRef) %>% 
  ggplot(aes(x = factor(year), y = fastestLapSpeed, color = fastestLapSpeed)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = "loess", aes(group = 1), color = "red", lty = 2, size = 0.5) +
  scale_color_viridis() +
  dark_theme_minimal() +
  guides(color = F) +
  labs(title = "Sebastian Vettel Average Fastest Lap Speed", subtitle = "2007 - 2019 | Speed in Seconds", x = "", y = "")

topDrivers_2019_fastestLapSpeed = results_5 %>% 
  filter(driverRef %in% c("hamilton", "bottas", "max_verstappen", "leclerc", "vettel", "sainz", "gasly", "albon", "ricciardo", "perez")) %>% 
  select(year, driverRef, fastestLapSpeed) %>% 
  group_by(driverRef) %>%
  ggplot(aes(x = driverRef, y = fastestLapSpeed, color = fastestLapSpeed)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.7) +
  scale_color_viridis() +
  dark_theme_minimal() +
  guides(color = F) +
  labs(title = "Distribution of fastest lap speed", subtitle = "Top 10 drivers of 2019", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45))
  #theme_void() # sets the backgorund to white. can be used for case study or ppt

  # ggMarginal() use to show the distribution along the x and y axis 
ggExtra::ggMarginal(topDrivers_2019_fastestLapSpeed + dark_theme_minimal(), 
           type = "histogram", # type of graph to be displayed along the axis (example = "density", "histogram")
           size = 10, # size of graph
           margins = c("y"), # name of axis on which the graph to be displayed
           groupColour = T, # if set to TRUE, color matches with scatter plot
           groupFill = T)

 # histogram showing distribution of fastes lap speed of top 10 drivers of 2019
topDrivers_2019_fastestLapSpeed_histogram = results_5 %>% 
  filter(driverRef %in% c("hamilton", "bottas", "max_verstappen", "leclerc", "vettel", "sainz", "gasly", "albon", "ricciardo", "perez")) %>% 
  select(year, driverRef, fastestLapSpeed) %>% 
  group_by(driverRef) %>%
  ggplot(aes(x = fastestLapSpeed,y = )) +
  geom_histogram() +
  coord_flip() +
  dark_theme_minimal() +
  guides(color = F) +
  labs(title = "Distribution of fastest lap speed", subtitle = "Top 10 drivers of 2019", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45))

## Relationship between drivers and constructors
  # create temp for this which contains name of constructors with name of drivers for only those whose winning is above 5
str(results_4)
temp = data.frame(results_4 %>% 
                    filter(position == 1) %>% 
                    group_by(name_constructor, driverRef) %>% 
                    summarize(count = n()) %>% 
                    filter(count > 5) %>% 
                    na.omit())

  # prepare colors for constructors
names = sort(unique(temp$name_constructor)) # sort out unique names by alphabetical order
color = c('#9B0000', "gray50" ,"gray50" ,"#FFFFE0" ,"gray50",
          "#006400" ,'#bfe843' ,'#DC0000' ,'gray50' ,'#006400',
          '#7F7F7F' ,'#7F7F7F' ,'#FF8700', '#00D2BE' ,'#d99836',
          '#FFF500' ,'#ad58db', "#703b67", "#14676b", "#e0faa7")
COL = data.frame(name_constructor = names, color) # create date frame with constructors name and another column with color 
temp2 = data.frame(left_join(temp, COL, by = "name_constructor"))


chordDiagram(temp2[,c(1:2)], transparency = 0.5, grid.col = append(color, rep("aliceblue", 38)),
             col = as.character(temp2$color), annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), 
              ylim[1],
              sector.name,
              facing = "clockwise",
              niceFacing = T,
              adj = c(-0.12, 0.30),
              cex = .6)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index  = 2)
}, bg.border = NA)

circos.clear()


# Analyzing Mercedes Wins
str(results_4)
mercedes_win_2014_2019_by_circuit = results_4 %>% 
  filter(name_constructor == "Mercedes") %>% 
  filter(position == 1) %>% 
  filter(year >=2014) %>% 
  select(year, points, name) %>% 
  group_by(year, name)

str(mercedes_win_2014_2019_by_circuit)
mercedes_win_2014_2019_by_circuit = data.frame(mercedes_win_2014_2019_by_circuit)
mercedes_win_2014_2019_by_circuit %>% spread(year, points)
