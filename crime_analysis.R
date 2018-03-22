library(ggplot2)
library(dplyr)
library(gganimate)
library(ggmap)
library(gridExtra)
library(broom)

sf_crime <- read.csv(file = "~/Desktop/sf_crime/Police_Department_Incidents.csv")

length(unique(sf_crime$Date))

year_2017 <- sf_crime$Year == 17
sf_crime_2017 <- sf_crime[year_2017,]

# TODO:
# General Mon-Fri Distributions
day_order <- c('Sunday', 'Saturday', 'Friday', 'Thursday', 'Wednesday', 'Tuesday', 'Monday')

day_tbl <- sf_crime_2017 %>%
  group_by(DayOfWeek) %>%
  summarise(n())

colnames(day_tbl) <- c('DayOfWeek', 'Count')
day_tbl$DayOfWeek <- factor(day_tbl$DayOfWeek, levels = day_order)
day_tbl[order(day_tbl$DayOfWeek), ]

mean(day_tbl$Count) # 22,010.57
sd(day_tbl$Count) # 839.4702

color_scheme <- c("#09C2FF", "#004E68", "#09C2FF", "#004E68", "#09C2FF", "#004E68", "#09C2FF")

day_plot <- ggplot(data = day_tbl, aes(x = DayOfWeek, y = Count)) +
  geom_bar(stat = "identity", fill = color_scheme) + 
  coord_flip()

# There seems to be a bias particularly towards Friday 
# Any difference between the types of crimes committed on a Friday relative to other days?
day_crime_tbl <- sf_crime_2017 %>%
  group_by(DayOfWeek, Category) %>%
  summarise(n())
colnames(day_crime_tbl) <- c("Category", "DayOfWeek", "Count")

day_crime_tbl$Category <- factor(day_crime_tbl$Category, levels = day_crime_tbl$Category[order(-day_crime_tbl$Count)])
day_crime_tbl[order(day_crime_tbl$Category),]

monday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Monday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[1]) + 
  ylim(0, 8000) + ggtitle('Monday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

tuesday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Tuesday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[1]) +
  ylim(0, 8000) + ggtitle('Tuesday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

wednesday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Wednesday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[1]) +
  ylim(0, 8000) + ggtitle('Wednesday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

thursday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Thursday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[1]) +
  ylim(0, 8000) + ggtitle('Thursday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

friday <-ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Friday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[2]) + 
  ylim(0, 8000) + ggtitle('Friday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

saturday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Saturday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[2]) +
  ylim(0, 8000) + ggtitle('Saturday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

sunday <- ggplot(data = sf_crime_2017[sf_crime_2017$DayOfWeek == 'Sunday',], aes(x = Category)) +
  geom_bar(fill = color_scheme[1]) +
  ylim(0, 8000) + ggtitle('Sunday') + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1))

# Larceny/Theft stand out on Saturdays and Sundays (1000+ more during each of those two days)
# Deeper look at Larceny/Theft:
theft_2017 <- sf_crime_2017[sf_crime_2017$Category == "LARCENY/THEFT",]
rb_classifier <- function(description) {
  if (any(grep('.*auto.*|.*vehicle.*', description, ignore.case = TRUE))) {
    'Vehicle'
    }
  else if (any(grep('.*person.*|.*pickpocket.*|.*property.*', description, ignore.case = TRUE))) {
    'Person'
    }
  else if (any(grep('.*building.*', description, ignore.case = TRUE))) {
    'Building'
  }
  else ('Other')
}
theft_type <- as.vector(sapply((theft_2017$Descript), rb_classifier))
theft_2017$theftType <- theft_type
theft_2017$theftType <- as.factor(theft_2017$theftType)

# Is there a statistically significant difference between Fridays and Saturdays 
# and the rest of the week?
# Poisson Test
theft_on_friday_2 <- theft_2017 %>%
  mutate(total_thefts = n()) %>%
  ungroup()
poisson_data_2 <- theft_on_friday_2 %>%
  group_by(DayOfWeek, theftType, total_thefts) %>% 
  summarize(theft_types = n()) %>%
  ungroup()

poisson_refined_2 <- poisson_data_2[poisson_data_2$DayOfWeek=='Friday' | poisson_data_2$DayOfWeek=='Saturday',]

poisson_refined_final_2 <- poisson_refined_2 %>%
  group_by(theftType) %>%
  do(tidy(poisson.test(.$theft_types, .$total_thefts, alternative = "greater", conf.level=0.95)))

poisson_refined_final_2$theftType <- factor(poisson_refined_final_2$theftType, levels = poisson_refined_final_2$theftType[order(poisson_refined_final_2$estimate)])
poisson_refined_final_2[order(poisson_refined_final_2$theftType),]

# Visualization of Poisson one-sided confidence intervals
ci_visualization <- ggplot(poisson_refined_final_2, aes(x = (estimate - 1) * 100, y = theftType)) + 
  geom_errorbarh(color = "#09C2FF", aes(xmax = (estimate - 1) * 100, xmin = (conf.low - 1) * 100, y = theftType, height = 0.1)) + 
  ylab("Theft Type") + xlab("% Increase in Theft on Friday and Saturday")

# Most common crimes per district (Map)
most_common_per_district <- sf_crime_2017 %>%
  group_by(PdDistrict, Category) %>%
  summarize(CategoryCount = n())
most_common_per_district_2 <- most_common_per_district %>%
  summarize(MostCommon = max(CategoryCount))
most_common <- merge(most_common_per_district, most_common_per_district_2)
most_common[most_common$CategoryCount == most_common$MostCommon,]

# ---------------------------------- NOT IMPLEMENTED -------------------------------------------#
# Map parameters
GC <- as.numeric(geocode('San Francisco', source = 'google'))
ZOOM <- 12
MAPTYPE <- "roadmap"
COLOR <- "bw"
STYLE <- c(feature = "all", element = "labels", visibility = "off")

# Map with parameters 
map <- get_googlemap(center = GC, zoom = ZOOM, maptype = MAPTYPE, color = COLOR,
  style = STYLE)
map_data <- sf_crime_2017[sf_crime_2017$Category == "Larceny/Theft",]
map_plt <- ggmap(map) 
map_plt +
  geom_point(aes(x = X, y = Y, color = "#004E68", alpha = 0.01, size = 0.01), 
    data = sf_crime_2017[sf_crime_2017$Category == "LARCENY/THEFT",])
#---------------------------------------------------------------------------------------------#

# General Overview of Year (is seasonality something to consider? I would think so)
timeseries_2017 <- sf_crime_2017 %>%
  group_by(Date) %>%
  summarize(CrimesPerDay = n())



timeseries_plot <- ggplot(timeseries_2017, aes(as.Date(Date, format = "%m/%d/%Y"), CrimesPerDay, color = "#004E68")) + 
  geom_line(color = "#004E68") + 
  scale_x_date() + xlab("Date") + ylab("Crimes Per Day")

# March 17, 2017: St. Patrick's Day (obvious one)
# June 25, 2017: Crime spikes to annual high (557) day of Pride (maybe not because of the protests per se but
# because there's greater opportunity with so many people flocking to the city)
# Dec 25, 2017: Crime drops to annual low (260)
