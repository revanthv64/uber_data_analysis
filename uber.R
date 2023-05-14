library('tidyverse')  # This package has ggplot2 for visualizations, dplyr for data manipulation and tidyr for tidying the data 
library('ggthemes')   # Extra themes for ggplot2
library('DT')         # Data tables in JS
library('scales')     # Automatically map the data to the correct scales with well-placed axes and legends.
library('lubridate')  # Use time-frames in the data sets

# Creating a vector of colors for th plot
colours = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# Read the data from each time-frame

apr   <- read_csv("data/uber-raw-data-apr14.csv") 
may   <- read_csv("data/uber-raw-data-may14.csv")
june  <- read_csv("data/uber-raw-data-jun14.csv")
july  <- read_csv("data/uber-raw-data-jul14.csv")
aug   <- read_csv("data/uber-raw-data-aug14.csv")
sept  <- read_csv("data/uber-raw-data-sep14.csv")

# Append the data
data_all <-  bind_rows(apr, may, june, july, aug, sept)
 
data_all$`Date/Time`  <- as.POSIXct(data_all$`Date/Time`, format = "%m/%d/%Y %H:%M:%S")
data_all$Time         <- format(as.POSIXct(data_all$`Date/Time`, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")
data_all$`Date/Time`  <- ymd_hms(data_all$`Date/Time`)
 
# Create individual columns for month, day and year
data_all$day        <- factor(day(data_all$`Date/Time`))
data_all$month      <- factor(month(data_all$`Date/Time`, label = T))
data_all$year       <- factor(year(data_all$`Date/Time`))
data_all$dayofweek  <- factor(wday(data_all$`Date/Time`, label = T))

# Create individual columns for hour, minute and second
data_all$second <- factor(second(hms(data_all$Time)))
data_all$minute <- factor(minute(hms(data_all$Time)))
data_all$hour <- factor(hour(hms(data_all$Time)))

# Aggregate by hour
hourly_data <- data_all |> group_by(hour) |> summarise(Total = n())

# Plot the data by hour
 ggplot(hourly_data, aes(x = hour, y = Total)) +                          # creates x-axis and y-axis with values
   geom_bar(stat = "identity",
            fill = "steelblue",
             color = "red" ) +                                            # makes a bar graph with  sum of values in a particular variable (stat) ,bar graph colour blue and outline color red
ggtitle("Trips Every Hour", subtitle = "aggregated today") +              # adds title and subtitle to graph
theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
       plot.subtitle = element_text(hjust = 0.5)) +                       # places title and subtitle in the center of the page
scale_y_continuous(labels = comma)                                        # makes y-axis numbers into whole numbers from exponential 

# Aggregate the data by month by month and hour
 month_hour_data <- data_all |> group_by(month,hour) |> summarise(Total = n())

# Plot trips by month and hour
 ggplot(month_hour_data, aes(x = hour, y = Total, fill = month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# Aggregate data by day of the month
 day_data <-  data_all |> group_by(day) |> summarise(Trips = n())
 
# Plot the data for the day
 ggplot(day_data, aes(day, Trips)) +
  geom_bar(stat = "identity",  fill = "steelblue") +
  ggtitle("Trips by day of the month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
 
# Collect data by day of the week and month
 day_month_data <- data_all |> group_by(dayofweek, month) |> summarise(Trips = n())
 
# Plot the above data
 ggplot(day_month_data, aes(dayofweek, Trips, fill =  month)) +
 geom_bar(stat = "identity", aes(fill = month), position = "dodge") + # dodge is to make split bar graph 
 ggtitle("Trips by day and month") + 
 scale_y_continuous(label = comma) +
 scale_fill_manual(values = colours) #Takes colours from the preset color list we created in the start   
 
# Trips during each month
 month_data <- data_all |> group_by(month) |> summarise(Total = n())
 
# Plot the above data
 ggplot(month_data, aes(month, Total, fill = month)) +
 ggtitle("Trips in a month") +
 theme(legend.position = "none") +
 scale_y_continuous(labels = comma) +
 scale_fill_manual(values = colours)   
 
 # Heat-map  by hour and day
day_hour_data <- data_all |> group_by(day, hour) |> summarise(Total = n())

# Plot above data with heat-map
ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
geom_tile(color = "white") +
ggtitle("Heatmap by Hour and Day")

# Heat-map by Day and Month
month_day_data <- data_all |> group_by(month, day) |> summarise(Trips = n())

# Plot the above data with a heat-map
ggplot(month_day_data, aes(day, month, fill = Trips)) +
geom_tile(color = "white") +
ggtitle("Heatmap by Month and Day")  

# Plot data with day of the week and month
ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) +
geom_tile(color= "white") +
ggtitle("Heat map by month and day")
 
