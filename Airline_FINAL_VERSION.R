# Project: Airline Satisfaction
# Group 2, TTh Class
# by: Shalini Chandra, Vrushti Khajanchi, Yojita Lalpuria, Joy Milan, and Prachi Patel


#import libraries
library(janitor)
library(tidyverse)
library(corrplot)
library(dplyr)
library(dendextend)
library(factoextra)
library(scatterplot3d)
library(tidyr)
library(stats)
library(stargazer)
library(lubridate)
library(ggplot2)
library(caret)
library(Metrics)
library(knitr)

###########################################
# 1. PROBLEM STATEMENT: 
#The goal of this study is to help airline companies improve customer satisfaction 
#and their own performance by analyzing the factors that affect airline customer satisfaction. 


###########################################
# 2. DATA PREPARATION AND CLEANING

# Import file AirlineSatisfaction and make a copy of the dataframe
df <- Airline_Satisfaction_Data
class(df)
names(df)
df <- clean_names(df)
names(df)

# Shape of Dataframe
str(df)
dim(df)

# What does each row represent?
#Each row is represented by each customer. This is also represented by column "customer_id".

# Change data type to factor
str(df)
df[,c(2,3,5,6,9,11,12,14,16,19,20)]<-lapply(df[,c(2,3,5,6,9,11,12,14,16,19,20)],factor)
str(df)
summary(df)
# DEALING WITH NAs

# Checking for NAs
sum(is.na(df))
colSums(is.na(df))
#NAs from columns: flight_time (418), arrival_delay(418), departure_delay (379)
#There are 2 options on how to deal with NAs: drop NA or fill NA with a value. 
#Let's look at both approaches below.

# Option1: Drop NAs
df_drop<-df
df_drop<-na.omit(df_drop)
colSums(is.na(df_drop))
str(df_drop)


# Let's view these columns
df_with_na<-df[which(rowSums(is.na(df)) > 0), c("satisfaction", "airline_name","flight_cancelled",
                                                "flight_time", "arrival_delay", "departure_delay")]
colSums(is.na(df_with_na))
#From the table of df_with_na, the NAs line up in the same rows/customer.
#Total number of rows/customers with NAs is 418 (3.9% of the dataset). 
#This is a small number so we can potentially drop NAs.
#Notice that the column "flight_cancelled" are mostly "Yes", this indicates that the NAs are not missing values
#but means "None" or cancelled flight, therefore we should fill the NAs with a value.
#Also, the "satisfaction" column is showing mostly "Not satisfied" so this might impact the accuracy of the model.


# Option2: Fill NAs
str(df_with_na)
#the data types of the columns with NAs are numeric. For numerical data, we usually replace NA with mean or median
#so the distribution will not change.

#Look at descriptive statistics
df %>% select(flight_time, arrival_delay, departure_delay) %>% summary()
#Looking at the mean and median, for arrival_delay and departure_delay, the median is zero. 
#Given that most of the value in "satisfaction" is "Not satisfied", we should not fill NA with median.

#We will fill it with the mean instead.
df$flight_time[is.na(df$flight_time)] <- mean(df$flight_time, na.rm = TRUE)
df$arrival_delay[is.na(df$arrival_delay)] <- mean(df$arrival_delay, na.rm = TRUE)
df$departure_delay[is.na(df$departure_delay)] <- mean(df$departure_delay, na.rm = TRUE)

#Check if there are still NAs
sum(is.na(df))
dim(df)

###########################################
# 3. EXPLORATORY DATA ANALYSIS (EDA)

# 3.1 EXPLORE NUMERICAL VARIABLES

# Split df to df_num and df_cat
str(df)
df_num<-df[,c(1,4,7,8,10,13,15,17,18,21,22)]
str(df_num)
# customer_id: drop column
# age: numeric, customer's age
# shopping_amount_at_airport: numeric, $ amount spent by customer while shopping
# eating_and_drinking_amounts_at_airport: numeric, $ amount spent by customer eating and drinking
# flight_date: POSIXct, format: "YYYY-MM-DD", create new column day_of_week that refers to Mon, Tues, etc and convert to Factor
# no_of_flights: numeric, number of previous flights by customer
# flight_time: numeric, Number of minutes from origin to destination
# arrival_delay: numeric, minutes delayed for arrival
# departure_delay: numeric, minutes delayed from for departure
# scheduled_departure_hour: numeric, the hour flight is scheduled for departure in military time
# flight_distance: numeric, flight distance in miles


# Create a copy of df and remove customer_id
df2<-df
df2<-subset(df2, select = -customer_id)
#From here on, we will use df2. df will be intact with the original columns

# DO WE NEED ALL THE NUMERICAL COLUMNS? 
#To answer this, we will look at the correlation matrix and then visualize each column 
#and make assessments on whether the column will be used or dropped.


df2_num<-df2[,c(3,6,7,12,14,16,17,20,21)]
str(df2_num)
summary(df2_num)

# Create correlation matrix
cor_matrix<-cor(df2_num)
corrplot(cor_matrix, type = 'upper', method = 'color', 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         tl.cex = 0.6, number.cex = 0.6)

#flight_time and flight_distance are highly correlated (0.94) -> Keep 1, drop the other
#arrival delay and departure delay are highly correlated (0.96) -> Keep 1, drop the other
#all other variables have correlation coefficients less than 0.5 so we can keep those.

#Create pairplot for selected columns (need to create df for selected columns first)
selected_cols <- c("flight_time", "flight_distance", "arrival_delay", "departure_delay","satisfaction")
df_selected <- data.frame(df2[selected_cols])
df_selected$satisfaction <- factor(df_selected$satisfaction)


# Create probability density chart for flight_time and flight_distance to determine which one to keep and drop
ggplot(data = df2, aes(x = flight_time, fill = satisfaction)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Flight Time by Satisfaction", x = "Flight Time", y = "Density") +
  scale_fill_manual(values = c("darkturquoise", "salmon"), labels = c("Satisfied", "Not Satisfied"))

df_flight<-subset(df2, select = c(flight_time, satisfaction, airline_name))
#df_flight_mean <- df_flight %>%
#  summarise(mean_fligttime = mean(flight_time))

mean_flighttime = mean(df_flight$flight_time)
df_flight$Hi_Lo <- ifelse(df_flight$flight_time >= mean_flighttime, 1, 0)
#High Mean = 1, Low Mean = 0
#Mutate df2 to group by gender and satisfaction
df_flight_agg <- df_flight %>%
  group_by(Hi_Lo, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of gender vs satisfaction
ggplot(data = df_flight_agg, aes(x = Hi_Lo, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Hi_Lo Flight Time", x = "Flight Time Hi_Lo", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))


#df_flight_mean <- df_flight %>%
#  group_by(satisfaction) %>%
#  summarise(mean_flight = mean(no_of_flights))
# Create a bar chart of mean no_of_flights by satisfaction
#ggplot(data = df2_mean_flights, aes(x = satisfaction, y = mean_flight, fill = satisfaction)) +
#  geom_col() +
#  geom_text(aes(label = round(mean_flight, 0)), 
#            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
#  labs(title = "Mean Number of Flights by Satisfaction", x = "Satisfaction", y = "Mean Number of Flights") +
#  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied")) +
#  scale_x_discrete(labels = c("Not Satisfied","Satisfied"))


  
ggplot(data = df2, aes(x = flight_distance, fill = satisfaction)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Flight Distance by Satisfaction", x = "Flight Distance", y = "Density") +
  scale_fill_manual(values = c("darkturquoise", "salmon"), labels = c("Satisfied", "Not Satisfied"))
#flight_time [KEEP] There is more separation of satisfied and not satisfied
#flight_distance [DROP] The probability densities of satisfied and not satisfied 
#are overlapping for almost all the points

# Create probability density chart for arrival_delay and departure_delay to determine which one to keep and drop
ggplot(data = df2, aes(x = arrival_delay, fill = satisfaction)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Flight Time by Satisfaction", x = "Arrival Delay", y = "Density") +
  scale_fill_manual(values = c("darkturquoise", "salmon"), labels = c("Satisfied", "Not Satisfied")) +
  ylim(0, 0.015) + 
  xlim(0,250)

ggplot(data = df2, aes(x = departure_delay, fill = satisfaction)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Flight Distance by Satisfaction", x = "Departure Delay", y = "Density") +
  scale_fill_manual(values = c("darkturquoise", "salmon"), labels = c("Satisfied", "Not Satisfied")) +
  ylim(0, 0.015) + 
  xlim(0,250)
#both probability density charts are showing that satisfied and not satisfied are almost overlapping. 
#arrival_delay [KEEP] There is a little bit of separation in satisfaction vs departure_delay. 
#The value of the probability are a little bit higher vs departure_delay.
#departure_delay [DROP]


# Let's visualize each column and further assess if we will keep or drop it.
#age: [KEEP] Age can impact satisfaction levels,  Younger people like gen-Z and millennials might find 
#traditional airline services to be outdated, while older people might face difficulties using new technology
#and have higher expectations. Middle-aged people have more experience and realistic expectations,
#leading to higher satisfaction levels. The density chart supports the idea that customers in the middle
#range are more satisfied, while younger and older customers tend to be less satisfied. 

#Create a density chart of age by satisfaction
ggplot(data = df2, aes(x = age, fill = satisfaction)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Satisfaction", x = "Age", y = "Density") +
  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied"))


#shopping_amount_at_airport: [DROP] 
#There is not much difference on the mean of shopping amount
# Calculate mean shopping amount by satisfaction
df2_mean_shopping <- df2 %>%
  group_by(satisfaction) %>%
  summarise(mean_shopping = mean(shopping_amount_at_airport))
# Create a bar chart of mean shopping amount by satisfaction
ggplot(data = df2_mean_shopping, aes(x = satisfaction, y = mean_shopping, fill = satisfaction)) +
  geom_col() +
  geom_text(aes(label = paste0("$", round(mean_shopping, 0))), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Mean Shopping Amount by Satisfaction", x = "Satisfaction", y = "Mean Shopping Amount") +
  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied")) +
  scale_x_discrete(labels = c("Not Satisfied","Satisfied"))



#eating_and_drinking_amounts_at_airport: [DROP] 
# Calculate mean eating_and_drinking_amounts_at_airport by satisfaction
df2_mean_drinking <- df2 %>%
  group_by(satisfaction) %>%
  summarise(mean_drinking = mean(eating_and_drinking_amounts_at_airport))
# Create a bar chart of mean eating_and_drinking_amounts_at_airport by satisfaction
ggplot(data = df2_mean_drinking, aes(x = satisfaction, y = mean_drinking, fill = satisfaction)) +
  geom_col() +
  geom_text(aes(label = paste0("$", round(mean_drinking, 0))), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Mean Eating and Drinking Amount by Satisfaction", x = "Satisfaction", y = "Mean Eating and Drinking Amount") +
  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied")) +
  scale_x_discrete(labels = c("Not Satisfied","Satisfied"))


#no_of_flights: [KEEP] 
#The mean number of flights of not satisfied customers were 24, while the mean number of flights of satisfied customers were 17.
#The difference seem material so we can keep this column.
# Calculate mean no_of_flights by satisfaction
df2_mean_flights <- df2 %>%
  group_by(satisfaction) %>%
  summarise(mean_flight = mean(no_of_flights))
# Create a bar chart of mean no_of_flights by satisfaction
ggplot(data = df2_mean_flights, aes(x = satisfaction, y = mean_flight, fill = satisfaction)) +
  geom_col() +
  geom_text(aes(label = round(mean_flight, 0)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Mean Number of Flights by Satisfaction", x = "Satisfaction", y = "Mean Number of Flights") +
  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied")) +
  scale_x_discrete(labels = c("Not Satisfied","Satisfied"))

#flight_date: [DROP]
#The visuals of satisfied and not satisfied seem the same. we can drop. 
#we will check other variables that are related to fligh_date
ggplot(df2, aes(x = flight_date, fill = satisfaction)) +
  geom_bar(position = "dodge")

df2_satisfied <- df2 %>% filter(satisfaction == "Satisfied")
ggplot(df2_satisfied, aes(x = flight_date)) +
  geom_bar(fill = "darkturquoise", position = "dodge") +
  labs(title = "Number of Satisfied Flights by Date", x = "Flight Date", y = "Count") +
  scale_y_continuous(labels = scales::comma)

df2_notsatisfied <- df2 %>% filter(satisfaction == "Not Satisfied")
ggplot(df2_notsatisfied, aes(x = flight_date)) +
  geom_bar(fill = "salmon", position = "dodge") +
  labs(title = "Number of Not Satisfied Flights by Date", x = "Flight Date", y = "Count") +
  scale_y_continuous(labels = scales::comma)

#scheduled_departure_hour: [DROP]
#The proportion of Satisfied and Not Satisfied customers are around the same (50%) across all hours.
#Mutate df2 to group by scheduled departure hour and satisfaction
df2_agg_scheddephour <- df2 %>%
  group_by(scheduled_departure_hour, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_scheddephour, aes(x = scheduled_departure_hour, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Scheduled Departure Hour", x = "Scheduled Departure Hour", y = "Count") +
  scale_fill_manual(values = c("salmon", "darkturquoise"), labels = c("Not Satisfied","Satisfied")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))




# 3.2 EXPLORE CATEGORICAL VARIABLES
str(df2)
df_cat<-df2[,c(1,2,4,5,8,10,11,13,15,18,19)]
str(df_cat)

# Create new column "day_of_week" from existing column "flight_date"
df2$day_of_week<-weekdays(df2$flight_date)
df2$day_of_week <- as.factor(df2$day_of_week)
str(df2$day_of_week)
df_cat<-df2[,c(1,2,4,5,8,10,11,13,15,18,19,22)]
# day_of_week: Factor w/ 7 levels "Friday","Monday", convert to dummy, drop 1 dummy col


# DO WE NEED ALL THE CATEGORICAL COLUMNS? 

#To answer this, we will look at each column and make qualitative and quantitative assessments 
#on whether the column will be used or dropped.

summary(df_cat)

#satisfaction: [KEEP] This is the target variable.

#airline_status: [KEEP] values are Blue,Silver, Gold, and Platinum. 
#Let's say the levels are based on reputation, quality of service, safety, etc., 
#The percentage of not satisfied customers is higher in the Blue category as compared to the other categories. 
#ne common perception is that higher categories customer receives better service and quality as compared to general 
#categories, thus higher customer satisfaction. Therefore, we can conclude that the "airline_status" variable is
#significant for determining satisfaction as it shows a difference in satisfaction levels among different categories.

#Mutate df2 to group by airline_status and satisfaction
df2_agg_airlinestatus <- df2 %>%
  group_by(airline_status, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)

#Create a bar chart of airline_status vs satisfaction
ggplot(data = df2_agg_airlinestatus, aes(x = airline_status, y = count, fill = satisfaction)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Airline Status vs. Satisfaction", x = "Airline Status", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))

#gender: [DROP] although small difference, can drop if the factors affecting gender are in the data set
#Gender may impact customer satisfaction in a way that female passengers may have more concerns
#on safety, security, privacy, cleanliness, etc. Male passengers are generally taller than female, 
#so they might have issues on leg room. Other preferences like shopping, food, drinks, may impact gender. 
#From the given data, we can see that both males and females have a significant percentage of satisfied customers.
#However, the percentage of satisfied males is slightly higher than that of females. but it might not be the most
#significant factor.The values are not that different as they are around to 50% so it can go either way.
#We noted earlier that some preferences that may impact gender are shopping, food, drinks, etc.
#These are available in the dataset so we can drop "Gender".
#Create a bar chart of satisfaction by gender
#Mutate df2 to group by gender and satisfaction
df2_agg_gender <- df2 %>%
  group_by(gender, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of gender vs satisfaction
ggplot(data = df2_agg_gender, aes(x = gender, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Gender", x = "Airline Status", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))

#type_of_travel: [KEEP] 
#Business travelers have the highest satisfaction levels, followed by mileage ticket holders and personal travelers. 
#Airlines provide more amenities to business travelers such as priority boarding, lounge access, and more comfortable
#seating option, leading to higher satisfaction. Personal travelers may have have lower expectations and fewer amenities
#leading to lower satisfaction levels. However, they have different priorities such as affordability and flexibility, 
#which may not be captured in this dataset. Type of travel is a significant variable in predicting satisfaction levels.
#Mutate df2 to group by gender and satisfaction
df2_agg_typetravel <- df2 %>%
  group_by(type_of_travel, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of gender vs satisfaction
ggplot(data = df2_agg_typetravel, aes(x = type_of_travel, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Gender", x = "Type of Travel", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

#class: [DROP] 
#From the chart, there is not much difference from the each category of class. The values are around 50% 
#(ranging from 40% to 60% per satisfaction). Also the category with the highest count is "Eco" 
#(around 8000 customers out of 10,000) has values of 51% for satisfied and 49% for not satisfied. 
#Hence, we will drop this variable
#Create column chart
#Mutate df2 to group by class and satisfaction
df2_agg_class <- df2 %>%
  group_by(class, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_class, aes(x = class, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Class", x = "Class", y = "Count") +
  scale_fill_manual(values =  c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied")) +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

#day_of_week: [DROP] 
#The proportion of Satisfied and Not Satisfied customers are around the same (50%) across all days of the week.
#Mutate df2 to group by class and satisfaction
df2_agg_dayofweek <- df2 %>%
  group_by(day_of_week, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_dayofweek, aes(x = day_of_week, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Day of Week", x = "Day of Week", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5))

#day_of_flight_date: [KEEP] 
#There seems to be more satisfied customers during the middle of the month.
#Mutate df2 to group by class and satisfaction
df2_agg_dayofflight <- df2 %>%
  group_by(day_of_flight_date, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_dayofflight, aes(x = day_of_flight_date, y = count, fill = satisfaction)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +   # Adjust the position and width of the bars
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Day of Flight", x = "Day of Flight Date", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#month_of_flight_date: [DROP]
#Months of Jan to Mar and Nov Dec have high number of customers, 
#but the proportion of satisfied and dissatisfied customers is the same 
#so we can drop this variable. The months with varying proportion of satisfaction
#have very low number of customers so this will most likely not have an impact on satisfaction level.
#It is interesting to see the peak and non-peak months. There is opportunity for the
#airline company to promote the non-peak months. Note that airplanes have fixed capacity, 
#which is different from a manufacturing company where you can play around with
#capacities of each production line for peak and non-peak seasons. 
#Mutate df2 to group by class and satisfaction
df2_agg_monthofflight <- df2 %>%
  group_by(month_of_flight_date, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)

df2_agg_monthofflight$month_of_flight_num <- month(parse_date_time(df2_agg_monthofflight$month_of_flight_date, "b"))

#Create a column chart of month_of_flight_date vs satisfaction
ggplot(data = df2_agg_monthofflight, aes(x = reorder(month_of_flight_date, month_of_flight_num), y = count, fill = satisfaction)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +   # Adjust the position and width of the bars
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Month of Flight", x = "Month of Flight", y = "Count") +
  scale_fill_manual(values =c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#airline_name: [DROP]
#Not much difference between Primera Air and Wow Air. We can drop this variable.
#Mutate df2 to group by airline_name and satisfaction
df2_agg_airlinename <- df2 %>%
  group_by(airline_name, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of airline_name vs satisfaction
ggplot(data = df2_agg_airlinename, aes(x = airline_name, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Airline", x = "Airline", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))

#flight_cancelled: [DROP]
#There is a big imbalance of data, 90+% of the data has flights not cancelled and the difference
#of satisfied vs not satisfied is very minimal (around 50%) so we can drop this variable.
df2_agg_flightcancel <- df2 %>%
  group_by(flight_cancelled, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of airline_name vs satisfaction
ggplot(data = df2_agg_flightcancel, aes(x = flight_cancelled, y = count, fill = satisfaction)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Flight Cancellation", x = "Flight Cancelled", y = "Count") +
  scale_fill_manual(values =c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))

#origin_state: [DROP] 
#The top states are Texas, Illinois, Florida, and New York. The proportion of satisfied 
#and not satisfied is around 50% so we can drop this variable.
#Mutate df2 to group by class and satisfaction
df2_agg_origin <- df2 %>%
  group_by(origin_state, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_origin, aes(x = origin_state, y = count, fill = satisfaction)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +   # Adjust the position and width of the bars
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Day of Origin State", x = "Origin State", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#destination_state: [DROP] 
#The top states are Texas, Illinois, Florida, and New York. The proportion of satisfied 
#and not satisfied is around 50% so we can drop this variable.
#Mutate df2 to group by class and satisfaction
df2_agg_dest <- df2 %>%
  group_by(destination_state, satisfaction) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count) * 100)
#Create a column chart of class vs satisfaction
ggplot(data = df2_agg_dest, aes(x = destination_state, y = count, fill = satisfaction)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +   # Adjust the position and width of the bars
  geom_text(aes(label = paste0(round(percent), "%")), position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "% Satisfaction and Count of Satisfaction by Day of Destination State", x = "Destination State", y = "Count") +
  scale_fill_manual(values = c("salmon","darkturquoise"), labels = c( "Not Satisfied","Satisfied")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


###########################################
# 4. DATA PREPROCESSING

# DROP COLUMNS
#flight_distance [DROP]
#departure_delay [DROP]
#shopping_amount_at_airport: [DROP] 
#eating_and_drinking_amounts_at_airport: [DROP] 
#flight_date: [DROP]
#scheduled_departure_hour: [DROP]
#gender: [DROP]
#class: [DROP]
#day_of_week: [DROP] 
#month_of_flight_date: [DROP]
#airline_name: [DROP]
#flight_cancelled: [DROP]
#origin_state: [DROP] 
#destination_state: [DROP] 
df3 <- subset(df2, select = -c(flight_distance, departure_delay, shopping_amount_at_airport,
                               eating_and_drinking_amounts_at_airport, flight_date,
                               scheduled_departure_hour, gender, class, day_of_week,
                               month_of_flight_date, airline_name, flight_cancelled,
                               origin_state, destination_state))
colnames(df3)
dim(df3)
#For the final dataset, we have a total of 8 variables. 
#The independent variable is satisfaction.
# satisfaction: [Factor w/ 2 levels] convert to binary where 1 is satisfied, and 0 not satisfied

#The remaining independent variables are as follows:
# airline_status: [Factor w/ 4 levels] "Blue","Gold","Silver", "Platinum", convert to dummy and drop 1 dummy col (comparison group)
# age: [Numeric]
# type_of_travel: [Factor] w/ 3 levels "Business travel","Mileage tickets","Personal travel", convert to dummy and drop 1 dummy col
# day_of_flight_date  : [Factor w/ 31 levels] "1","2","3","4", convert to dummy and drop 1 of the dummy col
# no_of_flights [Numeric]
# flight_time [Numeric]
# arrival_delay [Numeric]

#Create new df (mydata) that with dummy conversions
# Create binary for satisfaction, where 1 = Satisfied, 0 = Not Satisfied, drop satisfaction
mydata<-df3
mydata$satisfaction_binary <- ifelse(mydata$satisfaction == "Satisfied", 1, 0)
mydata<-subset(mydata, select = -c(satisfaction))


# Create dummy variables and drop 1 of the columns(comparison group) for airline_status, type_of_travel, day_of_flight_date
# View unique values for multiple columns
lapply(mydata[, c("airline_status", "type_of_travel", "day_of_flight_date")], unique)

# Create dummy variables for airline_status, type_of_travel, and day_of_flight_date
mydata <- cbind(mydata, model.matrix(~ airline_status + type_of_travel + 
                                       day_of_flight_date - 1, data = mydata))

#the matrix model will automatically drop 1 column as comparison group. 
#type_of_travelBusiness.travel and day_of_flight_date1 were dropped.
#none was dropped from airline_status so we will drop one

#Drop one column from airline status as the comparison group. Drop original columns too.
summary(mydata$airline_status)
#For the comparison group, this is arbitrary, but a common approach is to use the most frequent category
#or most conceptually similar to other categories. Hence, we dropped airline_statusBlue (comparison group)
mydata <- subset(mydata, select = -c(airline_statusBlue, airline_status,
                                     type_of_travel, day_of_flight_date))


colnames(mydata)
mydata <- clean_names(mydata)
colnames(mydata)
str(mydata)

# Check Numeric data that are integers if they are discrete or continuous
unique(mydata$no_of_flights)
summary(mydata$no_of_flights)
summary(mydata$age)
#We can see that there is a wide range of values for both age and no_of_flights, 
#so we can consider this as continuous.

# Check normal distribution of non-binary variables

#library(psych)
#describe(mydata[c("age", "no_of_flights", "flight_time", "arrival_delay")])
#arrival delay has high skewness, we might need to normalize

#library(moments)
#skewness(mydata$age)
#skewness(mydata$no_of_flights)
#skewness(mydata$flight_time)
#skewness(mydata$arrival_delay)




# FINAL LOOK BEFORE PROCEEEDING TO REGRESSION MODELS

# Create correlation matrix, show only a subset of variables with correlation value >= 0.15 or <= -0.15
mydata_corr_subset <- mydata[, which(apply(mydata, 2, 
                                           function(x) abs(cor(x, mydata$satisfaction_binary))) >= 0.15)]
mydata_corr_subset <- cor(mydata_corr_subset)
corrplot(mydata_corr_subset, type = 'upper', method = 'color', 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         tl.cex = 0.6, number.cex = 0.8)
# We can see that the 

# Create a table of correlation for the top 5 variables that are highly correlated to "satisfaction_binary"
corr_mydata <- cor(mydata)
corr_satisfaction <- corr_mydata["satisfaction_binary",]
corr_satisfaction <- corr_satisfaction[order(abs(corr_satisfaction), decreasing = TRUE)]
top_correlations <- corr_satisfaction[1:6]
top_correlations_table <- data.frame(variable = names(top_correlations),
                                     correlation = round(top_correlations, 2))
top_correlations_table <- subset(top_correlations_table, variable != "satisfaction_binary")
top_correlations_table

#These are the top correlation
#type_of_travel_personal_travel -0.54
#no_of_flights                  -0.24
#airline_status_silver           0.24
#age                            -0.21
#arrival_delay                  -0.08


# SPLIT DATA SET: training, validation, and test samples
set.seed(123)
Samples<-sample(seq(1,3),size=nrow(mydata),replace=TRUE,prob=c(0.8,0.2,0.2))

table(Samples)
Samples
Train_data<-mydata[Samples==1,]  # comma then blank means all columns
Validation_data<-mydata[Samples==2,]
Test_data<-mydata[Samples==3,]

# Scale data sets of continuous variables
Train_data_scaled<-Train_data
Train_data_scaled$age_scaled<-scale(Train_data_scaled$age)
Train_data_scaled$no_of_flights_scaled<-scale(Train_data_scaled$no_of_flights)
Train_data_scaled$flight_time_scaled<-scale(Train_data_scaled$flight_time)
Train_data_scaled$arrival_delay_scaled<-scale(Train_data_scaled$arrival_delay)

Validation_data_scaled<-Validation_data
Validation_data_scaled$age_scaled<-scale(Validation_data_scaled$age)
Validation_data_scaled$no_of_flights_scaled<-scale(Validation_data_scaled$no_of_flights)
Validation_data_scaled$flight_time_scaled<-scale(Validation_data_scaled$flight_time)
Validation_data_scaled$arrival_delay_scaled<-scale(Validation_data_scaled$arrival_delay)

Test_data_scaled<-Test_data
Test_data_scaled$age_scaled<-scale(Test_data_scaled$age)
Test_data_scaled$no_of_flights_scaled<-scale(Test_data_scaled$no_of_flights)
Test_data_scaled$flight_time_scaled<-scale(Test_data_scaled$flight_time)
Test_data_scaled$arrival_delay_scaled<-scale(Test_data_scaled$arrival_delay)

colnames(Train_data_scaled)

# Drop original continuous variables that are unscaled
Train_data_scaled<-subset(Train_data_scaled, select = -c(age, no_of_flights, flight_time, arrival_delay))
Validation_data_scaled<-subset(Validation_data_scaled, select = -c(age, no_of_flights, flight_time, arrival_delay))
Test_data_scaled<-subset(Test_data_scaled, select = -c(age, no_of_flights, flight_time, arrival_delay))

colnames(Train_data_scaled)
colnames(Validation_data_scaled)
colnames(Test_data_scaled)

###########################################
# 5. LOGISTIC REGRESSION - UNSCALED
#The target variable is binary so we will use Logistic Regression.

# Create model using all variables
mylogit_unscaled<-glm(satisfaction_binary ~ .,
                      data=Train_data,family='binomial')

summary(mylogit_unscaled)
exp(coef(mylogit_unscaled))
#####insert interpretation of model using coef
# Check diagnostics
AIC(mylogit_unscaled)
BIC(mylogit_unscaled)

# Plot model
#plot(mylogit_unscaled)


# Probabilites
newdata_unscaled<-Train_data
probabilities_unscaled<-predict(mylogit_unscaled, newdata_unscaled,type='response')
threshold<-0.5
predicted_class_unscaled<-ifelse(probabilities_unscaled >= threshold, 1, 0)

# Create table
prob_table_unscaled<-cbind(newdata_unscaled$satisfaction_binary, probabilities_unscaled,predicted_class_unscaled)
prob_df_unscaled<-data.frame(prob_table_unscaled)
prob_df_unscaled[1:5,]
#probabilites_scaled shows the probability that the customer is satisfied = 1


# Repeat using only statistically significant variables
# Create model
Train_mylogit_unscaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                              type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                              day_of_flight_date15+age + no_of_flights+flight_time+arrival_delay,
                            data=Train_data, family='binomial')
summary(Train_mylogit_unscaled)
exp(coef(Train_mylogit_unscaled))
AIC(Train_mylogit_unscaled)
BIC(Train_mylogit_unscaled)

# Plot model
#plot(Train_mylogit_unscaled)

# Probabilites
Train_newdata_unscaled<-Train_data
Train_probabilities_unscaled<-predict(Train_mylogit_unscaled, Train_newdata_unscaled,type='response')
Train_predicted_class_unscaled<-ifelse(Train_probabilities_unscaled >= threshold, 1, 0)



#Check against Validation set
Validation_mylogit_unscaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                                   type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                                   day_of_flight_date15+age + no_of_flights+flight_time+arrival_delay,
                                 data=Validation_data, family='binomial')
summary(Validation_mylogit_unscaled)
exp(coef(Validation_mylogit_unscaled))
AIC(Validation_mylogit_unscaled)
BIC(Validation_mylogit_unscaled)

#plot(Validation_mylogit_unscaled)

Validation_newdata_unscaled<-Validation_data 
Validation_probabilities_unscaled<-predict(Validation_mylogit_unscaled, Validation_newdata_unscaled,type='response')
Validation_predicted_class_unscaled<-ifelse(Validation_probabilities_unscaled >= threshold, 1, 0)

#Check against Test set
Test_mylogit_unscaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                             type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                             day_of_flight_date15+age + no_of_flights+flight_time+arrival_delay,
                           data=Test_data, family='binomial')
summary(Test_mylogit_unscaled)
exp(coef(Test_mylogit_unscaled))
AIC(Test_mylogit_unscaled)
BIC(Test_mylogit_unscaled)

#plot(Test_mylogit_unscaled)

Test_newdata_unscaled<-Test_data
Test_probabilities_unscaled<-predict(Test_mylogit_unscaled, Test_newdata_unscaled,type='response')
Test_predicted_class_unscaled<-ifelse(Test_probabilities_unscaled >= threshold, 1, 0)


###########################################
# 6. LOGISTIC REGRESSION - SCALED
#The target variable is binary so we will use Logistic Regression.

# Create model using all variables
mylogit_scaled<-glm(satisfaction_binary ~ .,
                    data=Train_data_scaled,family='binomial')
summary(mylogit_scaled)
exp(coef(mylogit_scaled))

# Check diagnostics
AIC(mylogit_scaled)
BIC(mylogit_scaled)

# Plot model
#plot(mylogit_scaled)

# Probabilites
newdata_scaled<-Train_data_scaled  
probabilities_scaled<-predict(mylogit_scaled, newdata_scaled,type='response')
predicted_class_scaled<-ifelse(probabilities_scaled >= threshold, 1, 0)

# Create table
prob_table_scaled<-cbind(newdata_scaled$satisfaction_binary, probabilities_scaled,predicted_class_scaled)
prob_df_scaled<-data.frame(prob_table_scaled)
prob_df_scaled[1:5,]
#probabilites_scaled shows the probability that the customer is satisfied = 1


# Repeat using only statistically significant variables
# Create model
Train_mylogit_scaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                            type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                            day_of_flight_date15+age_scaled + no_of_flights_scaled+flight_time_scaled+arrival_delay_scaled,
                          data=Train_data_scaled, family='binomial')
summary(Train_mylogit_scaled)
coef(Train_mylogit_scaled)
exp(coef(Train_mylogit_scaled))
AIC(Train_mylogit_scaled)
BIC(Train_mylogit_scaled)

# Plot model
#plot(Train_mylogit_scaled)

# Probabilites
Train_newdata_scaled<-Train_data_scaled 
Train_probabilities_scaled<-predict(Train_mylogit_scaled, Train_newdata_scaled,type='response')
Train_predicted_class_scaled<-ifelse(Train_probabilities_scaled >= threshold, 1, 0)

# calculate confusion matrix and classification scores
Train_data_scaled$satisfaction_binary<-as.factor(Train_data_scaled$satisfaction_binary)
Train_predicted_class_scaled<-as.factor(Train_predicted_class_scaled)
Train_conf_mat<-confusionMatrix(Train_data_scaled$satisfaction_binary, 
                                Train_predicted_class_scaled,
                                positive = "1",mode = "prec_recall")
print(Train_conf_mat)
#results discussed in next section in model evaluation

#Check against Validation set
Validation_mylogit_scaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                                 type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                                 day_of_flight_date15+age_scaled + no_of_flights_scaled+flight_time_scaled+arrival_delay_scaled,
                               data=Validation_data_scaled, family='binomial')
summary(Validation_mylogit_scaled)
exp(coef(Validation_mylogit_scaled))
AIC(Validation_mylogit_scaled)
BIC(Validation_mylogit_scaled)

#plot(Validation_mylogit_scaled)

Validation_newdata_scaled<-Validation_data_scaled  #[1:2,]  #first and 2nd row, all columns
Validation_probabilities_scaled<-predict(Validation_mylogit_scaled, Validation_newdata_scaled,type='response')
Validation_predicted_class_scaled<-ifelse(Validation_probabilities_scaled >= threshold, 1, 0)

# calculate confusion matrix and classification scores
Validation_data_scaled$satisfaction_binary<-as.factor(Validation_data_scaled$satisfaction_binary)
Validation_predicted_class_scaled<-as.factor(Validation_predicted_class_scaled)
Validation_conf_mat<-confusionMatrix(Validation_data_scaled$satisfaction_binary, 
                                     Validation_predicted_class_scaled,
                                     positive = "1",mode = "prec_recall")
print(Validation_conf_mat)
#results discussed in next section in model evaluation

#Check against Test set
Test_mylogit_scaled<-glm(satisfaction_binary~airline_status_gold+airline_status_platinum+airline_status_silver+
                           type_of_travel_mileage_tickets +type_of_travel_personal_travel+day_of_flight_date6+
                           day_of_flight_date15+age_scaled + no_of_flights_scaled+flight_time_scaled+arrival_delay_scaled,
                         data=Test_data_scaled, family='binomial')
summary(Test_mylogit_scaled)
exp(coef(Test_mylogit_scaled))
AIC(Test_mylogit_scaled)
BIC(Test_mylogit_scaled)

#plot(Test_mylogit_scaled)

Test_newdata_scaled<-Test_data_scaled  
Test_probabilities_scaled<-predict(Test_mylogit_scaled, Test_newdata_scaled,type='response')
Test_predicted_class_scaled<-ifelse(Test_probabilities_scaled >= threshold, 1, 0)

# calculate confusion matrix and classification scores
Test_data_scaled$satisfaction_binary<-as.factor(Test_data_scaled$satisfaction_binary)
Test_predicted_class_scaled<-as.factor(Test_predicted_class_scaled)
Test_conf_mat<-confusionMatrix(Test_data_scaled$satisfaction_binary, 
                               Test_predicted_class_scaled,
                               positive = "1",mode = "prec_recall")
print(Test_conf_mat)
#results discussed in next section in model evaluation

###########################################
# 6. LOGISTIC REGRESSION - BACKWARD ELIMINATION

# Create the logistic regression model with all variables
#Recall that the full model was already created: "mylogit_scaled"
mylogit_scaled

# Perform backward elimination with the step function
mylogit_scaled_bwd<-step(mylogit_scaled, direction = "backward")

#Result
#Step:  AIC=6880.85
#satisfaction_binary ~ airline_status_gold + airline_status_platinum + 
#  airline_status_silver + type_of_travel_mileage_tickets + 
#  type_of_travel_personal_travel + day_of_flight_date5 + day_of_flight_date6 + 
#  day_of_flight_date15 + day_of_flight_date27 + age_scaled + 
#  no_of_flights_scaled + flight_time_scaled + arrival_delay_scaled

#Df Deviance    AIC
#<none>                                6852.8 6880.8
#- day_of_flight_date5             1   6855.3 6881.3
#- age_scaled                      1   6857.9 6883.9
#- day_of_flight_date27            1   6858.0 6884.0
#- flight_time_scaled              1   6858.8 6884.8
#- day_of_flight_date15            1   6859.2 6885.2
#- airline_status_platinum         1   6859.6 6885.6
#- day_of_flight_date6             1   6862.2 6888.2
#- type_of_travel_mileage_tickets  1   6872.9 6898.9
#- no_of_flights_scaled            1   6891.3 6917.3
#- airline_status_gold             1   6911.4 6937.4
#- arrival_delay_scaled            1   6915.2 6941.2
#- airline_status_silver           1   7284.6 7310.6
#- type_of_travel_personal_travel  1   8666.3 8692.3


# Use Train_data_scaled
Train_mylogit_scaled_bwd_reduced<-glm(satisfaction_binary ~airline_status_gold + airline_status_platinum + 
                                        airline_status_silver + type_of_travel_mileage_tickets + 
                                        type_of_travel_personal_travel + day_of_flight_date5 + day_of_flight_date6 + 
                                        day_of_flight_date15 + day_of_flight_date27 + age_scaled + 
                                        no_of_flights_scaled + flight_time_scaled + arrival_delay_scaled, 
                                      data=Train_data_scaled,family='binomial')

summary(Train_mylogit_scaled_bwd_reduced)
exp(coef(Train_mylogit_scaled_bwd_reduced))

# Coefficients:
# > exp(coef(mylogit_scaled_bwd_reduced))
# (Intercept)            airline_status_gold        airline_status_platinum 
# 1.57597636                     2.23469823                     1.50073235 
# airline_status_silver type_of_travel_mileage_tickets type_of_travel_personal_travel 
# 5.36256113                     0.64041614                     0.04735646 
# day_of_flight_date5            day_of_flight_date6           day_of_flight_date15 
# 1.31271423                     0.61758032                     1.55255078 
# day_of_flight_date27                     age_scaled           no_of_flights_scaled 
# 1.45138235                     0.92594351                     0.81694949 
# flight_time_scaled           arrival_delay_scaled 
# 0.93035514                     0.77495733 

Train_newdata_scaled_bwd<-Train_data_scaled  
Train_probabilities_scaled_bwd<-predict(Train_mylogit_scaled_bwd_reduced, Train_newdata_scaled_bwd,type='response')
Train_predicted_class_scaled_bwd<-ifelse(Train_probabilities_scaled_bwd >= threshold, 1, 0)

# calculate confusion matrix and classification scores
#Validation_data_scaled$satisfaction_binary<-as.factor(Validation_data_scaled$satisfaction_binary)
Train_predicted_class_scaled_bwd<-as.factor(Train_predicted_class_scaled_bwd)
Train_conf_mat_bwd<-confusionMatrix(Train_data_scaled$satisfaction_binary, 
                                    Train_predicted_class_scaled_bwd,
                                    positive = "1",mode = "prec_recall")
print(Train_conf_mat_bwd)
#results discussed in next section in model evaluation


# Use Validation_data_scaled
Validation_mylogit_scaled_bwd_reduced<-glm(satisfaction_binary ~airline_status_gold + airline_status_platinum + 
                                             airline_status_silver + type_of_travel_mileage_tickets + 
                                             type_of_travel_personal_travel + day_of_flight_date5 + day_of_flight_date6 + 
                                             day_of_flight_date15 + day_of_flight_date27 + age_scaled + 
                                             no_of_flights_scaled + flight_time_scaled + arrival_delay_scaled, 
                                           data=Validation_data_scaled,family='binomial')

summary(Validation_mylogit_scaled_bwd_reduced)
exp(coef(Validation_mylogit_scaled_bwd_reduced))

Validation_newdata_scaled_bwd<-Validation_data_scaled  
Validation_probabilities_scaled_bwd<-predict(Validation_mylogit_scaled_bwd_reduced, Validation_newdata_scaled_bwd,type='response')
Validation_predicted_class_scaled_bwd<-ifelse(Validation_probabilities_scaled_bwd >= threshold, 1, 0)

# calculate confusion matrix and classification scores
#Validation_data_scaled$satisfaction_binary<-as.factor(Validation_data_scaled$satisfaction_binary)
Validation_predicted_class_scaled_bwd<-as.factor(Validation_predicted_class_scaled_bwd)
Validation_conf_mat_bwd<-confusionMatrix(Validation_data_scaled$satisfaction_binary, 
                                         Validation_predicted_class_scaled_bwd,
                                         positive = "1",mode = "prec_recall")
print(Validation_conf_mat_bwd)
#results discussed in next section in model evaluation

# Use Test_data_scaled
Test_mylogit_scaled_bwd_reduced<-glm(satisfaction_binary ~airline_status_gold + airline_status_platinum + 
                                       airline_status_silver + type_of_travel_mileage_tickets + 
                                       type_of_travel_personal_travel + day_of_flight_date5 + day_of_flight_date6 + 
                                       day_of_flight_date15 + day_of_flight_date27 + age_scaled + 
                                       no_of_flights_scaled + flight_time_scaled + arrival_delay_scaled, 
                                     data=Test_data_scaled,family='binomial')

summary(Test_mylogit_scaled_bwd_reduced)
exp(coef(Test_mylogit_scaled_bwd_reduced))

Test_newdata_scaled_bwd<-Test_data_scaled  
Test_probabilities_scaled_bwd<-predict(Test_mylogit_scaled_bwd_reduced, Test_newdata_scaled_bwd,type='response')
Test_predicted_class_scaled_bwd<-ifelse(Test_probabilities_scaled_bwd >= threshold, 1, 0)

# calculate confusion matrix and classification scores
#Test_data_scaled$satisfaction_binary<-as.factor(Test_data_scaled$satisfaction_binary)
Test_predicted_class_scaled_bwd<-as.factor(Test_predicted_class_scaled_bwd)
Test_conf_mat_bwd<-confusionMatrix(Test_data_scaled$satisfaction_binary, 
                                   Test_predicted_class_scaled_bwd,
                                   positive = "1",mode = "prec_recall")
print(Test_conf_mat_bwd)
#results discussed in next section in model evaluation



###########################################
# 7. MODEL EVALUATION

#Results of Logistic Regression of Selected Variables for unscaled and scaled are the same.
#Therefore, we can choose either of them. 
#Here, we are focusing on evaluation Selected Variables(scaled) and Backward Elimination.

# COMPARE AIC and BIC

# Summarize AIC and BIC results in 1 table
results_Logit<-data.frame(MODEL = 'Logistic Regression (All variables,Train)',
                          AIC = AIC(mylogit_scaled),
                          BIC = BIC(mylogit_scaled))
new_row2<-data.frame(MODEL = 'Logistic Regression - SelectedVar (Train)',
                     AIC = AIC(Train_mylogit_scaled),
                     BIC = BIC(Train_mylogit_scaled))
new_row4<-data.frame(MODEL = 'Logistic Regression - SelectedVar (Validation)',
                     AIC = AIC(Validation_mylogit_scaled),
                     BIC = BIC(Validation_mylogit_scaled))
new_row6<-data.frame(MODEL = 'Logistic Regression - SelectedVar (Test)',
                     AIC = AIC(Test_mylogit_scaled),
                     BIC = BIC(Test_mylogit_scaled))
new_row3<-data.frame(MODEL = 'Logistic Regression-Backward (Train)',
                     AIC = AIC(Train_mylogit_scaled_bwd_reduced),
                     BIC = BIC(Train_mylogit_scaled_bwd_reduced))
new_row5<-data.frame(MODEL = 'Logistic Regression-Backward (Validation)',
                     AIC = AIC(Validation_mylogit_scaled_bwd_reduced),
                     BIC = BIC(Validation_mylogit_scaled_bwd_reduced))
new_row7<-data.frame(MODEL = 'Logistic Regression-Backward (Test)',
                     AIC = AIC(Test_mylogit_scaled_bwd_reduced),
                     BIC = BIC(Test_mylogit_scaled_bwd_reduced))


results_Logit<-rbind(results_Logit,new_row2,new_row3,new_row4,new_row5,new_row6,new_row7)
results_Logit

# Results
#> results_Logit
#                                          MODEL      AIC      BIC
# 1      Logistic Regression (All variables,Train) 6917.447 7192.313
# 2      Logistic Regression - SelectedVar (Train) 6884.172 6966.632
# 3           Logistic Regression-Backward (Train) 6880.846 6977.049
# 4 Logistic Regression - SelectedVar (Validation) 1641.694 1706.997
# 5      Logistic Regression-Backward (Validation) 1644.356 1720.542
# 6       Logistic Regression - SelectedVar (Test) 1793.528 1859.252
# 7            Logistic Regression-Backward (Test) 1795.358 1872.036

#Looking at Train test:
#the "Backward" model has lower AIC than "Selected Variables"
#the "Selected" model has lower BIC than "Backward"
#the difference is very small

#Looking at Validation test:
#the "Selected" model has lower AIC and BIC than "Backward" but the difference is very small
#From here, we can select "Selected Variables" as the model.

#Looking at Train test:
#the "Selected" model has lower AIC and BIC than "Backward" but the difference is very small
#Validataion and train tests are consistent with "Selected Variables" having better results.
#Note that the difference in the test scores are very small, we can attribute this to randomness.
#Either way, both models seem good.

# COMPARE CLASSIFICATION REPORTS

#Accuracy: fraction of predictions our model got right (TP+TN/TP+TN+FP+FN)
#Precision: proportion of correct positive predictions relative to total positive predictions (TP/TP+FP)
#Recall: proportion of correct positive predictions relative to total actual positives.(TP/TP+FN)
#F1: A weighted harmonic mean of precision and recall. The closer to 1, the better the model; 
#F1 = 2*(Precision * Recall) / (Precision + Recall)
#source: https://www.statology.org/sklearn-classification-report/

# Classification Report - Selected Variables based on statistical significance
print(Train_conf_mat)
print(Validation_conf_mat)
print(Test_conf_mat)

# Classification Report - Backward selection
print(Train_conf_mat_bwd)
print(Validation_conf_mat_bwd)
print(Test_conf_mat_bwd)

# Summarize Classification Report into a table
# Create a list
metrics_list<-list(
  train_selected = list(
    accuracy = 0.7681,
    precision = 0.8985,
    recall = 0.7199,
    f1 = 0.7994
  ),
  train_bwd = list(
    accuracy = 0.769,
    precision = 0.8982,
    recall = 0.7211,
    f1 = 0.8000
  ),
  validation_selected = list(
    accuracy = 0.7608,
    precision = 0.8677,
    recall = 0.7102,
    f1 = 0.7811
  ),
  validation_bwd = list(
    accuracy = 0.7608,
    precision = 0.8665,
    recall = 0.7107,
    f1 = 0.7809
  ),
  test_selected = list(
    accuracy = 0.7538,
    precision = 0.9071,
    recall = 0.7065,
    f1 = 0.7943
  ),
  test_bwd = list(
    accuracy = 0.7538,
    precision = 0.9028,
    recall = 0.7079,
    f1 = 0.7935
  )
)

# Convert list to matrix
metrics_matrix<-matrix(unlist(metrics_list),nrow = 6, byrow = TRUE)

# Add row and column names to the matrix
row_names<-c("Train - Selected Variables", "Train - Bwd", "Validation - Selected Variables",
             "Validation - Bwd", "Test - Selected Variables","Test - Bwd")
col_names<-c("Accuracy", "Precision", "Recall", "F1")
dimnames(metrics_matrix)<-list(row_names, col_names)
kable(metrics_matrix)

# Summary of Classification Report
#  |                                | Accuracy| Precision| Recall|     F1|
#  |:-------------------------------|--------:|---------:|------:|------:|
#  |Train - Selected Variables      |   0.7681|    0.8985| 0.7199| 0.7994|
#  |Train - Bwd                     |   0.7690|    0.8982| 0.7211| 0.8000|
#  |Validation - Selected Variables |   0.7608|    0.8677| 0.7102| 0.7811|
#  |Validation - Bwd                |   0.7608|    0.8665| 0.7107| 0.7809|
#  |Test - Selected Variables       |   0.7538|    0.9071| 0.7065| 0.7943|
#  |Test - Bwd                      |   0.7538|    0.9028| 0.7079| 0.7935|
#The positive class is 1, which is "Satisfied". Looking at training sets: 
#accuracy: backward selection is slightly higher than selected variables by 0.001.
#precision: results are the same
#recall: backward selection is slightly higher by 0.01
#f1: results are the same

#Look at validation sets to compare which model is better
#all results are the same if we are rounding to 2 decimal places

#Look at validation sets to compare which model is better
#all results are the very similar with slight differences in decimal places
#The precision for "Selected Variables" is higher (90.7%) vs backward (90.3%)

#Based on this, we can pick "Selected Variables" as the model.

# Let's look at the confusion matrix

# Combine the confusion matrices into one table
combined_conf_mat<-rbind(Training = Train_conf_mat$table,
                         Validation = Validation_conf_mat$table,
                         Test = Test_conf_mat$table)
combined_conf_mat<-cbind(Data_Set = c("Training","Training","Validation","Validation","Test","Test"),
                         combined_conf_mat)
print(combined_conf_mat)

# Combine the confusion matrices into one table
combined_conf_mat_bwd<-rbind(Training = Train_conf_mat_bwd$table,
                             Validation = Validation_conf_mat_bwd$table,
                             Test = Test_conf_mat_bwd$table)
combined_conf_mat_bwd<-cbind(Data_Set = c("Training-bwd","Training-bwd","Validation-bwd","Validation-bwd","Test-bwd","Test-bwd"),
                             combined_conf_mat_bwd)
print(combined_conf_mat_bwd)


# Selected Variables              # Backward Selection
# > print(combined_conf_mat)      # > print(combined_conf_mat_bwd)

#      Data_Set     0      1      #     Data_Set         0      1
# 0 "Training"   "2181" "1281"    # 0 "Training-bwd"   "2189" "1273"
# 1 "Training"   "372"  "3293"    # 1 "Training-bwd"   "373"  "3292"

# 0 "Validation" "570"  "297"     # 0 "Validation-bwd" "571"  "296" 
# 1 "Validation" "111"  "728"     # 1 "Validation-bwd" "112"  "727" 

# 0 "Test"       "492"  "349"     # 0 "Test-bwd"       "496"  "345" 
# 1 "Test"       "86"   "840"     # 1 "Test-bwd"       "90"   "836"

#Looking at the classification results above, it is a tie for all sets. Either model will work.

#Overall, we will pick "Selected Variables" on the basis of 
#lower AIC and BIC scores in the validation and test sets. 

###########################################
# 8. INTERPRETATION OF THE FINAL MODEL (SELECTED VARIABLES)

Train_coef_table<-data.frame(
  Feature = names(exp(coef(Train_mylogit_scaled))),
  Coefficient = exp(coef(Train_mylogit_scaled)))
print(Train_coef_table)
#logit(p) = 1.61 + 2.24*airline_status_gold + 1.50*airline_status_platinum + 5.36*airline_status_silver+
#           0.64*type_of_travel_mileage_tickets + 0.04*type_of_travel_personal_travel+
#           0.60*day_of_flight_date6 + 1.52*day_of_flight_date15 + 0.93*age_scaled+
#           0.82*no_of_flights_scaled + 0.93*flight_time_scaled + 0.78*arrival_delay_scaled


#In summary, airline_status (all types) have the highest probabilities.
#Next is day of flight 15


# Classification Report
#  |                                | Accuracy| Precision| Recall|     F1|
#  |:-------------------------------|--------:|---------:|------:|------:|
#  |Train - Selected Variables      |   0.7681|    0.8985| 0.7199| 0.7994|
#  |Validation - Selected Variables |   0.7608|    0.8677| 0.7102| 0.7811|
#  |Test - Selected Variables       |   0.7538|    0.9071| 0.7065| 0.7943|

# Confustion Matrix
# > print(combined_conf_mat)
#     Data_Set     0      1     
# 0 "Training"   "2181" "1281"
# 1 "Training"   "372"  "3293"
# 0 "Validation" "570"  "297" 
# 1 "Validation" "111"  "728" 
# 0 "Test"       "492"  "349" 
# 1 "Test"       "86"   "840" 


#####################


#CLUSTERING
#Here we have made a new df_v( copy of original dataframe) for clustering

df_v<-df

#Checking nan
sum(is.na(df_v))

colSums(is.na(df_v))

df_v2<-na.omit(df_v)
sum(is.na(df_v2))

#cleaning names
df_v2 <- clean_names(df_v2)




#Data prepossesing 
#factoring columns
df_v2[,c(2,3,5,6,9,11,12,14,16,19,20)]<-lapply(df_v2[,c(2,3,5,6,9,11,12,14,16,19,20)],factor)
dim(df_v2)

#Transforming satisfaction column into binary, Satisfied= 1, Not satisfied= 0
df_v2$satisfaction_binary <- ifelse(df_v2$satisfaction == "Satisfied", 1, 0)
df_v2<-subset(df_v2, select = -c(satisfaction))
#df_v2<-subset(df_v2, select = -c(customer_id))

colnames(df_v2)




lapply(df_v2[, c("airline_status", "type_of_travel", "day_of_flight_date", 'airline_name')], unique)

#Transforming gender column into binary, female=1, Male=0
df_v2$gender_binary <- ifelse(df_v2$gender == "Female", 1, 0)

#Transforming airline column into binary, Primera Air=1, NotPrimera Air =0
df_v2$airline_binary <- ifelse(df_v2$airline_name == "Primera Air", 1, 0)

#Converting Categorical columns to Numerical
df_v2$airline_status_num<-sapply(df_v2$airline_status, unclass)

df_v2$traveltype_num<-sapply(df_v2$type_of_travel, unclass)

df_v2$class_num<-sapply(df_v2$class, unclass)

colnames(df_v2)

#New dataset df_v3 for clustering model
df_v3<-df_v2


#we converted all 0's to 0.5 
df_v3$shopping_amount_at_airport <- ifelse(df_v3$shopping_amount_at_airport == "0", 0.5, df_v3$shopping_amount_at_airport)

df_v3$eating_and_drinking_amounts_at_airport <- ifelse(df_v3$eating_and_drinking_amounts_at_airport == "0", 0.5, df_v3$eating_and_drinking_amounts_at_airport)

#Below function is stripping the date format and returning the underlying numerical 
#representation of the dates.
df_v3$month_of_flight_date_num<-sapply(df_v3$month_of_flight_date, unclass)

df_v3$day_of_flight_date<-as.numeric(as.character(df_v3$day_of_flight_date))

df_v3$departure_delay <- ifelse(df_v3$departure_delay == "0", 0.5, df_v3$departure_delay)
df_v3$arrival_delay <- ifelse(df_v3$arrival_delay == "0", 0.5, df_v3$arrival_delay)


df_v3<-subset(df_v3, select = -c(no_of_flights, flight_cancelled, origin_state, destination_state, gender, airline_status, type_of_travel,
                                 class, airline_name, month_of_flight_date, flight_date, eating_and_drinking_amounts_at_airport,
                                 
                                 scheduled_departure_hour, day_of_flight_date))

str(df_v3)

#checking correlation between variables
cor_matrix<-cor(df_v3)
corrplot(cor_matrix, type = 'upper', method = 'color', 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         tl.cex = 0.6, number.cex = 0.6)
# For clustering, we removed some variables such as no_of_flights, scheduled_departure_hour
# because they may not add much value to the satisfaction level  
# to dropped a few categorical columns such as origin_state, destination_state, month_of_flight_date, flight_date to reduce computation complexity. 
# Also, looking at correlation plot we can infer that gender binary, airline status and travel type have a comparatively more significant correlation with satisfaction level than other variables. 
# However, as most of our variables are categorical that have been numerically encoded, it is difficult to find any significant correlation between them

#split dataset with two categories: satisfied and not satisfied

df_split <- split(df_v3, f = df_v3$satisfaction_binary)
df_split_not_satisfied<-data.frame(df_split[1])
df_split_satisfied<-data.frame(df_split[2])

#scaling all numeric columns
scaled_cols <- scale(df_v3[, c("age", "flight_time", 'shopping_amount_at_airport', 'arrival_delay', 'departure_delay',
                               'flight_distance')])

# Replace original columns with scaled columns in the dataframe
df_v3[, c("age", "flight_time", 'shopping_amount_at_airport', 'arrival_delay', 'departure_delay',
          'flight_distance')] <- scaled_cols


#As eating_and_drinking_amounts_at_airport, day of flight date and scheduled departure hour - remove bcz least correlated
#we are using df_clustering dataset which contains all scaled values
df_clustering<-df_v3

#Splitting dataset into satisfied and not satisfied customers.

df_clustering_split <- split(df_clustering, f = df_clustering$satisfaction_binary) 

#Created new data frame for converted orders
df_not_satisfied<-data.frame(df_clustering_split[1])
df_satisfied<-data.frame(df_clustering_split[2])

#In order to understand characteristics of satisfied and not satisfied 
#customers, we have done clustering separately for both sections.

df_satisfied1<-subset(df_satisfied, select = -c(X1.satisfaction_binary))


##########Analysing Satisfied Customers##########

#FIND OPTIMAL NUMBER OF CLUSTERS for satisfied customers

# Find optimal numbers using WSS method
fviz_nbclust(df_satisfied1,kmeans,method="wss")
#Using the Within-Cluster-Sum of Squared Errors (WSS) method, the optimal number of clusters can be 3 or 4.
#It is where the WSS starts to diminish or decrease at a slower rate. This is visible as an "elbow".


# Find optimal numbers using Silhouette method
fviz_nbclust(df_satisfied1,kmeans,method="silhouette")
fviz_nbclust(df_satisfied1,FUN=hcut,method="silhouette")

#According to Silhouette method, the optiomal number of cluster is 2. For the customer base of airline 2 and 3 
#seem more practical therefore we will 
#look at number of clusters: 2 and 3.



#HIERARCHICAL CLUSTERING

#calculate distance matrix-default is euclidean


distance<-dist(df_satisfied1)
df_satisfied1.hcluster<-hclust(distance)
plot(df_satisfied1.hcluster)

#Dendogram for 2 clusters:
dend2<-as.dendrogram(df_satisfied1.hcluster)
dend2<-color_labels(dend2,k=2)
dend2<-color_branches(dend2,k=2)
dend2<-assign_values_to_leaves_nodePar(dend2,10,"pch")
dend2<-assign_values_to_leaves_nodePar(dend2,20,"lab.cex")
plot(dend2)

#Dendogram for 3 clusters:
dend3<-as.dendrogram(df_satisfied1.hcluster)
dend3<-color_labels(dend3,k=3)
dend3<-color_branches(dend3,k=3)
dend3<-assign_values_to_leaves_nodePar(dend3,10,"pch")
dend3<-assign_values_to_leaves_nodePar(dend3,20,"lab.cex")
plot(dend3)

#Explanation of above code-computes the distance matrix using the dist() function 
#on the satisfied customers data stored in df_satisfied, then performs hierarchical clustering using the hclust() function, 
#and finally converts the result to a dendrogram object using the as.dendrogram() function.

#the dist() function calculates the Euclidean distance between each pair of rows in the matrix.
#This distance matrix is then used as input to the hclust() function, 
#which performs agglomerative hierarchical clustering using the complete linkage method by default.
#The resulting dendrogram shows the hierarchical relationships between the observations in the data,

#characterize cluster based on cutree
hcluster2<-cutree(df_satisfied1.hcluster,2)
hcluster3<-cutree(df_satisfied1.hcluster,3)

#Meaning of above code-- The cutree() function in R is used to cut a dendrogram
#into a specified number of clusters. the cutree() function 
#is being applied to the hierarchical clustering result stored in df_satisfied.hcluster, 
#with the argument 2 indicating that the dendrogram be cut into 2 clusters.


#tabulate membership
table(hcluster2)
table(hcluster3)

aggregate(df_split_satisfied,by=list(hcluster2),FUN=mean)


#explanation of above code- Aggregate calculates the mean values of each variable in the data, 
#grouped by the cluster assignments in hcluster. The resulting object is a matrix  that 
#shows the average values of each variable for each of the two clusters.This help us gain insights 
#into the characteristics of the different clusters and how they differ from each other in terms of the variables

# Cluster 1: Average age is 43. 
#Their average shopping spend at airport is $27.
#Their travel distance is on average 471 miles i.e they travel at moderate distances. There are less females than males. 

# Cluster 2: Average age is 42. 
#Their average shopping spend at airport is $29.looks like they shop good.
#Their travel distance is on average 444 miles i.e they travel at moderate distances. There are more females than males as compared to group1.



aggregate(df_split_satisfied,by=list(hcluster3),FUN=mean)

#Cluster 1:

#Average age is 43. 
#Their average shopping spend at airport is $27.looks like they shop good.
#Their travel distance is on average 408 miles i.e they travel at moderate distances. 
#There are less females than males.


#Cluster 2:

#Average age is 43 similar to group 1. 
#Their average shopping spend at airport is $28.looks like they too shop good.
#Their travel distance is on average 518 miles i.e they travel at long distances. 
#There are less females than males.


#Cluster 3:

#Average age is 42. 
#Their average shopping spend at airport is $29.looks like they shop really good.
#Their travel distance is on average 444 miles i.e they travel at moderate distances. 
#There are more females than males as compared to other groups.



#visualizing the clusters

fviz_cluster(list(data = df_satisfied1, cluster = hcluster3, center=TRUE, scale=TRUE))


#---------------

# 4. K-MEANS CLUSTERING


set.seed(123)
df_satisfied1.kcluster2<-kmeans(df_satisfied1, 2, nstart = 20)
df_satisfied1$kcluster2<-df_satisfied1.kcluster2$cluster


# Look at cluster centers on each of the variables
# for 2 clusters:
df_satisfied1.kcluster2$centers[1:15]
kcluster2_center_data<-data.frame("cluster"=c(1,2),
                                  "X1.departure_delay"=df_satisfied1.kcluster2$centers[1:2],
                                  "X1.airline_status_num"=df_satisfied1.kcluster2$centers[3:4],
                                  "X1.class-num"=df_satisfied1.kcluster2$centers[5:6])

# for 3 clusters:
df_satisfied1.kcluster3<-kmeans(df_satisfied1,3,nstart=20)
df_satisfied1$kcluster3<-df_satisfied1$cluster

df_satisfied1.kcluster3$centers[1:15]
kcluster3_center_data<-data.frame("cluster"=c(1,2,3),
                                  "X1.departure_delay"=df_satisfied1.kcluster3$centers[1:3],
                                  "X1.airline_status_num"=df_satisfied1.kcluster3$centers[4:6],
                                  "X1.class-num"=df_satisfied1.kcluster3$centers[7:9])


# Is there a sigificant difference in mean value between clusters?
# for 2 clusters
model_k2_dep<-aov(X1.departure_delay~cluster,data=kcluster2_center_data)
summary(model_k2_dep)
model_k2_status<-aov(X1.airline_status_num~cluster,data=kcluster2_center_data)
summary(model_k2_status)
model_k2_class<-aov(X1.class.num~cluster,data=kcluster2_center_data)
summary(model_k2_class)
residuals <- resid(model_k2_dep)
residuals

#The ANOVA result for 2 clusters did not show an F-value and p-value because the value of the 
#residuals are zero which means the model perfectly explains the variation in the data. 
#The data for 2 clusters may not be suitable for ANOVA or #there is an issue with the model having 2 clusters.


# for 3 clusters

model_k3_dep<-aov(X1.departure_delay~cluster,data=kcluster3_center_data)
summary(model_k3_dep)
model_k3_status<-aov(X1.airline_status_num~cluster,data=kcluster3_center_data)
summary(model_k3_status)
model_k3_class<-aov(X1.class.num~cluster,data=kcluster3_center_data)
summary(model_k3_class)

# Visualize clusters - k means

fviz_cluster(df_satisfied1.kcluster2,df_satisfied1)
fviz_cluster(df_satisfied1.kcluster3,df_satisfied1)



###########Analysing Not-Satisfied Customers###############

# 2. FIND OPTIMAL NUMBER OF CLUSTERS

df_not_satisfied1<-subset(df_not_satisfied, select = -c(X0.satisfaction_binary))



#Optimum clusters for not satisfied customers
fviz_nbclust(df_not_satisfied1,kmeans,method="wss")


# Find optimal numbers using Silhouette method
fviz_nbclust(df_not_satisfied1,kmeans,method="silhouette")
fviz_nbclust(df_not_satisfied1,FUN=hcut,method="silhouette")

#In general, the silhouette method will be more appropriate for airline data 
#as expectation is to ensure that each cluster is well-separated and distinct.
#The method suggests us that the optimal number of clusters should be 2


# 3. HIERARCHICAL CLUSTERING


# calculate distance matrix-default is euclidean
distance2=dist(df_not_satisfied1)
df_not_satisfied1.hcluster<-hclust(distance2)
plot(df_not_satisfied1.hcluster)


#the dist() function calculates the Euclidean distance between each pair of rows in the matrix.
#This distance matrix is then used as input to the hclust() function, 
#which performs agglomerative hierarchical clustering using the complete linkage method by default.
#The resulting dendrogram shows the hierarchical relationships between the observations in the data,

# Create dendrograms for 2 clusters
dend2_ns<-as.dendrogram(df_not_satisfied1.hcluster)
dend2_ns<-color_labels(dend2_ns,k=2)
dend2_ns<-color_branches(dend2_ns,k=2)
dend2_ns<-assign_values_to_leaves_nodePar(dend2_ns,19,"pch")
dend2_ns<-assign_values_to_leaves_nodePar(dend2_ns,0.5,"lab.cex")
plot(dend2_ns)

# Create dendrograms for 3 clusters
dend3_ns<-as.dendrogram(df_not_satisfied1.hcluster)
dend3_ns<-color_labels(dend3_ns,k=3)
dend3_ns<-color_branches(dend3_ns,k=3)
dend3_ns<-assign_values_to_leaves_nodePar(dend3_ns,19,"pch")
dend3_ns<-assign_values_to_leaves_nodePar(dend3_ns,0.5,"lab.cex")
plot(dend3_ns)


#characterize cluster based on cutree
hcluster2<-cutree(df_not_satisfied1.hcluster,2)
hcluster3<-cutree(df_not_satisfied1.hcluster,3)

#tabulate membership
table(hcluster2)
table(hcluster3)

aggregate(df_split_not_satisfied,by=list(hcluster2),FUN=mean)
#Cluster1
#Average age is 50. 
#Their average shopping spend at airport is $24.looks like they shop good.
#Their travel distance is on average 482 miles i.e they travel at moderate distances. 
#There are more females than males.

# Cluster 2: #Average age is 49. 
#Their average shopping spend at airport is $26.looks like they too shop good.
#Their travel distance is on average 472 miles i.e they travel at moderate distances. 
#There are less females than males.

aggregate(df_split_not_satisfied,by=list(hcluster3),FUN=mean)

#Cluster 1:

#Average age is 50. 
#Their average shopping spend at airport is $24.looks like they too shop good.
#Their travel distance is on average 482 miles i.e they travel at moderate distances. 
#There are more females than males.



#Cluster 2:

#Average age is 49. 
#Their average shopping spend at airport is $28.looks like they too shop good.
#Their travel distance is on average 441 miles i.e they travel at moderate distances. 
#Average number of females are more than males.



#Cluster 3:

#Average age is 49. 
#Their average shopping spend at airport is $25.looks like they shop good.
#Their travel distance is on average 496 miles i.e they travel at moderate distances. 
#Average number of females are more than males.




#######visualizing the clusters- 

fviz_cluster(list(data = df_not_satisfied1, cluster = hcluster2))
fviz_cluster(list(data = df_not_satisfied1, cluster = hcluster3))


# 4. K-MEANS CLUSTERING

set.seed(123)
df_not_satisfied1.kcluster2<-kmeans(df_not_satisfied1,2,nstart=20)
df_not_satisfied1$kcluster2<-df_not_satisfied1.kcluster2$cluster


# Look at cluster centers on each of the variables
# for 2 clusters:
df_not_satisfied1.kcluster2$centers[1:15]

#from the above code we see values of the cluster centers for the 
#df_not_satisfied dataset and the kcluster2 clustering algorithm.
#The cluster centers represent the average values of each variable for each cluster.

kcluster2_center_data<-data.frame("cluster"=c(1,2),
                                  "X0.5.departure_delay"=df_not_satisfied1.kcluster2$centers[1:2],
                                  "X0.5.airline_status_num"=df_not_satisfied1.kcluster2$centers[3:4],
                                  "X0.5.class_num"=df_not_satisfied1.kcluster2$centers[5:6])



# for 3 clusters:
df_not_satisfied1.kcluster3<-kmeans(df_not_satisfied1,3,nstart=20)
df_not_satisfied1$kcluster3<-df_not_satisfied1.kcluster3$cluster

df_not_satisfied1.kcluster3$centers[1:15]
kcluster3_center_data<-data.frame("cluster"=c(1:3),
                                  "X0.5.departure_delay"=df_not_satisfied1.kcluster3$centers[1:3],
                                  "X0.5.airline_status_num"=df_not_satisfied1.kcluster3$centers[4:6],
                                  "X0.5.class_num"=df_not_satisfied1.kcluster3$centers[7:9])


# Is there a sigificant difference in mean value between clusters?
# for 2 clusters
model_k2_delay<-aov(X0.5.departure_delay~cluster,data=kcluster2_center_data)
summary(model_k2_delay)
#The ANOVA results show that the sum of squares (SS) for the 
#cluster variable is 0.001607 and the mean square (MS) is also 0.001607. The degrees of freedom (Df) for the cluster variable is 1.
#The cluster variable explains a small proportion of the variance in the departure_delay.

model_k2_status<-aov(X0.5.airline_status_num ~ cluster, data = kcluster2_center_data)
summary(model_k2_status)

model_k2_class<-aov(X0.5.class_num~cluster,data=kcluster2_center_data)
summary(model_k2_class)


#for 3 clusters
model_k3_delay<-aov(X0.5.departure_delay~cluster,data=kcluster3_center_data)
summary(model_k3_delay)

#In this case, the p-value is 0.667, which is higher than the typical 
#significance level of 0.05. Therefore, we fail to reject the null hypothesis 
#and conclude that there is no statistically significant difference in the X0.departure_delay 
#variable between the two clusters.

model_k3_status<-aov(X0.5.airline_status_num~cluster,data=kcluster3_center_data)
summary(model_k3_status)

model_k3_class<-aov(X0.5.class_num~cluster,data=kcluster3_center_data)
summary(model_k3_class)


# Visualize clusters - k means- to disscuss with prof
fviz_cluster(df_not_satisfied1.kcluster2,df_not_satisfied1)
fviz_cluster(df_not_satisfied1.kcluster3,df_not_satisfied1)






