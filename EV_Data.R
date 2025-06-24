#Importing data
setwd("/Users/sdo/Desktop/Data analytics protfolio /EV Project")
getwd()
View(EV_Data)

#Inspecting Data 
glimpse(EV_Data)
summarise(EV_Data)
head(EV_Data)
any(is.na(EV_Data)) # Result: [1] TRUE, so there is some Null data. 
colSums(is.na(EV_Data))
EV_Data[!complete.cases(EV_Data), ]

#Creating a separate temp. dataset for only complete data throughout the years. 

incomplete_states <- EV_Data %>%
  group_by(State) %>% 
  filter(any(is.na(across(everything()))))  
  
complete_states <- na.omit(incomplete_states)


any(is.na(complete_states))  # Result:[1] FALSE

complete_states %>%
  group_by(Year) %>% 
  distinct(State) %>% 
  count() 

# Checking if there is indeed no complete data in 2019. 
complete_states_2019 <- complete_states %>% 
  filter(Year == 2019)

#creation of basic graphs 
# Creation of EV Registrations over time in the US 

ev_registration_trend <- EV_Data %>%
  group_by(Year) %>%
  summarise(total_ev_registrations = sum(`EV Registrations`, na.rm = TRUE))

ggplot(data = ev_registration_trend, aes(x = Year, y = total_ev_registrations)) +
  geom_line(color = "darkgreen", size = 1.2) +
  labs(
    title = "Total EV Registrations Over Time in the U.S.",
    x = "Year",
    y = "Total EV Registrations"
  ) +
  scale_y_continuous(labels = comma)+ 
  theme_minimal()

# Creation of EV Stations in the U.S over time 

ev_station_trend <- EV_Data %>%
  group_by(Year) %>%
  summarise(total_ev_station = sum(`Stations`, na.rm = TRUE))

ggplot(data = ev_station_trend, aes(x = Year, y = total_ev_station)) +
  geom_line(color = "darkred", size = 1.2) +
  labs(
    title = "Total EV Stations Over Time in the U.S.",
    x = "Year",
    y = "Total EV Stations"
  ) +
  scale_y_continuous(labels = comma)+ 
  theme_minimal()
  
# Creation of Charging stations over time 

ev_station_charging_type_trend <- complete_states %>%
  group_by(Year) %>%
  summarise(
    `Total Level 1 Stations` = sum(`Level 1`, na.rm = TRUE), 
    `Total Level 2 Stations` = sum(`Level 2`, na.rm = TRUE), 
    `Total DC Fast Stations` = sum(`DC Fast`, na.rm = TRUE)
    )%>%
  pivot_longer(
    cols = c(`Total Level 1 Stations`, `Total Level 2 Stations`, `Total DC Fast Stations`),
    names_to = "Charging_Type",
    values_to = "Station_Count"
  )

ggplot( data = ev_station_charging_type_trend, aes(x= Year, y= Station_Count, colour = Charging_Type))+
  geom_point()+
  geom_smooth( method = "loess", se= FALSE )+
  
labs(
  title = "Charging Station Types Over Time in the U.S.",
  x = "Year",
  y = "Number of Stations",
  colour = "Charging_Type"
) +
  scale_y_continuous(labels = comma)+ 
  theme_minimal()

# Creation of total incentives over time in the U.S.

total_incentives_trend <- EV_Data %>% 
  group_by(Year) %>% 
  summarise(`Total Incentives` =sum(`Incentives`, na.rm = TRUE))

ggplot( data =total_incentives_trend, aes(x= Year, y= `Total Incentives`, colour = "orange" ))+
  geom_point()+
  geom_line( size = 1)+
  labs(
    title = "Total Incentives Granted From 2016 to 2023 in the U.S"
  )+ 
  theme_minimal()

#Creation of Total Vehicles and EVs in the U.S from 2016 to 2023

`total_EVs&vehicles` <- `EV_Data` %>% 
  group_by(Year) %>% 
  summarise( `Total Vehicles`= sum(`Total Vehicles`,na.rm = TRUE),
             `Total EVs`= sum(`EV Registrations`,na.rm = TRUE)
             )%>%
  pivot_longer(
    cols = c(`Total Vehicles`, `Total EVs`),
    names_to = "Vehicle_Type",
    values_to = "Count"
  )

ggplot( data = `total_EVs&vehicles`, aes(x= Year, y= Count, colour = Vehicle_Type ))+
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Vehicles and EVs in the U.S from 2016 to 2023",
     y = "Vehicle amount in units"
  )
  theme_minimal()

summary(EV_Data$`Total Vehicles`)
summary(EV_Data$`EV Registrations`)
# Creation of median EV % share from 2016 to 2023
  
median_ev_share <- `EV_Data` %>% 
  group_by(Year) %>% 
  summarise(`Median EV Share (%)`= median(`EV Share (%)`), na.rm = TRUE)


ggplot( data =`median_ev_share`, aes(x= Year, y= `Median EV Share (%)`))+
  geom_line()+
  labs(
    title = "Median EV Share (%) Over Time in the U.S.",
    x = "Year",
    y = "Median EV Share (%)"
  )+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  theme_minimal()
# Creation of distribution of EV share (%) in 2016 compared to 2023. 
EV_Data %>%
  filter(Year == 2023) %>%
  ggplot(aes(x = `EV Share (%)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of EV Share (%) in 2023")

EV_Data %>%
  filter(Year == 2016) %>%
  ggplot(aes(x = `EV Share (%)`)) +
  geom_histogram(bins = 30, fill = "gold", color = "white") +
  labs(title = "Distribution of EV Share (%) in 2016")

#Summary of EV Share (%) and a boxplot graph
summary(EV_Data$`EV Share (%)`)
boxplot(EV_Data$`EV Share (%)`)

summary(EV_Data$`EV Registrations`)

# Creation of EV registrations and EV stations over time 

`total_EVs_Registrations_and_EV_Stations` <- `EV_Data` %>% 
  group_by(Year) %>% 
  summarise( `EV Registrations`= sum(`EV Registrations`,na.rm = TRUE),
             `EV Stations`= sum(`Stations`,na.rm = TRUE)
  ) %>%
 pivot_longer(
    cols = c(`EV Registrations`, `EV Stations`),
    names_to = "Data_Type",
    values_to = "Count"
  )

ggplot( data = `total_EVs_Registrations_and_EV_Stations`, aes(x= Year, y= Count, colour = Data_Type ))+
  geom_point() +
  geom_line()+
  scale_y_continuous(labels = comma)+
  labs(
    title = "Total Registration and EV Stations in the U.S from 2016 to 2023",
    y = "Amount in units"
  )
theme_minimal()

write.csv(complete_states,"complete_states.csv",)
