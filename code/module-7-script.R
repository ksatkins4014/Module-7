########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: October 10 2019
Author: Kelsey Atkins
########################################################################################


# Clear workspace & load packages ----
rm(list = ls(all = TRUE))
library(tidyverse)
library(stringr)
library(lubridate)
library(forcats)
library(dplyr)

####Read & inspect the dataset ----

Lake_mead_depth <- read_csv("data/lake_mead_depth.csv")

#### inspection including: str(), head(), tail(), dim(), and external data source summary using Summary()
str(Lake_mead_depth)
head(Lake_mead_depth)
tail(Lake_mead_depth)
dim(Lake_mead_depth)
summary(Lake_mead_depth)

####Gather months into one column

Lake_mead_depth %>%
  gather("JAN",  "FEB",  "MAR",  "APR",  "MAY",  "JUN",  "JUL",  "AUG",  "SEP",  "OCT",  "NOV", "DEC",
         key = "month", 
         value = "Lake Mead depth (ft)") -> Lake_mead_depth



####Create Separate month and year columns because they will be lost in the date conversion

Lake_mead_depth %>%
  mutate(Month = month, Year = year) -> Lake_mead_depth

####Create a day column for date conversion and unite year, month, day

Lake_mead_depth %>%
  mutate(day = 01) %>%
  unite(day, month, year, col = "date", sep = " ") -> Lake_mead_depth

Lake_mead_depth$date <- dmy(Lake_mead_depth$date)

####Drop NA values from depth column

Lake_mead_depth %>%
  drop_na(`Lake Mead depth (ft)`) -> Lake_mead_depth

####Order levels for month column 

Lake_mead_depth$Month <- factor(Lake_mead_depth$Month, levels = c("JAN",  "FEB",  "MAR",  "APR",  "MAY",  "JUN",  "JUL",  "AUG",  "SEP",  "OCT",  "NOV", "DEC"))

####Create Pre/Post Dam Labels into a new column

Lake_mead_depth %>%
  mutate("Pre/Post 1964" = ifelse(date < "1964-01-01","Pre Glen Canyon Dam (1936-1964)", "Post Glen Canyon Dam (1964-2014)")) -> Lake_mead_depth

####Order Pre/Post 1964 column

Lake_mead_depth$`Pre/Post 1964` <- factor(Lake_mead_depth$`Pre/Post 1964`, 
                                          levels = c("Pre Glen Canyon Dam (1936-1964)", 
                                                     "Post Glen Canyon Dam (1964-2014)"))


####Creat visulization for Lake Mead Depth over time before nd after Dam

Lake_mead_depth %>%
  ggplot(mapping = aes(Month, `Lake Mead depth (ft)`)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~`Pre/Post 1964`)


##############################################################################################

####Flows Along the Colorado River

# Clear workspace & load packages ----
rm(list = ls(all = TRUE))
library(tidyverse)
library(stringr)
library(lubridate)
library(forcats)
library(dplyr)
           
#####Read datasets

Border <- read_csv("data/border_flow.csv")
Lees_Ferry <- read_csv("data/lees_ferry_flow.csv")
Lake_Powell <- read_csv("data/lake_powell_flow.csv")

### inspection including: str(), head(), tail(), dim(), and external data source summary using Summary()
str(Border)
head(Border)
tail(Border)
dim(Border)
summary(Border)   

str(Lake_Powell)
head(Lake_Powell)
tail(Lake_Powell)
dim(Lake_Powell)
summary(Lake_Powell) 

str(Lees_Ferry)
head(Lees_Ferry)
tail(Lees_Ferry)
dim(Lees_Ferry)
summary(Lees_Ferry) 


#### View Datasets

view(Border)
view(Lees_Ferry)
view(Lake_Powell)


#### Convert Border Date column into ymd format

Border$Date <- ymd(Border$Date)
  
#### Convert Lees Ferry Date column into ymd format & add column with label "Date" to allow for joining data sets

Lees_Ferry$Date <- mdy(Lees_Ferry$date)

Lees_Ferry %>%
  select(-date) -> Lees_Ferry

#### Convert Lake Powell year, month, day columns into ymd format

Lake_Powell %>%
  unite(day, month, year, col = "Date", sep = "-") -> Lake_Powell

Lake_Powell$Date <- dmy(Lake_Powell$Date)

#### Join data tables, by "Date"

full_join(Border,Lake_Powell) -> Colorado_River_Flows

full_join(Colorado_River_Flows,Lees_Ferry) -> Colorado_River_Flows

####gather data to have a single flow column

Colorado_River_Flows %>%
  gather(-Date,
         key = "Location",
         value = "Flow (cfs)") -> Colorado_River_Flows


#### Rename and order Location labels

Colorado_River_Flows$Location <- factor(Colorado_River_Flows$Location, 
                                            levels = c("lakepowell_flow", 
                                                       "leesferry_flow",
                                                       "border_flow")) 
Colorado_River_Flows %>%
  mutate(Location_recode = fct_recode(Location,
             "Border" = "border_flow",
             "Lees Ferry" = "leesferry_flow",
             "Lake Powell" = "lakepowell_flow")) -> Colorado_River_Flows


##### Visualize Data

Colorado_River_Flows %>%
  ggplot(mapping = aes(x = Date, y = `Flow (cfs)`)) +
  geom_line() +
  facet_wrap("Location_recode", dir = "v") +
  geom_vline(xintercept = ymd("1964-01-01"), color = "red") 
  
  
