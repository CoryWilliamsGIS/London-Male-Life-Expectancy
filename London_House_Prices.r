#-----SET WORKING DIRECTORY-----#


#-----LOAD REQUIRED PACKAGES-----#
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggmap) 
library(RColorBrewer)
library(readxl)
library(corrplot)

#-----LOAD LONDON BOROUGH PROFILES-----#
borough_data <- read.csv("london-borough-profiles.csv", header = T, sep = ",", 
                         stringsAsFactors = F, check.names = T)

#-----LOAD LONDON BOROUGH SHAPEFILE-----#
boroughs <- readOGR(dsn=".", layer = "London_Borough_Excluding_MHW")

#-----CLEAN THE DATA -----#

#REPLACE MISC VALUES WITH NA
borough_data[ borough_data == "."] <- "NA"
borough_data[ borough_data == "n/a"] <- "NA"
borough_data[ borough_data == "-"] <- "NA"

#CLASSIFY NA FACTOR AS NA VALUE
borough_data[borough_data=="NA"] <- NA

#RENAME COLUMN HEADER FOR JOIN WITH SHAPEFILE LATER
borough_data <- rename(borough_data, NAME = Area_name)
borough_data <- borough_data[-34:-38, ] # focuses dataset to london

#-----INPUT ADDITIONAL DATA SETS-----#
library("readxl")
traffic_accidents <- read_excel("temporal borough traffic accidents.xls", sheet = "Borough KSI trend")      #KSI traffic accidents
homelessness <- read_excel("statutory-homelessness-borough.xls", sheet = "2013-14")                         #Statutory homelessness
LLW <- read_excel("london-low-income.xls", sheet = "LLW")                                                   #London living wage
NMW <- read_excel("london-low-income.xls", sheet = "NMW")                                                   #National minimum wage
school_readiness <- read_excel("Health Inequalities Strategy Indicators 2016.xls", sheet = "Indicator 4")   #School readiness at age 5

#CLEAN AND SUBSET TRAFFIC ACCIDENTS DATA
colnames(traffic_accidents) = traffic_accidents[1, ]
traffic_accidents <- traffic_accidents[-1,  ]
traffic_accidents <- traffic_accidents[-34:-51, ]
traffic_accidents <- rename(traffic_accidents, NAME = "Local Authority (Borough/District)")
traffic_accidents_subset <- cbind(traffic_accidents$NAME, traffic_accidents$`2014`)
colnames(traffic_accidents_subset) <- c("NAME", "Killed or Seriously Injured in Road Traffic Collisions (2014)")
traffic_accidents_subset <- as.data.frame(traffic_accidents_subset)

#CLEAN AND SUBSET HOMELESSNESS DATA
homelessness <- cbind(homelessness$X__3, homelessness$X__13)
colnames(homelessness) <- c("NAME", "Number of homeless and in priority need per 1,000 households")
homelessness <- homelessness[-1:-9, ]
homelessness <- homelessness[-15:-16, ]
homelessness <- homelessness[-34, ]
homelessness <- as.data.frame(homelessness)

#CLEAN AND SUBSET LONDON LIVING WAGE
LLW <- cbind(LLW$X__2, LLW$X__9)
colnames(LLW) <- c("NAME", "Percentage of people earning less than london living wage (£9.40ph) in 2015")
LLW <- LLW[-1:-3, ]
LLW <- LLW[-34:-52, ]
LLW <- as.data.frame(LLW)

#CLEAN AND SUBSET NATIONAL MINIUMUM WAGE
NMW <- cbind(NMW$X__2, NMW$X__9)
colnames(NMW) <- c("NAME", "Percentage of people earning less than national minimum wage (6.70ph) in 2015")
NMW <- NMW[-1:-3, ]
NMW <- NMW[-34:-52, ]
NMW <- as.data.frame(NMW)

#CLEAN AND SUBSET SCHOOL READINESS
school_readiness <- cbind(school_readiness$X__1, school_readiness$X__2)
colnames(school_readiness) <- c("NAME", "% of the 5-year old population who meet the 'school ready' criteria for development")
school_readiness <- school_readiness[-1:-6, ]
school_readiness <- as.data.frame(school_readiness)

#-----JOIN DATA SETS-----#
library(dplyr)
borough_data_join <- left_join(borough_data, homelessness, by = c("NAME"="NAME"))
borough_data_join <- left_join(borough_data_join, traffic_accidents_subset, by = c("NAME"="NAME"))
borough_data_join <- left_join(borough_data_join, LLW, by =c("NAME"="NAME"))
borough_data_join <- left_join(borough_data_join, NMW, by =c("NAME"="NAME"))
borough_data_join <- left_join(borough_data_join, school_readiness, by =c("NAME"="NAME"))

#COMPARE IF ALL AREA NAMES MATCH BETWEEN DATASET AND SHAPEFILE
borough_data_join$NAME %in% boroughs$NAME

#JOIN DATAFRAME TO SHAPEFILE
boroughs <- merge(boroughs, borough_data_join)

#-----PERFORM CORRELATION BETWEEN MALE LIFE EXPECTANCY AND HEALTH INDICATORS-----#

#--PERFORM CORRELATION WITH SPECIFIC INDICATORS OF HEALTH INEQUALITY--#

#SCHOOL READINESS AT AGE 5 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 89]), method = "pearson", use = "complete.obs")

#PERCENTAGE OF PUPILS ACHIEVEING 5+ GCSE A*-C GRADES (INCL. ENGLISH & MATHS)
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 66]), method = "pearson", use = "complete.obs")

#KSI ROAD TRAFFIC COLLISIONS
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 86]), method = "pearson", use = "complete.obs")

#STATUTORY HOMELESS
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 85]), method = "pearson", use = "complete.obs")

#UNEMPLOYMENT RATES
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 32]), method = "pearson", use = "complete.obs")

#CHILDHOOD OBESITY 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 77]), method = "pearson", use = "complete.obs")

#PERCENTAGE OF PEOPLE EARNING LESS THAN LONDON LIVING WAGE
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 87]), method = "pearson", use = "complete.obs")

#PERCENTAGE OF PEOPLE EARNING LESS THAN NATIONAL MINIMUM WAGE
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 88]), method = "pearson", use = "complete.obs")

#PERCENTAGE OF CHILDREN MEETING THE 'READY FOR SCHOOL' CRITERIA AT AGE 5 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 89]), method = "pearson", use = "complete.obs")

#--PERFROM CORRELATION WITH SELF-REPORTED WELLBEING--#
#HAPPINESS SCORE
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 75]), method = "pearson", use = "complete.obs")

#LIFE SATISFACTION SCORE 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 73]), method = "pearson", use = "complete.obs")

#WORTHWHILENESS SCORE 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 74]), method = "pearson", use = "complete.obs")

#ANXIETY SCORE 
cor(as.numeric(borough_data_join[, 70]), as.numeric(borough_data_join[, 76]), method = "pearson", use = "complete.obs")


#-----EXPORT DATA FRAME AS .CSV-----#
write.csv(borough_data_join, file = "borough_data_join.csv")

#-----REGRESSION PLOTS-----#
library(ggplot2)
library(RColorBrewer)

#MALE LIFE EXPECTANCY AND CHILDHOOD OBESITY PREVELANCE
plot1 <- ggplot(borough_data_join,
       aes(x = as.numeric(borough_data_join$Childhood_Obesity_Prevalance_..._2015.16), 
           y = as.numeric(borough_data_join$Male_life_expectancy._.2012.14.))) +
  geom_point(shape=21, fill="red", color="black", size=2) + geom_smooth(method='lm')+
  xlab("Childhood Obesity Prevelance (2015)") + ylab("Male Life Expectancy (2012-14)")
plot1 <- plot1 + ggtitle("Linear Regression Between Male Life Expectancy and Childhood Obesity")
plot1

#MALE LIFE EXPECTANCY AND % pupils achieveing 5 A*-C GCSE'S (inc. english and maths)
plot2 <- ggplot(borough_data_join,
                aes(x = as.numeric(borough_data_join$Achievement_of_5_or_more_A.._C_grades_at_GCSE_or_equivalent_including_English_and_Maths._2013.14 ), 
                    y = as.numeric(borough_data_join$Male_life_expectancy._.2012.14.))) +
  geom_point(shape=21, fill="red", color="black", size=2) + geom_smooth(method='lm')+
  xlab("Percentage achieving 5+ A*-C Grades incl. English & Mathematics GCSE's (2013-14)") + ylab("Male Life Expectancy (2013-14)")
plot2 <- plot2 + ggtitle("Linear Regression Between Male Life Expectancy and Educational Achievement")
plot2 + coord_cartesian(xlim = c(55, 70))


#MALE LIFE EXPECTANCY AND "LIFE SATISFACTION SCORE
plot3 <- ggplot(borough_data_join,
                aes(x = as.numeric(borough_data_join$Life_satisfaction_score_2011.14_.out_of_10.), 
                    y = as.numeric(borough_data_join$Male_life_expectancy._.2012.14.))) +
  geom_point(shape=21, fill="red", color="black", size=2) + geom_smooth(method='lm')+
  xlab("Life Satisfaction Score out of 10 (2011-2014))") + ylab("Male Life Expectancy (2012-14)")
plot3 <- plot3 + ggtitle("Linear Regression Between Male Life Expectancy and Life Satisfaction Score")
plot3 + coord_cartesian(xlim = c(7, 7.6))



#-----MAPS-----#

#MALE LIFE EXPECTANCY MAP
tm_shape(boroughs) +
  tm_fill("Male_life_expectancy._.2012.14.",title="Age", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Male Life Expectancy", 
            legend.title.size = 0.8,
            legend.text.size = 0.57, 
            legend.position = c("right","bottom"))


#EDUCATIONAL ACHIEVEMENT MAP
tm_shape(boroughs) +
  tm_fill("Achievement_of_5_or_more_A.._C_grades_at_GCSE_or_equivalent_including_English_and_Maths._2013.14",title="Percentage", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Educational Achievement", 
            legend.title.size = 0.7,
            legend.text.size = 0.49, 
            legend.position = c("right", "bottom"))

#LIFE SATISFACTION MAP
tm_shape(boroughs) +
  tm_fill("Life_satisfaction_score_2011.14_.out_of_10.",title="Score (/10)", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Life Satisfaction Score", 
            legend.title.size = 1.5,
            legend.text.size = 1, 
            legend.position = c("right", "bottom"))

#CHILDHOOD OBESITY MAP
tm_shape(boroughs) +
  tm_fill("Childhood_Obesity_Prevalance_..._2015.16",title="Percentage", palette = "Reds")+
  tm_borders(alpha =.5)+
  tm_layout("Childhood Obesity
Prevelance", 
            legend.title.size = 0.7,
            legend.text.size = 0.5, 
            legend.position = c("right", "bottom"))



















