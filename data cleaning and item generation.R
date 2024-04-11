rm(list=ls())
setwd("folder_directory")#set the directory
getwd()#see the directory
file <- 'dataset.csv'
data <- read.csv(file, sep = ";")
data
#Check of some basic characteristics of the dataset
head(data)
colnames(data)
dim(data)
ncol(data)
nrow(data)
str(data)

#The following lines were written to clean a dataset on university students studying a STEM subject.
#STEM is an acronym for science, technology, engineering and mathematics.

#CLEANING DATA 
#Drop unhelpful columns from the data
data$otheruniversity
data <- subset(data, select = -c(IDSE)) #remove the variable IDSE
ncol(data) 
colnames(data)
table(is.na(data$currentlystudying))  #number of MISSING
table(data$currentlystudying) #descriptive table
table(data$gender)

##
##RECODE VARIABLE DEPARTMENT IN STEM AND NO-STEM
install.packages(dplyr)
library(dplyr)  ##mutate---> dyplr
data <- data %>%
  mutate(dep = recode(dep,
                         "Law" = 0,
                      "Philosophy and Letters" = 0,
                      "Sociology and Social Research" = 0, #0 is 'Slightly STEM'
                      "Economy and management" = 1, #1 is 'Moderately STEM'
                      "Cellular, Computational and Integrated Biology, CIBIO" = 2,
                      "Civil, Environmental and Mechanical Engineering" = 2, #2 is 'Highly STEM'
                      "Information Engineering and Information Science" = 2,
                      "Industrial engineering" = 2,
                      "Mathematics" = 2
                      ))

data$dep

##CHECK FOR BREADWINNER
data$breadwinner
data$breadwinnero #considering this case for the value 'Other'
data <- data %>%
  mutate(breadwinner = recode(breadwinner,
                              "My father is the main breadwinner" = 0,
                              "My mother is the main breadwinner" = 1,
                              "My parents are equally breadwinner" = 2,
                              "Other" = 2))
data$breadwinner # to check the MISSING within the variable
# EDUCATIONAL LEVEL OF THE FATHER
data$fatheredulevel
data <- data %>%
  mutate(fatheredulevel = recode(fatheredulevel,
                                 "Elementary school" = 0, #Low educatinal level
                                 "Middle school" = 0,
                                 "High school" = 1, #middle level of education
                                 "Bachelor or Master degree" = 2, #high level of education
                                 "Doctorate" = 2))
data$fatheredulevel # check the missing for this variable
#EDUCATIONAL LEVEL OF THE MOTHER
data$motheredulevel
data <- data %>%
  mutate(motheredulevel = recode(motheredulevel,
                                 "Elementary school" = 0, #Low educatinal level
                                 "Middle school" = 0,
                                 "High school" = 1, #middle level of education
                                 "Bachelor or Master degree" = 2, #high level of education
                                 "Doctorate" = 2,
                                 "Other" =2 ))
data$motheredulevel #check the number of missing
####
  ##SCALE CREATION##
                  ####
##FOR THE VARIABLE GENDER
library(dplyr)  # the operation mutate is within the package dyplr
data <- data %>%
  mutate(gender = recode(gender,
                         "Female" = 1,
                         "Male" = 0,
                         "Other" = 2))
data <- filter(data, data$gender<2)
data$gender #new dataset to be used without 'Other' in gender
table(data$gender) #28 Male and 46 Female, no Other
data$gender <- as.integer(data$gender)
##CHORE SCALE  variables
data <- data %>%
  mutate (chorestakecare = recode(chorestakecare,
                              "Never" =0,
                              "In extraordinary occasions" = 1,
                              "At least once a month" = 2,
                              "At least once a week" = 3,
                              "At least once a day" = 4)) 
data <- data %>%
  mutate (choresclean = recode (choresclean,
                                'Never' =0,
                                'In extraordinary occasions' = 1,
                                'At least once a month' = 2,
                                'At least once a week' = 3,
                                'At least once a day' = 4))
data <- data %>%
  mutate (choreswash = recode (choreswash,
                                  'Never' =0,
                                  'In extraordinary occasions' = 1,
                                  'At least once a month' = 2,
                                  'At least once a week' = 3,
                                  'At least once a day' = 4))
data <- data %>%
  mutate (choresmeals = recode (choresmeals,
                                'Never' =0,
                                'In extraordinary occasions' = 1,
                                'At least once a month' = 2,
                                'At least once a week' = 3,
                                'At least once a day' = 4))
data$choresmeals
data$choresclean
data$choreswash
data$chorestakecare
#drop a particular row that has some missing values
data <- data %>% slice(-c(24)) #here is dropped the 24th row within data 
#re-check the variables after the drop
data$choresmeals
data$choresclean
data$choreswash
data$chorestakecare
##
# Items necessary to generate Self-Efficacy (from self1 to self10)
#
data <- data %>% 
  mutate (self1 = recode(self1,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4))
data <- data %>%
  mutate (self2 = recode(self2,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self3 = recode(self3,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self4 = recode(self4,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self5 = recode(self5,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self6 = recode(self6,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self7 = recode(self7,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self8 = recode(self8,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self9 = recode(self9,
                        '1 - Not at all true' = 1,
                        '2 - Hardly true' = 2,
                        '3 - Moderately true' = 3,
                        '4 - Exactly true' = 4)) 
data <- data %>%
  mutate (self10 = recode(self10,
                         '1 - Not at all true' = 1,
                         '2 - Hardly true' = 2,
                         '3 - Moderately true' = 3,
                         '4 - Exactly true' = 4))
#Consider all the previous variables as numeric. If you have more variables i suggest to implement the package 'foreach'
data$self1 <- as.numeric(data$self1)
data$self2 <- as.numeric(data$self2)
data$self3 <- as.numeric(data$self3)
data$self4 <- as.numeric(data$self4)
data$self5 <- as.numeric(data$self5)
data$self6 <- as.numeric(data$self6)
data$self7 <- as.numeric(data$self7)
data$self8 <- as.numeric(data$self8)
data$self9 <- as.numeric(data$self9)
data$self10 <- as.numeric(data$self10)
#check for MISSING within every variable
table(is.na(data$self1))
table(is.na(data$self2))
table(is.na(data$self3))  #1 missing
table(is.na(data$self4))
table(is.na(data$self5))
table(is.na(data$self6))
table(is.na(data$self7))  #1 missing
table(is.na(data$self8))
table(is.na(data$self9))
table(is.na(data$self10))
#There are two missing within self3 and self7. Therefore:
data$self3  #NA at row 33
data$self7   #NA at row 33 
data <- data %>% slice(-c(33))
data$self3
data$self7
##CHECK FOR MISSING VALUES IN 7 VARIABLES, INCLUDING 2 INDEXES
table(is.na(data$dep))  #0 missing
table(is.na(data$gender))  #0 missing
table(is.na(data$self))  #0 missing
table(is.na(data$motheredulevel))  #1 missing
table(is.na(data$fatheredulevel))  #1 missing
table(is.na(data$back))  #0 missing
table(is.na(data$breadwinner))  #1 missing
##ROW POSITION OF MISSING VALUES
data$motheredulevel #1st row
data$fatheredulevel #1st row
data$breadwinner #18th row
#DROP THE 1st AND 18th ROWS
data <- data %>% slice(-c(1))
data <- data %>% slice(-c(17))
####
##ITEM CREATION##
####
install.packages("psych")
library(psych)
##ITEM FAMILY BACKGROUND
#checking for CRONBACH'S ALPHA
familybackground <- data [, c("choresmeals", 
                                  "choresclean",
                                  "choreswash",
                                  "chorestakecare")
                          ]
familybackground   #ITEM
alpha(familybackground)  # Cronbach's alpha = 0.71, an acceptable reliability of the item.
# CREATION OF THE INDEXES THROUGH THE ADDITIVE METHOD:
familyback <- ((data$choresmeals + data$choresclean + data$choreswash + data$chorestakecare)/4)
data$back <- ((data$choresmeals + data$choresclean + data$choreswash + data$chorestakecare)/4)
##
#### ITEM SELF EFFICACY
#
# checking for ALPHA:
data$selfefficacy <- data [,c("self1", "self2","self3","self4","self5","self6", "self7",
                             "self8", "self9", "self10")]
data$selfefficacy
alpha(data$selfefficacy) # Cronbach's alpha = 0.91, which indicates an excellent reliability.
#ADDITIVE INDEX
selfeff  <- ((data$self1 + data$self2 +data$self3 +data$self4+data$self5+data$self6
              +data$self7+data$self8+data$self9+data$self10)/10)
data$self <- ((data$self1 + data$self2 +data$self3 +data$self4+data$self5+data$self6
                   +data$self7+data$self8+data$self9+data$self10)/10)
table(data$gender,data$dep)
table(data$fatheredulevel)
table(data$motheredulevel)
table(data$breadwinner)

# EXPLORATORY FACTOR ANALYSIS FOR THE CATEGORICAL VARIABLES
# fatheredulevel ---> fatheredu
data$fatheredu <- as.factor(ifelse(data$fatheredulevel==0,'low',
                                   ifelse(data$fatheredulevel==1,'middle',
                                          ifelse(data$fatheredulevel==2,'high',0))))
data$fatheredu
# motheredulevel ---> motheredu
data$motheredu <- as.factor(ifelse(data$motheredulevel==0,'low',
                                   ifelse(data$motheredulevel==1,'middle',
                                          ifelse(data$motheredulevel==2,'high',0))))
data$motheredu
# USING BOTH LOW AND MIDDLE VALUES AS REFERENCE CATEGORIES
# motheredulevel ---> motheredu2
data$motheredu2 <- as.factor(ifelse(data$motheredulevel<2,'low and middle',
                                    ifelse(data$motheredulevel==2, 'high',0)))
data$motheredu2
# breadwinner ---> breadw
data$breadw <- as.factor(ifelse(data$breadwinner==0,'father',
                                ifelse(data$breadwinner==1,'mother',
                                       ifelse(data$breadwinner==2,'parents',0))))
data$breadw