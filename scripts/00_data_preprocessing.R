#Load Packages

library(tidyverse)
library(naniar)
library(readxl)
library(ggplot2)
library(skimr)
library(stringr)

#import data
data <- read_excel("data/AMR_Parental_KAP2.xlsx")
data

#check missing value
is.na(data)
sum(is.na(data))
colSums(is.na(data))
duplicated(data)
sum(duplicated(data))

gg_miss_var(data)
miss_var_summary(data)
miss_var_which(data)

#check data structure
glimpse(data)


# mean(Numeric Imputation)
data$`Child’s age (years)` = as.integer(data$`Child’s age (years)`)

data <- data |> 
  mutate(`Child’s age (years)` = replace_na(mean(`Child’s age (years)`, na.rm = T)))

data <- data |> 
  mutate(`Lack of trust in medical service` = replace_na(mean(`Lack of trust in medical service`, na.rm = T)))

data <- data |> 
  mutate(`No confidence with doctor's medication` = replace_na(mean(`No confidence with doctor's medication`, na.rm = T)))

data <- data |> 
  mutate(`Easier to apply previous prescription` = replace_na(mean(`Easier to apply previous prescription`, na.rm = T)))

data <- data |> 
  mutate(`Suggestions from others` = replace_na(mean(`Suggestions from others`, na.rm = T)))

data <- data |> 
  mutate(`Knowledge of antibiotics` = replace_na(mean(`Knowledge of antibiotics`, na.rm = T)))

data <- data |> 
  mutate(`I don't give antibiotics without doctor's consultation` = replace_na(mean(`I don't give antibiotics without doctor's consultation`, na.rm = T)))

data <- data |> 
  mutate(Others...68 = replace_na(mean(Others...68, na.rm = T)))

data <- data |> 
  mutate(`It is convenient to purchase antibiotics from retail pharmacies` = replace_na(mean(`Lack of hospitals in the nearest place`, na.rm = T)))


data <- data |> 
  mutate(`Lack of hospitals in the nearest place` = replace_na(mean(`Lack of hospitals in the nearest place`, na.rm = T)))
 
data <- data |> 
  mutate( `I didn't have enough money to pay for the hospital visit`= replace_na(mean(`I didn't have enough money to pay for the hospital visit`, na.rm = T)))

data <- data |> 
  mutate(`I didn't have enough time to visit a doctor` = replace_na(mean(`I didn't have enough time to visit a doctor`, na.rm = T)))

data <- data |> 
  mutate(`I thought that my child's condition was not severe enough`= replace_na(mean(`I thought that my child's condition was not severe enough`,na.rm = T)))

data <- data |> 
  mutate(Child_Age = replace_na(mean(`Child’s age (years)`, na.rm = T)))

#Categorical Value Imputation
data$`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)`= as.factor(data$`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)`)


data <- data |> 
  mutate_if(is.character, as.factor)

data <- data |> 
  replace_na(list(`Why would you give your child antibiotics without a physician's advice? (You can choose more than one)`='Others'))

glimpse(data)

data <- data |> 
  replace_na(list(Your_Average_Household_Income_Per_month='Low (less than 30000 BDT)'))

#Parents Age Binary from code
data<- data |> 
  mutate(Parents_Age = case_when(
    `Parent’s age (years)` == 1 ~ "<25", #1  
    `Parent’s age (years)` == 2 ~ "25-35" , #2
    `Parent’s age (years)` == 3 ~ "35-45", #3
    `Parent’s age (years)` == 4 ~ ">45"
    ))
#Parents Sex  from code
data <- data |> 
  mutate(Parents_Sex = case_when(
    `Parent’s sex` == 1 ~' Male',
    `Parent’s sex` == 2 ~ 'Female'
  ))

#Parents Education from code
data <- data |> 
  mutate(Parents_Education = case_when(
    `Parent’s education level` == 1 ~' primary School',
    `Parent’s education level` == 2 ~ 'High School',
    `Parent’s education level` == 3 ~ 'College',
    `Parent’s education level` == 4 ~ 'Diploma',
    `Parent’s education level` == 5 ~ 'Undergraduate',
    `Parent’s education level` == 6 ~ 'Postgraduate',
    `Parent’s education level` == 7 ~ 'No Formaml Education'
  ))

#Family type
data <- data |> 
  mutate(Family_type = case_when(
    `Family type` == 1 ~' Nuclear Family',
    `Family type` == 2 ~' Single  Parent Family',
    `Family type` == 3 ~' Extended Family',
  ))
#Employment status
data <- data |> 
  mutate(Employment_status = case_when(
    `Employment status` == 1 ~'Employed',
    `Employment status` == 2 ~'Self Employed',
    `Employment status`== 3 ~'Not Employed',
  ))

#Your Average Household Income
data <- data |> 
  mutate(Your_Average_Household_Income_Per_month = case_when(
    `Your average household income per month (BDT)` == 1 ~'Low (less than 30000 BDT)',
    `Your average household income per month (BDT)` == 2 ~'Middle (less than 50000 BDT)',
    `Your average household income per month (BDT)` == 3 ~'High (greater than 50000 BDT)',
  ))

#Child sex

data <- data |> 
  mutate(Child_Sex = case_when(
    `Child’s sex` == 1 ~ 'Male',
    `Child’s sex` == 2 ~ 'Female'
  ))
#Child Age
data <- data |> 
  mutate(Child_Age = case_when(
    `Child’s age (years)` ==  1 ~ '<5',
    `Child’s age (years)` == 2 ~ '5-9',
    `Child’s age (years)` == 3  ~ '>10'
  ))

#Number Of Children
data <- data |> 
  mutate(Number_of_children = case_when(
    `Number of children` == 1 ~ '1',
    `Number of children` == 2 ~ '2',
    `Number of children` == 3 ~ '>=3'
  ))

#Leading cacregiver
data <- data |> 
  mutate(Who_is_Leading_child_Caregiver_at_home = case_when(
    `Who is the leading child caregiver at home?` == 1 ~ 'Father',
    `Who is the leading child caregiver at home?` == 2 ~ 'Mother',
    `Who is the leading child caregiver at home?` == 3 ~ 'Grandfather',
    `Who is the leading child caregiver at home?` == 4 ~ 'Grandmother',
    `Who is the leading child caregiver at home?` == 5 ~ 'Others',
  ))


miss_var_which(data)
data$Child_Age

data |> 
  count(`Child’s age (years)`) |> 
  arrange(desc(n))

glimpse(data)
