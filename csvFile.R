library(tidyverse)

#Reading from files
#Get a wroking directory

getwd()

##Set the working directory
setwd("C:/Users/LENOVO/OneDrive/document/R Programming")

#Reading the csv file
Titanic <- read.csv("titanic_data.csv", header= T)

#structure of the file/Dataframe
str(Titanic)

head(Titanic) #First 6 rows

#Last 6 rows
tail(Titanic)

#summary
summary(Titanic)

Titanic$Name
Titanic$Gender

#Multiple Rows
Titanic[,c("Name","Fare","Gender")]
#using Indexing
Titanic[,1:5]

#____FILTER____#

head(Titanic[Titanic$Age > 35, c("PassengerId", "Cabin","Pclass")])

#Using Pipe Operator
set1 <-  Titanic %>% select(Pclass, Age, Fare, Survived)

female <-  Titanic %>% filter(Gender == "female") %>% select(Pclass, Fare, Age, Survived)

male <-  Titanic %>% filter(Gender == "male") %>% select(Pclass, Fare, Age, Survived)

#Adding a new column 
#Ifelse statement  ------ ifelse(condition, true statement, false statement)

Titanic$Survived.orNo <- ifelse(Titanic$Survived == 1, "Survived", "Not Survived")

#deleting a column
Titanic$newSurvived <- NULL

#Idk if this will work but im trying to interchange the survived column with survived.orNo
#with a temp column
Titanic$temp <- Titanic$Survived
Titanic$Survived <- Titanic$Survived.orNo
Titanic$Survived.orNo <- Titanic$temp
Titanic$temp <-  NULL
  
#Feature engineering
Titanic %>% mutate(AgeGroup = ifelse(Titanic$Age >18, "Adult", "Child")) %>% head()

Titanic <- Titanic %>% mutate(FamilyMembers = Titanic$SibSp+ Titanic$Parch)

#______Sorting______#
#sorting wrt ascending fare amnts
fare.asc <-  Titanic %>%  arrange(Fare)
#or
Titanic$Fare[order(Titanic$Fare)]

#sorting wrt descnding fare amnt 
fare.dsc <- Titanic %>% arrange(desc(Fare))
Titanic$Fare[order(Titanic$Fare, decreasing = T)]

#Updating individual elements
Titanic$Age[Titanic$PassengerId == 1] <- 23
#or
Titanic[Titanic$PassengerId == 1, "Age"] <- 22

#Group By
Titanic %>% group_by(Gender) %>%  summarise(avgFare = mean(Fare))

Titanic %>% group_by(Pclass, Gender) %>%  summarise(Count = n())

Titanic %>% group_by(Pclass) %>%  summarise(Survivors = sum(Survived.orNo)) %>% 
  arrange(Survivors)

Titanic %>% group_by(Gender) %>% summarise(rate = mean(Survived.orNo))
Titanic %>% group_by(Pclass) %>% summarise(rate = mean(Survived.orNo))
colSums(is.na(Titanic) | Titanic == "")

#writing data to a csv file
write.csv(Titanic,"Titanic_modified.csv")

newTitanic <- read.csv("Titanic_modified.csv", header = T)


#plot to see which gender and class survived the most
Titanic %>% group_by(Pclass, Gender) %>%
  summarise(rate = mean(Survived.orNo), .groups = "drop") %>%
  ggplot(aes(factor(Pclass), rate, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(x = "Class", y = "Survival rate", title = "Survival by class and gender")
