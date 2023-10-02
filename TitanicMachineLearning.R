#Wczytywanie potrzebnych bibliotek
library(tidyverse)
library(caret)
library(randomForest)

#Wczytanie danych oraz przyjrzenie się im
Data <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = TRUE)
TestData <- read.csv(file = "test.csv", header = TRUE, stringsAsFactors = TRUE)
head(Data, 20)
str(Data)
summary(Data)

#Szukanie zależności między Sex i Survived
ggplot(Data,
       aes(x = Sex,
           fill = as.factor(Survived))) + 
  geom_bar(position = "dodge",
           width = 0.7) +
   labs(fill = "Survived")
##Wniosek: Kobiety miały większe szanse na przeżycie niż mężczyźni 

#Szukanie zależności między Pclass i Survived
ggplot(Data,
       aes(x = Pclass,
           fill = as.factor(Survived)))+ 
  geom_bar(position = "dodge",
           width = 0.7)+
  labs(fill = "Survived")
##Wniosek: Patrząc na trzecią klasę, Pclass może mieć duże znaczenie dla Surviced

#Szukanie zależności między Sibsp i Survived
ggplot(Data,
       aes(x = SibSp,
           fill = as.factor(Survived)))+
geom_bar(position = "dodge",
         width = 0.7)+
  labs(fill = "Survived")
##Wniosek: Brak widocznej zależności między Sibsp i Survived

#Szukanie zależności między Parch i Survived
ggplot(Data,
       aes(x = Parch,
           fill = as.factor(Survived)))+
  geom_bar(position = "dodge",
           width = 0.7)+
  labs(fill = "Survived")
##Wniosek; Brak widocznej zależności między Parch i Survived

#Szukanie zależności między Fare i Survived
ggplot(Data,
       aes(x = Fare,
           fill = as.factor(Survived)))+
  geom_bar(position = "dodge",
           width = 0.7)+
  labs(fill = "Survived")
##Wniosek: Widać, że najmniej osób przeżyło w grupie, gdzie płacano najmniej za bilety

#Tworzenie modelu, do predykatów nie zalicze PassengerID, Name, Cabin, Embarked, bo nje mają znaczenia, pominę też Age, bo brakuje danych
Data$Survived <- as.factor(Data$Survived)
model <- randomForest(formula = Survived ~ Pclass + Sex + SibSp + Parch,
                      data = Data,
                      type = "classification")
predictions <- predict(model, TestData)

#Zapisywanie wyniku do pliku csv

result <- data.frame(PassengerID = TestData$PassengerId, Survived = predictions)
result$Survived <- ifelse(result$Survived == "1",1,0)
write.csv(result, file = "submission.csv", row.names = FALSE)
