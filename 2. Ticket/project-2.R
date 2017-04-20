library(dplyr)
library(data.table)
library(plsdepot)
setwd("H:/Big data Analysis/Ticket")
concert <- fread("concert.csv")
# above <- concert[,like(TicketName, concert[-])]

table(above$SoldDate)
#==========untile here===========================================

plot(ticket$SoldTime)
library(randomForest)


other <- ticket[-c(2,8)]
head(other)
fitS <- randomForest(as.factor(ticket$SoldDate,ticket$SoldPrice) ~  TicketCode,data=train,importance=TRUE,ntree=2000)
Prediction <- predict(fit, test);Prediction #fit to predict test
confusion_matrix<-(table(test$Survived,Prediction))
print(confusionMatrix(confusion_matrix))

library(pls)
plsr(other, ticket$SoldPrice)

summary(ticket$TicketName)
