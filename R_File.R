library(ggplot2)
library(Hmisc)
library(caret)
library(ggeasy)
library(rpart)
library(rpart.plot)
library(data.table)
library(dplyr)
library(DAAG)
library(party)
library(mlbench)
library(pROC)
library(tree)
library(randomForest)
churn<- read.csv(file = '~/Documents/BUAN 6356- Zhe Zhang/Project/Telco_customer_churn.csv')

# Understanding the features 
names(churn)

# Summarizing the data
str(churn)
summary(churn)

# Except Churn_Reason with 5000+ Null rows every column has 0 null rows
# Checking for duplicates

sum(duplicated(churn))

# No duplicate rows
make.names(names(churn), unique= TRUE)
names(churn)
describe(churn)

# EDA- Data visualization exploring the distribution of different variables and 
# relationships between the features and target.

# Basic barplot
cvaldist <-ggplot(data=churn, aes(x= factor(Churn.Value))) +
  geom_bar(stat="count", color = c("red", "blue"), fill= c("red", "blue")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "Label distribution", x = "Churn No= 0/Churn yes=1")

cvaldist <-ggplot(data=churn, aes(x= factor(Churn.Reason))) +
  geom_bar(stat="count") + 
  theme(axis.text.x = element_text(face="bold", angle = 90, hjust =1 , vjust = 0.5, size= 8)) +
  ggeasy::easy_center_title() 

cvaldist + labs(title = "Distribution of reasons to leave", x = "Churn Reason") 

cvaldist <-ggplot(data=churn, aes(x= factor(Gender))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("yellow", "blue")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "Gender distribution", x = "Male/Female")

cvaldist <-ggplot(data=churn, aes(x = factor(Tenure.Months))) +
  geom_bar(stat = "count") + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "Tenure Months", x = "Number of Months")

cvaldist_churnscore <-ggplot(data=churn, aes(x = Churn.Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") + 
  theme(axis.text = element_text(face="bold", size = 10))

cvaldist_churnscore + labs(title = "Churn Score histogram", x = "Churn Score on a scale of 100")

cvaldist <-ggplot(data=churn, aes(x= factor(Senior.Citizen))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("green", "yellow")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "If Senior citizen or not", x = "Senior Citizen status")

cvaldist <-ggplot(data=churn, aes(x= factor(Partner))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("red", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "If Married or not", x = "Unmarried/Married")

cvaldist <-ggplot(data=churn, aes(x= factor(Dependents))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("red", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist + labs(title = "Are there any dependents", x = "No/Yes")

cvaldist_phoneservice <-ggplot(data=churn, aes(x= factor(Phone.Service))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("red", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_phoneservice + labs(title = "If the customer has opted for phone service", x = "No/Yes")

cvaldist_phoneservice <-ggplot(data=churn, aes(x= factor(Multiple.Lines))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_phoneservice + labs(title = "If the customer has opted for multiple-lines", x = "No/No lines/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Internet.Service))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("blue", "yellow", "red")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has opted for Internet-Service", x = "Fiber_optic/DSL/No")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Online.Security))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has subscribed to Online Security", x = "No/No-Internet/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Online.Backup))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has subscribed to Online-Backup", x = "No/No-Internet/Yes")


cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Device.Protection))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has subscribed to Device-protection", x = "No/No-Internet/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Tech.Support))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has subscribed to Tech-Support", x = "No/No-Internet/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Streaming.TV))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has Streams-TV online", x = "No/No-Internet/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Streaming.Movies))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("red", "orange", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "If the customer has Streams-Movies online", x = "No/No-Internet/Yes")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Contract))) +
  geom_bar(stat="count", color = c("black", "black", "black"), fill= c("blue", "red", "violet")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "Type of contract", x = "Month-to-month/One-year/Two-year")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Paperless.Billing))) +
  geom_bar(stat="count", color = c("black", "black"), fill= c("red", "green")) + 
  theme(axis.text = element_text(face="bold"))

cvaldist_internetservice + labs(title = "E-billing or Paper billing", x = "Paperless/ Paper")

cvaldist_internetservice <-ggplot(data=churn, aes(x= factor(Payment.Method))) +
  geom_bar(stat="count", color = c("black", "black", "black", "black"), fill= c("green4", "lawngreen", "orange", "red4")) + 
  theme(axis.text.x = element_text(face="bold", angle= 90))

cvaldist_internetservice + labs(title = "Payment method opted for ", x = "Mode of Payment")

barfill <- "#4271AE"
barlines <- "#1F3552"

cvaldist_internetservice <-ggplot(churn, aes(x = Monthly.Charges)) +
  geom_histogram(aes(y = ..count..), colour = barlines, fill = barfill) + 
  theme(axis.text.x = element_text(face="bold", angle= 90))

cvaldist_internetservice + labs(title = "Charges paid on a Monthly basis ", x = "Amount")

cvaldist_internetservice <-ggplot(churn, aes(x = Total.Charges)) +
  geom_histogram(aes(y = ..count..), colour = barlines, fill = barfill) + 
  theme(axis.text.x = element_text(face="bold", angle= 90))

cvaldist_internetservice + labs(title = "Total Charges paid", x = "Amount")

cvaldist_internetservice <-ggplot(churn, aes(x = CLTV)) +
  geom_bar(stat= "count", colour = barlines, fill = barfill) + 
  theme(axis.text.x = element_text(face="bold", angle= 90))

# Dropping redundant columns and columns with no useful information

churn <- churn[!(names(churn) %in% c("Country", "City", "State", "Latitude", " Longitude", "CustomerID", "Count", "Lat.Long"))]
names(churn)

# Encoding categorical variables to numerical variables

unique(churn$Churn.Reason)

churn$Customer.Service <- as.factor(ifelse(churn$Churn.Reason == "Attitude of support person" | 
                                               churn$Churn.Reason == "Attitude of service provider" | 
                                               churn$Churn.Reason == "Service dissatisfaction" | 
                                               churn$Churn.Reason == "Lack of self-service on Website" | 
                                               churn$Churn.Reason == "Poor expertise of phone support" | 
                                               churn$Churn.Reason == "Poor expertise of online support", 
                                               1,0))
churn$Competitor <- as.factor(ifelse(churn$Churn.Reason == "Competitor offered higher download speeds" |
                                               churn$Churn.Reason == "Competitor offered more data" | 
                                               churn$Churn.Reason == "Competitor had better devices" |
                                               churn$Churn.Reason == "Competitor made better offer" , 
                                               1,0))
churn$Product <- as.factor(ifelse(churn$Churn.Reason == "Network reliability" | 
                                     churn$Churn.Reason == "Product dissatisfaction" | 
                                     churn$Churn.Reason == "Limited range of services",
                                  1,0))
                                     
churn$Pricing <- as.factor(ifelse(churn$Churn.Reason == "Price too high" |
                                  churn$Churn.Reason == "Extra data charges" | 
                                  churn$Churn.Reason == "Long distance charges" |
                                  churn$Churn.Reason == "Lack of affordable download/upload speed" ,
                                  1,0))

churn$Uncontrollable <- as.factor(ifelse(churn$Churn.Reason == "Moved" |
                                         churn$Churn.Reason == "Deceased",
                                         1,0))
                                               
names(churn)

# Converting other categorical columns to numerical

churn$Genderenco <- as.factor(ifelse(churn$Gender == "Male" ,
                                         0,1))
churn$Senior.Citizenenco <- as.factor(ifelse(churn$Senior.Citizen == "No", 0, 1))

churn$Partnerenco <- as.factor(ifelse(churn$Partner == "No", 0, 1))

churn$Dependentsenco <- as.factor(ifelse(churn$Dependents == "No", 0, 1))

churn$Phone.Serviceenco <- as.factor(ifelse(churn$Phone.Service == "No", 0, 1))

churn$Multiple.Linesenco <- as.factor(ifelse(churn$Multiple.Lines == "No" | 
                                         churn$Multiple.Lines == "No phone service", 0, 1))

churn$Online.Securityenco <- as.factor(ifelse(churn$Online.Security == "No" |
                                          churn$Online.Security == "No internet service", 0, 1))

churn$Online.Backupenco <- as.factor(ifelse(churn$Online.Backup == "No" |
                                            churn$Online.Backup == "No internet service", 0, 1))

churn$Device.Protectionenco <- as.factor(ifelse(churn$Device.Protection == "No" |
                                                  churn$Device.Protection == "No internet service", 0, 1))

churn$Tech.Supportenco <- as.factor(ifelse(churn$Tech.Support == "No" |
                                             churn$Tech.Support == "No internet service", 0, 1))

churn$Streaming.TVenco <- as.factor(ifelse(churn$Streaming.TV == "No" |
                                         churn$Streaming.TV == "No internet service", 0, 1))

churn$Streaming.Moviesenco <- as.factor(ifelse(churn$Streaming.Movies == "No" |
                                             churn$Streaming.Movies == "No internet service", 0, 1))

churn$Paperless.Billingenco <- as.factor(ifelse(churn$Paperless.Billing == "No", 0, 1))

churn$Churn.Labelenco <- as.factor(ifelse(churn$Churn.Label == "No", 0, 1))

churn$Internet.Serviceenco <- as.factor(ifelse(churn$Internet.Service == "No", 0, 
                                           ifelse(churn$Internet.Service == "DSL", 1, 2)))

churn$Contractenco <- as.factor(ifelse(churn$Contract == "One year", 0, 
                                       ifelse(churn$Contract == "Two year", 1, 2)))

churn$Payment.Methodenco <- as.factor(ifelse(churn$Payment.Method == "Credit card (automatic)", 0,
                                             ifelse(churn$Payment.Method == "Bank transfer (automatic)", 1,
                                                    ifelse(churn$Payment.Method == "Electronic check", 2, 3))))

# Checking for null values in all columns

lapply(churn,function(x) { length(which(is.na(x)))})

# Total.charges have 11 null columns.
# Upon inspection the result is they have null because of 0 months in tenure, hence total charges should be 0
# Imputing 0 for all null rows in total charges

churn$Total.Charges[is.na(churn$Total.Charges)] <- 0

lapply(churn,function(x) { length(which(is.na(x)))})

# Finishing all the data cleaning 
head(churn, 3)
churnft <- copy(churn)
head(churnft,3)

churnft <- churnft[!(names(churn) %in% c("Gender", "Senior.Citizen", "Partner", "Dependents", "Phone.Service", "Multiple.Lines",
                                         "Internet.Service", "Online.Security", "Online.Backup", "Device.Protection", 
                                         "Tech.Support", "Streaming.TV", "Streaming.Movies", "Contract", "Paperless.Billing",
                                         "Payment.Method", "Churn.Reason", "Churn.Label", "Churn.Value"))]
names(churnft)

# Replacing Zipcodes with the average churn score of people from that Zipcode.
agg_tbl <- data.frame(churnft %>% group_by(Zip.Code) %>% 
  summarise(Avg_Score=mean(Churn.Score)))

churnft <- merge(x=churnft,y=agg_tbl,by="Zip.Code",all.x=TRUE)

head(churnft, 3)

churnft <- churnft[!(names(churnft) %in% c("Zip.Code"))]

head(churnft,3)
# Creating correlation matrix

cor <- cor(churnft)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor, col = palette, symm = TRUE)


# creating train test validation datasets
sample <- sample(c(TRUE, FALSE), nrow(churnft), replace=TRUE, prob=c(0.7,0.3))
train  <- churnft[sample, ]
test   <- churnft[!sample, ]
test_val <- sample(c(TRUE, FALSE), nrow(test), replace=TRUE, prob=c(0.5,0.5))
validation <- test[test_val,]
test <- test[!test_val,]

# Decision Trees

tree <- rpart(Churn.Labelenco ~., data = train, cp = 0.01, maxdepth = 22)
rpart.plot(tree)
rpart.rules(tree)
ctree_ <- ctree(Churn.Labelenco ~ ., train)
plot(ctree_)
printcp(tree)
prp(tree, type = 1, extra = 1, under = FALSE, split.font= 1, varlen = -10)

# Checking the models on validation set
pred_trees <- predict(tree, validation, type = "class")
summary(pred_trees)
confusionMatrix(pred_trees,validation$Churn.Labelenco )

# Testing the model on testing set
pred_trees <- predict(tree, test, type = "class")
summary(pred_trees)
confusionMatrix(pred_trees,test$Churn.Labelenco )

# Pruned Tree

model_pruned <- rpart(Churn.Labelenco ~ ., data = train, method = "class", control = rpart.control(cp = 0.01, minsplit = 7, xval = 5))
printcp(model_pruned)
prp(model_pruned, type = 1, extra =1 , split.font= 1, varlen = -10)

prunedct <- prune(model_pruned, cp = model_pruned$cptable[which.min(model_pruned$cptable[,"xerror"]), "CP"])
cp_pruned <- data.frame(prunedct$cptable)
max(cp_pruned$nsplit)

length(prunedct$frame$var[prunedct$frame$var == "<leaf>"])

prp(prunedct, type = 1, extra = 1, split.font= 1, varlen = -10 ,
    box.col = ifelse(prunedct$frame$var == "<leaf>", "gray", "white"))

prune_pred <- predict(prunedct, validation, type = "class")
confusionMatrix(prune_pred, validation$Churn.Labelenco)

prune_pred <- predict(prunedct, test, type = "class")
confusionMatrix(prune_pred, test$Churn.Labelenco)
# Random Forest

model_random <- randomForest(Churn.Labelenco ~ ., data = train, proximity= TRUE)
varImpPlot(model, type = 2, main= "Importance of Feature variables")

# Checking the models on validation set
pred_random <- predict(model_random, validation, type= "class")
confusionMatrix(pred_random, validation$Churn.Labelenco)

# Checking the models on test set
pred_random <- predict(model_random, test, type= "class")
confusionMatrix(pred_random, test$Churn.Labelenco)

# Logistic Regression

logit_model <- glm(Churn.Labelenco ~., data = train, family = "binomial")
options(scipen = 11)
summary(logit_model)

preds_logit <- predict(logit_model, validation, type = "response")

# Choosing the best model
# Random Forest

# Our recommendations































