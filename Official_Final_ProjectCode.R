Data = read.csv("games-features.csv")  

library(SDMTools)
library(rlang)
library(dplyr)
library(arules)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(pROC)
library(ROCR)
library(mlbench)
library(forecast)

# DATA CLEANING STEPS
# Check for missing variables
anyNA(Data)

#Remove irrelevant columns
#Lines 26, 27,30,41, 44, 52 were removed due to the insignificance of the variables as there were an extreme low number of either "TRUE" or "FALSE"
Data_adj=subset(Data,select=-c(1,2,4,5,17,19,21,26,27,30,41,44,52,57,59:70,72:78))
str(Data_adj)

#Get rid of values = 0 under Metacritic
Data_cleaned=Data_adj %>% filter(Metacritic != 0)

#checking for duplicates in QueryName
length(unique(Data_cleaned$QueryName))
sum(duplicated(Data_cleaned$QueryName))

#remove repeated rows for QueryName
Data_final=Data_cleaned %>% distinct(QueryName, .keep_all = TRUE)


#Finding outliers for numeric variables
boxplot(Data_final$RequiredAge,main="Required Age Outliers")
boxplot(Data_final$RecommendationCount,main="Recommendation Count Outliers")
boxplot(Data_final$PriceInitial,main="Price Initial Outliers")


#Delete rows with Price Outliers
Data_final=Data_final %>% filter(PriceInitial != 159.99)
Data_final=Data_final %>% filter(PriceInitial != 99.99)

#Move QueryName to side bar
row.names(Data_final) = Data_final[,1]
Data_final = Data_final[,-1]

plot(Data_final$SteamSpyOwnersVariance, Data_final$Metacritic) #Check for colinearity? ????


#Reformat Language Column
language_options <- c("English", "French", "German", "Italian", "Spanish", "Simplified Chinese", "Traditional Chinese", "Korean", "Russian", "Dutch", "Danish", "Finnish", "Japanese", "Norwegian", "Polish", "Portuguese", "Portuguese-Brazil", "Swedish", "Thai", "Turkish", "Czech", "Hungarian", "Romanian", "Ukrainian", "Slovakian", "Arabic")
numlanguages <- vector()

for (text in Data_final$SupportedLanguages) {
  counter <- 0
  
  for (option in language_options) {
    if (grepl(option, text)) {
      counter <- counter + 1
    }
  }
  
  numlanguages <- c(numlanguages, counter)
}

Data_final$NumLanguages=numlanguages
#removing list of languages after finding number of languages per game
Data_final = Data_final[,-44]

#as.factor
Data_final[,14] = as.factor(ifelse(Data_final[,14]=="True",1,0))
Data_final[,15] = as.factor(ifelse(Data_final[,15]=="True",1,0))
Data_final[,16] = as.factor(ifelse(Data_final[,16]=="True",1,0))
Data_final[,17] = as.factor(ifelse(Data_final[,17]=="True",1,0))
Data_final[,18] = as.factor(ifelse(Data_final[,18]=="True",1,0))
Data_final[,19] = as.factor(ifelse(Data_final[,19]=="True",1,0))
Data_final[,20] = as.factor(ifelse(Data_final[,20]=="True",1,0))
Data_final[,21] = as.factor(ifelse(Data_final[,21]=="True",1,0))
Data_final[,22] = as.factor(ifelse(Data_final[,22]=="True",1,0))
Data_final[,23] = as.factor(ifelse(Data_final[,23]=="True",1,0))
Data_final[,24] = as.factor(ifelse(Data_final[,24]=="True",1,0))
Data_final[,25] = as.factor(ifelse(Data_final[,25]=="True",1,0))
Data_final[,26] = as.factor(ifelse(Data_final[,26]=="True",1,0))
Data_final[,27] = as.factor(ifelse(Data_final[,27]=="True",1,0))
Data_final[,28] = as.factor(ifelse(Data_final[,28]=="True",1,0))
Data_final[,29] = as.factor(ifelse(Data_final[,29]=="True",1,0))
Data_final[,30] = as.factor(ifelse(Data_final[,30]=="True",1,0))
Data_final[,31] = as.factor(ifelse(Data_final[,31]=="True",1,0))
Data_final[,32] = as.factor(ifelse(Data_final[,32]=="True",1,0))
Data_final[,33] = as.factor(ifelse(Data_final[,33]=="True",1,0))
Data_final[,34] = as.factor(ifelse(Data_final[,34]=="True",1,0))
Data_final[,35] = as.factor(ifelse(Data_final[,35]=="True",1,0))
Data_final[,36] = as.factor(ifelse(Data_final[,36]=="True",1,0))
Data_final[,37] = as.factor(ifelse(Data_final[,37]=="True",1,0))
Data_final[,38] = as.factor(ifelse(Data_final[,38]=="True",1,0))
Data_final[,39] = as.factor(ifelse(Data_final[,39]=="True",1,0))
Data_final[,40] = as.factor(ifelse(Data_final[,40]=="True",1,0))
Data_final[,41] = as.factor(ifelse(Data_final[,41]=="True",1,0))
Data_final[,42] = as.factor(ifelse(Data_final[,42]=="True",1,0))


##looking for insignificant variables
Data_reg=Data_final

sapply(Data_reg, levels)
table(Data_reg$GenreIsMassivelyMultiplayer)
table(Data_reg$GenreIsRacing)
table(Data_reg$GenreIsSports)
table(Data_reg$GenreIsFreeToPlay)
table(Data_reg$GenreIsEarlyAccess) #insignficant
table(Data_reg$GenreIsSimulation)
table(Data_reg$GenreIsRPG)
table(Data_reg$GenreIsStrategy)
table(Data_reg$GenreIsCasual)
table(Data_reg$GenreIsAdventure)
table(Data_reg$GenreIsAction)
table(Data_reg$GenreIsIndie)
table(Data_reg$CategoryVRSupport)
table(Data_reg$CategoryIncludeLevelEditor)
table(Data_reg$CategoryIncludeSrcSDK) #insignificant
table(Data_reg$CategoryInAppPurchase)
table(Data_reg$CategoryMMO)
table(Data_reg$CategoryCoop)      
table(Data_reg$CategoryMultiplayer)
table(Data_reg$CategorySinglePlayer)
table(Data_reg$MacReqsHaveRec)
table(Data_reg$MacReqsHaveMin)      
table(Data_reg$LinuxReqsHaveMin)
table(Data_reg$LinuxReqsHaveRec)
table(Data_reg$PCReqsHaveMin) #insignificant
table(Data_reg$PCReqsHaveRec)
table(Data_reg$PlatformLinux)
table(Data_reg$PlatformMac)
table(Data_reg$SubscriptionAvail) #insignifcant
table(Data_reg$PurchaseAvail)
table(Data_reg$IsFree)
table(Data_reg$ControllerSupport)


########Regression Analysis########
##setting training and validation set
n=nrow(Data_final)
set.seed(12345)
trainindex_reg=sample(n,n*.8,replace=FALSE)
training_reg=Data_reg[trainindex_reg,]
validation_reg=Data_reg[-trainindex_reg,]

#multiple linear regression nmodel with all the predictors.
reg_all=lm(Metacritic~.,training_reg)
summary(reg_all)

PredBase=predict(reg_all,validation_reg)


#A base model with only the intercept as the starting model for Forward Selection method
reg_null=lm(Metacritic~1,training_reg)
summary(reg_null)

#Stepwise Regression
#Forward Selection
reg_forward=step(reg_null,scope=list(upper=reg_all),direction='forward')
summary(reg_forward)

#Backward Selection
reg_backward=step(reg_all,direction='backward')
summary(reg_backward)

#Stepwise selection
reg_both=step(reg_all,direction='both')
summary(reg_both)

#Check prediction power/accuracy of the models over validation dataset
#Predicting the validation dataset

PredBase=predict(reg_all,validation_reg)
PredForward=predict(reg_forward,validation_reg)
PredBackward=predict(reg_backward,validation_reg)
PredBoth=predict(reg_both,validation_reg)

library(forecast)
accuracy(PredBase,validation_reg$Metacritic)
accuracy(PredForward,validation_reg$Metacritic)
accuracy(PredBackward,validation_reg$Metacritic)
accuracy(PredBoth,validation_reg$Metacritic)

#######KNN#######
Data_knn=Data_final

#### Split the data ####
#We're putting 80% of the data into training and the rest in validation

n = nrow(Data_knn)
set.seed(413) #Setting a fix seed to make the results reproducible
trainIndex_knn = sample(n,0.8*n)
training_knn = Data_knn[trainIndex_knn,]
validation_knn = Data_knn[-trainIndex_knn,]

trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3)

fit <- train(Metacritic ~.,
             data = training_knn,
             tuneGrid = expand.grid(k=1:80),
             method = 'knn',
             #metric = 'Rsquared',
             trControl = trControl,
             preProc = c('center', 'scale'))

# Model Performance
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = validation_knn)
RMSE(pred, validation_knn$Metacritic)
MAE(pred, validation_knn$Metacritic)
plot(pred ~ validation_knn$Metacritic)


#################Regression Tree###############
Data_tree = Data_final

n = nrow(Data_tree)
set.seed(12345) #Setting a fix seed to make the results reproducible
trainIndex_tree = sample(n,0.8*n)
training_tree = Data_tree[trainIndex_tree,]
validation_tree = Data_final[-trainIndex_tree,]

# automatic prune
model=rpart(Metacritic~.,data=training_tree, method = "anova")
prp(model,type=1,extra=1) #a basic plot

asRules(model) # display the set of rules

printcp(model) #summary of the results

pred_model_tree=predict(model,validation_tree)


#Calculate measures all at once and saving them
measures_model_tree = accuracy(pred_model_tree,validation_tree$Metacritic)
measures_model_tree

# unpruned model
stoppingRules = rpart.control(cp = 0, minsplit=2, minbucket=1)
unpruned=rpart(Metacritic~.,data=training_tree,control=stoppingRules)
prp(unpruned,type=1,extra=1)
printcp(unpruned)

pred_unpruned=predict(unpruned,validation_tree)
measures_unpruned=accuracy(validation_tree$Metacritic,pred_unpruned)
measures_unpruned

#prune 1
stoppingRules1=rpart.control(cp= 6.6326e-03 ,minsplit=2,minbucket=1)
prune1=rpart(Metacritic~.,data=training_tree,control=stoppingRules1)
prp(prune1,type=1,extra=1)
printcp(prune1)


pred_prune1=predict(prune1,validation_tree)
measures_pruned = accuracy(validation_tree$Metacritic,pred_prune1)
measures_pruned


#*********************************************************************

######Classification#####

#Grouping the metacritic scores into 5 categories


Data_final_class = subset(Data_final,select=-c(52))

median(Data_final$Metacritic)
Data_final_class$categories = cut(Data_final_class$Metacritic, 
                                  breaks = c(0,73,100),
                                  labels = c(0,1),
                                  right = FALSE)

##Logistic

Data_logistic = subset(Data_final_class,select=-c(5))
str(Data_logistic)

#### Split the data ####
#We're putting 80% of the data into training and the rest in validation
n = nrow(Data_logistic)
set.seed(12345) #Setting a fix seed to make the results reproducible
trainIndex_class = sample(n,0.8*n)
training_class = Data_logistic[trainIndex_class,]
validation_class = Data_logistic[-trainIndex_class,]

#For running logsitic regression, we use function glm() function by setting the family equal to "binomial"
meta_logistic = glm(categories ~ ., training_class, family="binomial")
summary(meta_logistic)

#Predciting the probability of class memberships
#This estimates the probability of 'belonging' to the class of interest (here, delayed)
#Do not forget the set the type equal to "response"
pred_probs_logistic = predict(meta_logistic, validation_class,type="response")
pred_probs_logistic


#For confusion matrix, we can use function confusion.matrix() from 'SDMTools' package
#The function accuracy() gives us some of the performance measures

#NOTE: Confusion matrix built with this function has actual observations as columns and predictions as rows
#So we can use the transpose function t() to switch rows and columns and have actual observations as rows and predictions as columns
t(confusion.matrix(validation_class$categories,pred_probs_logistic)) 

detach("package:forecast",unload=TRUE)
accu_measures_logistic = accuracy(validation_class$categories,pred_probs_logistic)

#Extracting specific values from accuracy table
accu_measures_logistic$prop.correct
accu_measures_logistic$sensitivity
accu_measures_logistic$specificity
accu_measures_logistic$AUC # 'Area Under the Curve': Refers to ROC curve


#Variable Selection using stepwise regression techniques
category_null = glm(categories ~ 1, training_class, family="binomial") #Build a null model to use in 'forward' part
backward = step(meta_logistic,direction="backward")
forward = step(category_null,scope=list(upper=meta_logistic),direction="forward")
both = step(meta_logistic,direction="both")


#Prediction using the stepwise models. 
pred_probs_backward_class = predict(backward, validation_class,type="response")
pred_probs_forward_class = predict(forward, validation_class,type="response")
pred_probs_both_class = predict(both, validation_class,type="response")

accu_measures_logistic_back = accuracy(validation_class$categories,pred_probs_backward_class)
accu_measures_logistic_back$prop.correct
accu_measures_logistic_back$sensitivity
accu_measures_logistic_back$specificity
accu_measures_logistic_back$AUC # 'Area Under the Curve': Refers to ROC curve

accu_measures_logistic_both = accuracy(validation_class$categories,pred_probs_both_class)
accu_measures_logistic_both$prop.correct
accu_measures_logistic_both$sensitivity
accu_measures_logistic_both$specificity
accu_measures_logistic_both$AUC # 'Area Under the Curve': Refers to ROC curve

accu_measures_logistic_forward = accuracy(validation_class$categories,pred_probs_forward_class)
accu_measures_logistic_forward$prop.correct
accu_measures_logistic_forward$sensitivity
accu_measures_logistic_forward$specificity
accu_measures_logistic_forward$AUC # 'Area Under the Curve': Refers to ROC curve

################ Performance Curves #################
#Sensitivity analysis and comparative measures using plots
#In all these plots, if you want to add a curve to the already-exisiting curve:
# --> write a new plot statement and set the arguemnt 'add' equal to TRUE.
#Overlaying plots is very practical when comparing performance of different model
pred_logistic_for_plot = prediction(pred_probs_logistic,validation_class$categories)
pred_forward_for_plot = prediction(pred_probs_forward_class,validation_class$categories)
pred_backward_for_plot = prediction(pred_probs_backward_class,validation_class$categories)
pred_both_for_plot = prediction(pred_probs_both_class,validation_class$categories)

acc_logistic = performance(pred_logistic_for_plot,"acc")
acc_forward = performance(pred_forward_for_plot,"acc")
acc_backward = performance(pred_backward_for_plot,"acc")
acc_both = performance(pred_both_for_plot,"acc")

plot(acc_logistic,main = "Accuracy for different cutoffs")
plot(acc_forward,main = "Accuracy for different cutoffs")
plot(acc_both,main = "Accuracy for different cutoffs")
plot(acc_backward,main = "Accuracy for different cutoffs")

#Adding the result of the model called "both" to ROC curve for comparison to the full model
roc_logistic = performance(pred_logistic_for_plot,"tpr","fpr")
roc_both = performance(pred_both_for_plot,"tpr","fpr")
roc_backward = performance(pred_backward_for_plot,"tpr","fpr")
roc_forward = performance(pred_forward_for_plot,"tpr","fpr")

plot(roc_logistic, main = "ROC Chart")
plot(roc_both, add=TRUE,col="red") #NOTE: We ser add=TRUE to overlay this on the last plot!
plot(roc_forward, add=TRUE,col="brown")
plot(roc_backward, add=TRUE,col="yellow")
lines(x=c(0,1),y=c(0,1),lty=3)


#####KNN Classification ####

Data_cknn = subset(Data_final_class,select=-c(5))

#### Split the data ####
set.seed(12345) #Setting a fix seed to make the results reproducible
n = nrow(Data_cknn)
trainIndex_cknn = sample(n,0.8*n) #Generate a random index sample of size 18
training_cknn = Data_cknn[trainIndex_cknn,]
validation_cknn = Data_cknn[-trainIndex_cknn,]

variableChange = c(1:48)
Data_cknn[variableChange] <- lapply(Data_cknn[variableChange] , as.numeric)
str(Data_cknn)

trctrl_cnn <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(categories ~., 
                 data = training_cknn, 
                 tuneGrid = expand.grid(k=1:80),
                 method = "knn",
                 trControl=trctrl_cnn,
                 preProcess = c("center", "scale"))
knn_fit
plot(knn_fit)
test_pred_cknn <- predict(knn_fit, newdata = validation_cknn)
test_pred_cknn
t(confusion.matrix(validation_cknn$categories,test_pred_cknn)) 

detach("package:forecast",unload=TRUE)
accu_measures_cknn = accuracy(validation_cknn$categories,test_pred_cknn)

#Extracting specific values from accuracy table
accu_measures_cknn$prop.correct
accu_measures_cknn$sensitivity
accu_measures_cknn$specificity
accu_measures_cknn$AUC # 'Area Under the Curve': Refers to ROC curve

