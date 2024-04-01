# Installing and Loading the packages
library(dplyr)
library(corrplot)
library(GGally)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(tree)
library(plotly)
library(ggcorrplot)
library(glmnet)
library(Metrics)
library(utils)
library(psych)
library(skimr)
library(wesanderson)
library(visdat)
library(grid)
library(gridExtra)
library(webr)
library(HH)
library(caret)
library(pROC)

# Importing the dataset
churn<-read.csv("churn_Modelling.csv")

# Checking for NA values
complete.cases(churn)
which(!complete.cases(churn))
vis_miss(churn)
sum(is.na(churn))
sum(is.null(churn))

# Checking for Duplication
duplicated(churn)
anyDuplicated(churn)

# Rounding the values for Balance and Estimated Salary
churn$Balance<-round(churn$Balance,0)
churn$EstimatedSalary<-round(churn$EstimatedSalary,0)

# Data Manipulation
churn$Status<-ifelse(churn$Exited=="1","Churned","Retained")
churn<-churn %>% mutate(HasCrCard = recode(HasCrCard, '0'='No', '1'='Yes'))
churn<-churn %>% mutate(IsActiveMember = recode(IsActiveMember, '0'='Inactive', '1'='Active'))

# Changing the column names
colnames(churn)[11]<-"Credit_Card"
colnames(churn)[12]<-"Membership"

# Changing datatypes to factors
churn$Geography<-as.factor(churn$Geography)
churn$Gender<-as.factor(churn$Gender)
churn$Credit_Card<-as.factor(churn$Credit_Card)
churn$Membership<-as.factor(churn$Membership)
churn$Status<-as.factor(churn$Status)
churn$Tenure<-as.factor(churn$Tenure)
churn$NumOfProducts<-as.factor(churn$NumOfProducts)

# Dropping column Rownumber
drop<-c("RowNumber")
churn<-churn[,!(names(churn) %in% drop)]

# Understanding the dataset
str(churn)
View(churn)
glimpse(churn)
skim(churn)
headTail(churn,5)
dim(churn)
summary(churn)
describe(churn)
describeBy(churn,group="Geography")
describeBy(churn,group="Gender")

# EDA
# Histograms
hist5<-ggplot(churn, aes(x=CreditScore, fill=Status),labels=TRUE) + geom_histogram(binwidth=50, alpha=.5,colour="black") + scale_x_continuous(breaks=0:5)+ggtitle("Plot 1: Histogram of Credit Score by Status")+ geom_vline(aes(xintercept=mean(CreditScore, na.rm=T)),color="red", linetype="dashed", size=1)
hist6<-ggplot(churn, aes(x=Age, fill=Status),labels=TRUE) + geom_histogram(binwidth=5, alpha=.5,colour="black") + scale_x_continuous(breaks=0:5)+ggtitle("Plot 2: Histogram of Age by Status")+ geom_vline(aes(xintercept=mean(Age, na.rm=T)),color="red", linetype="dashed", size=1) 
hist7<-ggplot(churn, aes(x=Balance, fill=Status),labels=TRUE) + geom_histogram(binwidth=10000, alpha=.5,colour="black") + scale_x_continuous(breaks=0:5)+ggtitle("Plot 3: Histogram of balance by Status")+ geom_vline(aes(xintercept=mean(Balance, na.rm=T)),color="red", linetype="dashed", size=1) 
hist8<-ggplot(churn, aes(x=EstimatedSalary, fill=Status),labels=TRUE) +geom_histogram(binwidth=10000, alpha=.5,colour="black") +scale_x_continuous(breaks=0:5)+ggtitle("Plot 4: Histogram of Estimated Salary by Status")+ geom_vline(aes(xintercept=mean(EstimatedSalary, na.rm=T)),color="red", linetype="dashed", size=1) 
grid.arrange(hist5,hist6,hist7,hist8,top=textGrob("Histograms of Variables"))

# QQplots
qqnorm(churn$CreditScore, pch = 1, frame = FALSE,main="Q-Q Plot (Credit score)")
qqline(churn$CreditScore, col = "yellowgreen", lwd = 2)
qqnorm(churn$Age, pch = 1, frame = FALSE,main="Q-Q Plot (Age)")
qqline(churn$Age, col = "cornflowerblue", lwd = 2)
qqnorm(churn$Balance, pch = 1, frame = FALSE,main="Q-Q Plot (Balance)")
qqline(churn$Balance, col = "tomato2", lwd = 2)
qqnorm(churn$EstimatedSalary, pch = 1, frame = FALSE,main="Q-Q Plot (Estimated Salary)")
qqline(churn$EstimatedSalary, col = "mediumorchid1", lwd = 2)

# Barplots
# Barplot 0 for Status
ggplot(churn, aes(x=Status, fill=Status)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_manual(values=c("#56B4E9", "#E69F00"))
# Barplot 1 for Geography
ggplot(churn, aes(x=Geography, fill=Geography)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest1"))
# Barplot 2 for gender
ggplot(churn, aes(x=Gender, fill=Gender)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_brewer(palette="Accent")
# Barplot 3 for HasCrCard
ggplot(churn, aes(x=Credit_Card, fill=Credit_Card)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_brewer(palette="Set1")
# Barplot 4 for Tenure
ggplot(churn, aes(x=Tenure, fill=Tenure)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_brewer(palette="Set3")
# Barplot 5 for NumofProducts
ggplot(churn, aes(x=NumOfProducts, fill=NumOfProducts)) + geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)+scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"))

# Piecharts
## categorical pie chart = HasCrcard
pie1 <- churn %>% group_by(Status, Credit_Card) %>% summarize(Freq=n())
PieDonut(pie1, aes(Credit_Card, Status, count=Freq), title = "Churned by Credit_Card")

## categorical pie chart = IsActiveMember
pie2 <- churn %>% group_by(Status, Membership) %>% summarize(Freq=n())
PieDonut(pie2, aes(Membership, Status, count=Freq), title = "Churned by IsActiveMember")

# Age with his & box
Age.hist <- ggplot(churn, aes(x=Age, fill=Status, color=Status)) + geom_histogram(position="identity", alpha=0.5)+
  theme(axis.title.x=element_blank())+ theme(legend.position = c(0.9, 0.5))
Age.box <- ggplot(churn, aes(x=Age, y=Status, fill=Status)) +  geom_boxplot(alpha=0.5)+theme(legend.position = "none")
grid.arrange(Age.hist,Age.box,nrow=2)

## Balance with his & box
B.hist <- ggplot(churn, aes(x=Balance, fill=Status, color=Status)) +
  geom_histogram(position="identity", alpha=0.5)+
  theme(axis.title.x=element_blank())+ theme(legend.position = c(0.9, 0.5))
B.box <- ggplot(churn, aes(x=Balance, y=Status, fill=Status)) +
  geom_boxplot(alpha=0.5)+theme(legend.position = "none")
grid.arrange(B.hist,B.box,nrow=2)

## Estimated Salary with his & box
S.hist <- ggplot(churn, aes(x=EstimatedSalary, fill=Status, color=Status)) +
  geom_histogram(position="identity", alpha=0.5)+
  theme(axis.title.x=element_blank())+theme(legend.position = c(0.9, 0.8))
S.box <- ggplot(churn, aes(x=EstimatedSalary, y=Status, fill=Status)) +
  geom_boxplot(alpha=0.5)+ theme(legend.position = "none")
grid.arrange(S.hist,S.box,nrow=2)

# 3d plot
plot_ly(churn, x = ~Age, y = ~EstimatedSalary, z = ~CreditScore,mode = 'markers',marker = list(size = 6),color = ~Status,alpha=0.9)

# Stacked bargraph
df1<-churn %>% group_by(Geography,Status) %>% summarise(count = n())
ggplot(df1, aes(y=count, x=Geography,fill=Status)) + geom_bar(position="stack",stat="identity") + scale_fill_manual(values=wes_palette(n=2, name="FantasticFox1"))
df2<-churn %>% group_by(Gender,Status) %>% summarise(count = n())
ggplot(df2, aes(y=count, x=Gender,fill=Status)) + geom_bar(position="stack",stat="identity") +  scale_fill_manual(values=wes_palette(n=2, name="Cavalcanti1"))

# Correlation Matrix
corr <- select_if(churn, is.numeric)
cormatrix<-round(cor(corr,method = "pearson"),digits=2)
ggcorrplot(cor(corr,use = "complete.obs"),lab = TRUE,hc.order = TRUE)
ggpairs(corr)

# Hypothesis Testing
# One sample test
# null hypothesis: mean credit score is less than 600
# alternate hypothesis: mean credit score is <= 600
t.test(churn$CreditScore, mu=600, alternative = "less")

#null hypothesis: mean age is less than 50
#alternate hypothesis: mean age is >= 50
t.test(churn$Age, mu=50, alternative = "greater")

# Two Sample t test
# null hypothesis: both male and female have same mean credit score.
# alternate hypothesis: both male and female do not have same mean credit score.
Male<-subset(churn,Gender=="Male")
Female<-subset(churn,Gender=="Female")
t.test(Male$CreditScore, Female$CreditScore, var.equal=TRUE)

# F test
# null hypothesis: both male and female have same mean salary.
# alternate hypothesis: both male and female do not have same mean salary.
var.test(Male$EstimatedSalary, Female$EstimatedSalary, alternative = "two.sided")

# One-way Anova
# Null: There is no difference in mean of CreditScore according to Geography
# Alternative: At least one mean is different from the others (claim).
fit <- aov(Balance ~ Geography, data=churn)
summary(fit)
fit.tukey <- TukeyHSD(fit)

opar <- par(no.readonly = TRUE)
par(fig=c(0.2, 1, 0, 1))
plot(fit.tukey, col="blue", las=1)
par(opar)

balance.geo <- churn %>% group_by(Geography) %>% 
  summarise(mean_Balance=mean(Balance),
            .groups = 'drop')
balance.geo

# Two-way Anova
# Null: There is no difference in mean of CreditScore according to Geography
# Alternative: At least one mean is different from the others (claim).
fit2 <- aov(Balance ~ Exited*Gender, data=churn)
summary(fit2)
attach(churn)

#plots
interaction2wt(Balance ~ Exited*Gender, data=churn)
detach(churn)

# Linear regression
cor(corr,use = "complete.obs") 
linear_model <- lm (Exited~Balance+Age+NumOfProducts, data = churn)
summary(linear_model)

#Logistic Regression 
set.seed(3456) 
split.data1 <- createDataPartition(churn$Exited, p = 0.7, list = FALSE, times = 1) 
data_train <- churn[ split.data1,] 
data_test <- churn[-split.data1,] 

logistic.m1 <- glm(Exited~CreditScore + Geography + Gender + Age + Tenure +
                     Balance + NumOfProducts + Credit_Card + Membership + EstimatedSalary, data=data_train, family = "binomial")
summary(logistic.m1)

logistic.m2 <- glm(Exited~Balance + Age + Membership + Gender , data=data_train, family = "binomial")
summary(logistic.m2)

#Confusion matrix
#train data
prob.train <- predict(logistic.m2, newdata=data_train, type="response")
predicted <- as.factor(ifelse(prob.train>=0.5, "Yes", "No"))
data_train$Exited <- as.factor(data_train$Exited)
data_train$Exited <- factor(ifelse(data_train$Exited==1, "Yes", "No"))
confusionMatrix(predicted, data_train$Exited, positive = "Yes")

#test data
prob.test <- predict(logistic.m2, newdata = data_test, type="response")                
predicted1<- as.factor(ifelse(prob.test>=0.5, "Yes", "No"))                
data_test$Exited <- as.factor(data_test$Exited)
data_test$Exited <- factor(ifelse(data_test$Exited==1, "Yes", "No"))
confusionMatrix(predicted1, data_test$Exited, positive = "Yes")

ROC <- roc(data_train$Exited, prob.train)
plot(ROC, col="red", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

#AUC
AUC1 <- auc(ROC)

# Importing the dataset again
churn2<-read.csv("churn_Modelling.csv")

# LASSO Logistic Regression 
set.seed(3456) 
str(churn2)
drop<-c("CustomerId", "Surname","RowNumber")
churn2<-churn2[,!(names(churn2) %in% drop)]

split.data1 <- createDataPartition(churn2$Exited, p = 0.7, list = FALSE, times = 1) 
data_train2 <- churn2[ split.data1,] 
data_test2 <- churn2[-split.data1,] 
train_x <- model.matrix(Exited~.,data_train2)
train_y <- data_train2$Exited

test_x <- model.matrix(Exited~.,data_test2)
test_y <- data_test2$Exited

cv.out <- cv.glmnet(train_x,train_y,alpha=1, family="binomial",type.measure = "mse")
summary(cv.out)
plot(cv.out)

# optimal value of lambda; minimizes the prediction error
# lambda min - minimizes out of sample loss
# labmda 1se - largest value of lambda within 1 standard error of lambda min 
log(cv.out$lambda.min)
log(cv.out$lambda.1se)

cv.out$lambda.min
cv.out$lambda.1se

##########
# Fit models based on lambda
##########

# Fit the finla model on the training data using lambda.min 
# alpha = 1 for Lasso (L1)
# alpha = 0 for Ridge (L2)
lasso.model.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.out$lambda.min)
lasso.model.min

# Display regression coefficients
coef(lasso.model.min)

# Fit the final model on the training data using lambda.1se
lasso.model.1se <- glmnet(train_x, train_y, alpha =1, lambda = cv.out$lambda.1se)
lasso.model.1se

# Display regression coefficients
coef(lasso.model.1se)

# Ridge Logistic Regression 
set.seed(3456) 
str(churn2)

cv.out2 <- cv.glmnet(train_x,train_y,alpha=0, family="binomial",type.measure = "mse")
summary(cv.out2)
plot(cv.out2)

# optimal value of lambda; minimizes the prediction error
# lambda min - minimizes out of sample loss
# labmda 1se - largest value of lambda within 1 standard error of lambda min 
log(cv.out2$lambda.min)
log(cv.out2$lambda.1se)
cv.out2$lambda.min
cv.out2$lambda.1se

##########
# Fit models based on lambda
##########

# Fit the finla model on the training data using lambda.min 
# alpha = 1 for Lasso (L2)
# alpha = 0 for Ridge (L1)
ridge.model.min <- glmnet(train_x, train_y, alpha = 0, lambda = cv.out$lambda.min)
ridge.model.min

# Display regression coefficients
coef(ridge.model.min)

# Fit the final model on the training data using lambda.1se
ridge.model.1se <- glmnet(train_x, train_y, alpha =0, lambda = cv.out$lambda.1se)
ridge.model.1se

# Display regression coefficients
coef(ridge.model.1se)

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
D.tree = rpart(Exited ~Age+Balance+Geography+IsActiveMember+NumOfProducts, data = data_train, method = "class")
printcp(D.tree)
prp(D.tree, type = 2, extra = 1, under = TRUE, split.font = 2,border.col = 2, varlen = 0)
