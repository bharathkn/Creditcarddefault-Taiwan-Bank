#Set working directory
getwd()
setwd("X:/Workspace")

#Read data to a variable
CustData <- read.csv("Taiwan-Customer defaults.csv", header = TRUE)
str(CustData)
View(CustData)
library(corrplot)
library(ggplot2)
library(psych)
library(dplyr)
library(car)
library(nFactors)
attach(CustData)
###################################################


Custcor=cor(CustData)
corrplot(Custcor, method = "number")
Custcor
corrplot(Custcor)
summary(CustData)
###################################################

(imbalance <- table(CustData$default.payment.next.month))

###########################################

#Converting selected variables to factor and then defining levels for subsequent factors. 
CustData$SEX<-as.factor(CustData$SEX)
levels(CustData$SEX) <- c("Male", "Female")
CustData$EDUCATION <- as.factor(CustData$EDUCATION) 
levels(CustData$EDUCATION) <- c("unknown","Graduate", 
                                "university",
                                "high school", 
                                "others", 
                                "unknown", 
                                "unknown")

CustData$MARRIAGE <- as.factor(CustData$MARRIAGE)
levels(CustData$MARRIAGE) <- c("unknown","married"
                               ,"single","others")

View(CustData)
CustData$Sept <- as.factor(CustData$Sept)
CustData$Aug <- as.factor(CustData$Aug)
CustData$Jul <- as.factor(CustData$Jul)
CustData$Jun <- as.factor(CustData$Jun)
CustData$May <- as.factor(CustData$May)
CustData$Apr <- as.factor(CustData$Apr)
CustData$default.payment.next.month <- as.factor(CustData$default.payment.next.month)
View(CustData)

#Checking the imbalance in data
(imbalance <- table(CustData$default.payment.next.month))

View(CustData)

#Using str and summary for deeper understanding of the variables
head(CustData)
str(CustData)
summary(CustData)
dim(CustData)

#For the ease of using variables
attach(CustData)


#Checking if we have any NA's in the data, sneaky guys that these guys are, we need a special check for them.
#Yay! No Na's. 
sum(is.na(CustData))

#Loading the library for plots
library(dplyr)
library(ggplot2)

#Checking distributions by Gender
ggplot(data=CustData,aes(x=CustData$SEX, fill=SEX)
       )+geom_histogram(stat="count")+
  labs(title="By Gender", x="Gender", fill="Gender")+
  scale_fill_manual(values=c("RED", "Green"))+
  theme(axis.text.x = element_text(angle=90,hjust=1))

#Checking distribution by education level
ggplot(data=CustData, aes(x=EDUCATION,fill=EDUCATION))+
  geom_histogram(stat="count")+
  labs(title="By Education", 
       x="Education level", fill="Education level")+
  scale_fill_manual(values=c("orange", "Yellow", "purple","green", "black"))+
  theme(axis.text.x = element_text(angle=90, hjust=1))

#check the general distribution based on Marital status
ggplot(data=CustData, aes(x=MARRIAGE,fill=MARRIAGE))+
  geom_histogram(stat="count")+
  labs(title=" Marriage wise distribution", 
       x="Marital status", fill="Marital status")+
  theme(axis.text.x = element_text(angle=90, hjust=1))


#Lets check the type of audience we are catering to?
ggplot(data=CustData, aes(x=EDUCATION,fill=SEX))+ 
  geom_bar(position='dodge')+
  labs(title = "Distribution by Education & Gender",
       x ="EDUCATION",fill = "GENDER")+
  scale_fill_manual(values=c("pink", "violet"))+
  theme(axis.text.x = element_text(angle = 90,hjust=1))




#How are the categories weigh up to defaults?
ggplot(data=CustData, aes(x=SEX,
                          fill=default.payment.next.month))+
  geom_histogram(stat="count")+
  labs(title=" Gender vs Default ", 
       x="Gender", fill="Default")+
  theme(axis.text.x = element_text(angle=45, hjust=1))


#Let's check on how marital status has an impact on the defaulters?
ggplot(data=CustData, aes(x=MARRIAGE,
                          fill=default.payment.next.month))+
  geom_histogram(stat="count")+
  labs(title=" Marital status vs Default ", 
       x="Marital status", fill="Default")+
  theme(axis.text.x = element_text(angle=45, hjust=1))


#DoesEducation & Gender have a pattern when it comes to Defaulting?
ggplot(data=CustData, aes(x=EDUCATION,fill=default.payment.next.month))+ 
  geom_histogram(position='dodge', stat="count")+
  facet_grid(SEX~.)+
  labs(title = "Distribution by Education & Gender", x ="EDUCATION",fill = "Default")+
  scale_fill_manual(values=c("violet", "Pink"))+
  theme(axis.text.x = element_text(angle = 90,hjust=1))


#Does Age and gender have anything to do with defaults?
ggplot(data=CustData, aes(x=AGE,fill=default.payment.next.month))+ 
  geom_histogram( stat="count")+
  facet_grid(EDUCATION~SEX)+
  labs(title = "Distribution of defaulters by Age", x ="EDUCATION",fill = "Default")+
  scale_fill_manual(values=c("Orange", "Yellow"))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))


#Dividing into two subsets to do EDA based on the default:: NOTE:: We have used this subsets to understand in depth the charecteristics of the group of defaulters and non defaulters. 
NoDefaults <- CustData %>%
  subset(default.payment.next.month==0)
View(NoDefaults)  
YesDefaults <- CustData %>%
  subset(default.payment.next.month==1)

#Plot to check the range of the age group defaulting: NOTE:: This data only includes people who have defaulted. 
ggplot(data = YesDefaults, aes(x = AGE)) + 
  geom_histogram(bins = 50, fill = "orange", 
                 col = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)



#Credit limit given across age, what better way than to plot a scatter plot (For Default)
ggplot(data = CustData, aes(x = AGE, y = LIMIT_BAL))+
  geom_count(col="magenta", show.legend=F) +
  labs(title="Counts Plot", 
       subtitle="Age Vs Credit Amount", 
       caption="source: Taiwan customer default")


library(GGally)

ggcorr(CustData, geom = "blank", label = TRUE, hjust = 1) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)+
  ggtitle("Correlation Plot") +
  theme(axis.text.x=element_blank())



#Making feature boxplot across all the variables
boxplot(CustData)


#Let's check the overall default pattern in our data
ggplot(CustData, aes(y= LIMIT_BAL,
                     x= default.payment.next.month))+
  geom_boxplot(notch = FALSE, 
               outlier.shape=1, 
               outlier.colour="red" 
               )+
  stat_summary(fun.y = mean, geom="line",aes(group=1))+
  stat_summary(fun.y = mean, geom="point")



#Checking the overall customer repayment history then we compare this with customers who default. 
#For the love of programming
pay.cols.names <- c("Apr", "May", "Jun", "Jul",
                    "Aug", "Sept")


library(Rmisc)
library(ggpubr)
library(gridExtra)
library(grid)

#Defining a function which will write these 6 graphs. Post that we will use a graph aggregation function to list all them all together. 
pay.histograms <- 
  lapply(pay.cols.names, 
         function (pay.col.name){ 
           ggplot(data = CustData[, pay.cols.names], 
                  aes(x = CustData[, pay.col.name],fill=CustData$default.payment.next.month)) +
             geom_bar(stat = "count") + 
             theme_minimal() +
             theme(legend.position = "none")+
             xlab(paste0("Repayment status ", pay.col.name))+
             ylab("Observations count with Defaults")
         })
multiplot(plotlist=pay.histograms, cols = 3)



#checking Education profiles with respect to Default next month
ggplot(data=CustData, aes(x=EDUCATION,
                          fill=default.payment.next.month))+
  geom_histogram(stat="count")+
  labs(title=" Education vs Default ", 
       x="Education", fill="Default")+
  theme(axis.text.x = element_text(angle=45, hjust=1))


#Fun fact: Violin graph is a way to represent numeric data. It is somehow similar to that of a boxplot but with a rotated density plot on each side. 
#Violin plot provide us the best of both the world and thus, we shall hop onto to that for the analysis. 


#plotting a violin graph between Education and Credit Amount to understand the density of data
ggplot(data=CustData, aes(x=EDUCATION, y=LIMIT_BAL,
                          color= EDUCATION, 
                          fill= EDUCATION))+
  geom_violin()+
  labs(title= "Violin plot",
       subtitle = "Education vs Balance limit",
       caption= "source:Taiwan customer default ")+
  theme(axis.text.x = element_text(angle=45, vjust=0.6))
  



ggplot(data=CustData, aes(x=MARRIAGE, y=LIMIT_BAL,
                          color= MARRIAGE, 
                          fill= MARRIAGE))+
  geom_violin()+
  labs(title= "Violin plot",
       subtitle = "Marital status vs Balance limit",
       caption= "source:Taiwan customer default")+
  theme(axis.text.x = element_text(angle=45, vjust=0.6))


ggplot(data = CustData,aes(x = default.payment.next.month, y = LIMIT_BAL,
                                 fill = default.payment.next.month,
                                 color = default.payment.next.month )) +
  geom_violin() +
  labs(title="Violin Plot", 
       subtitle="Default Payment Vs Credit Amount", 
       caption="source: Taiwan customer defaults")


#Lets see if there are any overlapping of the defaulters wrt to the non defaulters. We use this graph in the future as a checkopint to ensure that the model built is able to distinguish, thus a graph with no overlapping will mean that there is a clear distinction between the defaulters and non defaulters. 
library(caret)
featurePlot(x = LIMIT_BAL,                       
            y = default.payment.next.month,    
            plot = "density",                         
            auto.key = T)


# Balance limits by gender and education
d1 <- ggplot(CustData, aes(SEX, (LIMIT_BAL/1000), fill=EDUCATION)) + 
  geom_boxplot() +
  xlab("Gender") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Accent")

# Balance limits by education and gender
d2 <- ggplot(CustData,aes(EDUCATION, (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("BLimit(x1000 NT$)") + 
  scale_fill_brewer(palette = "Paired")


grid.arrange(d1, d2)

ggplot(aes(x = CustData$LIMIT_BAL/1000), data = CustData) +
  geom_histogram(aes(fill = CustData$default.payment.next.month)) +
  xlab("Balance Limit x 1000") +
  ylab("Count") +
  scale_fill_discrete(name="Default Payment Next Month",
                      breaks=c(0, 1),
                      labels=c("No", "Yes")) +
  xlim(c(0,750)) +
  facet_wrap(~EDUCATION)


PAY_VAR<-lapply(CustData[,c("Sept","Aug","Jul","Jun","May","Apr")], function(x) table(x))
print(PAY_VAR)



Outlier<-data.frame(apply(CustData[,c("LIMIT_BAL","BILL_Sep","BILL_Aug","BILL_Jul","BILL_Jun","BILL_May","BILL_Apr",
                                "PAY_Sep","PAY_Aug","PAY_Jul","PAY_Jun","PAY_May","PAY_Apr")],
                          2, function(x) quantile(x, probs = seq(0, 1, by= 0.00001))))
head(Outlier)
tail(Outlier)

CustData<-subset(CustData,!(CustData$LIMIT_BAL> quantile(CustData$LIMIT_BAL, 0.99999) |
                   CustData$BILL_Apr< quantile(CustData$BILL_Apr, 0.00001)))


#Collinearity check

numeric_fields<-c("LIMIT_BAL","BILL_Sep","BILL_Aug","BILL_Jul",
                  "BILL_Jun","BILL_May","BILL_Apr",
                  "PAY_Sep","PAY_Aug","PAY_Jul","PAY_Jun","PAY_May","PAY_Apr")
CustData_numeric<-subset(CustData, select=numeric_fields)
str(CustData_numeric)
library(DataExplorer)
plot_correlation(CustData_numeric)
CustData_numeric <- lapply(CustData_numeric, as.numeric)
str(CustData_numeric)
library(car)
library(psych)
library(usdm)
vif(CustData_numeric)


CustData$PAY_RATIO_APR<-ifelse(is.nan(CustData$PAY_Apr/CustData$BILL_Apr),0,
                         ifelse(is.infinite(CustData$PAY_Apr/CustData$BILL_Apr),
                                0,round(CustData$PAY_Apr/CustData$BILL_Apr,2)))

CustData$PAY_RATIO_MAY<-ifelse(is.nan(CustData$PAY_May/CustData$BILL_May),0,
                         ifelse(is.infinite(CustData$PAY_May/CustData$BILL_May),0,round(CustData$PAY_May/CustData$BILL_May,2)))

CustData$PAY_RATIO_JUNE<-ifelse(is.nan(CustData$PAY_Jun/CustData$BILL_Jun),0,
                          ifelse(is.infinite(CustData$PAY_Jun/CustData$BILL_Jun),0,round(CustData$PAY_Jun/CustData$BILL_Jun,2)))

CustData$PAY_RATIO_JULY<-ifelse(is.nan(CustData$PAY_Jul/CustData$BILL_Jul),0,
                          ifelse(is.infinite(CustData$PAY_Jul/CustData$BILL_Jul),0,round(CustData$PAY_Jul/CustData$BILL_Jul,2)))

CustData$PAY_RATIO_AUG<-ifelse(is.nan(CustData$PAY_Aug/CustData$BILL_Aug),0,
                         ifelse(is.infinite(CustData$PAY_Aug/CustData$BILL_Aug),0,round(CustData$PAY_Aug/CustData$BILL_Aug,2)))

CustData$PAY_RATIO_SEPT<-ifelse(is.nan(CustData$PAY_Sep/CustData$BILL_Sep),0,
                                ifelse(is.infinite(CustData$PAY_Sep/CustData$BILL_Sep),0,round(CustData$PAY_Sep/CustData$BILL_Sep,2)))


numeric_fields<-c("LIMIT_BAL","BILL_Apr","BILL_May","BILL_Jun","BILL_Jul","BILL_Aug",
                  "BILL_Sep","PAY_RATIO_APR","PAY_RATIO_MAY","PAY_RATIO_JUNE",
                  "PAY_RATIO_JULY","PAY_RATIO_AUG","PAY_RATIO_SEPT",
                  "PAY_Apr","PAY_May","PAY_Jun","PAY_May","PAY_Jun","PAY_Jul", "PAY_Sep")
View(CustData)
CustData_numeric<-subset(CustData, select = numeric_fields)

plot_correlation(CustData_numeric)

ggplot(data=CustData,mapping = aes(x=AGE,y=CustData$LIMIT_BAL,
                                   fill=default.payment.next.month)) + geom_boxplot() 
library(ggthemes)
ggplot(data=CustData, mapping = aes(x=MARRIAGE, 
                                    fill=default.payment.next.month)) + geom_bar()+theme_few()

ggplot(data=CustData, mapping = aes(x=SEX, fill=default.payment.next.month)) + geom_bar()+theme_few()

ggplot(data=CustData,mapping = aes(x=EDUCATION,
                                   y=CustData$LIMIT_BAL,fill=default.payment.next.month)) + geom_boxplot()

ggplot(CustData, aes(x = SEX, fill = default.payment.next.month)) + geom_density()

ggplot(CustData, aes(x = EDUCATION, fill = default.payment.next.month)) + geom_density() +
  xlab("Default Payment Status") + ylab("Customer Count") 


CustData %>% group_by(EDUCATION,AGE) %>% summarise(mn_creditlmt=mean(LIMIT_BAL)) -> df
ggplot(df, aes(x=EDUCATION, y=AGE, fill=mn_creditlmt)) + geom_tile() + scale_fill_gradient(low="white", high="steelblue")

ggplot(CustData, aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Age')


ggplot(CustData, aes(x=EDUCATION, fill = default.payment.next.month))+
  geom_bar(width = 1)+
  coord_polar()


ggplot(CustData, aes(x=EDUCATION, fill = SEX))+
  geom_bar(width = 1)+
  coord_polar()

qplot(AGE, data = CustData, geom = "density", fill = default.payment.next.month)

qplot(SEX, data = CustData, geom = "density", fill = EDUCATION)

qplot(AGE, data = CustData, geom = "density", fill = SEX)


#  Trying to see even in such sparse data

featurePlot(x = CustData[, c(3,4,5,2)],    # MathsMarks & scienceteacher
            y = CustData$AGE,    # Develop relationship with y
            plot = "pairs",          # Plot in pairs
            auto.key = T             # Show legend
)

library(ff)
library(ggplot2)
library(tabplot)
tableplot(CustData,
          #sortCol = AGE,    # Sorted on target variable
          select = c(3:7,9,10,15:21,25)) # How these behave vis-a-vis target

tableplot(CustData,
          #sortCol = AGE,    # Target variable
          select = c(1:6,11:14,21,25))






#Building models and diving the dataset into training and test data

set.seed(101)
#Train and Test Data
train_df <- sample_frac(CustData, 0.75)
test_df <- subset(CustData, !(CustData$ID %in% train_df$ID))

#Imbalance check in the dependent variable for the Train and Test data

Imbalance_Check_train_df<-aggregate(ID ~ default.payment.next.month,train_df,length)
colnames(Imbalance_Check_train_df)[2]<-"Client_Count"
Imbalance_Check_train_df$Contribution<-(Imbalance_Check_train_df$Client_Count/sum(Imbalance_Check_train_df$Client_Count))*100
Imbalance_Check_train_df


Imbalance_Check_test_df<-aggregate(ID~default.payment.next.month,test_df,length)
colnames(Imbalance_Check_test_df)[2]<-"Client_Count"
Imbalance_Check_test_df$Contribution<-(Imbalance_Check_test_df$Client_Count/sum(Imbalance_Check_test_df$Client_Count))*100
Imbalance_Check_test_df


#Logistic regression

mod_l <- glm(default.payment.next.month~. -(ID+BILL_Apr+BILL_May+BILL_Jun+BILL_Jul+BILL_Aug
                                        +BILL_Sep+SEX+MARRIAGE), train_df, family=binomial)
summary(mod_l)
predict_l <- predict(mod_l, train_df, type='response')
prob_l <- ifelse(predict_l > 0.5,1,0)

#Confusion matrix
confusion_matrix<-table(prob_l, train_df$default.payment.next.month)
print(confusion_matrix)

#model Accuracy
Accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(Accuracy*100)

library(prediction)
library(ROCR)
pred0 <- prediction(predict_l, train_df$default.payment.next.month)
pred0 <- performance(pred0, "tpr", "fpr")
plot(pred0)



#Step AIC criterion
step_AIC <- stepAIC(mod_l, direction='backward')

#Re-running the model with the best subset obtained
mod_2 <- glm(default.payment.next.month ~ LIMIT_BAL+ EDUCATION + PAY_Apr+ PAY_May
             +PAY_Jun+PAY_Jul+ PAY_Aug+ PAY_Sep +Apr + May + Jun+ Jul + Aug + Sept,
             train_df, family='binomial')
summary(mod_2)

predict_2<-predict(mod_2,train_df,type='response')
prob_2<-ifelse(predict_2>0.5,1,0)

#Confusion Matrix
confusion_matrix<-table(prob_2, train_df$default.payment.next.month)
print(confusion_matrix)

#Model Accuracy
Accuracy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
print(Accuracy*100)

#The ROCR Curve
library(prediction)
library(ROCR)
pred1 <- prediction(predict(mod_2), train_df$default.payment.next.month)
pred1 <- performance(pred1, "tpr", "fpr")
plot(pred1)
auc.tmp<- performance(predict(mod_2),"auc")

#There isn't any significant change in accuracy of the 
#model so we can assume our model performs well. But can we 
#raise the accuracy of the model using some other technique? 
#So let us try the Decision Tree approach

#Decision tree

library(rpart)
set.seed(1234)
mymod<-rpart(default.payment.next.month ~ PAY_Apr+PAY_May+PAY_Jun+PAY_Jul+PAY_Aug+PAY_Sep+LIMIT_BAL+
               EDUCATION+Sept+Aug+Jul+Jun+May+Apr+
               BILL_Sep+BILL_Aug+BILL_Jul+BILL_Jun+BILL_May+BILL_Apr+AGE+SEX+MARRIAGE,
             data= train_df, method="class",
             control = rpart.control(cp = 0.0001,minsplit = 30,  minbucket = 30*2,
                                     maxsurrogate = 5, usesurrogate = 2,xval=10, maxdepth = 30))
printcp(mymod)

#Let's have a look at the variable importance and drop 
#the variables that are not of much importance.

mymod$variable.importance

#Let's re-run the model after dropping the 
#variables "EDUCATION","SEX","MARRIAGE"

mymod<-rpart(default.payment.next.month ~ PAY_Apr+PAY_May+PAY_Jun+PAY_Jul+PAY_Aug+PAY_Sep+LIMIT_BAL+
               EDUCATION+Sept+Aug+Jul+Jun+May+Apr+
               BILL_Sep+BILL_Aug+BILL_Jul+BILL_Jun+BILL_May+BILL_Apr+AGE,
             data= train_df, method="class",
             control = rpart.control(cp = 0.0001,minsplit = 30,  minbucket = 30*2,
                                     maxsurrogate = 5, usesurrogate = 2,xval=10, maxdepth = 30))
printcp(mymod)

#We will prune the tree based on the best complexity 
#parameter that has the least error.

set.seed(1234)
bestcp <- mymod$cptable[which.min(mymod$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
tree.pruned <- prune(mymod, cp = bestcp)


#Let us evaluate and the model accuracy
# confusion matrix (training data)
conf.matrix <- table("Predicted"=predict(tree.pruned,type="class"),"Actual"=train_df$default.payment.next.month)
print(conf.matrix)
Accuracy_dt<-sum(diag(conf.matrix))/sum((conf.matrix))
print(Accuracy_dt * 100)

library(rpart.plot)
rpart.plot(mymod,fallen.leaves = F ,extra=3)

conf.matrix_test <- table("Predicted"=predict(tree.pruned,test_df, type="class"),"Actual"=test_df$default.payment.next.month)
print(conf.matrix_test)
Accuracy_test<-sum(diag(conf.matrix_test))/sum((conf.matrix_test))
print(Accuracy_test * 100)

#The accuracy achieved using the data set aside 
#for validation is also similar hence our model 
#fits the data well. Overall, we see some improvement in 
#the accuracy of the model using the Decision Tree approach. 
#Let's try random forest to understand 
#if any further imrovement can be brought to the model.

seed<-7
mtry <- floor(sqrt(ncol(train_df[,c("PAY_Sep","PAY_Aug","PAY_Jul","PAY_Jun","PAY_May",
                                    "PAY_Apr","LIMIT_BAL","EDUCATION","Sept","Aug","Jul",
                                    "Jun","May","Apr","BILL_Sep","BILL_Aug","BILL_Jul",
                                    "BILL_Jun","BILL_May","BILL_Apr","SEX","AGE","MARRIAGE")])))
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(1:mtry))

modellist <- list()
for (ntree in c(500,1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(default.payment.next.month~.-(ID+PAY_RATIO_APR+PAY_RATIO_MAY+PAY_RATIO_JUNE+
                                    PAY_RATIO_JULY+PAY_RATIO_AUG+PAY_RATIO_SEPT), 
               data=train_df, method="rf", metric=metric, 
               tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)



