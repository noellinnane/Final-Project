###########################################################
################## Read the dataset #######################
###########################################################

# This takes approx 5 mins to read in

data = read.csv("C:\\Users\\Noel\\Documents\\College\\Project\\Lending Club\\Datasets\\loan.csv")

# See the structure of the dataset

str(data)

###########################################################
################## CLEAN THE DATA #########################
###########################################################

# How many nulls in each column

colSums(is.na(data))

# Remove columns with too many nulls

data2 <- data[,-which(colSums(is.na(data))>2000)] # remove columns with >2000 NAs in them

# Remove rows with Nulls

data2 <- na.omit(data2)

# Subset data to include loans which are "Fully Paid" and "Charged Off" only

data2 <- subset(data2, loan_status == "Fully Paid" | loan_status == "Charged Off")

dim(data2)

# Convert into correct data formats, i.e. dates, numerics, factors, etc

library(lubridate)
library(tidyverse)

data2$loan_amnt <- as.numeric(data2$loan_amnt)
data2$funded_amnt <- as.numeric(data2$funded_amnt)
data2$funded_amnt_inv <- as.numeric(data2$funded_amnt_inv)
data2$term <- as.numeric(as.factor(data2$term))
data2$int_rate <- as.numeric(data2$int_rate)
data2$installment <- as.numeric(data2$installment)
data2$grade <- as.numeric(as.factor(data2$grade))
data2$sub_grade <- as.numeric(as.factor(data2$sub_grade))
data2$emp_title <- as.numeric(as.factor(data2$emp_title))
data2$emp_length <- as.numeric(as.factor(data2$emp_length))
data2$home_ownership <- as.numeric(as.factor(data2$home_ownership))
data2$annual_inc <- as.numeric(data2$annual_inc)
data2$verification_status <- as.numeric(as.factor(data2$verification_status))
data2$issue_d <- parse_date_time(data2$issue_d, orders = "my")
data2 <- mutate(data2, class=as.integer(as.factor(ifelse(data2$loan_status == "Fully Paid",1,2)))) 
data2$pymnt_plan <- as.numeric(as.factor(data2$pymnt_plan))
data2$desc <- as.numeric(as.factor(data2$desc))
data2$purpose <- as.numeric(as.factor(data2$purpose))
data2$title <- as.numeric(as.factor(data2$title))
data2$zip_code <- as.numeric(as.factor(data2$zip_code))
data2$addr_state <- as.numeric(as.factor(data2$addr_state))
data2$dti <- as.numeric(data2$dti)
data2$delinq_2yrs <- as.numeric(data2$delinq_2yrs)
data2$earliest_cr_line <- parse_date_time(data2$earliest_cr_line, orders = "my")                                
data2$inq_last_6mths <- as.numeric(data2$inq_last_6mths)
data2$open_acc <- as.numeric(data2$open_acc)
data2$pub_rec <- as.numeric(data2$pub_rec)
data2$revol_bal <- as.numeric(data2$revol_bal)
data2$revol_util <- as.numeric(data2$revol_util)
data2$total_acc <- as.numeric(data2$total_acc)
data2$initial_list_status <- as.numeric(as.factor(data2$initial_list_status))
data2$out_prncp <- as.numeric(data2$out_prncp)
data2$out_prncp_inv <- as.numeric(data2$out_prncp_inv)
data2$total_pymnt <- as.numeric(data2$total_pymnt)
data2$total_pymnt_inv <- as.numeric(data2$total_pymnt_inv)
data2$total_rec_prncp <- as.numeric(data2$total_rec_prncp)
data2$total_rec_int <- as.numeric(data2$total_rec_int)
data2$total_rec_late_fee <- as.numeric(data2$total_rec_late_fee)
data2$recoveries <- as.numeric(data2$recoveries)
data2$collection_recovery_fee <- as.numeric(data2$collection_recovery_fee)
data2$last_pymnt_d <- parse_date_time(data2$last_pymnt_d, orders = "my")
data2$last_pymnt_amnt <- as.numeric(data2$last_pymnt_amnt)
data2$next_pymnt_d <- parse_date_time(data2$next_pymnt_d, orders = "my")
data2$last_credit_pull_d <- parse_date_time(data2$last_credit_pull_d, orders = "my")
data2$collections_12_mths_ex_med <- as.numeric(data2$collections_12_mths_ex_med)
data2$policy_code <- as.numeric(data2$policy_code)
data2$application_type <- as.numeric(as.factor(data2$application_type))
data2$verification_status_joint <- as.numeric(as.factor(data2$verification_status_joint))
data2$acc_now_delinq <- as.numeric(data2$acc_now_delinq)
data2$chargeoff_within_12_mths <- as.numeric(data2$chargeoff_within_12_mths)
data2$delinq_amnt <- as.numeric(data2$delinq_amnt)
data2$pub_rec_bankruptcies <- as.numeric(data2$pub_rec_bankruptcies)
data2$tax_liens <- as.numeric(data2$tax_liens)
data2$sec_app_earliest_cr_line <- parse_date_time(data2$sec_app_earliest_cr_line, orders = "my")
data2$hardship_flag <- as.numeric(as.factor(data2$hardship_flag))
data2$hardship_type <- as.numeric(as.factor(data2$hardship_type))
data2$hardship_reason <- as.numeric(as.factor(data2$hardship_reason))
data2$hardship_status <- as.numeric(as.factor(data2$hardship_status))
data2$hardship_start_date <- parse_date_time(data2$hardship_start_date, orders = "my")
data2$hardship_end_date <- parse_date_time(data2$hardship_end_date, orders = "my")
data2$payment_plan_start_date <- parse_date_time(data2$payment_plan_start_date, orders = "my")
data2$hardship_loan_status <- as.numeric(as.factor(data2$hardship_loan_status))
data2$disbursement_method <- as.numeric(as.factor(data2$disbursement_method))
data2$debt_settlement_flag <- as.numeric(as.factor(data2$debt_settlement_flag))
data2$debt_settlement_flag_date <- parse_date_time(data2$debt_settlement_flag_date, orders = "my")
data2$settlement_status <- as.numeric(as.factor(data2$settlement_status))
data2$settlement_date <- parse_date_time(data2$settlement_date, orders = "my")

# Subset data to loans issues in 2018 only (now that dates have been formatted)

data2 <- subset(data2, issue_d >= "2018-01-01" & issue_d <= "2018-12-31" )


##############################################################
#################### DO CORRELATION ##########################
##############################################################

library(corrplot)
library(GGally)

c <- cor(data2[,unlist(lapply(data2, is.numeric))], use = "complete.obs")
 
ggcorr(c, label = FALSE, hjust= 0.75, layout.exp = 0) #Plot the correlations


############################################################
################# Feature selection ########################
############################################################

# Choose features to keep going forward
# Features selected based on correlation (above) and reading the data dictionary included with dataset

cols <- c("loan_amnt","term","int_rate","installment","grade","sub_grade","home_ownership","annual_inc"
          ,"verification_status","issue_d","class","pymnt_plan","purpose","addr_state","dti","inq_last_6mths",
          "open_acc","pub_rec","revol_bal","revol_util","total_acc","initial_list_status","total_pymnt","total_rec_prncp",
          "last_pymnt_amnt","acc_now_delinq","delinq_amnt","pub_rec_bankruptcies","hardship_flag") #removed emp_length for now

data2 <- data2[names(data2) %in% cols]

str(data2)


###########################################################
################# MODELLING ###############################
###########################################################

library(rpart)
library(rpart.plot)
library(ROCR)
library(ROSE)

# split data in training and test sets

n=nrow(data2) 
indexes = sample(n,n*(80/100)) 
trainset = data2[indexes,] 
testset = data2[-indexes,] 

dim(data2)
dim(testset)
dim(trainset)

# Decision Tree

dt <- rpart(class ~ ., data = trainset, method = "class") 
predictedvalues = predict(dt, testset, type = 'class')
tab_dt = table(predictedvalues, actualvalues=testset[,29]) #confusion matrix
accuracy_dt=sum(tab_dt[row(tab_dt)==col(tab_dt)])/sum(tab_dt) 
accuracy_dt

rpart.plot(dt, cex=0.75) # Visualise the Decison Tree
tab_dt #Confusion matrix
roc.curve(data2$class, predict(dt, data2, type = "prob")[,1],plot=TRUE)

# Random Forest

library(randomForest)
library(ggplot2)
library(caret)

rf=randomForest(as.factor(class) ~ ., type='class', data = trainset, ntree=10) #as.factor makes it run much faster
predictedvalues = predict(rf, testset, type='class')
tab_rf = table(predictedvalues, actualvalues=testset[,29]) #confusion matrix
accuracy_rf= sum(tab_rf[row(tab_rf)==col(tab_rf)])/sum(tab_rf)
accuracy_rf
tab_rf
roc.curve(data2$class, predict(rf, data2, type = "prob")[,1],plot=TRUE)

# Visualise Variable Importance
# Ggplot requires a dataframe, so convery VarImp in to a dataframe

vi <- data.frame(Variable = c(names(data2[,-29])), #removed the class variable as thats not in VarImp
                 Overall = c(varImp(rf)[1]))

v <- ggplot(data=vi,aes(x = reorder(Variable, Overall),y=Overall))+
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  #coord_cartesian(ylim = c(90,100)) +
  labs(x = "Variable")
v + coord_flip()

# GLM

trainset.glm <- glm(class-1 ~.,trainset, family="binomial") # class-1 makes the variable values 0 and 1
phati=predict(trainset.glm,testset, type="response")  # p hat i 
predictedvalues=rep(0,length(phati)) 
predictedvalues[phati>0.5]=1   # probability of x being 1, if p<0.5 then x=0 
actual=testset[,29]-1 
tab_glm=table( predictedvalues, actualvalues=testset[,29]-1) 
accuracy_gl=mean(predictedvalues == testset[,29]-1) 
accuracy_gl #higher better
tab_glm #Confusion matrix
summary(trainset.glm)

#couldnt get ROC to work here#



#####################################################
############# TESTING AND EVALUATION ################
#####################################################

# Plot the accuracy of each model

df <- data.frame(Model = c("Decision Tree",
                           "Random Forest",
                           "GLM"
),
Accuracy = c(round(accuracy_dt,4)*100, 
             round(accuracy_rf,4)*100, 
             round(accuracy_gl,4)*100
))
head(df)

p<-ggplot(data=df,aes(x = reorder(Model, -Accuracy),y=Accuracy))+
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Accuracy), vjust=1.6, color="white",size=3.5) +
  theme_minimal() +
  coord_cartesian(ylim = c(80,100)) +
  labs(x = "Model")
p + ggtitle("Model Accuracy")


# Confusion matrixes (matrices)

tab_dt
tab_rf
tab_glm




#######################################################################################
######################### Deal with imbalanced data ###################################
#######################################################################################

# Balancing the dataset (if necessary)

# DECISION TREE
# OVERSAMPLING THE MINORITY CLASS

dim(trainset)
table(trainset$class)
prop.table(table(trainset$class)) #distribution of values

train_over <- ovun.sample(class ~ ., data = trainset, p=0.5, method = "over")$data
table(train_over$class)       

dt_over <- rpart(class ~ ., data = train_over, method = "class") 
pred_dt_over = predict(dt_over,testset,type='class') 
tab_dt_over = table(pred_dt_over,actual = testset$class)  # confusion matrix  
accuracy_dt_over = sum(tab_dt_over[row(tab_dt_over)==col(tab_dt_over)])/sum(tab_dt_over) 
accuracy_dt_over
tab_dt_over
roc.curve(data2$class, predict(dt_over, data2, type = "prob")[,1],plot=TRUE)
rpart.plot(dt_over)


# UNDERSAMPLING THE MAJORITY CLASS

train_under <- ovun.sample(class~., data = trainset, p=0.5, method = "under")$data
table(train_under$class)        

dt_under <- rpart(class~., data=train_under, method="class")
pred_dt_under = predict(dt_under, testset, type = 'class')
tab_dt_under = table(pred_dt_under, actual = testset$class) #confusion matrix
accuracy_dt_under = sum(tab_dt_under[row(tab_dt_under)==col(tab_dt_under)])/sum(tab_dt_under) 
accuracy_dt_under
tab_dt_under
roc.curve(data2$class, predict(dt_under, data2, type = "prob")[,1],plot=TRUE)
rpart.plot(dt_under)


# BOTH OVERSAMPLING AND UNDERSAMPLING

train_both <- ovun.sample(class~., data = trainset, N=nrow(trainset), p=0.5, method="both")$data
table(train_both$class)

dt_both <- rpart(class~., data=train_both, method="class")
pred_dt_both = predict(dt_both, testset, type = 'class')
tab_dt_both = table(pred_dt_both, actual = testset$class) #confusion matrix
accuracy_dt_both = sum(tab_dt_both[row(tab_dt_both)==col(tab_dt_both)])/sum(tab_dt_both) 
accuracy_dt_both
tab_dt_both
roc.curve(data2$class, predict(dt_both, data2, type = "prob")[,1],plot=TRUE)
rpart.plot(dt_both)


# SYNTHETIC DATA GENERATION

#need to remove no-numeric columns

#names(trainset)

train_rose <- ROSE(class~., data=trainset[,-10])$data #removed issue date as causing errors algorithm only acceots numerical variables
table(train_rose$class)
dt_rose <- rpart(class~., data=train_rose, method="class")
pred_dt_rose = predict(dt_rose, testset, type = 'class')
tab_dt_rose = table(pred_dt_rose, actual = testset$class) #confusion matrix
accuracy_dt_rose = sum(tab_dt_rose[row(tab_dt_rose)==col(tab_dt_rose)])/sum(tab_dt_rose) 
accuracy_dt_rose
tab_dt_rose
roc.curve(data2$class, predict(dt_rose, data2, type = "prob")[,1],plot=TRUE)
rpart.plot(dt_rose)



###############################################################################
################### Which model performed best? ###############################
###############################################################################

# Plot accuracy for each model

df <- data.frame(Model = c("Decision Tree",
                           "Random Forest",
                           "GLM",
                           "Decision Tree Over",
                           "Decision Tree Under",
                           "Decision Tree Both",
                           "Decision Tree Rose"
),
Accuracy = c(round(accuracy_dt,4)*100, 
             round(accuracy_rf,4)*100, 
             round(accuracy_gl,4)*100,
             round(accuracy_dt_over,4)*100,
             round(accuracy_dt_under,4)*100,
             round(accuracy_dt_both,4)*100,
             round(accuracy_dt_rose,4)*100
))
#head(df)

p2 <-ggplot(data=df,aes(x = reorder(Model, -Accuracy),y=Accuracy))+
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Accuracy), vjust=1.6, color="white",size=3.5) +
  theme_minimal() +
  coord_cartesian(ylim = c(90,100)) +
  labs(x = "Model")
#p2 + ggtitle("Model Accuracy")


# Plot AUC (Area UNder the Curve) for each model

#Decision Tree:
rc_dt <- roc.curve(data2$class, predict(dt, data2, type = "prob")[,1])
rc_dt <- as.character(rc_dt)
rc_dt <- as.numeric(rc_dt[2])
rc_dt

# Random Forest:
rc_rf <- roc.curve(data2$class, predict(rf, data2, type = "prob")[,1])
rc_rf <- as.character(rc_rf)
rc_rf <- as.numeric(rc_rf[2])
rc_rf

# Decision Tree Over Sampled:

rc_dt_over <- roc.curve(data2$class, predict(dt_over, data2, type = "prob")[,1])
rc_dt_over <- as.character(rc_dt_over)
rc_dt_over <- as.numeric(rc_dt_over[2])
rc_dt_over

# Decision Tree Under Sampled:

rc_dt_under <- roc.curve(data2$class, predict(dt_under, data2, type = "prob")[,1])
rc_dt_under <- as.character(rc_dt_under)
rc_dt_under <- as.numeric(rc_dt_under[2])
rc_dt_under

# Decision Over and Under Sampled:

rc_dt_both <- roc.curve(data2$class, predict(dt_both, data2, type = "prob")[,1])
rc_dt_both <- as.character(rc_dt_both)
rc_dt_both <- as.numeric(rc_dt_both[2])
rc_dt_both

# Decision TRee with Synthetic Data

rc_dt_rose <- roc.curve(data2$class, predict(dt_rose, data2, type = "prob")[,1])
rc_dt_rose <- as.character(rc_dt_rose)
rc_dt_rose <- as.numeric(rc_dt_rose[2])
rc_dt_rose

df2 <- data.frame(Model = c("Decision Tree",
                            "Random Forest",
                            "Decision Tree Over",
                            "Decision Tree Under",
                            "Decision Tree Both",
                            "Decision Tree Rose"
),
AUC = c(round(rc_dt,4)*100, 
        round(rc_rf,4)*100, 
        round(rc_dt_over,4)*100,
        round(rc_dt_under,4)*100,
        round(rc_dt_both,4)*100,
        round(rc_dt_rose,4)*100
))

head(df2)

p3 <- ggplot(data=df2,aes(x = reorder(Model, -AUC),y=AUC))+
  geom_bar(stat="identity", fill="red") +
  geom_text(aes(label=AUC), vjust=1.6, color="white",size=3.5) +
  theme_minimal() +
  coord_cartesian(ylim = c(90,100)) +
  labs(x = "Model")

p2 + ggtitle("Model Accuracy")
p3 + ggtitle("Model ROC")




