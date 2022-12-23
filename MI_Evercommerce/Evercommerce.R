# Marketing Second Project
# Team 9
# Nolan, Nasim, Shushimitha, Vivi, Kevin

# Profiled prospective customers from the Evercommerce customers 
# database and create a marketing strategy to acquire new customers for Customer
# Lobby SaaS by cross-selling

# Classifier used here: Neural Network, Random Forest, Logistic Regression
# Eventually, we use Logistic Regression since we have to check if our analysis
# is similar to TA's model (And he is using Logistic Regression model)
# However, RF seems to have the best performance among all classifiers we built.

# Library  --------------------------------------------------------------
library(neuralnet) # neural network
library(caret) # ML tools
library(tidyverse) # tidyr + dplyr + ggplot2 and others
library(lolcat) # statistical test
library(readr) # read in data set
library(formattable) # table design 
library(plotly) # interactive plots
library(gt) # gt <- good table 
library(patchwork) # plots composition
library(sjPlot) # plot tool
library(glmnet) # glm with elastic regularization l1+l2
library(randomForest) # RF
library(varImp) # variables importance
library(car)
library(psych)
library(caTools)
library(GGally) # ggpairs for correlation coefficient
library(arsenal) # compare two df
library(sqldf) # manipulate df like you do in SQL
library(purrr)
library(stringr)
library(corrr)


# Function   --------------------------------------------------------------
## Coorelation - Find the n highest coorelation pairs ------------
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

# ex: mosthighlycorrelated(Q6[2:13], 10)

# Import data  --------------------------------------------------------------
EC1 <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/First data set.csv", stringsAsFactors=TRUE)
EC2 <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/Second data set.csv", stringsAsFactors=TRUE)

# Question
# 1.	Based on your analysis, create a customer profile for Customer Lobby platform. 
# a.	What are the firmographic characteristics of Customer Lobby's customers?  
#   To answer this question, run logistic regression analysis and interpret the 
# coefficients in your model. Include some rationale or speculation for why some 
# coefficients are significant and others are not (80 points).
# 
# b.	Create a correlation matrix and have a discussion on multicollinearity in 
# your analysis (40 points)

# Data preparation  --------------------------------------------------------------

PseudoR2<-function(glmModel){
  #if (length(class(object)) > 1)
  #if (class(glmModel)!="glm" | class(glmModel)!="lm"){
  #stop("Object not of class 'glm'")
  #}
  #else {
  #	glmModel<-glmModel
  #	}
  
  logLikN<-glmModel$null/-2  ##log likelihood, null model
  logLikF<-glmModel$dev/-2  ##log likelihood, full model
  G2<-glmModel$null - glmModel$deviance
  n <- length(glmModel$y)	
  ystar <- predict(glmModel, type="response") 
  class1<-ifelse(ystar >.5,1,0) 
  classtab<-table(class1, glmModel$y, dnn=c("Predicted", "Actual")) ; maxOut<-max(margin.table(classtab, 2))
  p<-glmModel$rank
  penaltyN<- 2*(p)*(p+1)  ; penaltyD<- n-p-1  ; penalty<-penaltyN/penaltyD
  MZystar <- predict(glmModel); sse <- sum((MZystar - mean(MZystar))^2) ; s2 <- switch(glmModel$family$link, "probit" = 1, "logit" = pi^2/3, NA)  #Needed for MZ R2
  Enum<-sum((glmModel$y - ystar)^2); Edenom<-sum((glmModel$y - mean(glmModel$y))^2) #Needed for Effron R2
  
  #R2s
  r2McF<-1-logLikF/logLikN  #Mcfadden's R2
  r2McFA<-1-(logLikF - p-1 )/logLikN #Mcfadden's Adj R2
  r2CS<-1-exp(-G2/n) #ML Cox/Snell R2
  r2N<-(1 - exp((glmModel$dev - glmModel$null)/n))/(1 - exp(-glmModel$null/n))# Nagelkerke/Cragg-Uhler R2
  r2MZ<-sse / (n * s2 + sse)  #McKelvey and Zavoina pseudo R^2, using either the logit or probit link
  r2E<-1-(Enum/Edenom) #Effron R2
  r2C<-(classtab[1] + classtab[4])/n##Count R2 (proportion correctly classified)
  r2CA<-(classtab[1] + classtab[4] - maxOut)/(n - maxOut) ##Adjusted Count R2 (proportion correctly classified)
  aic<-2*(p)+glmModel$dev # AIC
  Caic<-aic + penalty # AIC with a correction for finite sample size; useful with small sample sizes or a lot of predictors
  
  results<-c(McFadden=r2McF, Adj.McFadden=r2McFA, Cox.Snell=r2CS, Nagelkerke=r2N, McKelvey.Zavoina=r2MZ, Effron=r2E, Count=r2C, Adj.Count=r2CA, AIC=aic, Corrected.AIC=Caic)
  return(results)
  
}

# Function for performance metrics
prf <- function(predAct){
  ## predAct is two col dataframe of pred,act
  preds = predAct[,1]
  trues = predAct[,2]
  xTab <- table(preds, trues)
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 7, nrow = 1, 
              dimnames = list(c(),c('Acc',
                                    paste("P",clss[1],sep='_'), 
                                    paste("R",clss[1],sep='_'), 
                                    paste("F",clss[1],sep='_'), 
                                    paste("P",clss[2],sep='_'), 
                                    paste("R",clss[2],sep='_'), 
                                    paste("F",clss[2],sep='_'))))
  r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
  r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
  r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
  r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
  r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
  r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
  r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
  r}


# Making new easy to work with data frames:
# Load csv files
# EC1 <- read.csv("Data/Appendix A - N3120.csv", stringsAsFactors = TRUE)
# Code to restore zip codes
EC1$zip <- as.character(EC1$zip)
for(i in 1:length(EC1$zip)){
  if(as.numeric(EC1$zip[i]) < 10000){
    EC1$zip[i] <- paste0("0", EC1$zip[i])
  }
}
EC1 <- as.data.frame(unclass(EC1), stringsAsFactors = TRUE)
head(EC1$zip)
str(EC1)

# EC2 <- read.csv("Data/Appendix B - N9318.csv", stringsAsFactors = TRUE)
# Code to resotre zip codes
EC2$zip <- as.character(EC2$zip)
for(i in 1:length(EC2$zip)){
  if(as.numeric(EC2$zip[i]) < 10000){
    EC2$zip[i] <- paste0("0", EC2$zip[i])
  }
}
EC2 <- as.data.frame(unclass(EC2), stringsAsFactors = TRUE)
head(EC2$zip)
str(EC2)


# Adding NumberHH and MedianIncome
## First read data
# us_zip <- read.csv("US zip data/zip data.csv")
us_zip <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/zip data.csv")

## Code to remove first six characters before zipcode
us_zip$NAME[ us_zip$NAME != 'Geographic Area Name'] <- (gsub("^.{0,6}", "", us_zip$NAME[ us_zip$NAME != 'Geographic Area Name']))
## Remove first row
us_zip = us_zip[-1,]
## Create groups by first four numbers of zip
us_zip$group <- as.factor(str_sub(us_zip$NAME,1,4))
## Convert to numeric and make rest factors
us_zip <- as.data.frame(unclass(us_zip), stringsAsFactors = TRUE)
us_zip <- transform(us_zip, 
                    S1901_C01_001E = as.numeric(S1901_C01_001E), 
                    S1901_C01_012E = as.numeric(S1901_C01_012E))
## Create new data frame for imputed values
us_zip_group <- aggregate(cbind(S1901_C01_001E, S1901_C01_012E) ~ group, data = us_zip, FUN = median)
## Check updates
nrow(us_zip)
head(us_zip$NAME)

## Now merge data by zip code and save as new EC1
EC1 <- left_join(EC1, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC1)[27] <- 'NumberHH'
colnames(EC1)[28] <- 'MedianIncome'
## Add group to EC1
EC1$group <- str_sub(EC1$zip,1,4)
## Join with imputed data frame
EC1 <- left_join(EC1, us_zip_group, by='group')
## Impute nulls
EC1$NumberHH <- ifelse(is.na(EC1$NumberHH), EC1$S1901_C01_001E, EC1$NumberHH)
EC1$MedianIncome <- ifelse(is.na(EC1$MedianIncome), EC1$S1901_C01_012E, EC1$MedianIncome)
## Remove group and last two columns
EC1$group <- NULL
EC1$S1901_C01_001E <- NULL
EC1$S1901_C01_012E <- NULL
## Review structure
str(EC1)
## Check nulls in each column 
colSums(is.na(EC1))
## Zip codes with missing values
EC1[rowSums(is.na(EC1)) > 0, 'zip']
## Remove nulls
EC1 <- na.omit(EC1)
nrow(EC1)

## Now merge data by zip code and save as new EC2
EC2 <- left_join(EC2, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC2)[24] <- 'NumberHH'
colnames(EC2)[25] <- 'MedianIncome'
## Add group to EC2
EC2$group <- str_sub(EC2$zip,1,4)
## Join with imputed data frame
EC2 <- left_join(EC2, us_zip_group, by='group')
## Impute nulls
EC2$NumberHH <- ifelse(is.na(EC2$NumberHH), EC2$S1901_C01_001E, EC2$NumberHH)
EC2$MedianIncome <- ifelse(is.na(EC2$MedianIncome), EC2$S1901_C01_012E, EC2$MedianIncome)
## Remove group and last two columns
EC2$group <- NULL
EC2$S1901_C01_001E <- NULL
EC2$S1901_C01_012E <- NULL
## Review structure
str(EC2)
## Check nulls in each column 
colSums(is.na(EC2))
## Zip codes with missing values
EC2[rowSums(is.na(EC2)) > 0, 'zip']
## Remove nulls
EC2 <- na.omit(EC2)
nrow(EC2)

# create dummies for 'vertical'
EC1$isFinance <- ifelse(EC1$vertical == "finance", 1, 0)
EC1$isFitness <- ifelse(EC1$vertical == "fitness", 1, 0)
EC1$isHealthC <- ifelse(EC1$vertical == "healthca", 1, 0)
EC1$isHomeImp <- ifelse(EC1$vertical == "homeimp", 1, 0)
EC1$isLegal <- ifelse(EC1$vertical == "legal", 1, 0)
EC1$isOnline <- ifelse(EC1$vertical == "online", 1, 0)
EC1$isRealEst <- ifelse(EC1$vertical == "realesta", 1, 0)
EC1$isSecurity <- ifelse(EC1$vertical == "security", 1, 0)
EC1$isTherapy <- ifelse(EC1$vertical == "therapy", 1, 0)

# drop 'vertical' and one dummies flag (regard it as the baseline)
EC1 <- EC1 %>% select(-vertical,-isHealthC,-zip) 

# Data transformation for 2nd data set 
EC2$Cust_MHW <- 0
EC2$Cust_L360 <- 0
EC2$isFinance <- ifelse(EC2$vertical == "finance", 1, 0)
EC2$isFitness <- ifelse(EC2$vertical == "fitness", 1, 0)
EC2$isHealthC <- ifelse(EC2$vertical == "healthca", 1, 0)
EC2$isHomeImp <- ifelse(EC2$vertical == "homeimp", 1, 0)
EC2$isLegal <- ifelse(EC2$vertical == "legal", 1, 0)
EC2$isOnline <- ifelse(EC2$vertical == "online", 1, 0)
EC2$isRealEst <- ifelse(EC2$vertical == "realesta", 1, 0)
EC2$isSecurity <- ifelse(EC2$vertical == "security", 1, 0)
EC2$isTherapy <- ifelse(EC2$vertical == "therapy", 1, 0)

EC2 <- EC2 %>% select(-vertical,-isHealthC,-zip) 

names(EC1)
names(EC2)

# align variable in both data set
# Cust_Psim contribute nothing to the model, since they are all the same.
# 1st data and 2nd date both use pay simple, model can't get any info base on this variable.
# Could just drop it
EC1$Cust_Psim <- NULL


# Check number of variables
length(EC1)
length(EC2)

custid <- EC2$custid
EC2$custid <- NULL

# Convert multiple columns to factor  
str(EC1)
cols <- c("referral", "latepay", "Psim_dsf", "Cust_MHW",'Cust_Lobb','Cust_L360')
EC1[cols] <- lapply(EC1[cols], factor)
EC1[,26:33] <- lapply(EC1[,26:33], factor)
str(EC1)

# for EC2
str(EC2)
cols <- c("referral", "latepay", "Psim_dsf", "Cust_MHW",'Cust_L360')
EC2[cols] <- lapply(EC2[cols], factor)
EC2[,23:32] <- lapply(EC2[,23:32], factor)
str(EC2)

# For multicollinearity ----------------
# aggregate your Psim_rev
EC1 <- EC1 %>% rowwise() %>% mutate(total_rev = sum(c_across(starts_with("PsimRev"))))
EC1 <- EC1 %>% select(-starts_with('PsimRev'))
# drop Psim_vol and tenure
EC1 <- EC1 %>% select(-Psim_vol,-total_rev)

# Same for EC2
EC2 <- EC2 %>% rowwise() %>% mutate(total_rev = sum(c_across(starts_with("PsimRev"))))
EC2 <- EC2 %>% select(-starts_with('PsimRev'))
# drop Psim_vol and tenure
EC2 <- EC2 %>% select(-Psim_vol,-total_rev)

# Split data
sample <- sample.split(EC1$Cust_Lobb, SplitRatio = 0.7)
train  <- subset(EC1, sample == TRUE)
test   <- subset(EC1, sample == FALSE)
X_test <- test %>% select(-Cust_Lobb)
y_test <- test %>% select(Cust_Lobb)

# Build model --------------------------------------------------------------
lobb.model <- glm(formula = Cust_Lobb ~., data = train, family = 'binomial')
summary(lobb.model)

# Use test set
test.prob <- predict(lobb.model,newdata = X_test,type = 'response')
test.pred <- rep(0, 936)
test.pred[test.prob > .5] = 1
table(test.pred , y_test$Cust_Lobb) 

vif(lobb.model) # check for multicollinearity - should be lower than 10


PseudoR2(lobb.model)

# predict 
EC2_pred_glm <- predict(lobb.model,newdata = EC2, type = 'response')
EC2$custid <- custid
EC2$prob <- EC2_pred_glm
EC2_prediction_glm <- EC2 %>% arrange(desc(prob)) %>% head(1000)
write.csv(EC2_prediction_glm,
  "C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check_glm2.csv")

### Note:
# glm runnning with factor, takes too much time. since too much category in lobb model.
# Change zip back to num now, and we probably won't use that in the following

# Multicollinearity --------------------------------------------------------
# Create a correlation matrix and have a discussion on multicollinearity in your analysis
# pairs.panels(EC1) # too many variables.
coor <- EC1[, sapply(EC1, is.numeric)]
coor$zip <- NULL
cor(coor)
ggpairs(coor)
# Most highly related columns
mosthighlycorrelated(coor,15)

# coor heatmap --------------------------------------------------
cormat <- round(cor(coor),2)
head(cormat)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# check percentage
# x <- glm with dropping highly correlated variables
x <- read.table("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check_glm1.csv", quote="\"", comment.char="")
# y <- Random Forest result
y <- read.table("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check_RF_failed.csv", quote="\"", comment.char="")
# z <- glm with all variables, do nothing about multidisciplinary
z <- read.table("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check_glm2.csv", quote="\"", comment.char="")

anti_join(x,y) # prediction result between glm and Rf ~ 6xx results are different  
anti_join(x,z) # 2xx different if we drop highlu correlated vars

# join passed result with original data set
customers_pot <- read.table("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check_glm1_passed.csv", quote="\"", comment.char="")
customers_pot1 <- dplyr::left_join(customers_pot,EC2,by=c('V1'='custid'))
# write into a new csv
write.csv(customers_pot1,"C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/1000_potential_customers.csv")

# Appendix ------------------------------------------------------------------------------------
# test, if don't use dummies for 'vertical'
check <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/First data set.csv", stringsAsFactors=TRUE)
str(check)
cols <- c("vertical","referral", "latepay", "Psim_dsf", "Cust_Psim","Cust_MHW",'Cust_Lobb','Cust_L360')
check[cols] <- lapply(check[cols], factor)
output.RF <- randomForest(formula = Cust_Lobb ~ .,data = check,importance = T)
print(output.RF) 
print(importance(output.RF,type = 2))
varImpPlot(output.RF,main="Variable importance_CustomerProfile")

# compare two group
EC2_dummies_pot_cust <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_dummies_pot_cust.csv")

EC2_dummies_pot_cust$potential_top1000 <- EC2_dummies_pot_cust$potential_top1000 %>% as.factor()

ggplot(EC2_dummies_pot_cust, aes(Psim_CC,fill = potential_top1000,group = potential_top1000)) + 
  geom_histogram(position = 'identity',color = 'black')+
  scale_fill_discrete(labels = c("Non Top 1000","Top 1000"))+
  theme_bw()+
  ggtitle('Psim_CC distribution between top 1000 potential customer and the rest')


# NN
nn=neuralnet(Cust_Lobb~.,data=train, hidden=c(300,400,500),act.fct = "logistic",
             linear.output = FALSE)
plot(nn)
Predict=neuralnet::compute(nn,train)
Predict$net.result
prob <- Predict$net.result
pred <- ifelse(prob>0.50, 1, 0)
pred
table(train$Cust_Lobb,pred)

# find important features; By Ramdom Forest ------------------------------
output.RF <- randomForest(formula = Cust_Lobb ~ .,data = EC1,importance = T)
print(output.RF) 
print(importance(output.RF,type = 2))
varImpPlot(output.RF,main="Variable importance_CustomerProfile")

# RF1 --------------------------------------------------------------

RF_MDA <- EC1 %>% select(Psim_ACH,Psim_mob,Psim_dsf,org_size,referral,Psim_CC,
                         tenure,Cust_Lobb)
output.RF1 <- randomForest(formula = Cust_Lobb ~ .,data = RF_MDA,importance = T)
print(output.RF1) 
print(importance(output.RF1,type = 2))
varImpPlot(output.RF1,main="Variable importance_CustomerProfile_feature selected")

RF_MDG <- EC1 %>% select(Psim_ACH,Psim_mob,zip,org_size,Psim_CC,Psim_vol,touches,
                         tenure,Cust_Lobb)
output.RF2 <- randomForest(formula = Cust_Lobb ~ .,data = RF_MDG,importance = T)
print(output.RF2) 
print(importance(output.RF2,type = 2))
varImpPlot(output.RF2,main="Variable importance_CustomerProfile_feature selected")


# RF2 -----------------------------------------------------------
rf.model <- randomForest(formula = Cust_Lobb ~ .,data = train,importance = T)
test.pred <- predict(rf.model,newdata = X_test)
# Confusion matrix
table(test.pred , y_test$Cust_Lobb) # error rate = 0.2521
# Probability for ranking
prob_rf <- predict(rf.model,newdata = X_test,type = 'prob')
prob_rf <- prob_rf %>% as.data.frame()
# Variable importance
varImpPlot(rf.model,main="Variable importance_CustomerProfile")
# test.pred   0   1
#         0 588 182
#         1  54 112

EC2_pred <- predict(rf.model,newdata = check)
EC2_prob <- predict(rf.model,newdata = check,type = 'prob')
# workaround for predict() bug
EC2 <- rbind(X_test[1, ] , EC2)
EC2 <- EC2[-1,]

EC2$pred <- EC2_pred
EC2$prob <- EC2_prob
EC2$custid <- custid
write.csv(EC2,"C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MBAX 6330 Market Intelligence/EverCommerce/EC2_check.csv")