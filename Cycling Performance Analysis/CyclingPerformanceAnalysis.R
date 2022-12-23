# ADA R project
# Kevin / Nasim / Pushkar

# Load Library
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

# Min - Max normalization
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


### Follow up:
# 1. More EDA
# summary()
# sapply(df,mean) # check for mean/sd...


# Data set 
activities <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MSBX 5415 R Advanced Data Analytics/Project/i67172_activities_1024.csv")
segment <- read.csv("C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MSBX 5415 R Advanced Data Analytics/Project/segment.csv")
str(segment)



# EDA ------------------
str(activities)
View(activities)
is.na(activities)
colSums(is.na(activities))

# Split riding data out 
ride <- activities[activities$type %in% c('Ride','VirtualRide'),]
View(ride)

# Commute Ride doesn't have power metric, split it again
commute <- ride[is.na(ride$icu_average_watts),]
# filter out the commute and get training Ride df
trainingRide <- ride[!is.na(ride$icu_average_watts),]

# Check data integrity
is.na(trainingRide)
colSums(is.na(trainingRide))
# na.omit eliminate na cells (5 rows in this case)
trainingRide <- na.omit(trainingRide)

# Data transformation
# data transformation
trainingRide$date <- as.Date(trainingRide$start_date_local, format="%Y-%m-%d")
trainingRide$month <- months(trainingRide$date)
trainingRide$week <- strftime(trainingRide$date,format = '%V') # '%V' Week of the year as decimal number (01-53) as defined in ISO 8601
# Moving time, from sec to hour
trainingRide$moving_hour <- trainingRide$moving_time / 60 / 60
trainingRide$distance_km <- trainingRide$distance / 1000
trainingRide$max_speed_kmhr <- trainingRide$max_speed * 3.6
trainingRide$average_speed_kmhr <- trainingRide$average_speed * 3.6

# Round the table
trainingRide <- trainingRide %>% mutate(across(where(is.numeric), round, digits=3))
trainingRide$month <- factor(trainingRide$month, levels = month.name)
min(trainingRide$date)
max(trainingRide$date)
# since the the output csv use meter and sec to represent distance and moving time
# the speed is also calculate based on this 
# Have to times 3.6 to change from m/s to km/hr

View(trainingRide)
# write to csv. can use for other vis tool
# write.csv(trainingRide,"C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MSBX 5415 R Advanced Data Analytics/Project/trainingRide.csv",row.names = FALSE)
str(trainingRide)

# Day -----------------------------------------------------------------------
# heart rate vs normalized power dist
trainingRide %>% ggplot(aes(average_heartrate,icu_normalized_watts))+
  geom_point()+
  geom_smooth(data = trainingRide %>% filter(average_heartrate>110,icu_normalized_watts>100),
              method = lm)+
  coord_cartesian(xlim = c(110,155),ylim = c(100,215))+
  theme_bw()+
  ggtitle('Heart Rate vs Normalized Power',subtitle = 'Training Activities')+
  ylab('Normalized Power') +
  xlab('Avg_HeartRate')

# fitness and fatigue
# Create by excel

# training effect categorization 
# Create by excel

# Imputation
#Imputation for Na cell: Use linear regression between average_heartrate and 
#Normalized power to calculate the power for blank cell

#Removing the null cells from both average_heartrate column and Normalized Power column
notNull_NP_AvgHR <- activities[!is.na(activities$icu_normalized_watts) & !is.na(activities$average_heartrate),]

lm1 <-  lm(icu_normalized_watts ~ average_heartrate, data = notNull_NP_AvgHR)
summary(lm1)

#colSums(is.na(activities))
#nrow(activities)

#Removing the null cells of average_heartrate column.
#There are some cells which have value for normalized power but don't have
#value for average_heartrate column. That's why we are getting rid of these cells.

activities <- activities[!is.na(activities$average_heartrate),]

#Dividing the activities dataset into 2 based on normalized power(icu_normalized_watts)
null_NP <- activities[is.na(activities$icu_normalized_watts),]
notNull_NP <- activities[!is.na(activities$icu_normalized_watts),]

#storing the values of average_heartrate column for which we wanna predict
#normalized power.
only_avgHR <- data.frame(average_heartrate <- null_NP$average_heartrate)
predicted_np <- predict(lm1, newdata = only_avgHR)

#Imputing the values for normalized power
null_NP$icu_normalized_watts <- predicted_np

imputed_NP <- rbind(notNull_NP, null_NP)

# write.csv(imputed_NP,"D:/AUniversity_of_Colorado_Boulder/5_Fall Courses/Advanced Data Analytics/Project/imputed_NP.csv", row.names = FALSE)
# str(imputed_NP)


# week summary --------------------------------------------------------------
wk_summary <- trainingRide %>% 
  group_by(week) %>%  
  summarise(Hrs = sum(moving_hour),
            Distance = sum(distance_km),
            Speed = mean(average_speed_kmhr),
            FTP = mean(icu_eftp),
            load = sum(icu_training_load),
            Heartrate = mean(average_heartrate))
# write.csv(wk_summary,"C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MSBX 5415 R Advanced Data Analytics/Project/weekly.csv")

# Round
wk_summary[2:6] <- wk_summary[2:6] %>% round(2)

wk_summary[1:14,] %>% 
    gt(rowname_col = 'week') %>%   
      tab_header(
        title = md("**Weekly descriptive Summary**"),
        subtitle = md("Hrs/ Distance/ Speed/ Estimate FTP/ wk_load")
      )

wk_summary[15:27,] %>% 
  gt(rowname_col = 'week') %>%   
  tab_header(
    title = md("**Weekly descriptive Summary**"),
    subtitle = md("Hrs/ Distance/ Speed/ Estimate FTP/ wk_load")
  )

# histogram for distance
c4 <- wk_summary %>% ggplot() + 
  geom_col(aes(week,Distance),fill = '#F3C222',color = 'black')+
  theme_bw()

# curve - power and heart rate
c1 <- wk_summary %>% ggplot(aes(week,FTP)) + 
  geom_line(group = 1,size=1,linetype = 1,color = '#31a354')+
  geom_point(color = '#31a354',size = 2.5)+
  theme_bw()+
  theme(axis.title.x = element_blank())
c2 <- wk_summary %>% ggplot(aes(week,Heartrate)) + 
  geom_line(group = 1,size=1,linetype = 1,color = 'red')+
  geom_point(color = 'red',size = 2.5)+
  theme_bw()+
  theme(axis.title.x = element_blank())
c3 <- wk_summary %>% ggplot() + 
  geom_col(aes(week,load),fill = 'pink',color = 'black')+
  theme_bw()+
  ylab('Tss loading')+
  theme(axis.title.x = element_blank())

patchwork <- c1/c2/c3/c4

# put them all together
patchwork + plot_annotation(
  title = 'Trend Chart from early April to Mid October',
  subtitle = 'Functional Threshold Power / Heart Rate / Tss Loading / Weekly milages',
  caption = 'Heart rate remain at same level while power steadily goes up'
)  

# ggplotly with multiple ggplot2 objects - make it into interactive plot
fig1 <- c1 %>% ggplotly()
fig2 <- c2 %>% ggplotly()
fig3 <- c3 %>% ggplotly()
fig4 <- c4 %>% ggplotly()
subplot(fig1,fig2,fig3,fig4,nrows = 4)

# Month --------------------------------------------------------------
mth_summary <- trainingRide %>% 
  group_by(month) %>%  
  summarise(Hrs = sum(moving_hour),
            Distance = sum(distance_km),
            Speed = mean(average_speed_kmhr),
            FTP = mean(icu_eftp),
            load = sum(icu_training_load))

mth_summary[2:6] <- mth_summary[2:6] %>% round(2)

mth_summary %>% gt()%>%   
  tab_header(
    title = md("**Monthly descriptive Summary**"),
    subtitle = md("Hrs/ Distance/ Speed/ Estimate FTP/ wk_load")
  )  %>% tab_source_note('Data collected on Oct, 24th')

# write.csv(mth_summary,"C:/Users/asus/Desktop/CU boulder/Course/Fall 22/MSBX 5415 R Advanced Data Analytics/Project/monthly.csv")

### Model - segment -----------------------------------------------

# EDA of segment data
segment %>% ggplot(aes(Seg1_1,Seg1,color = FTP))+geom_point(size = 3,alpha = 1) +
  scale_color_distiller(palette = 'Spectral')+
  theme_bw()+
  ggtitle('Routes result vs Power ratio')

# FTP ratio histogram
segment %>% ggplot(aes(FTP)) + geom_histogram(color = 'black', fill = 'lightblue')+
  ggtitle('FTP distribution')+
  coord_cartesian(xlim=c(0,7))+theme_bw()

# power dist
p1 <- segment %>% ggplot(aes(pwr_10min)) + 
  geom_histogram(fill = '#F2C333',color = 'black',binwidth = 0.2)+
  ggtitle('Max average power for 10 min')+
  coord_cartesian(xlim=c(0,7))+theme_bw()

p2 <- segment %>% ggplot(aes(pwr_20min)) + 
  geom_histogram(fill = '#a1d99b',color = 'black',binwidth = 0.2)+
  ggtitle('Max average power for 20 min')+
  coord_cartesian(xlim=c(0,7))+theme_bw()

p3 <- segment %>% ggplot(aes(pwr_60min)) + 
  geom_histogram(fill = '#c994c7',color = 'black',binwidth = 0.2)+
  ggtitle('Max average power for 60 min')+
  coord_cartesian(xlim=c(0,7))+theme_bw()

p4 <- segment %>% ggplot(aes(pwr_90min)) + 
  geom_histogram(fill = '#9ecae1',color = 'black',binwidth = 0.2)+
  ggtitle('Max average power for 90 min')+
  coord_cartesian(xlim=c(0,7))+theme_bw()

# put them all together
(p1+p2)/(p3+p4)

# Male and female, under 40 or not
segment$age_under40 <- as.factor(segment$age_under40)
segment$gender <- as.factor(segment$gender)

segment %>% ggplot(aes(Seg1,fill = gender)) + 
  geom_histogram(position = 'identity',alpha = 0.5,binwidth = 150,color = 'black') +
  ggtitle('Result by gender')+
  theme_bw()+
  scale_fill_discrete(name = "Gender", labels = c('Male','Female'))
  
# linear regression model ----- #1 ------

# data prep
# segment test
segment <- segment %>% select(-act_title)
segment$age_under40 <- segment$age_under40 %>% as.factor()
segment$gender <- segment$gender %>% as.factor()
segment$Has_HeartRate <- segment$Has_HeartRate %>% as.factor()
str(segment)

# Check multicollinearity ---------------
id <- segment$id
segment <- segment %>% select(-id)
coor <- segment[, sapply(segment, is.numeric)]
cor(coor)
# coor heatmap ----
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
segment$id <- id

# Discuss the relationship 
# https://stats.stackexchange.com/questions/445189/why-high-correlation-coefficient-doesnt-guarantee-high-vif

# Hypothesis testing for the mean value in summer and winter -------------------
# make dummies variable 
segment$attempt_date <- as.Date(segment$attempt_date)
segment$month <- months(segment$attempt_date)
for (i in 1:nrow(segment)){
  if (segment$month[i] %in% c('May','June','July','August','September','October')){
    segment$warm_month[i] <- 1
  }
  else{
    segment$warm_month[i] <- 0
  }
}
segment$warm_month <- as.factor(segment$warm_month)
table(segment$warm_month)
segment %>% group_by(warm_month) %>% summarise(ave = mean(Seg1_1))
segment %>% group_by(warm_month) %>% summarise(ave = mean(Seg1))


# seperate data
warm <- segment %>% dplyr::filter(warm_month == 1)
cold <- segment %>% dplyr::filter(warm_month == 0)

# hypothesis testing
t.test(x = warm$Seg1,y = cold$Seg1,alternative = 'two.sided',conf.level = .95)
# Welch Two Sample t-test
#   data:  warm$Seg1 and cold$Seg1
#   t = 0.080057, df = 5872.3, p-value = 0.9362
#   alternative hypothesis: true difference in means is not equal to 0

# create boxplot with test result
ggplot(segment,aes(warm_month,Seg1,group = warm_month,fill = warm_month)) + geom_boxplot() +
  ggtitle(subtitle = 'p-value = 0.9362; true difference in means is not equal to 0',
          label = 'Result in warm/cold weather - Hypothesis testing')+
  theme_bw()+
  scale_x_discrete(labels = c('Cold','Warm'))+
  scale_fill_discrete(labels = c('Cold','Warm'))+
  ylab(label = 'Seg1 finish time in second')+
  xlab(label = 'Temperature')
  
# select data for building model
str(segment)
segment <- segment %>% select(-attempt_date)
# convert data type to factor
for (i in 9:15){
segment[,i] <- as.factor(segment[,i])
}
str(segment)


# Linear Regression Model ----------------
# drop CPU column
# segment <- segment %>% select(-first_bikeCPU_brand,-second_CPU_brand)
# segment <- segment %>% select(-warm_month)
segment <- segment %>% select(-id)

sg1.model <- lm(Seg1~.,data = segment)
summary(sg1.model)
vif(sg1.model)
tab_model(sg1.model)

sg1.model2 <- lm(Seg1~pwr_90min + pwr_60min + pwr_10min + FTP + pwr_20min + Seg1_1 + bikeweight_kg ,data = segment)
summary(sg1.model2)
vif(sg1.model2)
tab_model(sg1.model2)

sg1.model3 <- lm(Seg1~pwr_90min + pwr_60min + pwr_10min + FTP + bikeweight_kg ,data = segment)
summary(sg1.model3)
vif(sg1.model3)
tab_model(sg1.model3)
# make the prediction
ftp <- 3.99
pwr60 <- 2.96
pwr10 <- 3.91
pwr90 <- 2.76
bkw <- 7.5

fx <- 7033.77 + ftp * (-309.05) + pwr60 * (-630.18) + pwr10 * 377.22 - pwr90 * 458.72 - 5.58246 * bkw
# seg1 result <-  4102 sec
pnorm(4102,mean = mean(segment$Seg1),sd = sd(segment$Seg1)) # outperform 60% of other riders


# Random Forest model - Regression ----------------------------------------
RF.reg <- randomForest(Seg1~.,data=segment,importance = T)
RF.reg
varImpPlot(RF.reg)

# Use variabels that are more important
RF.reg1 <- randomForest(Seg1~ pwr_90min + pwr_60min + pwr_10min+ FTP + pwr_20min + Seg1_1,data=segment,importance = T)
RF.reg1
varImpPlot(RF.reg1)


# RF.reg_1000 <- randomForest(Seg1~.,data=segment,importance = T,ntree = 1000)
# RF.reg_1000
# varImpPlot(RF.reg_1000)


# Classification
# cross - selling
# create dummy flag - one hot coding
segment$first_isGarmin <- ifelse(segment$first_bikeCPU_brand == "Garmin", 1, 0)
segment$first_isBryton <- ifelse(segment$first_bikeCPU_brand == "Bryton", 1, 0)
segment$first_isWahoo <- ifelse(segment$first_bikeCPU_brand == "Wahoo", 1, 0)
segment$first_isOther <- ifelse(segment$first_bikeCPU_brand == "Other", 1, 0)
segment$first_NotUsing <- ifelse(segment$first_bikeCPU_brand == "Not using", 1, 0)

segment$second_isGarmin <- ifelse(segment$second_CPU_brand == "Garmin", 1, 0)
segment$second_isBryton <- ifelse(segment$second_CPU_brand == "Bryton", 1, 0)
segment$second_isWahoo <- ifelse(segment$second_CPU_brand == "Wahoo", 1, 0)
segment$second_isOther <- ifelse(segment$second_CPU_brand == "Other", 1, 0)
segment$second_NotUsing <- ifelse(segment$second_CPU_brand == "Not using", 1, 0)
# drop column
segment <- segment %>% select(-first_bikeCPU_brand,-second_CPU_brand) 
str(segment)
for (i in 14:23){
  segment[,i] <- as.factor(segment[,i])
}
str(segment)

segment$id <- id

# predict who will buy Garmin as their as second bike CPU
# get training data set
garmin <- segment %>% dplyr::filter(second_NotUsing == 0) %>% select(1:19)
str(garmin)

rf.garmin <- randomForest(formula = second_isGarmin~.,data = garmin,importance = T)
rf.garmin
varImpPlot(rf.garmin)

rf2.garmin <- randomForest(formula = second_isGarmin~ first_isBryton + first_isGarmin + first_isWahoo + pwr_20min +FTP,
                          data = garmin,importance = T)
rf2.garmin
varImpPlot(rf2.garmin)

# Use stratified sample simce the data set is unbalanced.
garmin <- segment %>% dplyr::filter(second_NotUsing == 0) %>% select(1:19) %>% 
  group_by(second_isGarmin) %>%
  sample_n(size=2428)

table(garmin$second_isGarmin)
str(garmin)

rf3.garmin <- randomForest(formula = second_isGarmin~.,data = garmin,importance = T)
rf3.garmin
varImpPlot(rf3.garmin)

rf4.garmin <- randomForest(formula = second_isGarmin~ first_isGarmin + first_isWahoo + first_isOther + first_isBryton + pwr_20min + pwr_60min + pwr_10min + FTP + first_NotUsing,
                           data = garmin,importance = T)
rf4.garmin
varImpPlot(rf4.garmin)


# Wahoo
wahoo <- segment %>% dplyr::filter(second_NotUsing == 0) %>% select(c(1:18,21))
str(wahoo)

rf.wahoo <- randomForest(formula = second_isWahoo~.,data = wahoo,importance = T)
rf.wahoo
varImpPlot(rf.wahoo)

rf2.wahoo <- randomForest(formula = second_isWahoo~ first_isBryton + first_isGarmin + first_isWahoo + pwr_20min + pwr_60min + Seg1_1 +pwr_10min,
                           data = wahoo,importance = T)
rf2.wahoo
varImpPlot(rf2.wahoo)

# stratified sample
wahoo <- segment %>% dplyr::filter(second_NotUsing == 0) %>% select(c(1:18,21)) %>% 
  group_by(second_isWahoo) %>%
  sample_n(size=2292)

table(wahoo$second_isWahoo)
str(wahoo)

rf3.wahoo <- randomForest(formula = second_isWahoo~.,data = wahoo,importance = T)
rf3.wahoo
varImpPlot(rf3.wahoo)

rf4.wahoo <- randomForest(formula = second_isWahoo~ first_isGarmin + first_isWahoo + first_isOther + first_isBryton + pwr_20min + pwr_60min + pwr_90min + FTP ,
                           data = wahoo,importance = T)
rf4.wahoo
varImpPlot(rf4.wahoo)


# predict who will buy wahoo
predict <- segment %>% dplyr::filter(second_NotUsing == 1)
predict_fit <- predict %>% select(first_isGarmin, first_isWahoo, first_isOther, first_isBryton, pwr_20min, pwr_60min, pwr_90min, FTP)
prob_rf <- predict(rf4.wahoo,newdata = predict_fit,type = 'prob')
prob_rf <- as.data.frame(prob_rf)
prob_rf$id <- predict$id
names(prob_rf)
prob_rf <- prob_rf %>% rename('Prob_NotBuy'='0','Prob_Buy'='1')
prob_rf <- prob_rf %>% arrange(desc(Prob_Buy))
# customer with prob > 65%
prob.65 <- prob_rf %>% filter(Prob_Buy > 0.65)
View(prob.65)

# appendix ------------------------
# fatigue and fitness
# 1. https://joefrieltraining.com/managing-training-using-tsb/
# 2. https://forum.bikehub.co.za/articles/advice/monitoring-your-training-load-r7477/
# 3. https://www.roadbikerider.com/ctl-important-training-number/#:~:text=It's%20calculated%20as%20a%20rolling,important%20number%20in%20bike%20training.

# regression model tbl
# https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_model_estimates.html

# glmnet l1+l2 regressor
# https://www.rdocumentation.org/packages/glmnet/versions/4.1-4

# ggplot2 
# label/legend manipulation
# https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
# palette
# https://ggplot2.tidyverse.org/reference/scale_brewer.html

# Logistic regression
# Scale
# sg_scale <- as.data.frame(scale(segment[, sapply(segment, is.numeric)]))
# sg_scale$Seg1 <- segment$Seg1
# sg_scale$age_under40 <- segment$age_under40
# sg_scale$gender <- segment$gender
# sgsc.model <- lm(Seg1~.,data = segment)
# summary(sgsc.model)
# tab_model(sgsc.model)
# 
# # -pwr_20min ----- #2 ------
# str(segment$pwr_20min)
# sg2.model <- lm(Seg1 ~ . -pwr_20min, data = segment)
# summary(sg2.model)
# tab_model(sg2.model)
# 
# # - Seg1_1 ----- #3 ------
# str(segment$pwr_20min)
# sg3.model <- lm(Seg1 ~ . - pwr_20min - Seg1_1, data = segment)
# summary(sg3.model)
# tab_model(sg3.model)
# 
# # - bikeweight ----- #4 ------
# str(segment$pwr_20min)
# sg4.model <- lm(Seg1 ~ . - pwr_20min - Seg1_1 - bikeweight, data = segment)
# summary(sg4.model)
# tab_model(sg4.model)
# 
# # - age_under
# sg5.model <- lm(Seg1 ~ . - pwr_20min - Seg1_1 - gender - age_under40, data = segment)
# summary(sg5.model)
# tab_model(sg5.model)