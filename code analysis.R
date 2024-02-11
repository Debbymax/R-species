library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(moments)

#How are the species different from the two periods
#1) null hypothesis: they aren't different
#2) there is a significant difference.


setwd('C:/Users/User/OneDrive - University of Essex/COURSEWORK/2nd SEMESTER/MA334/MA334/Assignment')
prop_sp <- read.csv('proportional_species_richness_V3.csv')


#Convert the period and land class to factor
prop_sp$period <- as.factor(prop_sp$period)
prop_sp$dominantLandClass <- as.factor(prop_sp$dominantLandClass)

#Getting my randomly allocated taxonomic group
BD7<- select(prop_sp, -c(1,5,7,9,12,13,14))
BD4<- select(prop_sp, c(5,7,9,12))
#Taking the mean for proportional species richness for my BD7
BD7$BD7mean <- rowMeans(BD7[1:7])

#Data Exploration
#I will be exploring some selected features against the two time periods
#Bees
eco_period <- prop_sp%>%pull(period)
par(mfrow=c(1, 1))
skewness(BD7$Bees)
hist(BD7$Bees) # Here a bulk of the species fall between 0.2 and 1.0 with the highest being over a thousand
summary(BD7$Bees)
bm<-mean(BD7$Bees,trim=0.1) # trim to extract outliers and normalize the distribution
boxplot(BD7$Bees) 
plot(BD7$Bees~eco_period)
#Difference between the two periods for this species
hist(BD7%>%filter(period=="Y70")%>%pull(Bees),
     xlab="Bees70 Mean",
     main="Y70 Mean");hist(BD7%>%filter(period=="Y00")%>%pull(Bees),
                                     xlab="Bees00 Mean",main="Y00 Mean")
#Check for the mean
b1<-mean(BD7%>%filter(period=="Y70")%>%pull(Bees), trim = 0.1)
b2<-mean(BD7%>%filter(period=="Y00")%>%pull(Bees), trim = 0.1)
(b2-b1)*100


#Grasshoppers
par(mfrow=c(1, 1))
skewness(BD7%>%filter(period=="Y70")%>%pull(Grasshoppers_._Crickets))
skewness(BD7%>%filter(period=="Y00")%>%pull(Grasshoppers_._Crickets))
hist(BD7$Grasshoppers_._Crickets)
summary(BD7$Grasshoppers_._Crickets)
mean(BD7$Grasshoppers_._Crickets,trim=0.1)
boxplot(BD7$Grasshoppers_._Crickets)
plot(BD7$Grasshoppers_._Crickets~eco_period)
#Difference between the two period for this specie
hist(BD7%>%filter(period=="Y70")%>%pull(Grasshoppers_._Crickets),
     xlab="G_C Mean",
     main="Y70 Mean");hist(BD7%>%filter(period=="Y00")%>%pull(Grasshoppers_._Crickets),
                           xlab="G_C Mean",main="Y00 Mean")
#Check for the mean
g1<-mean(BD7%>%filter(period=="Y70")%>%pull(Grasshoppers_._Crickets), trim = 0.1)
g2<-mean(BD7%>%filter(period=="Y00")%>%pull(Grasshoppers_._Crickets), trim = 0.1)
(g2-g1)*100

#How similar are the species
selected7 <- BD7%>%select(c(1:7))
#Calculating the correlation
cormat <- round(x = cor(selected7,use="pairwise.complete.obs"), digits = 2)
#Visualizing the correlation
corrplot(cormat, type = "full", method = "color", 
         addCoef.col = "black", tl.col = "black", tl.srt = 90,number.cex = 0.7)

#Comparing the mean, sd and skewness of each specie
names7 <- names(selected7)#selecting the columns names
table <- data.frame() # creating a data frame
for(i in names7){
  idx <- match(i, names7) # get the index of the column
  table <- rbind(table,
                 c(i, round(mean(selected7[, idx]), digits = 2),
                   round(sd(selected7[, idx]), digits = 2),
                   round(skewness(selected7[, idx]), digits = 2)
                 ))
}
colnames(table) <- c("taxi_group","mean","sd","skewness")
table %>% arrange(sd, skewness)

#eco_names[i-1],
#exploring the land classification
#The land class count was the same between the two periods
BD7%>%group_by(period,dominantLandClass)%>%count()%>%
  arrange(desc(n))%>%pivot_wider(names_from = period,
                                               values_from = n, values_fill = 0)%>%print(n=45)

#Checking the difference in specie for each land class between the two periods
BD7_change1<- BD7%>%group_by(dominantLandClass,period)%>%
  summarise(LC_mean = mean(BD7mean))%>%
  pivot_wider(names_from = period,values_from = LC_mean, values_fill = 0)%>%
  mutate(eco_change7=Y00-Y70)%>%
  arrange(desc(eco_change7))%>%print(n=45)


# Plot may be too large so I am setting plot size and exporting to png first
png("myplot.png", width = 400, height = 850)
# Set colors based on positive or negative values
colours <- ifelse(BD7_change1$eco_change7 >= 0, "green", "red")
barplot(BD7_change1$eco_change7, horiz = TRUE, beside = TRUE,
        col = colours,width = 0.08, ylim = c(-1, 4), 
        ylab = "Value", names.arg = BD7_change1$dominantLandClass,las = 1)
# Close the PDF file
dev.off()

# Finding the decrease and increase in BD7 mean between the two periods.
sum(BD7_change1$eco_change7<0) # there are 35 out of 45 eco_change less than 0
sum(BD7_change1$eco_change7>0) # there are 10 out of 45 eco_change less than 0
round(sum(BD7_change1$eco_change7<0) / nrow(BD7_change1) * 100,2)
round(sum(BD7_change1$eco_change7>0) / nrow(BD7_change1) * 100,2)


#Exploring the proportional species richness of the BD7 between two periods
Y00_7<- BD7%>%filter(period=="Y00")%>%pull(BD7mean)
Y70_7<- BD7%>%filter(period=="Y70")%>%pull(BD7mean)
skewness(Y00_7)
skewness(Y70_7)

# Combine the data into a data frame
df <- data.frame(value = c(Y00_7, Y70_7), group = rep(c("Y00", "Y70")))
# Plot the density of Y00 and Y70 with ggplot to see similarity
ggplot(df, aes(x = value, fill = group)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Overlayed Density Plot", x = "Period", y = "Density") +
  theme_minimal()

par(mfrow=c(1, 2))  # divide graph area in 2 columns
hist(Y00_7,xlab="Y00 Mean",main="Histogram Y00 Mean");hist(Y70_7,xlab="Y07 Mean",main="Histogram Y07 Mean")

#Hypothesis Test
#Assuming the BD7 mean for the two periods are the same, Lets find out the alternative test
## extract data of the two period separately
#T-test
## run the test to examine the null hypothesis saying the means of the two periods are equal
t.test(Y70_7, Y00_7, alternative = "two.sided")

#F-test
var.test(Y70_7, Y00_7, alternative = "two.sided")


#Simple Linear Regression
#Here using a regression model to show how BD7 matches BD11
#Two periods for the BD7 is Y00_7 and Y70_7.
#Splitting BD1 by period
Y00_11<- BD7%>%filter(period=="Y00")%>%pull(ecologicalStatus)
Y70_11<- BD7%>%filter(period=="Y70")%>%pull(ecologicalStatus)

# Perform a simple linear regression
model1 <- lm(Y00_7 ~ Y00_11)
model2 <- lm(Y70_7 ~ Y70_11)

#For the third model, First i want to get the indexs of the selected 7 from the main data prop_sp

names4 <- names(BD4)
eco_4 <- which(names(prop_sp) %in% names4)
eco_7<-which(names(prop_sp) %in% names7)
eco_sp7 <- names(prop_sp)[eco_7]
eco_sp4 <- names(prop_sp)[eco_4]
#Pasting the BD7Mean to the main data
prop_sp$BD7mean<- BD7$BD7mean
#Calculating the BD4Mean to the main data
prop_sp$BD4mean<- rowMeans(BD4)

model3 <- lm(prop_sp$BD7mean~prop_sp$ecologicalStatus)
# View the results
summary(model1)
plot(model1)
summary(model2)
plot(model2)
summary(model3)
plot(model3)


#Multiple Linear Regression

head(prop_sp)

# now multiple linear regression BD4 against the selected 7 

# Create Training and Test data 
trainingRowIndex <- sample(1:nrow(prop_sp), 0.8*nrow(prop_sp))  # row indices for 80% training data
trainingData <- prop_sp[trainingRowIndex, ]  # model training data
testData  <- prop_sp[-trainingRowIndex, ] # for test data remove NAs 

# Build the model on training data
lmMod_train <- lm(BD4mean~.,
                  data=trainingData[c(eco_sp7,"BD4mean")],
                  na.action=na.omit,y=TRUE)
summary (lmMod_train)  # model summary
cor(lmMod_train$fitted.values,lmMod_train$y) # cor training data 
BD_4_Pred <- predict(lmMod_train, testData) # predict to check model on test Data
cor(BD_4_Pred,testData$BD4mean)
plot(BD_4_Pred~testData$BD4mean)
abline(0,1,col="red")

# mis_fit_to_testData are the residuals for the train model fit to the test data 
mis_fit_to_testData <- testData$BD4mean-BD_4_Pred
plot(mis_fit_to_testData~BD_4_Pred) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

#Feature selection using AIC
aic_val <- rep(NA, length(eco_sp7))
#Use a for loop to loop over each feature
for (i in seq_along(eco_sp7)) {
  print(i)
  features <- eco_sp7[1:i]
  print(features)
  model <- lm(BD4mean~.,
              data=trainingData[c(features,"BD4mean")],
              na.action=na.omit,y=TRUE)
  #view the p values of the selected feature
  print(summary(model))
  #store the aic value of each selection
  aic_val[i] <- AIC(model)
}

aic_val
which.min(aic_val)
best_features <- prop_sp$BD4mean[1:which.min(aic_val)]

#Analysis
#Checking the change in the location
BD7_change2<- prop_sp%>%group_by(Location,dominantLandClass,Easting,Northing,period)%>%
  summarise(LC_mean = mean(BD7mean))%>%
  pivot_wider(names_from = period,values_from = LC_mean, values_fill = 0)%>%
  mutate(eco_change7=Y00-Y70)%>%
  arrange(eco_change7)%>%print(n=45)
sum(BD7_change2$eco_change7<0)
sum(BD7_change2$eco_change7>0)
#Map showing the area of increase and decrease
ggplot(BD7_change2, aes(x=Easting, y=Northing, color=ifelse(eco_change7 > 0, "Increase", "Decrease"))) +
  geom_point() +
  scale_color_manual(values=c("Increase"="blue", "Decrease"="red")) +
  labs(x="Easting", y="Northing", color="eco_change7",
       title='Change in Biodiversity Status between the Periods')


