world_happiness <- read.csv("World-happiness.csv")
glimpse(world_happiness)
#Check the data type of this file?
class(world_happiness)
str(world_happiness)


#------------- Step 1 ----------

#The world_happiness data frame contains headings that could cause future issues when
# referencing them , So firstly rename them 

names(world_happiness)
names(world_happiness)[1]  <- "Country"
names(world_happiness)[3]  <- "Happiness_Score"
names(world_happiness)[4]  <- "GDP"
names(world_happiness)[5]  <- "Social_Support"
names(world_happiness)[6]  <- "Health"
names(world_happiness)[7]  <- "Freedom"
names(world_happiness)[9]  <- "Corruption"
names(world_happiness)[10]  <- "Positive_effect"
names(world_happiness)[11]  <- "Negative_effect"
names(world_happiness)
#------------- Step 2 ----------

# ----------------------------------------------------------
# --that there are 1949 records.
# --variable 1 is Char,variable 2 is int, and 3 to 11 is numeric in nature. 
# ----------------------------------------------------------

#Missing values dealing (View the records with NA)
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records

# 241 observation have NA
# Check the columns with  NA and sum the coloumn wise NA values
sapply(world_happiness, function(x) sum(is.na(x)))

# Corruption column has the maximum number of NA values so we will impute NA in this specific
# column with the mean value
world_happiness$Corruption[is.na(world_happiness$Corruption)] <- 
  mean(world_happiness$Corruption, na.rm=TRUE)

# Check that NA is replaced or not
sapply(world_happiness, function(x) sum(is.na(x)))
# Now corruption column have zero NA but other columns have the NA values which is not
#much in numbers

#Now check again for NA record
na_records <- world_happiness[!complete.cases(world_happiness),]
na_records
# only 72 rows have the NA value Now
#----- remove the remaining NA with na.omit()-------

world_happiness1 <- na.omit(world_happiness)

nrow(world_happiness1)
sapply(world_happiness1, function(x) sum(is.na(x)))
sum(is.na(world_happiness1))

#------------- Step 3 ----------

# Create a New data frame which has the numerical variable

numeric_variable_list <- sapply(world_happiness1, is.numeric)
numeric_variable_list

world_happiness_final <- world_happiness1[numeric_variable_list]
world_happiness_final

#-----Step 3  Exploratory Data Analysis------
# This step is used to explain or check any pattern that can indicates the any kind of correlation between 
#variables

pairs(world_happiness_final)


install.packages("psych")
library(psych)


pairs.panels(world_happiness_final,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 15,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 6,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
#It is clearly visible that GDP, Social_support and Health
#has the most positive correlation with Happiness_score the most.
# Happiness_Score is the score
#each country got for happiness, which will be our main focus in this data.


#implement a correlation plot to check the correlation betweeen variables
correlation_tab <- cor(world_happiness_final)
install.packages("corrplot")

library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(correlation_tab, method = "color", shade.col = NA, tl.col = "black", 
         tl.srt = 45,tl.cex =1,cl.cex=1,col = col(200), addCoef.col = "black",
         order = "AOE",number.cex = .5)
# Through this Correlation plot we can find correlation between our variables
# with some clarity to an extent
# For example GDP has strong Positive correlation with Happiness_Score
# which means that more the GDP, More will be the Happinesss_Score.


# Extract the variable of interest

varibles_of_interest <- names(world_happiness_final) %in% c("Happiness_Score",
                                                      "GDP",
                                                      "Social_Support",
                                                      "Health",
                                                      "Freedom")


varibles_of_interest
# Showing pairs plot for the variables of interest
pairs(world_happiness_final[varibles_of_interest])

attach(world_happiness_final)
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

# Showing scatter plot for each of the data variables against Happiness_Score
scatter.smooth(x = world_happiness_final$GDP,
               y = world_happiness_final$Happiness_Score,
               xlab = "GDP",
               ylab = "Happiness_Score", 
               main = "Happiness_Score ~ GDP")

scatter.smooth(x = world_happiness_final$Social_Support,
               y = world_happiness_final$Happiness_Score,
               xlab = "Social_Support",
               ylab = "Happiness_Score", 
               main = "Happiness_Score ~ Social_Support")

scatter.smooth(x = world_happiness_final$Health,
               y = world_happiness_final$Happiness_Score,
               xlab = "Health",
               ylab = "Happiness_Score", 
               main = "Happiness_Score ~ Health")

scatter.smooth(x = world_happiness_final$Freedom,
               y = world_happiness_final$Happiness_Score,
               xlab = "Freedom",
               ylab = "Happiness_Score", 
               main = "Happiness_Score ~ Freedom")
detach(world_happiness_final)

# Showing the correlation values for each of the 
# data variables again the Happiness_Score

paste("Correlation for Happiness_Score and GDP:", 
cor(world_happiness_final$GDP, world_happiness_final$Happiness_Score))

paste("Correlation for Happiness_Score and Social_Support:", 
cor(world_happiness_final$Social_Support, world_happiness_final$Happiness_Score))


paste("Correlation for Happiness_Score and Health:", 
cor(world_happiness_final$Health, world_happiness_final$Happiness_Score))

paste("Correlation for Happiness_Score and Freedom:", 
cor(world_happiness_final$Health, world_happiness_final$Freedom)


# Finding outliers in the data variables
install.packages("dplyr") # Install dplyr package
library("dplyr") 
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,3))# Load dplyr package
attach(world_happiness_final)


# box plot for 'Happiness_Score'
boxplot(GDP,
        main = "Happiness_Score",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Happiness_Score)$out))
# box plot for 'GDP'
boxplot(GDP,
        main = "GDP",
        sub = paste("Outlier rows: ",
boxplot.stats(GDP)$out))

# box plot for 'Social_Support'
boxplot(Social_Support,
        main = "Social_Support",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Social_Support)$out))
# box plot for 'Health'
boxplot(Health,
        main = "Health",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Health)$out))

# box plot for 'Freedom'
boxplot(Freedom,
        main = "Freedom",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Freedom)$out))
detach(world_happiness_final)
par(opar)

# Use boxplot.stats() function to detect relevant outliers

#Detect the Outlier Values For Social_Support
outlier_values <- boxplot.stats(world_happiness_final$Social_Support)$out # outlier values.
paste(" Social_Support outliers: ", paste(outlier_values, collapse=", "))

#Remove the Outliar Values For Social_Support
world_happiness_final <- subset(world_happiness_final,
                                world_happiness_final$Social_Support != 0.451
                                & world_happiness_final$Social_Support != 0.484
                                & world_happiness_final$Social_Support != 0.491
                                & world_happiness_final$Social_Support != 0.42
                                & world_happiness_final$Social_Support != 0.467
                                & world_happiness_final$Social_Support != 0.445
                                & world_happiness_final$Social_Support != 0.382
                                & world_happiness_final$Social_Support != 0.477
                                & world_happiness_final$Social_Support != 0.434
                                & world_happiness_final$Social_Support != 0.493
                                & world_happiness_final$Social_Support != 0.436
                                & world_happiness_final$Social_Support != 0.504
                                & world_happiness_final$Social_Support != 0.442
                                & world_happiness_final$Social_Support != 0.291
                                & world_happiness_final$Social_Support != 0.326
                                & world_happiness_final$Social_Support != 0.422
                                & world_happiness_final$Social_Support != 0.485
                                & world_happiness_final$Social_Support != 0.483
                                & world_happiness_final$Social_Support != 0.387
                                & world_happiness_final$Social_Support != 0.29
                                & world_happiness_final$Social_Support != 0.32
                                & world_happiness_final$Social_Support != 0.479
                                & world_happiness_final$Social_Support != 0.503
                                & world_happiness_final$Social_Support != 0.494
                                & world_happiness_final$Social_Support != 0.479
                                & world_happiness_final$Social_Support != 0.373
                                & world_happiness_final$Social_Support != 0.486
                                & world_happiness_final$Social_Support != 0.489
                                & world_happiness_final$Social_Support != 0.464
                                & world_happiness_final$Social_Support != 0.435
                                & world_happiness_final$Social_Support != 0.291
                                & world_happiness_final$Social_Support != 0.303
                                & world_happiness_final$Social_Support != 0.444
                                & world_happiness_final$Social_Support != 0.479
                                & world_happiness_final$Social_Support != 0.508
                                & world_happiness_final$Social_Support != 0.506
                                & world_happiness_final$Social_Support != 0.514
                                & world_happiness_final$Social_Support != 0.507
                                & world_happiness_final$Social_Support != 0.517
                                & world_happiness_final$Social_Support != 0.511
                                & world_happiness_final$Social_Support != 0.512
                                & world_happiness_final$Social_Support != 0.510
                                & world_happiness_final$Social_Support != 0.517
                                & world_happiness_final$Social_Support != 0.521
                                & world_happiness_final$Social_Support != 0.526
                                & world_happiness_final$Social_Support != 0.523
                                & world_happiness_final$Social_Support != 0.524
                                & world_happiness_final$Social_Support != 0.522
                                & world_happiness_final$Social_Support != 0.528
                                & world_happiness_final$Social_Support != 0.528
                                & world_happiness_final$Social_Support != 0.509
                                & world_happiness_final$Social_Support != 0.529
                                & world_happiness_final$Social_Support != 0.530
                                & world_happiness_final$Social_Support != 0.532
                                & world_happiness_final$Social_Support != 0.533
                                & world_happiness_final$Social_Support != 0.535
                                & world_happiness_final$Social_Support != 0.538)
#Detect Again that No outliars is present

outlier_values <- boxplot.stats(world_happiness_final$Social_Support)$out # outlier values.
paste(" Social_Support outliers: ", paste(outlier_values, collapse=", "))

#Detect the Outlier Values For Health
 outlier_values <- boxplot.stats(world_happiness_final$Health)$out # outlier values.
 paste(" Health outliers: ", paste(outlier_values, collapse=", ")) 
 #Remove Outliers for health
 world_happiness_final <- subset(world_happiness_final,
                                 world_happiness_final$Health != 40.9
                                 & world_happiness_final$Health != 43.18
                                 & world_happiness_final$Health != 44.14
                                 & world_happiness_final$Health != 44.62
                                 & world_happiness_final$Health != 40.38
                                 & world_happiness_final$Health != 32.30
                                 & world_happiness_final$Health != 36.86
                                 & world_happiness_final$Health != 41.42
                                 & world_happiness_final$Health != 44.88
                                 & world_happiness_final$Health != 44.80
                                 & world_happiness_final$Health != 44.64
                                 & world_happiness_final$Health != 40.30
                                 & world_happiness_final$Health != 41.20
                                 & world_happiness_final$Health != 42.10
                                 & world_happiness_final$Health != 43.90
                                 & world_happiness_final$Health != 44.32
                                 & world_happiness_final$Health != 40.808
                                 & world_happiness_final$Health != 44.20
                                 & world_happiness_final$Health != 41.58
                                 & world_happiness_final$Health != 42.86
                                 & world_happiness_final$Health != 45.10
                                 & world_happiness_final$Health != 45.42
                                 & world_happiness_final$Health != 45.50
                                 & world_happiness_final$Health != 45.16
                                 & world_happiness_final$Health != 44.26
                                 & world_happiness_final$Health != 45.42
                                 & world_happiness_final$Health != 45.74
                                 & world_happiness_final$Health != 45.78
                                 & world_happiness_final$Health != 45.74
                                 & world_happiness_final$Health != 44.12
                                 & world_happiness_final$Health != 45.68
                                 & world_happiness_final$Health != 45.58
                                 & world_happiness_final$Health != 45.72
                                 & world_happiness_final$Health != 45.10
                                 & world_happiness_final$Health != 45.42
                                 & world_happiness_final$Health != 45.55
                                 & world_happiness_final$Health != 45.16
                                 & world_happiness_final$Health != 45.16
                                 & world_happiness_final$Health != 45.48
                                 & world_happiness_final$Health != 45.42
                                 & world_happiness_final$Health != 45.98
                                 & world_happiness_final$Health != 46.06
                                 & world_happiness_final$Health != 45.98
                                 & world_happiness_final$Health != 45.92
                                 & world_happiness_final$Health != 46.20
                                 & world_happiness_final$Health != 46.20
                                 & world_happiness_final$Health != 46.00
                                 & world_happiness_final$Health != 46.20
                                 
                                 
                                 )
 # Detect again for outliers
 outlier_values <- boxplot.stats(world_happiness_final$Health)$out # outlier values.
 paste(" Health outliers: ", paste(outlier_values, collapse=", "))
 # there is no outliers now in health

 # Outliers detection for Freedom
 outlier_values <- boxplot.stats(world_happiness_final$Freedom)$out # outlier values.
 paste(" Freedom outliers: ", paste(outlier_values, collapse=", ")) 
 # remove outliars from Freedom
 
 world_happiness_final <- subset(world_happiness_final,
                                 world_happiness_final$Freedom != 0.342
                                 & world_happiness_final$Freedom != 0.347)
 # Detect again for outliers
 outlier_values <- boxplot.stats(world_happiness_final$Freedom)$out # outlier values.
 paste(" Freedom outliers: ", paste(outlier_values, collapse=", ")) 
 
 # Draw boxplot again for outlier detection
 opar <- par(no.readonly = TRUE)
 par(mfrow = c(2,3))# Load dplyr package
 attach(world_happiness_final)
 
 
 # box plot for 'Happiness_Score'
 boxplot(GDP,
         main = "Happiness_Score",
         sub = paste("Outlier rows: ",
                     boxplot.stats(Happiness_Score)$out))
 # box plot for 'GDP'
 boxplot(GDP,
         main = "GDP",
         sub = paste("Outlier rows: ",
                     boxplot.stats(GDP)$out))
 
 # box plot for 'Social_Support'
 boxplot(Social_Support,
         main = "Social_Support",
         sub = paste("Outlier rows: ",
                     boxplot.stats(Social_Support)$out))
 # box plot for 'Health'
 boxplot(Health,
         main = "Health",
         sub = paste("Outlier rows: ",
                     boxplot.stats(Health)$out))
 
 # box plot for 'Freedom'
 boxplot(Freedom,
         main = "Freedom",
         sub = paste("Outlier rows: ",
                     boxplot.stats(Freedom)$out))
 detach(world_happiness_final)
 par(opar)
 
 
 # Skewness function to examine normality
 #install.packages("e1071")
 library(e1071)
 opar <- par(no.readonly = TRUE)
 par(mfrow = c(2,5)) # divide graph area into 1 row x 2 cols
 # minimally skewed to the left
 # skewness of < -1 or > 1 = highly skewed
 # -1 to -0.5 and 0.5 to 1 = moderately skewed
 # Skewness of -0.5 to 0.5 = approx symetrical
 
 # Density plot for Happiness_Score
 
 plot(density(world_happiness_final$Happiness_Score),
      main = "Density plot : Happiness_Score",
      ylab = "Frequency", xlab = "Happiness_Score",
      sub = paste("Skewness : ", round(e1071::skewness(world_happiness_final$Happiness_Score), 2)))
 
 # fill the area under the plot
 
 polygon(density(world_happiness_final$Happiness_Score), col = "red")
 # Density plot for GDP
 
 plot(density(world_happiness_final$GDP),
      main = "Density plot : GDP",
      ylab = "Frequency", xlab = "GDP",
      sub = paste("Skewness : ", round(e1071::skewness(world_happiness_final$GDP), 2)))
 
 # fill the area under the plot
 

 polygon(density(world_happiness_final$GDP), col = "blue")
 # Density plot for Social_Support
 plot(density(world_happiness_final$Social_Support),
      main = "Density plot : Social_Support",
      ylab = "Frequency", xlab = "Social_Support",
      sub = paste("Skewness : ", round(e1071::skewness(world_happiness_final$Social_Support), 2)))
 
 # fill the area under the plot
 
 polygon(density(world_happiness_final$Social_Support), col = "green")
 
 # Density plot for Health
 plot(density(world_happiness_final$Health),
      main = "Density plot : Health",
      ylab = "Frequency", xlab = "Health",
      sub = paste("Skewness : ", round(e1071::skewness(world_happiness_final$Health), 2)))
 
 # fill the area under the plot
 
 polygon(density(world_happiness_final$Health), col = "black")
 
 # Density plot for Freedom
 plot(density(world_happiness_final$Freedom),
      main = "Density plot : Freedom",
      ylab = "Frequency", xlab = "Freedom",
      sub = paste("Skewness : ", round(e1071::skewness(world_happiness_final$Freedom), 2)))
 
 # fill the area under the plot
 
 polygon(density(world_happiness_final$Health), col = "yellow")
 
 
 #Checking the Normality test with qq norm
 opar <- par(no.readonly = TRUE)
 par(mfrow = c(1, 2)) # divide graph area in 2 columns
 hist(world_happiness_final$GDP, main = "Normality proportion of GDP", xlab = "GDP")
 
 qqnorm(world_happiness_final$GDP)
 qqline(world_happiness_final$GDP)
 
 hist(world_happiness_final$Social_Support, 
      main = "Normality proportion of Social_Support", xlab = "Social_Support")
 
 qqnorm(world_happiness_final$Social_Support)
 qqline(world_happiness_final$Social_Support)
 par <- opar
 
 # examine by building the model which variable
 # have higher influence on the model
 attach(world_happiness_final)
 set.seed(1)
 no_rows_data <- nrow(world_happiness_final)
 sample_data <- sample(1:no_rows_data,
                       size = round(0.8 * no_rows_data),
                       replace = FALSE)
 
 training_data <- world_happiness_final[sample_data, ]
 test_data <- world_happiness_final[-sample_data, ]
 
 Happiness_Score_model_1 <- lm(Happiness_Score ~
                          GDP +
                          Social_Support +
                          Health,
                        data = training_data)
 
 summary(Happiness_Score_model_1)
 
 
 #Check the accuracy of Happiness_Score_model.
 y_pred_lm_1 = predict(Happiness_Score_model, newdata = test_data)
 
 Pred_Actual_lm_1 <- as.data.frame(cbind(Prediction = y_pred_lm_1, Actual = test_data
                                         $Happiness_Score))
 
 gg.lm <- ggplot(Pred_Actual_lm_1, aes(Actual, Prediction )) +
   geom_point() + theme_bw() + geom_abline() +
   labs(title = "Multiple Linear Regression Model_1", x = "Actual happiness score",
        y = "Predicted happiness score") +
   theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
         axis.title = element_text(family = "Helvetica", size = (10))) 
 
 gg.lm                  
 
 
 #Build the model again
 library(caTools)
 set.seed(123)
 dataset <- world_happiness_final #relevant columns into training
 split = sample.split(dataset$Happiness_Score, SplitRatio = 0.7) #splitting
 training_set = subset(dataset, split == TRUE) #training set
 test_set = subset(dataset, split == FALSE) #test set
 
 
 regressor_lm = lm(formula = Happiness_Score ~ .,
                   data = training_set)
 
 summary(regressor_lm)
 library(ggplot2)
 #Check the accuracy of our regressor_lm.
 y_pred_lm = predict(regressor_lm, newdata = test_set)
 
 Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Happiness_Score))
 
 gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
   geom_point() + theme_bw() + geom_abline() +
   labs(title = "Multiple Linear Regression", x = "Actual happiness score",
        y = "Predicted happiness score") +
   theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
         axis.title = element_text(family = "Helvetica", size = (10))) 
 
 gg.lm                  
                                
 #Predicting data using the model             
 predicted_Score <- predict(regressor_lm, test_set)
 #make actuals_predicted dataframe.                               
 actuals_predictions <- data.frame(cbind(actuals = test_set$Happiness_Score, 
                                         predicted = predicted_Score))
 head(actuals_predictions)    
# calulate the correlation accuracy
 correlation_accuracy <- cor(actuals_predictions)
 correlation_accuracy

 #Now we’ll calculate min max accuracy and mean absolute percentage error (MAPE) 
# which is a measure of prediction accuracy.
 # Min - max accuracy
 min_max_accuracy <- mean(apply(actuals_predictions, 1, min) / apply(actuals_predictions, 1, max))
 min_max_accuracy


 # MAPE
 mape <- mean(abs((actuals_predictions$predicted - actuals_predictions$actuals)) / actuals_predictions$actuals)
 mape
 
 library(DAAG)
 cvResults <- suppressWarnings(CVlm(data = world_happiness_final, 
                                    form.lm = Happiness_Score ~ GDP, 
                                    m = 5, 
                                    dots = FALSE, 
                                    seed = 1, 
                                    legend.pos = "topleft", 
                                    printit = FALSE, 
                                    main = "
                                    Small symbols are predicted v
                                    alues while bigger ones are actuals."));



  df <- data.frame(GDP = c(9.497), Social_Support = c(0.710), 
                        Health = c(51.20), Happiness_Score=c(5.901))
 predicted_Score <- predict(regressor_lm, df)

 predicted_Score









