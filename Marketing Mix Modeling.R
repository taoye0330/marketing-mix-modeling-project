# Import Dataset
setwd("/Users/ianty/Desktop/Marketing Mix Modeling/R Script")
AF=read.csv("MMM_AF_S9.csv", header=TRUE)

########### Exploratory Analysis ##########

# Step 1: Check Missing Values
summary(is.na(AF))

# Step 2: Look at Trend
# Look at Peaks and valleys to determine seasonality

#Trend line
AF$Period=as.Date(AF$Period,"%m/%d/%Y")
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
     )
par(new=TRUE)
plot(AF$Period,AF$Sales.Event,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

#Trend Line: Sales vs. TV GRPs
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
)
par(new=TRUE)
plot(AF$Period,AF$National.TV.GRPs,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='TV GPRs by date',
     axes = FALSE)

#Trend Line: Sales vs. Magazine GRPs
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
)
par(new=TRUE)
plot(AF$Period,AF$Magazine.GRPs,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

#Trend Line: Sales vs. Paid Search
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
)
par(new=TRUE)
plot(AF$Period,AF$Paid.Search,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

#Trend Line: Sales vs. Display
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
)
par(new=TRUE)
plot(AF$Period,AF$Display,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

#Trend Line: Sales vs. Facebook Impressions
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
)
par(new=TRUE)
plot(AF$Period,AF$Facebook.Impressions,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

#Trend Line: Sales vs. Wechat
plot(AF$Period,AF$Sales,
     type='l',
     col='red',
     xlab = 'Period',
     ylab = 'Sales',
     main = 'Sales by date'
)
par(new=TRUE)
plot(AF$Period,AF$Wechat,
     type = 'l',
     col='navy',
     xlab = 'Period',
     ylab = '',
     main='Sales by date',
     axes = FALSE)

# Step 3: Scatter Plot
# Define colors and parameters
col_point <- "#1f77b4"
col_grid <- "#d9d9d9"
# Plot Facebook vs Sales
plot(AF$Facebook.Impressions, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs Facebook Impressions",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

# National TV vs Sales
plot(AF$National.TV.GRPs, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs National TV GRPs",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

# Magazine vs Sales
plot(AF$Magazine.GRPs, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs Magazine GRPs",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

# Display vs Sales
plot(AF$Display, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs Display",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

# Paid Search vs Sales
plot(AF$Paid.Search, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs Paid Search",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

# Wechat vs Sales
plot(AF$Wechat, AF$Sales, type='p', pch=16, col=col_point,
     xlab="Facebook Impressions", ylab="Sales", main="Sales vs Wechat",
     axes=FALSE)

# Draw axes
axis(1, col.axis="grey50", las=1)  # las=1 makes labels horizontal
axis(2, col.axis="grey50", las=1)

# Add grid
abline(h=pretty(AF$Sales), v=pretty(AF$Facebook.Impressions), col=col_grid, lty="dotted")

# Draw a box around the plot
box()

#Step 4: Correlation Matrix
# Look at correlation table and identify strong vs weak correlation

library(corrplot)
correlation=cor(AF[,3:14])
corrplot(correlation,
         tl.cex = 0.5,
         number.cex = 0.5,
         method = "color", 
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, type = "upper",
         addCoef.col = "black", addCoef.size = 2.5,
         col = colorRampPalette(c("red", "ivory", "green"))(100))

########## Data Transformation ##########

### Step 1: Lag Transformation ###
### Transformation 1 ###
AF=read.csv("MMM_AF_S9.csv", header=TRUE)
# TV GRPs no lag

# Magazine GRPs for 1 weeks lag
AF$Magazine.GRPs <- c(0, head(AF$Magazine.GRPs, -1))

# Paid Search no lag

# Display no lag

# Faceobok Impressions no lag

# Wechat no lag


### Step 2: Diminishing Return Transformation ###

# National TV GRPs to the power of 0.9
set_power_TV = 0.9
AF$National.TV.GRPs = AF$National.TV.GRPs^set_power_TV

# Magazine GRPs to the power of 0.6
set_power_Magazine = 0.6
AF$Magazine.GRPs = AF$Magazine.GRPs^set_power_Magazine

# Paid Search to the power of 1
set_power_search = 1
AF$Paid.Search = AF$Paid.Search^set_power_search

# Display to the power of 0.8
set_power_display = 0.8
AF$Display = AF$Display^set_power_display

# Facebook Impression to the power of 0.8
set_power_fb = 0.8
AF$Facebook.Impressions = AF$Facebook.Impressions^set_power_fb

# WeChat to the power of 0.9
set_power_wechat = 0.9
AF$Wechat = AF$Wechat^set_power_wechat

### Step 3: AdStock Transformation ###
library(dplyr)

# Adstock function
adstock_formula <- function(x, lambda) {
  adstocked <- numeric(length(x))
  adstocked[1] <- x[1] * lambda + (1 - lambda) * 0
  for (t in 2:length(x)) {
    adstocked[t] <- x[t] * lambda + (1 - lambda) * adstocked[t-1]
  }
  
  return(adstocked)
}

# Apply adstock transformation to National TV GRPs
lambda_tv <- 0.8  # TV Decay rate
AF$National.TV.GRPs = adstock_formula(AF$National.TV.GRPs, lambda_tv)

# Apply adstock transformation to Magazine GRPs
lambda_magazine <- 0.7  # Magazine Decay rate
AF$Magazine.GRPs = adstock_formula(AF$Magazine.GRPs, lambda_magazine)

#Apply adstock transformation to Paid Search
lambda_search <- 0.9
AF$Paid.Search = adstock_formula(AF$Paid.Search, lambda_search)

#Apply adstock transformation to Display 
lambda_display <- 0.8 
AF$Display = adstock_formula(AF$Display, lambda_display)

#Apply adstock transformation to Facebook Impression
lambda_fb <- 1
AF$Facebook.Impressions = adstock_formula(AF$Facebook.Impressions, lambda_fb)

#Apply adstock transformation to WeChat
lambda_wechat <- 0.8
AF$Wechat = adstock_formula(AF$Wechat, lambda_wechat)

colnames(AF)[colnames(AF) == "National.TV.GRPs"] <- "NationalTV1"
colnames(AF)[colnames(AF) == "Magazine.GRPs"] <- "Magazine1"
colnames(AF)[colnames(AF) == "Paid.Search"] <- "PaidSearch1"
colnames(AF)[colnames(AF) == "Display"] <- "Display1"
colnames(AF)[colnames(AF) == "Facebook.Impressions"] <- "Facebook1"
colnames(AF)[colnames(AF) == "Wechat"] <- "Wechat1"

write.csv(AF,"MMM_AF_1.csv",row.names = FALSE)


########## Transformation 2 ##########
#Read raw data agian for transformation
AF=read.csv("MMM_AF_S9.csv", header=TRUE)

### Step 1: Lag Transformation ###

# TV GRPs for 1 weeks lag
AF$National.TV.GRPs <- c(0, head(AF$National.TV.GRPs, -1))

# Magazine GRPs for 1 week lag
AF$Magazine.GRPs <- c(0, head(AF$Magazine.GRPs, -1))

# Paid Search for 1 week lag
AF$Paid.Search <- c(0, head(AF$Paid.Search, -1))

#Display no lag

# Facebook Impression for 1 week lag
AF$Facebook.Impressions <- c(0, head(AF$Facebook.Impressions, -1))

# Wechat for 1 week lag
AF$Wechat <- c(0, head(AF$Wechat, -1))



### Step 2: Diminishing Return Transformation ###

# National TV GRPs to the power of 0.6
set_power_TV = 0.6
AF$National.TV.GRPs = AF$National.TV.GRPs^set_power_TV

# Magazine GRPs to the power of 0.9
set_power_Magazine = 0.9
AF$Magazine.GRPs = AF$Magazine.GRPs^set_power_Magazine

# Paid Search to the power of 0.7
set_power_search = 0.7
AF$Paid.Search = AF$Paid.Search^set_power_search

# Display to the power of 1
set_power_display = 1
AF$Display = AF$Display^set_power_display

# Facebook Impression to the power of 1
set_power_fb = 1
AF$Facebook.Impressions = AF$Facebook.Impressions^set_power_fb

# WeChat to the power of 1
set_power_wechat = 1
AF$Wechat = AF$Wechat^set_power_wechat

### Step 3: AdStock Transformation ###
library(dplyr)

# Adstock function
adstock_formula <- function(x, lambda) {
  adstocked <- numeric(length(x))
  adstocked[1] <- x[1] * lambda + (1 - lambda) * 0
  for (t in 2:length(x)) {
    adstocked[t] <- x[t] * lambda + (1 - lambda) * adstocked[t-1]
  }
  
  return(adstocked)
}

# Apply adstock transformation to National TV GRPs
lambda_tv <- 0.8  # TV Decay rate
AF$National.TV.GRPs = adstock_formula(AF$National.TV.GRPs, lambda_tv)

# Apply adstock transformation to Magazine GRPs
lambda_magazine <- 0.9  # Magazine Decay rate
AF$Magazine.GRPs = adstock_formula(AF$Magazine.GRPs, lambda_magazine)

#Apply adstock transformation to Paid Search
lambda_search <- 0.9
AF$Paid.Search = adstock_formula(AF$Paid.Search, lambda_search)

#Apply adstock transformation to Display 
lambda_display <- 1
AF$Display = adstock_formula(AF$Display, lambda_display)

#Apply adstock transformation to Facebook Impression
lambda_fb <- 1
AF$Facebook.Impressions = adstock_formula(AF$Facebook.Impressions, lambda_fb)

#Apply adstock transformation to WeChat
lambda_wechat <- 0.9
AF$Wechat = adstock_formula(AF$Wechat, lambda_wechat)

colnames(AF)[colnames(AF) == "National.TV.GRPs"] <- "NationalTV2"
colnames(AF)[colnames(AF) == "Magazine.GRPs"] <- "Magazine2"
colnames(AF)[colnames(AF) == "Paid.Search"] <- "PaidSearch2"
colnames(AF)[colnames(AF) == "Display"] <- "Display2"
colnames(AF)[colnames(AF) == "Facebook.Impressions"] <- "Facebook2"
colnames(AF)[colnames(AF) == "Wechat"] <- "Wechat2"

write.csv(AF,"MMM_AF_2.csv",row.names = FALSE)

########## Marketing Mix Modeling ##########
AF_joined = read.csv("MMM_AF_joined.csv",header=TRUE)

#Test Model
#Step 1: Add base variables that do not require transformation
#Step 2: Add media variables that have been transformed sorting from large investment to small investment
mmm_model = lm(Sales~Black.Friday+July.4th+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat1+Magazine1+Display1+Facebook2,data=AF_joined)
summary(mmm_model)

# Check for muticollinearity using VIFs
library(mctest)
imcdiag(mmm_model, method = "VIF")

#Check for heteroscedasticity
#first, plot the model out and review the siduals vs fitted plot and the Sclae-Location plot
par(mfrow=c(2,2)) # put all 4 charts into 1 page
plot(mmm_model)

#Confirm with an objective test for heteroscedasticity using Breusch Pagan test and NCV test
library(lmtest)
lmtest::bptest(mmm_model)

library(car)
car::ncvTest(mmm_model)

# Find variables combination that does not have multicollinearity and heteroskedasticity
mmm_best_model = lm(Sales~Black.Friday+July.4th+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+Magazine2+PaidSearch1+Display2+Facebook2+Wechat2,data=AF_joined)
summary(mmm_best_model)

# Check for muticollinearity using VIFs
imcdiag(mmm_best_model, method = "VIF")

#Check for heteroscedasticity
#first, plot the model out and review the siduals vs fitted plot and the Sclae-Location plot
par(mfrow=c(2,2)) # put all 4 charts into 1 page
plot(mmm_model)

#Confirm with an objective test for heteroscedasticity using Breusch Pagan test and NCV test
# Breusch Pagan test
lmtest::bptest(mmm_best_model)
# NCV test
car::ncvTest(mmm_best_model)

#Output results
# Contribution(Incremental Volume)
# Add a parameter x= TRUE to return all of the variables in the model
mmm_best_model = lm(Sales~Black.Friday+July.4th+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+Magazine2+PaidSearch1+Display2+Facebook2+Wechat2,data=AF_joined,x=TRUE)

mmm_best_model$x

# Diagonal matrix * variables x, calculate sales impact each day
contribution = mmm_best_model$x%*%diag(mmm_best_model$coefficients)

#Convert contribution to data frame and add columns and rows
contribution =as.data.frame(contribution)
colnames(contribution) = names(mmm_best_model$coefficients)
contribution$Period=AF_joined$Period

#Unpivot Analytic File to Model Master File
library(reshape2)
contribution_unpivot = melt(contribution,id.vars='Period',measure.vars=names(mmm_best_model$coefficients))
write.csv(contribution_unpivot,"contribution.csv",row.names = FALSE)

# Actual Sales vs. Predicted Sales (AVM)
AVM = cbind(AF_joined[,c("Period","Sales")],mmm_best_model$fitted.values)
colnames(AVM)[3] = "Predicted Sales"

# Calculate MAPE
mape = mean(100 * abs((AVM$Sales-AVM$`Predicted Sales`)/AVM$Sales))
print(paste(mape,"%"))

# Plot Actual Sales vs. Predicted Sales
AVM$Period=as.Date(AVM$Period,"%m/%d/%Y")
par(mfrow=c(1,1))

library(ggplot2)
library(scales)
ggplot(AVM, aes(x = AVM$Period)) + 
  geom_line(aes(y = AVM$Sales, color = "Sales")) + 
  geom_line(aes(y = AVM$`Predicted Sales` , color = "Predicted Sales")) +
  labs(title = "Sales vs. Predicted Sales by Period",
       x = "Period",
       y = "Value",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Sales" = "blue", "Predicted Sales" = "red")) + 
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.line.y = element_line(color = "black", size = 1)) +
  scale_y_continuous(labels = comma) 


#Save AVM 
write.csv(AVM,"AVM.csv",row.names = FALSE)


########## Side Diagnostic ##########
# Look at what drives display campaign contribution
# There is no intercept because we are looking at side model and main model already take into account of intercept
side_model = lm(contribution$Display1~0+AF$DisplayAlwaysOnImpression+
     AF$DisplayBrandingImpression+
     AF$DisplayWebsiteImpression+
     AF$DisplayHolidayImpression,x=TRUE)
summary(side_model)
side_model_contribution = side_model$x%*%diag(side_model$coefficients)
colnames(side_model_contribution) = names(side_model$coefficients)
