library(forecast)
setwd('C:/Users/Elias/Documents/ManSci Year 2/Data Analytics II/SplitBanana') # Set work directory to organise files
# Load the Split Banana sales data. Create month as new factor variable.
sales.data <- read.csv('SplitBanana.csv', stringsAsFactors = FALSE)
sales.data$Date <- as.Date(sales.data$Date, format = "%m/%d/%Y") # Sets all dates to the same format

###### Add the 91 days of Apr, May, and June to the data frame
dates.test <- as.data.frame(seq(from = as.Date("04/01/14", "%m/%d/%y"), length.out = 91, by = "day"))
# Make the dates.test data frame to the same format as the sales.data data frame
dates.test$Sales <- NA
dates.test$Salesimp <- NA
# Set all rows of Staunton.Jams column to 0 meaning there is no festival
dates.test$Staunton.Jams <- 0
# We know that there was a Staunton Jams Festival on the 17th of May 2014. Therefore, set this row to 1
dates.test$Staunton.Jams[47] <- 1
dates.test$Salesadj <- NA
# Specify the names of the forecast dates data frame so that we can easily merge it with sales.data
colnames(dates.test) <- c("Date", "Sales", "Salesimp", "Staunton.Jams", "Salesadj")
# Merge the data frames using row-bind
sales.data <- as.data.frame(rbind(sales.data, dates.test))
tail(sales.data)
length(sales.data$Date)

####### Determine what month every date in sales.data is and store it in Month as a factor
Month <- factor(months(sales.data$Date, abbreviate = TRUE), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Another seasonality we noticed in Tableau was that sales increase towards the weekend
# We add this as another factor seasonality variable like we did with the month to later improve our prediction
# Use the R-function "weekdays" to determine what weekday a specific date was
Weekday <- factor(weekdays(sales.data$Date, abbreviate = TRUE), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Sales increase heavily during the Staunton Jams festival
# Therefore, we decided to incorporate this into our model to improve our predictions further
# ADD FESTIVAL Staunton.Jams as factor in a similar way to how we added month and weekday
festival <- factor(sales.data$Staunton.Jams)

# We want to use this festival factor as a dummy variable in our regression matrix
festival.dummies <- model.matrix(~ festival + 0) # Create the Staunton.Jams seasonal dummies using the festival factor variable in the model.matrix function.  The "+ 0" will make sure that one of the dummies is not automatically dropped.
# Look at the head of festival.dummies to see that everything is in order
head(festival.dummies)


#### Interaction matrix
# Initially we used dummy variables for month and weekday too. But we noticed how in Tableau there is some interaction between
# The weekdays and the months. To use this in our model we create an interaction matrix using the data from Month and Weekday
interaction.mat <- model.matrix(~ Month:Weekday + 0)

# Store the total number of rows in the variable ntotal
ntotal <- length(sales.data$Date)
# Check that ntotal is the correct number
ntotal

# Create new data frame with the interaction matrix and the festival dummies
x.df <- as.data.frame(cbind(interaction.mat, festival.dummies, seq(1, ntotal)))  # Put the festival seasonal dummies, the interaction matrix and a trend together into a data frame.

# Renaming x.df to x and removing two columns that are redundant because they are perfectly correlated
x <- x.df[ , -c(1,85)]

head(x)  # Inspect the design matrix.
tail(x)

# Create the y (or response variable).
y <- sales.data$Salesimp

######## LOG THE Y VARIABLE to linearize the relationship between the values
logy <- log(y)

# Create training and validation sets using our logy instead of y.
nvalid <- 365
ntrain <- ntotal - nvalid
xtrain <- x[1:ntrain, ]
ytrain <- logy[1:ntrain]
# We don't want the last 3 months in our validation set because the y-values for these dates are NA. Therefore,
# shift the validation set so that it ends before the 1t of April 2014
xvalid <- x[(ntrain + 1):ntotal - 91, ]
yvalid <- logy[(ntrain + 1):ntotal - 91]

# Fit a linear regression to the training set. Make forecasts in the validation set.
# Because we changed the training and validation y-sets, we don't need to change any code in this part
sales.lm <- lm(ytrain ~ ., data = xtrain)
options(scipen=999)
summary(sales.lm)
sales.lm.pred <- predict(sales.lm, newdata = xvalid, interval = "prediction", level = 0.95)  # Unlike ets, we set "level" to one number.  It gives the "level" central prediction interval. 
head(sales.lm.pred)  # The prediction matrix has three columns: mean and lower and upper limits of the prediction interval. For example, with level = 0.95, the lower limit is the 2.5%-quantile, and the upper limit is the 97.5%-quantile.
accuracy(sales.lm.pred[, 1], yvalid)  # Score the mean prediction, which is in the first column of the prediction matrix. 
# Trend and Monthly Seasonal Regression Model: RMSE = 351, MAE = 269, MAPE = 46.5

dates.train <- sales.data$Date[1:ntrain]
dates.valid <- seq(from = sales.data$Date[ntrain + 1], length.out = nvalid, by = 'day')

plot(x = dates.train, y = ytrain, # This gives R the data to be plotted.
     #     xlim = c(min(dates.train), max(dates.valid)),  # This sets the range on the x-axis.
     xlim = c(as.Date("2013-01-01"), max(dates.valid)),  # To zoom in, uncomment this line and comment the one above.
     type = "l",     # This tells R to 'connect the dots' and make a line plot. 
     las = 1,        # This flips the numbers along the y-axis to be horizontal.
     bty = "l",      # This controls the lines making up the box around the plots.
     xlab = "Date",  # This labels the x-axis.
     ylab = "Sales") # This labels the y-axis.
lines(x = dates.valid, y = sales.lm.pred[ ,1], col = "blue")
lines(x = dates.valid, y = yvalid, col = "black", lty = 2)

# re-fit model to the entire training set and make forecasts in the testing set (April 1, 2014 through June 31, 2014).

# Create a testing set of the last three months
xtest <- x[(ntotal-90):ntotal, ] # [, ] creates a matrix
ytest <- logy[(ntotal-90):ntotal] # [] creates a vector

# Create a prediction for the forecast period, using the model we built using the training and validations sets (sales.lm)
sales.lm.pred <- predict(sales.lm, newdata = xtest, interval = "prediction", level = 0.6)
accuracy(sales.lm.pred[, 1], yvalid)
#?accuracy # Get better understanding of what the different abbreviations in the accuracy means

# "Un-log'ing", using lapply exp function to reverse the logarithm functiong to get the y-values back to sales numbers 
fit.pred <- exp(sales.lm.pred)


# Write the mean predictions for daily sales in June 2014 to a csv file for use in the Staffing Model. You can open the csv file in Excel and copy and paste the forecasts into the Staffing Model.
pred.vec <- fit.pred
write.csv(pred.vec, file = "60ConfSplitBananaForecastLog.csv")
