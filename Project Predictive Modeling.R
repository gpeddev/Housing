## Problem.

# We want to predict the sale price of a house based on characteristics
# of the house such as elevation,Distance from amenities, number of bathrooms
# square feet of the house, parking type, and amount of precipitation.


## Data Collection and variables selections.

# Our predict variables from the dataset are elevation, dist_am1, dist_am2,
# dist_am3, bath, sqft, parking, and precip
# Our response variable is price.

# We load the dataset into the data frame housing.
housing<-read.csv("./Workspace/Predictive Modeling/housing.csv")

# We take a look at the variable types.
str(housing)
sapply(housing,class)

# We notice that we have a category variable called parking with more than two
# categories. It is of type factor so we dont have to change it.

# Next we check our data frame for missing values.
table(is.na(housing))

# Next we take a look at our variables values.
summary(housing)

# We notice a really extreme value in bath number. A house with 63 bathrooms!
# We also notice the values parking gets (Covered, No Parking, Not Provided,
# Open). That means that we have 4 categories in that variable.
# We also notice an big value in Price variable 12500000.
housing[housing$bath==63,]

# We notice that extreme max price, bath, sqft reffer to the same observation.
# That does make the particular observation a suspect of being an outlier.

# Another observation we can make is that there is minimum precipitation -110
# this is obviously a mistake so we will remove that observation.
housing[housing$precip==-110,]


# I remove the observation with the mistake value in the precipitation.
index_rem<-which(housing$precip==-110)
housing<-housing[-index_rem,]


# Now we run summary on our data again
summary(housing)

# We notice that our new minimum in precipitation is 0. Since the data are not
# specific from a area that we know it cant have precipitation 0 we leave it as
# it is. 


# We take another look of our data with ggpairs
library(ggplot2)
library(GGally)
ggpairs(housing)

# We notice a strong correlation between the variables dist_am3, dist_am2 and 
# dist_am1 and we notice a strong correlation between the sqft and the number 
# of baths.
# We also notice that there is an outlier. 
# In a lot of plots we see a dot that it is away from the rest.
# For example the scatter plots in price show a really high value.
# This is probably the extreme value with price 12500000.



## Model Specification and Method of fitting.

model_1<-lm(price~.,data=housing)
summary(model_1)

# We notice on our data that bath and sqft are statistical significant based on
# their p-value.

par(mfrow=c(2,2))
plot(model_1)

# We take a closer look
par(mfrow=c(1,1))
plot(model_1)

# Its obvious that we have an outlier in observation 348. Its the same
# observation we noticed on the previous steps
housing[housing$bath==63,]

# We can confirm it with the following

library(car)
outlierTest(model_1)

# We can also see it with cooks distance
library(ggfortify)
autoplot(model_1,1:6)

# We remove the outlier and repeat the process.

index_rem<-which(housing$bath == 63)
housing2<-housing[-index_rem,]

model_2<-lm(price~.,data=housing2)

# We plot again ggpairs
ggpairs(housing2)

# Now with the outlier removed we can clearly see the scatterplots in ggpairs.

autoplot(model_2,1:6)

# We notice the Residuals and they look ok.
# I tried log transformation on the response variable but although it fixed q-q
# the transformation broke the other plots so I decided not to apply it. 

summary(model_2)

# We notice on our data that bath is statistical significant based on
# their p-value.

par(mfrow=c(2,2))
plot(model_2)

# We take a closer look
par(mfrow=c(1,1))
plot(model_2)
outlierTest(model_2)
autoplot(model_2,1:6)


# We take a closer look at residuals since our model is based in a categorical 
# variable
library(moderndive)
temp<-get_regression_points(model_2)

ggplot(temp, aes(x = bath, y = residual)) +
  geom_jitter(width = 0.1) + 
  labs(x = "Bath", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue")

ggplot(temp, aes(x = residual)) +
  geom_histogram(binwidth = 40000, color = "white") +
  labs(x = "Residual")

sum(temp$residual)/length(temp$residual)
# Residuals seems to follow roughly a normal distribution
# and the mean of the errors is close to zero.


library(olsrr)

# Since we have a relative small number of variables we take all possible
# combinations
model.selection<-ols_step_all_possible(model_2)
model.selection

# I used model.selection to choose the variables I would examine. More
# particular I took the variables that appear as optimum at each column criteria
# of the variable model.selection

all_combinations_best<-ols_step_best_subset(model_2)
all_combinations_best
plot(all_combinations_best)

# from model.selection we can see the best combination with each criteria.
# basically we are between the following models
# price ~ bath + sqft + parking
# price ~ bath + parking
# price ~ bath + sqft
# price ~ bath
# The differences 
# We will create the appropriate models and we will use anova to compare and
# find the best model.

# We create models with the combinations of variables
model_p_bath_sqft_parking<-lm(price ~ bath + sqft + parking, data=housing2)
model_p_bath_parking<-lm(price ~ bath + parking, data=housing2)
model_p_bath_sqft<-lm(price ~ bath + sqft, data=housing2)
model_p_bath<-lm(price ~ bath, data=housing2)

# We make comparisons between the models we created.
# We care if every variable we add to our model improves it.
anova(model_p_bath,model_p_bath_sqft)
anova(model_p_bath,model_p_bath_parking)
anova(model_p_bath,model_p_bath_sqft_parking)
anova(model_p_bath_sqft,model_p_bath_sqft_parking)
anova(model_p_bath_parking,model_p_bath_sqft_parking)

# None of the above comparisons showed that adding an extra variable to the
# model price ~ bath improved our model.
# Adding another variable beside bath doesnt seem to improve significant our 
# model

summary(model_p_bath_sqft_parking)
summary(model_p_bath_sqft)
summary(model_p_bath_parking)
summary(model_p_bath)

# From the above models we see that price~bath+parking offers a good fitting.
# Variable parking is significant only when the parking is open. So I considered 
# if I included it in the final model if it improves it.
# We can see that when we compare anova model_p_bath with model_p_bath_parking
# we dont see any benefit from the extra variable.
# So our final model contains only bath.