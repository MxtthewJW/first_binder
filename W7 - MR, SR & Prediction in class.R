# Multiple regression for coffee 

#Importing data
coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

#Load in libraries

library(tidyverse) # Load the tidyverse packages
library(Hmisc) # Needed for correlation
library(MASS) # Needed for maths functions
library(car) # Needed for VIF calculation
library(olsrr) # Needed for stepwise regression 
library(performance) # Needed to check model assumptions
library(ggcorrplot)

# Correlation plots

corr <- cor(dplyr::select(coffee_ratings, c(aroma, aftertaste, acidity, body, 
                            balance, sweetness, uniformity, clean_cup, 
                            moisture)))

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

# Models

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model1 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + sweetness + 
             uniformity + clean_cup + moisture, data = coffee_ratings)

anova(model0, model1)

# Checking assumptions

check_model(model1)

#Remove outlier

coffee_filtered <- coffee_ratings %>% 
  filter(expiration != "April 28th, 2018" | color != "Green")

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model1 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + sweetness + 
               uniformity + clean_cup + moisture, data = coffee_filtered)

anova(model0, model1)

summary(model1)

# Model summary

summary(model1) 
summary(model2) #not sweetness, moisture, and uniformity

# Remodel 

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model2 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + 
                clean_cup, data = coffee_filtered)

anova(model1, model2)

model3 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + 
               clean_cup + sweetness + moisture, data = coffee_ratings)

anova(model2, model3)

#--------------------------Stepwise regression------------------------

steplimitsf <- step(model0, scope = list (lower = model0, upper = model1), 
                    direction = "forward")

summary(steplimitsf)

steplimitsb <- step(model1, direction = "back")

summary(steplimitsb)

steplimitsboth <- step(model0, scope = list (upper = model1), 
                       direction = "both")

summary(steplimitsboth)

# Checking our Assumptions

check_model(steplimitsboth)

summary(steplimitsboth)

# Entering predictors based on their p-values

library(olsrr)

pmodel <- ols_step_forward_p(model1)

summary(pmodel)

AIC(model1)
AIC(model2)
AIC(model3)

#-----------------------------Prediction--------------------------------

# Use the predict() function to predict what the flavour rating would be for the 
# following values of the predictors aroma = 8, acidity = 7, body = 5, 
# balance = 9, uniformity = 6, clean_cup = 9

andrews_model <- lm(flavor ~ aroma + acidity + body + balance + 
                      clean_cup + uniformity, data = coffee_filtered)

predicted_flavour1 <- data.frame(aroma = 8, acidity = 7, body = 5, 
balance = 9, uniformity = 6, clean_cup = 9)

predicted_flavour2 <- tibble(aroma = 8, acidity = 7, body = 5, 
                                balance = 9, uniformity = 6, clean_cup = 9)

predicted_flavour2

predict(andrews_model, newdata = predicted_flavour1)


