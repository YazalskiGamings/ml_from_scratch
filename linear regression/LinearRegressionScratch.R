# Linear Regression from Scratch
library(readr)
library(dplyr)
library(scatterplot3d)

fitness = read_csv(".../data.csv") #import fitness csv

cor(fitness) #Maxpulse & Pulse are correlated. That makes sense. Avoid Multicolinearity, so use Maxpulse only
fitness = fitness %>% select(-c(Pulse))

fitness$Calories = ifelse(is.na(fitness$Calories) == T, median(fitness$Calories, na.rm = T), fitness$Calories) #impute median for the na because skewed
hist(fitness$Calories) # Skewed, let's try to do a log transformation, so we limit outliers
fitness$Calories = log(fitness$Calories) # OLS is invariant -> so good to log transform

Y = as.matrix(fitness$Duration) # Y = 169 x 1
X = as.matrix(fitness[,-1]) # X = 169 x 2 
X = cbind(intercept = rep(1, nrow(fitness)),X) #add intercept column 169 x 3
N = nrow(fitness)
print(Y[1:10])
print(X[1:10])

# Gauss Markov Theorem (X^T X)^-1 * X^T * Y
B = solve(t(X) %*% X) %*% t(X) %*% Y
Y_pred = X %*% B # Calculate Y predicted
print(Y_pred[1:10])
MSE = sum((Y - Y_pred)^2) * (1/N) # Calculate MSE
print(MSE)

# Compare lm() built in function
model = lm(fitness$Duration ~ fitness$Maxpulse + fitness$Calories)

print(model)
prnt(B)

# Plot
plot3d = scatterplot3d(x = fitness$Calories, y = fitness$Duration,z = fitness$Maxpulse, angle = 45, main = "Duration vs Calories & Maxpulse",xlab = "Calories", ylab = "Duration", zlab = "Maxpulse")
