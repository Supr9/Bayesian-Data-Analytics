

# Using the stars dataset


stars = read.csv("star_classification.csv", sep=',')


names(stars)


library(reshape2)
library(ggplot2)

col_to_remove <- which(names(stars) == "obj_ID")
stars <- stars[, -col_to_remove]



col_to_remove <- which(names(stars) == "run_ID")
stars <- stars[, -col_to_remove]


col_to_remove <- which(names(stars) == "rerun_ID")
stars <- stars[, -col_to_remove]


col_to_remove <- which(names(stars) == "cam_col")
stars <- stars[, -col_to_remove]


col_to_remove <- which(names(stars) == "spec_obj_ID")
stars <- stars[, -col_to_remove]


col_to_remove <- which(names(stars) == "plate")
stars <- stars[, -col_to_remove]

col_to_remove <- which(names(stars) == "fiber_ID")
stars <- stars[, -col_to_remove]

col_to_remove <- which(names(stars) == "MJD")
stars <- stars[, -col_to_remove]

col_to_remove <- which(names(stars) == "field_ID")
stars <- stars[, -col_to_remove]


stars = stars[stars$class != 'GALAXY',]


# 0 for QSO, 1 for Star
levels(as.factor(stars$class))

stars$class = as.numeric(as.factor(stars$class))-1

stars = stars[stars$u != -9999.00,]
stars = stars[stars$g != -9999.00,]
stars = stars[stars$z != -9999.00,]


library(GGally)

ggcorr(stars, label = T)



# Can be used
boxplot(stars$u~stars$class)

# Can be used
boxplot(stars$g~stars$class)


# Can be used
boxplot(stars$r~stars$class)

# Can be used
boxplot(stars$i~stars$class)

# Can be used
boxplot(stars$z~stars$class)

# redshift may overpower the model and get high beta
boxplot(stars$redshift~stars$class)


# alpha may not be a good idea to use
boxplot(stars$alpha~stars$class)

# delta may not be indicative of the class
boxplot(stars$delta~stars$class)



quas_copy = stars[stars$class != '1',]
star_copy = stars[stars$class != '0',]


# getting only a small subset of the data so as to not make the 
# mcmc algorithm run for long

quas = quas_copy[1:500,]

star_class=star_copy[1:500,]

stars = rbind(quas, star_class)

summary(stars)

library(MCMCpack)

# high multicollinearity with redshift
predictors <- c("u","g", "r", "i", "z", "alpha", "delta")

predictors = paste(predictors, collapse = " + ")
formula <- as.formula(paste("class ~ ", predictors))

mcmc_output <- MCMClogit(formula, data = stars, thin=10, burnin=1000, mcmc=20000)

summary(mcmc_output)



# lets remove some variables in order to check if some are more significant
predictors <- c("u", "i")

predictors = paste(predictors, collapse = " + ")
formula <- as.formula(paste("class ~ ", predictors))

mcmc_output <- MCMClogit(formula, data = stars, thin=10, burnin=1000, mcmc=20000)

summary(mcmc_output)

# With the above we see a more narrow intercept quantile and also that u contributes positively to
# the prediction of 1, meanwhile i contributes negatively. This means that i is more indicative
# of being class 0, meanwhile u indicates more the class 1


# When doing this plot we see also a larger deviation in the classes according to the x
plot(stars$z+stars$i+stars$r,stars$class)

# Lets try using this as our predictors
predictors <- c("z", "i","r")

predictors = paste(predictors, collapse = " + ")
formula <- as.formula(paste("class ~ ", predictors))

mcmc_output <- MCMClogit(formula, data = stars, thin=10, burnin=1000, mcmc=20000)

summary(mcmc_output)


# z does not seem to correlate well with this combination (not significant)


predictors <- c("i","r")

predictors = paste(predictors, collapse = " + ")
formula <- as.formula(paste("class ~ ", predictors))

mcmc_output <- MCMClogit(formula, data = stars, thin=10, burnin=1000, mcmc=20000)

summary(mcmc_output)

plot(mcmc_output)


# Predicting the probability of the new observation

new_observation = c(1, 10, 10)
mcmc_samples <- as.mcmc(mcmc_output)  # Convert to 'mcmc' object for easy manipulation
coef_samples <- as.matrix(mcmc_samples)  # Get the samples in matrix form

# Compute log-odds for each sample
log_odds <- coef_samples %*% as.matrix(new_observation)

probabilities <- 1 / (1 + exp(-log_odds))

# Output probabilities to check results
print(probabilities[1])
print(density(probabilities))

plot(density(probabilities))







