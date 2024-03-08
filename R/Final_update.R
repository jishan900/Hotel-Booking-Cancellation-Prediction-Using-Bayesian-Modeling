# Install and load packages
library(tidyverse)
library(ggplot2)
library(rstan)
library(tidybayes)
library(modelr)
library(brms)
library(dplyr)
library(rstan)
library(bayesplot)
library(loo)


# Load dataset
booking_data <- read.csv("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/Applied Bayesian Data Analysis/R/booking.csv", header = TRUE)


colnames(booking_data)
head(booking_data)
head(booking_data)
booking_data
missing_values <- sum(is.na(booking_data))
nrow(booking_data)
ncol(booking_data)
summary(booking_data)
missing_values
str(booking_data)


# Explore your dataset
head(booking_data)
summary(booking_data)
str(booking_data)

# Set the seed for reproducibility (optional)
set.seed(123)

# Sample 10000 observations randomly
sampled_data <- booking_data %>%
sample_n(10000)

                                 # Check sampled data dimensions
print(dim(sampled_data))

# Specify Priors (optional)
#priors <- c(
#  prior(normal(0, 10), class = "Intercept"),
 # prior(normal(0, 5), class = "b"),
# # prior(normal(0, 5), class = "sd", coef = "sd")  
#)

priors <- c(
  prior(normal(3.5, 1), class = "Intercept"),  # Adjusted mean and smaller scale
  prior(normal(0, 0.5), class = "b"),         # Smaller scale
  prior(normal(0, 0.5), class = "sd", coef = "sd")  # Smaller scale
)


# Formulate the GLM formula
model <- brm(
  formula = booking.status ~
    number.of.adults + number.of.children + number.of.weekend.nights +
    number.of.week.nights + car.parking.space + lead.time + 
    P.C + P.not.C + average.price + special.requests + room.type, 
  data = sampled_data, 
  family = bernoulli("logit"),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  # prior = priors  # Use the specified priors here
)

# Display Model Summary
summary(model)




###########################################################################

# Create the plot objects
#all_histograms <- lapply(colnames(booking_data), function(variable) {
#  ggplot(booking_data, aes(x = get(variable))) +
#    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
#    theme_bw() +
#    ylab("Count\n") +
#    xlab(paste0("\n", variable)) +  # Label using variable name
#    theme(axis.text = element_text(size = 12),
#          axis.title = element_text(size = 14, face = "plain"))
#})

#Print the histograms to the screen
#lapply(all_histograms, print)

###########################################################################

# Formulate the GLM formula
#model <- brm(booking.status ~
#               number.of.adults + number.of.children + number.of.weekend.nights +
#               number.of.week.nights + type.of.meal + car.parking.space +
#               room.type + lead.time + market.segment.type + repeated +
#               P.C + P.not.C + average.price + special.requests, 
#             data = booking_data, family = bernoulli("logit"), control = list(max_treedepth = 15),

             # Specify Priors (optional)
#            prior <- c(
#                  prior(normal(0, 10), class = "Intercept"),
#                  prior(normal(0, 5), class = "b"),
#                  prior(normal(0, 5), class = "sd")
#            )
#)
##############################################################################
# Display Model Summary
summary(model)

# Posterior predictions
posterior_predict(model)

# MCMC Plot
mcmc_trace(model)

# Set PIP threshold for feature selection
pip_threshold <- 0.8

# Extract and filter features based on PIP
selected_features <- names(summary(brm_model, pars = ".inclusion")$inclusion)[summary(model, pars = ".inclusion")$inclusion$mean > pip_threshold]

# Print selected features
cat("Features with PIP above ", pip_threshold, ":", sep = "")
print(selected_features)

# Optional: Explore posterior distributions of selected features
summary(model, pars = selected_features)