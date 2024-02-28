# Load necessary libraries
library(brms)
library(dplyr)

# Load the Booking dataset (update the file path accordingly)
booking_data <- read.csv("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/Applied Bayesian Data Analysis/R/booking.csv", header = TRUE)

# Check column names and first few rows of the dataset
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

col_names <- c("Booking_ID", "number.of.adults", "number.of.children", "number.of.weekend.nights", "number.of.week.nights", "type.of.meal", "car.parking.space", "room.type", "lead.time", "market.segment.type", "repeated", "P.C", "P.not.C", "average.price", "special.requests", "date.of.reservation", "booking.status")
colnames(booking_data) <- col_names
summary(booking_data)
booking_data

# Preprocess the dataset (cleaning and encoding categorical variables)
booking_data_prep <- booking_data %>%
  mutate(booking.status = case_when(
    booking.status == "Canceled" ~ 1,
    booking.status == "Not_Canceled" ~ 2,
    TRUE ~ NA_real_
  ),
  booking.status = as.factor(booking.status),
  Booking_ID = as.factor(Booking_ID),
  number.of.adults = as.factor(number.of.adults),
  number.of.children = as.factor(number.of.children),
  number.of.weekend.nights = as.factor(number.of.weekend.nights),
  number.of.week.nights = as.factor(number.of.week.nights),
  type.of.meal = as.factor(type.of.meal),
  car.parking.space = as.factor(car.parking.space),
  room.type = as.factor(room.type),
  lead.time = as.factor(lead.time),
  market.segment.type = as.factor(market.segment.type),
  repeated = as.factor(repeated),
  P.C = as.factor(P.C),
  P.not.C = as.factor(P.not.C),
  average.price = as.factor(average.price),
  special.requests = as.factor(special.requests),
  date.of.reservation = as.factor(date.of.reservation),
  booking.status = as.factor(booking.status))
booking_data_prep$booking.status <- ordered(booking_data_prep$booking.status, levels = c("1", "2"))

# Check the structure of the preprocessed data
str(booking_data_prep)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(booking_data_prep), 0.04 * nrow(booking_data_prep))
train_data <- booking_data_prep[train_indices, ]
test_data <- booking_data_prep[-train_indices, ]

# Check the structure of the training data
str(train_data)

# Specify Priors with Normal Distribution
priors <- get_prior(booking.status ~ lead.time + average.price + number.of.week.nights + special.requests + number.of.weekend.nights + 
                      number.of.adults + number.of.children + type.of.meal + car.parking.space + room.type + market.segment.type + repeated,
                    data = train_data,
                    family = cumulative("logit"))

print(priors)
# Add specific coefficient priors with Normal distribution
priors <- c(
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("normal(0,1)", class = "b", coef = "lead.time1")
  #set_prior("normal(0,1)", class = "b", coef = "average.price"),
  #set_prior("normal(0,1)", class = "b", coef = "number.of.week.nights"),
  #set_prior("normal(0,1)", class = "b", coef = "special.requests"),
  #set_prior("normal(0,1)", class = "b", coef = "number.of.weekend.nights"),
  #set_prior("normal(0,1)", class = "b", coef = "number.of.adults"),
  #set_prior("normal(0,1)", class = "b", coef = "number.of.children"),
  #set_prior("normal(0,1)", class = "b", coef = "type.of.meal"),
  #set_prior("normal(0,1)", class = "b", coef = "car.parking.space"),
  #set_prior("normal(0,1)", class = "b", coef = "room.type"),
 # set_prior("normal(0,1)", class = "b", coef = "market.segment.type"),
 # set_prior("normal(0,1)", class = "b", coef = "repeated"),
  #set_prior("normal(0,1)", class = "b", coef = "P.C"),
 # set_prior("normal(0,1)", class = "b", coef = "P.not.C")
)

# Fit Bayesian logistic regression model using brms
model <- brm(booking.status ~ lead.time + average.price + number.of.week.nights + special.requests + number.of.weekend.nights + 
               number.of.adults + number.of.children + type.of.meal + car.parking.space + room.type + market.segment.type + repeated,
             data = train_data,
             prior = priors,
             #family = cumulative("logit"),
             family = bernoulli("logit"),
             cores = 4, # Set the number of cores for parallel processing
             control = list(adapt_delta = 0.95, max_treedepth = 10),
             chains = 4,
             iter = 2000, warmup = 1000) # Adjust adapt_delta for convergence


# Display model summary
summary(model)
plot(model)
summary(train_data)
unique(train_data$booking.status)




##############################################################

# Assuming train_data contains your numeric variables
numeric_variables <- train_data[, sapply(train_data, is.numeric)]

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_variables)

# Print the correlation matrix
print(correlation_matrix)

# Plot a heatmap of the correlation matrix+
install.packages("gplots")
library(gplots)  # You may need to install this package if not already installed

# Adjust the size of the plot
pdf("D:/Germany/Study Files-TUD/TU Dortmund/--------Semester-8-Winter Term--------2023-2024/Applied Bayesian Data Analysis/R/correlation_heatmap.pdf", width = 15, height = 10)

# Create a heatmap
heatmap(correlation_matrix, 
        #col = colorRampPalette(c("blue", "white", "red"))(100), 
        margins = c(5, 5), 
        cexRow = 1, 
        cexCol = 1,
        col = 
          main = "Correlation Heatmap")

dev.off()

