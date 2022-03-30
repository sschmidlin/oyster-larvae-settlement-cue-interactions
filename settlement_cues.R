# Set working directory where both the R script and the data are stored.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data
data <- read.csv2(file="Settlement_cue_data1-All_data.csv", check.names=FALSE, sep=",")

# Convert dates from chr to Date-format
data[, 'Start_date'] <- as.Date(data[, 'Start_date'])
data[, 'Fertilization_date'] <- as.Date(data[, 'Fertilization_date'])

# Convert Cue to factor
data[, 'Cue'] <- as.factor(data[, 'Cue'])
levels(data[,'Cue'])

# Calculate ages of larvae
data['age'] <- NA
for(i in 1:nrow(data)){
  data[i, 'age'] <- difftime(data[i, 'Start_date'], data[i, 'Fertilization_date'], units = "days")
}

# Does settlement differ between treatments?
# Type of model: generalized linear mixed-effect model; response variable is binary -> logistic regression
# Binary response variable: calculate per larva and save as "settlement"
# Predictor variable: Cue
# Random effect variables: Crab, age, Tray Number, Well

# 1. Calculate response variable from input data

# 2. Make a statistical model

# 3. Does data fulfill model assumptions?

# 4. Statistical test

# 5. Visualize model predictions

# 6. Power analysis