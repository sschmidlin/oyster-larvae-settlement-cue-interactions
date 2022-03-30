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
# Quick fix for the moment: concentrate on data after 30 hours
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes
mult_s <- rep(1:nrow(data), data[, 'settled_30hrs'])
mult_u <- rep(1:nrow(data), data[, 'unattached_30hrs'])
data_s <- data[mult_s,]
data_s[, 'settled_30hrs'] <- 1
data_s <- data_s[, !(names(data_s) %in% c('settled_10hrs', 'unattached_10hrs', 'settled_20hrs', 'unattached_20hrs', 'unattached_30hrs'))]
data_u <- data[mult_u,]
data_u[, 'unattached_30hrs'] <- 0
data_u <- data_u[, !(names(data_u) %in% c('settled_10hrs', 'unattached_10hrs', 'settled_20hrs', 'unattached_20hrs', 'settled_30hrs'))]
colnames(data_u)[7] <- 'settled_30hrs'
data <- rbind(data_s, data_u)

# 2. Make a statistical model
model <- lme(PI_endo ~ sqrt(length) + T_av + NH4_av + NO3_av, random=~1|site, data=data, na.action=na.omit)

# 3. Does data fulfill model assumptions?
plot(model)

# 4. Statistical test
summary(model)

# 5. Visualize model predictions

# 6. Power analysis