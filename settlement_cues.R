# Set working directory where both the R script and the data are stored.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
require(lme4)
require(ggeffects)
require(DHARMa)

# Load the data
data <- read.csv2(file="Settlement_cue_data1-All_data.csv", check.names=FALSE, sep=",")

# Convert dates from chr to Date-format
data[, 'Start_date'] <- as.Date(data[, 'Start_date'])
data[, 'Fertilization_date'] <- as.Date(data[, 'Fertilization_date'])

# Convert Cue and Tray Number to factor
colnames(data)[6] <- 'Tray_Number'
data[, 'Tray_Number'] <- as.factor(data[, 'Tray_Number'])
data[, 'Cue'] <- as.factor(data[, 'Cue'])
levels(data[,'Cue'])


# Does settlement differ between treatments?
# Type of model: generalized linear mixed-effect model; response variable is binary -> logistic regression
# Binary response variable: calculate per larva and save as "settlement"
# Predictor variables: shell, conspecific_cue, predator_cue
# Random effect variables: Crab, age, Tray_Number, Well

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
colnames(data_u)[9] <- 'settled_30hrs'
data <- rbind(data_s, data_u)
table(data[,'settled_30hrs']) # just to see how many larvae settled

# 2. Make new binary predictor variables from multilevel factor
# shell: sterilized vs. untreated
# conspecific cue: present vs. absent
# predator cue: present vs. absent
data['shell'] <- data['Cue']
levels(data$shell) <- c(rep('untreated', 3), rep('sterilized', 4))
data['conspecific_cue'] <- data['Cue']
levels(data$conspecific_cue) <- c('absent', 'present', rep('absent', 2), rep('present', 2), 'absent')
data['predator_cue'] <- data['Cue']
levels(data$predator_cue) <- c(rep('absent', 2), 'present', rep('absent', 2), rep('present', 2))

# 3. Make a statistical model
model <- glmer(settled_30hrs ~ shell * conspecific_cue * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)
# Experiment was unbalanced, lacking data for untreated shells with predator cues present -> repeat analysis with only sterilized shells
data0 <- data[data[, 'shell'] %in% 'sterilized', ]
table(data0[, c('conspecific_cue', 'predator_cue')])
model2 <- glmer(settled_30hrs ~ conspecific_cue * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)
# Testing model assumptions
testDispersion(model2)
simulationOutput <- simulateResiduals(fittedModel = model2, plot = F)
plot(simulationOutput)
# Test model
summary(model2)

# Checking for a batch effect
table(data2[, c('conspecific_cue', 'predator_cue')])
model2 <- glmer(settled_30hrs ~ conspecific_cue * predator_cue + (1 | age) + (1 | Crab) + (1 | Batch) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)
# Testing model assumptions
testDispersion(model2)
simulationOutput <- simulateResiduals(fittedModel = model2, plot = F)
plot(simulationOutput)
# Test model
summary(model2)
# Visualize model predictions
m2 <- ggpredict(model2, terms = c("conspecific_cue", "predator_cue"))
plot(m2)


# Repeat analysis once more without predator cues
data3 <- data[data[, 'predator_cue'] %in% 'absent', ]
table(data3[, c('shell', 'conspecific_cue')])
model3 <- glmer(settled_30hrs ~ shell * conspecific_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)
# Testing model assumptions
testDispersion(model3)
simulationOutput <- simulateResiduals(fittedModel = model3, plot = F)
plot(simulationOutput)
# Test model
summary(model3)
# Model with all data, but dropping unbalanced factor combinations
model4 <- glmer(settled_30hrs ~ shell + conspecific_cue + predator_cue + shell:conspecific_cue + conspecific_cue:predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)

# 4. Does data fulfill model assumptions?
plot(model)
plot(model2)
plot(model3)
plot(model4)

# 5. Statistical test
summary(model)
summary(model2)
summary(model3)
summary(model4)

# 6. Visualize model predictions
# About ggeffects: https://strengejacke.github.io/ggeffects/articles/ggeffects.html
# Try this out: https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html
m2 <- ggpredict(model2, terms = c("conspecific_cue", "predator_cue"))
plot(m2)
m3 <- ggpredict(model3, terms = c("shell", "conspecific_cue"))
plot(m3)
m4 <- ggpredict(model4, terms = c("shell", "conspecific_cue", "predator_cue"))
plot(m4)

#another model for shell * predator cue m5
data5 <- data[data[, 'conspecific_cue'] %in% 'present', ]
model5 <- glmer(settled_30hrs ~ shell * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data5, family = binomial)
m5 <-ggpredict(model5, terms = c("shell", "predator_cue"))
plot(m5)
summary(model5)


# visualizing m2

plot(m2) +
  labs(x = 'Conspecific Cue (waterborne)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 30hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "4DBBD5B2")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits


# visualizing m3
plot(m3) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 30hrs") +
  guides(color = guide_legend(title = "Conspecific Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "aquamarine3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits

# visualizing m5
plot(m5) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 30hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "darkorchid3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))   #to change the y axis limits


#graphs for 20 hours!!!!
data2 <- read.csv2(file="Settlement_cue_data1-All_data.csv", check.names=FALSE, sep=",")


# Convert Cue and Tray Number to factor
colnames(data2)[6] <- 'Tray_Number'
data2[, 'Tray_Number'] <- as.factor(data2[, 'Tray_Number'])
data2[, 'Cue'] <- as.factor(data2[, 'Cue'])
levels(data2[,'Cue'])

#Calculate response variable from input data
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes
mult_s20 <- rep(1:nrow(data2), data2[, 'settled_20hrs'])
mult_u20 <- rep(1:nrow(data2), data2[, 'unattached_20hrs'])
data_s20 <- data2[mult_s20,]
data_s20[, 'settled_20hrs'] <- 1
data_s20 <- data_s20[, !(names(data_s20) %in% c('settled_10hrs', 'unattached_10hrs', 'settled_30hrs', 'unattached_20hrs', 'unattached_30hrs'))]
data_u20 <- data2[mult_u20,]
data_u20[, 'unattached_20hrs'] <- 0
data_u20 <- data_u20[, !(names(data_u20) %in% c('settled_10hrs', 'unattached_10hrs', 'settled_20hrs', 'unattached_30hrs', 'settled_30hrs'))]
colnames(data_u20)[9] <- 'settled_20hrs'
data2 <- rbind(data_s20, data_u20)
table(data2[,'settled_20hrs']) # just to see how many larvae settled

# 2. Make new binary predictor variables from multilevel factor
# shell: sterilized vs. untreated
# conspecific cue: present vs. absent
# predator cue: present vs. absent
data2['shell'] <- data2['Cue']
levels(data2$shell) <- c(rep('untreated', 3), rep('sterilized', 4))
data2['conspecific_cue'] <- data2['Cue']
levels(data2$conspecific_cue) <- c('absent', 'present', rep('absent', 2), rep('present', 2), 'absent')
data2['predator_cue'] <- data2['Cue']
levels(data2$predator_cue) <- c(rep('absent', 2), 'present', rep('absent', 2), rep('present', 2))

# 3. Make a statistical model
model21 <-glmer(settled_20hrs ~ conspecific_cue * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data2, family = binomial)


#m31
data31 <- data2[data2[, 'predator_cue'] %in% 'absent', ]
table(data31[, c('shell', 'conspecific_cue')])
model31 <- glmer(settled_20hrs ~ shell * conspecific_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data31, family = binomial)

model41 <- glmer(settled_20hrs ~ shell * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data2, family = binomial)



# 4. Does data fulfill model assumptions?
plot(model21)
summary(model21)
plot(model31)
summary(model31)
plot(model41)
summary(model41)

# 6. Visualize model predictions
m21 <- ggpredict(model21, terms = c("conspecific_cue", "predator_cue"))
plot(m21)
m31 <- ggpredict(model31, terms = c("shell", "conspecific_cue"))
plot(m31)
m41 <- ggpredict(model41, terms = c("shell", "predator_cue"))
plot(m41)

#visualizing m21
plot(m21) +
  labs(x = 'Conspecific Cue (waterborne)', 
       y= 'Larvae Settled (%)',
       title = " Predictied settlement 20hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "4DBBD5B2")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits


# visualizing m31
plot(m31) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 20hrs") +
  guides(color = guide_legend(title = "Conspecific Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "aquamarine3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits

# visualizing m41
plot(m41) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 20hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "darkorchid3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))   #to change the y axis limits


# again for 10hrs 

data4 <- read.csv2(file="Settlement_cue_data1-All_data.csv", check.names=FALSE, sep=",")


# Convert Cue and Tray Number to factor
colnames(data4)[6] <- 'Tray_Number'
data4[, 'Tray_Number'] <- as.factor(data2[, 'Tray_Number'])
data4[, 'Cue'] <- as.factor(data4[, 'Cue'])
levels(data4[,'Cue'])

#Calculate response variable from input data
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes
mult_s10 <- rep(1:nrow(data4), data4[, 'settled_10hrs'])
mult_u10 <- rep(1:nrow(data4), data4[, 'unattached_10hrs'])
data_s10 <- data4[mult_s10,]
data_s10[, 'settled_10hrs'] <- 1
data_s10 <- data_s10[, !(names(data_s10) %in% c('unattached_10hrs', 'settled_20hrs', 'unattached_20hrs', 'settled_30hrs', 'unattached_30hrs'))]
data_u10 <- data4[mult_u10,]
data_u10[, 'unattached_10hrs'] <- 0
data_u10 <- data_u10[, !(names(data_u10) %in% c('settled_10hrs', 'unattached_20hrs', 'settled_20hrs', 'unattached_30hrs', 'settled_30hrs'))]
colnames(data_u10)[9] <- 'settled_10hrs'
data4 <- rbind(data_s10, data_u10)
table(data4[,'settled_10hrs']) # just to see how many larvae settled

# 2. Make new binary predictor variables from multilevel factor
# shell: sterilized vs. untreated
# conspecific cue: present vs. absent
# predator cue: present vs. absent
data4['shell'] <- data4['Cue']
levels(data4$shell) <- c(rep('untreated', 3), rep('sterilized', 4))
data4['conspecific_cue'] <- data4['Cue']
levels(data4$conspecific_cue) <- c('absent', 'present', rep('absent', 2), rep('present', 2), 'absent')
data4['predator_cue'] <- data4['Cue']
levels(data4$predator_cue) <- c(rep('absent', 2), 'present', rep('absent', 2), rep('present', 2))

# 3. Make a statistical model
model11 <-glmer(settled_10hrs ~ conspecific_cue * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data4, family = binomial)

#m12 without predator cues 
data12 <- data4[data4[, 'predator_cue'] %in% 'absent', ]
table(data12[, c('shell', 'conspecific_cue')])
model12 <- glmer(settled_10hrs ~ shell * conspecific_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data12, family = binomial)

model13 <- glmer(settled_10hrs ~ shell * predator_cue + (1 | age) + (1 | Batch) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data4, family = binomial)



# 4. Does data fulfill model assumptions?
plot(model11)
summary(model11)
plot(model12)
summary(model12)
summary(model13)

# 6. Visualize model predictions
m11 <- ggpredict(model11, terms = c("conspecific_cue", "predator_cue"))
plot(m11)
m12 <- ggpredict(model12, terms = c("shell", "conspecific_cue"))
plot(m12)
m13 <- ggpredict(model13, terms = c("shell", "predator_cue"))
plot(m13)




#visualizing m11
plot(m11) +
  labs(x = 'Conspecific Cue (waterborne)', 
       y= 'Larvae Settled (%)',
       title = " Predictied settlement 10hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "4DBBD5B2")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits


# visualizing m12
plot(m12) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 10hrs") +
  guides(color = guide_legend(title = "Conspecific Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "aquamarine3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits

# visualizing m13
plot(m13) +
  labs(x = 'Conspecific Cue (Shell)', 
       y= 'Larvae Settled (%)',
       title = "Predicted Settlement 10hrs") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Black", "darkorchid3")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))   #to change the y axis limits



#Power analysis
install.packages("effectsize")
install.packages("pwr")
require("effectsize")
require("pwr")

#using data from 20hr settled and from sterilized shell treatments
STRShell_20hr <- data[data$Cue == "Sterilized shell", "settled_20hrs"]
STRShellCC_20hr <- data[data$Cue == "Sterilized shell + conspecific cue", "settled_20hrs"]
STRShellCCPC_20hr <- data[data$Cue == "Sterilized shell + conspecific cue + predator cue", "settled_20hrs"]
STRShellPC_20hr <- data[data$Cue == "Sterilized shell + predator cue", "settled_20hrs"]

#find effect sizes for each combination of treatments 
cohens1 <- cohens_d(STRShell_20hr,STRShellCC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)
cohens2 <- cohens_d(STRShell_20hr,STRShellCCPC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)
cohens3 <- cohens_d(STRShell_20hr,STRShellPC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)
cohens4 <- cohens_d(STRShellCC_20hr, STRShellCCPC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)
cohens5 <- cohens_d(STRShellCC_20hr,STRShellPC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)
cohens6 <- cohens_d(STRShellPC_20hr,STRShellCCPC_20hr, pooled_sd = TRUE, mu=0, paired = FALSE, ci = 0.95, alterative = "two.sided", verbose = TRUE)

#power analysis
power1 <- pwr.r.test(r=cohens1$Cohens_d, sig.level=.05, power=.8)
power2 <- pwr.r.test(r=cohens2$Cohens_d, sig.level=.05, power=.8)
power3 <- pwr.r.test(r=cohens3$Cohens_d, sig.level=.05, power=.8)
power4 <- pwr.r.test(r=cohens4$Cohens_d, sig.level=.05, power=.8)
power5 <- pwr.r.test(r=cohens5$Cohens_d, sig.level=.05, power=.8)
power6 <- pwr.r.test(r=cohens6$Cohens_d, sig.level=.05, power=.8)

show(power1)
show(power2)
show(power3)
show(power4)
show(power5)
show(power6)

#load data from second trial 
data.2 <- read.csv2(file="Settelment cues experiment 1 NewData.csv", check.names=FALSE, sep=",")
data.2[, 'Tray_well'] <- as.factor(data.2[, 'Tray_well'])
data.2[, 'Cue']  <- as.factor(data.2[, 'Cue'])



# 1a. Calculate response variable from input data
# data after 30 hours
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes
mult_s30.2 <- rep(1:nrow(data.2), data.2[, 'settled_30hr'])
mult_u30.2 <- rep(1:nrow(data.2), data.2[, 'unattached_30hr'])
data_s30.2 <- data.2[mult_s30.2,]
data_s30.2[, 'settled_30hr'] <- 1
data_s30.2 <- data_s30.2[, !(names(data_s30.2) %in% c('settled_10hr', 'unattached_10hr', 'settled_20hr', 'unattached_20hr', 'unattached_30hr'))]
data_u30.2 <- data.2[mult_u30.2,]
data_u30.2[, 'unattached_30hr'] <- 0
data_u30.2 <- data_u30.2[, !(names(data_u30.2) %in% c('settled_10hr', 'unattached_10hr', 'settled_20hr', 'unattached_20hr', 'settled_30hr'))]
colnames(data_u30.2)[6] <- 'settled_30hr'
data30.2 <- rbind(data_s30.2, data_u30.2)
table(data30.2[,'settled_30hr']) # just to see how many larvae settled

# 2a. Make new binary predictor variables from multilevel factor
# shell: sterilized vs. untreated
# conspecific cue: present vs. absent
# predator cue: present vs. absent
data30.2['shell'] <- data30.2['Cue']
data30.2['predator_cue'] <- data30.2['Cue']
data30.2['conspecific_cue'] <- data30.2['Cue']
data30.2$predator_cue <- as.factor(data30.2$predator_cue)
data30.2$conspecific_cue <- as.factor(data30.2$conspecific_cue)
levels(data30.2$shell) <- c(rep('untreated', 3), rep('sterilized', 4))
levels(data30.2$conspecific_cue) <- c('absent', 'present', rep('absent', 2), rep('present', 2), 'absent')
levels(data30.2$predator_cue) <- c(rep('absent', 2), 'present', rep('absent', 2), rep('present', 2))
table(data30.2[, c('shell', 'conspecific_cue', 'predator_cue')]) # check if experiment was balanced ### unsure about this table


# 3a. Make a statistical model
model30.2 <- glmer(settled_30hr ~ shell * conspecific_cue * predator_cue + (1 | Age) + (1 | Tray_well) + (1 | Larvae_batch), data = data30.2, family = binomial)
table(data30.2[, c('conspecific_cue', 'predator_cue', 'shell')])

# repeating analysis with only sterilized shells
data30.22 <- data30.2[data30.2[, 'shell'] %in% 'sterilized', ]
table(data30.22[, c('conspecific_cue', 'predator_cue')])
model2.30.2 <- glmer(settled_30hr ~ conspecific_cue * predator_cue + (1 | Age) + (1 | Tray_well) + (1 | Larvae_batch), data = data30.2, family = binomial)
# Testing model assumptions
testDispersion(model2.30.2)
simulationOutput <- simulateResiduals(fittedModel = model2.30.2, plot = F)
plot(simulationOutput)
# Test model
summary(model2.30.2)

data30.22 <- data30.2[data30.2[, 'shell'] %in% 'sterilized', ]
table(data30.22[, c('conspecific_cue', 'predator_cue', 'Larvae_batch')])
model2.30.2 <- glmer(settled_30hr ~ conspecific_cue * predator_cue * Larvae_batch + (1 | Age) + (1 | Tray_well), data = data30.2, family = binomial)
# Testing model assumptions
testDispersion(model2.30.2)
simulationOutput <- simulateResiduals(fittedModel = model2.30.2, plot = F)
plot(simulationOutput)
# Test model
summary(model2.30.2)
# Visualize model predictions
m2.30.2 <- ggpredict(model2.30.2, terms = c("conspecific_cue", "predator_cue", "Larvae_batch"))
plot(m2.30.2)

# Repeat analysis once more without predator cues
data30.23 <- data30.2[data30.2[, 'predator_cue'] %in% 'absent', ]
table(data30.23[, c('shell', 'conspecific_cue')])
model3.30.2 <- glmer(settled_30hr ~ shell * conspecific_cue + (1 | Age) + (1 | Tray_well) + (1 | Larvae_batch), data = data30.2, family = binomial)
# Testing model assumptions
testDispersion(model3.30.2)
simulationOutput <- simulateResiduals(fittedModel = model3.30.2, plot = F)
plot(simulationOutput)
# Test model
summary(model3.30.2)

# Model with all data, but dropping unbalanced factor combinations
model4.30.2 <- glmer(settled_30hr ~ shell + conspecific_cue + predator_cue + shell:conspecific_cue + conspecific_cue:predator_cue + (1 | Age) + (1 | Tray_well) + (1 | Larvae_batch), data = data30.2, family = binomial)
table(data30.2[, c('shell', 'predator_cue', 'conspecific_cue')])
# Testing model assumptions
testDispersion(model4.30.2)
simulationOutput <- simulateResiduals(fittedModel = model4.30.2, plot = F)
plot(simulationOutput)
# Test model
summary(model4.30.2)

# 4a. Does data fulfill model assumptions?
plot(model30.2)
plot(model2.30.2)
plot(model3.30.2)
plot(model4.30.2)

# 5a. Statistical test
summary(model30.2)
summary(model2.30.2)
summary(model3.30.2)
summary(model4.30.2)

# 6a. Visualize model predictions
# About ggeffects: https://strengejacke.github.io/ggeffects/articles/ggeffects.html
# Try this out: https://strengejacke.github.io/ggeffects/articles/practical_logisticmixedmodel.html
m2.30.2 <- ggpredict(model2.30.2, terms = c("conspecific_cue", "predator_cue"))
plot(m2.30.2)
m3.30.2 <- ggpredict(model3.30.2, terms = c("shell", "conspecific_cue"))
plot(m3.30.2)
m4.30.2 <- ggpredict(model4.30.2, terms = c("shell", "conspecific_cue", "predator_cue"))
plot(m4.30.2)

