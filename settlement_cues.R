# Set working directory where both the R script and the data are stored.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
require(lme4)

# Load the data
data <- read.csv2(file="Settlement_cue_data1-All_data.csv", check.names=FALSE, sep=",")

# Convert dates from chr to Date-format
data[, 'Start_date'] <- as.Date(data[, 'Start_date'])
data[, 'Fertilization_date'] <- as.Date(data[, 'Fertilization_date'])

# Convert Cue and Tray Number to factor
colnames(data)[4] <- 'Tray_Number'
data[, 'Tray_Number'] <- as.factor(data[, 'Tray_Number'])
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
colnames(data_u)[7] <- 'settled_30hrs'
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
table(data[, c('shell', 'conspecific_cue', 'predator_cue')]) # check if experiment was balanced

# 3. Make a statistical model
model <- glmer(settled_30hrs ~ shell * conspecific_cue * predator_cue + (1 | age) + (1 | Crab) + (1 | Tray_Number) + (1 | Well), data = data, family = binomial)

# 4. Does data fulfill model assumptions?
plot(model)

# 5. Statistical test
summary(model)

# 6. Visualize model predictions
# Below there is some code that I used for another project, maybe it's useful as a source of inspiration
# ggpredict is probably the function that you have to use
fit <- lm(avlength ~ O2_sat_av + Con_av^2 + netcen + updist, data=environment2)
predict <- ggpredict(fit, terms = "Con_av")

png(file="figure.png", res=600, width=3000, height=3000)
par(mfrow=c(3,3))
ggplot(predict, aes(x, predicted)) +
  theme_bw() +
  geom_line(color="red", size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_point(data = environment2, aes(x=environment2$Con_av, y=avlength)) +
  labs(x=expression("Conductivity ["*mu*"S/cm]"), y=expression("Average length [mm SL]")) +
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

# 7. Power analysis