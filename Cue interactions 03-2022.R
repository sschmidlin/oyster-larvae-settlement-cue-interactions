setwd("~/GitHub/")
data <- read.csv2(file="Experiment 1 data1.csv", sep=",")

install.packages("dplyr")
library(dplyr)
install.packages("Matrix")
install.packages("lme4")
install.packages("ggeffects")
library(Matrix)
library(lme4)
library(ggeffects)

#deleting 10hr, 20hr data 
data <- select(data, -settled_10hrs, -unattached_10hrs, -settled_20hrs, -unattached_20hrs)
colnames(data)[5] <- 'Settled'
colnames(data)[6] <- 'Unattached'

# 1. Calculate response variable from input data
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes

mult_s <- rep(1:nrow(data), data[, 'Settled'])
mult_u <- rep(1:nrow(data), data[, 'Unattached'])
data_s <- data[mult_s,]
data_s[, 'Settled'] <- 1
data_s <- data_s[, !(names(data_s) %in% c('Unattached'))]
data_u <- data[mult_u,]
data_u[, 'Unattached'] <- 0
data_u <- data_u[, !(names(data_u) %in% c('Settled'))]
colnames(data_u)[5] <- 'Settled'
data <- rbind(data_s, data_u)

#binary predictor values

data['Shell'] <- data['Cue']
data['conspecific_cue'] <- data['Cue']
data['predator_cue'] <- data['Cue']

data$Shell <- ifelse(grepl("Conspecific shell", data$Shell), TRUE, FALSE)
data$conspecific_cue <- ifelse(grepl("conspecific cue", data$conspecific_cue), TRUE, FALSE)
data$predator_cue <- ifelse(grepl("predator cue", data$predator_cue), TRUE, FALSE)

data$Shell <-sub("TRUE", "untreated", data$Shell)
data$Shell <-sub("FALSE", "sterilized", data$Shell)
data$predator_cue <-sub("TRUE", "present", data$predator_cue)
data$predator_cue <-sub("FALSE", "absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "absent", data$conspecific_cue)

#taking out all data with con shell 
new_data <- subset(data, Shell == "sterilized")


#making model
model <- glmer(Settled ~ predator_cue * conspecific_cue + (1 | Larvae.batch) + (1 | Larvae.age) + (1 | Crab), data = data, family = binomial)
model2 <-glmer(Settled ~ conspecific_cue * predator_cue +(1 | Shell) + (1 | Larvae.batch) + (1 | Larvae.age) + (1 | Crab), data = data, family = binomial)
model3 <-glmer(Settled ~ predator_cue * conspecific_cue + (1 | Larvae.batch) + (1 | Larvae.age) + (1 | Crab), data = new_data, family = binomial)

summary(model3)

#visualizing model
m <- ggpredict(model3, terms = c("predator_cue", "conspecific_cue"))
plot(m)
m2 <- ggpredict(model2, terms = c("Shell", "conspecific_cue"))
plot(m2)

m <- ggpredict(model, terms = c("conspecific_cue", "predator_cue"))
plot(m, connect.lines = TRUE) +
  labs(x = 'Conspecific Cue', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("dodgerblue3", "orangered4")) +
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))

