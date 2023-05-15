setwd("~/GitHub/")
data <- read.csv2(file="Experiment 1 NewData.csv", sep=",")

install.packages("dplyr")
library(dplyr)
install.packages("Matrix")
install.packages("lme4")
install.packages("ggeffects")
library(Matrix)
library(lme4)
library(ggeffects)

#deleting 10hr, 20hr data
data <- select(data, -settled_20hr, -unattached_20hr, -settled_20hr, -unattached_20hr)
data <- select(data, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
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

data$Shell <- ifelse(grepl("conspecific shell", data$Shell), TRUE, FALSE)
data$conspecific_cue <- ifelse(grepl("conspecific cue", data$conspecific_cue), TRUE, FALSE)
data$predator_cue <- ifelse(grepl("predator cue", data$predator_cue), TRUE, FALSE)

data$Shell <-sub("TRUE", "untreated", data$Shell)
data$Shell <-sub("FALSE", "sterilized", data$Shell)
data$predator_cue <-sub("TRUE", "present", data$predator_cue)
data$predator_cue <-sub("FALSE", "absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "absent", data$conspecific_cue)


#making model

model <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + conspecific_cue:predator_cue + conspecific_cue:Shell + predator_cue:Shell + (1 | Larvae.age) + (1 | Larvae.batch), data = data, family = binomial)
model <- glmer(Settled ~ conspecific_cue * predator_cue + (1 | Larvae.batch) + (1 | Larvae.age), data = data, family = binomial)
model2 <-glmer(Settled ~ Shell + conspecific_cue + (1 | Larvae.batch) + (1 | Larvae.age), data = data, family = binomial)
model3 <-glmer(Settled ~ Shell + predator_cue + (1 | Larvae.batch) + (1 | Larvae.age), data = data, family = binomial)

summary(model)
summary(model2)
summary(model3)

#visualizing model
m <- ggpredict(model, terms = c("conspecific_cue", "predator_cue"))
plot(m)
m2 <- ggpredict(model, terms = c("Shell", "conspecific_cue"))
plot(m2)
m3 <- ggpredict(model, terms = c("Shell", "predator_cue"))
plot(m3)

m4 <- ggpredict(model, terms = c("Shell","predator_cue","conspecific_cue"))
plot(m4)
