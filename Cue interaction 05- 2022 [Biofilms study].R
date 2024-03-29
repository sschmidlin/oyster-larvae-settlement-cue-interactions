setwd("~/GitHub/")
data <- read.csv2(file="biofilm excel 04-22.csv", sep=",")

install.packages("dplyr")
library(dplyr)
install.packages("Matrix")
install.packages("lme4")
install.packages("ggeffects")
library(Matrix)
library(lme4)
library(ggeffects)

#deleting 10hr, 20hr data 
data <- select(data, -settled_10h, -unsettled_10h, -settled_20h, -unsettled_20h)
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

data['Biofilm'] <- data['Cue']
data['conspecific_cue'] <- data['Cue']
data['predator_cue'] <- data['Cue']

data$Biofilm <- ifelse(grepl("Biofilm", data$Biofilm), TRUE, FALSE)
data$conspecific_cue <- ifelse(grepl("Conspecific", data$conspecific_cue), TRUE, FALSE)
data$predator_cue <- ifelse(grepl("Predator", data$predator_cue), TRUE, FALSE)

data$Biofilm <-sub("TRUE", "present", data$Biofilm)
data$Biofilm <-sub("FALSE", "absent", data$Biofilm)
data$predator_cue <-sub("TRUE", "present", data$predator_cue)
data$predator_cue <-sub("FALSE", "absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "absent", data$conspecific_cue)


#making model
model <- glmer(Settled ~ conspecific_cue + predator_cue + Biofilm + conspecific_cue:predator_cue + conspecific_cue:Biofilm + predator_cue:Biofilm + (1 | Larvae.age), data = data, family = binomial)
model2 <- glmer(Settled ~ predator_cue + conspecific_cue +(1 | larvae.batch) + (1 | Larvae.age), data = data, family = binomial)
model3 <-glmer(Settled ~ Biofilm + conspecific_cue + (1 | larvae.batch) + (1 | Larvae.age), data = data, family = binomial)
model4 <-glmer(Settled ~ conspecific_cue + predator_cue + Biofilm + conspecific_cue:predator_cue + conspecific_cue:Biofilm + predator_cue:Biofilm + (1 | larvae.batch) + (1 | Larvae.age), data = data, family = binomial)

summary(model)

#visualizing model
m <- ggpredict(model, terms = c("predator_cue", "conspecific_cue"))
plot(m, connect.lines = TRUE)
m2 <- ggpredict(model2, terms = c("Biofilm", "conspecific_cue"))
plot(m2, connect.lines = TRUE)
m3 <- ggpredict(model3, terms = c("conspecific_cue", "predator_cue"))
plot(m3, connect.lines = TRUE)
m4 <- ggpredict(model3, terms = c("conspecific_cue", "Biofilm"))
plot(m4, connect.lines = TRUE)
m5 <- ggpredict(model3, terms = c("predator_cue", "Biofilm"))
plot(m5, connect.lines = TRUE)

