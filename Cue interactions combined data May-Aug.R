library(dplyr)
library(Matrix)
library(lme4)
library(ggeffects)
library(ggplot2)

#loading data from May
setwd("~/GitHub/Conspecific-Predator-Settlement-Cue")

data <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data <- select(data, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
colnames(data)[5] <- 'Settled'
colnames(data)[6] <- 'Unattached'
data$Larvae.batch = as.factor(data$Larvae.batch)

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

#adding data from august
data_aug <- read.csv2(file="Cue interactions 08-2022.csv", sep=",")
colnames(data_aug)[4] <- 'Well_Number'
colnames(data_aug)[2] <- 'Age'
colnames(data_aug)[3] <- 'Batch'
colnames(data_aug)[5] <- 'Cue'
colnames(data_aug)[6] <- 'Settled'
colnames(data_aug)[7] <- 'Unattached'
data_aug[, 'Cue'] <- as.factor(data_aug[, 'Cue'])


amult_s <- rep(1:nrow(data_aug), data_aug[, 'Settled'])
amult_u <- rep(1:nrow(data_aug), data_aug[, 'Unattached'])
adata_s <- data_aug[amult_s,]
adata_s[, 'Settled'] <- 1
adata_s <- adata_s[, !(names(adata_s) %in% c('Unattached'))]
adata_u <- data_aug[amult_u,]
adata_u[, 'Unattached'] <- 0
adata_u <- adata_u[, !(names(adata_u) %in% c('Settled'))]
colnames(adata_u)[6] <- 'Settled'
data_aug <- rbind(adata_s, adata_u)

data_aug['Shell'] <- data_aug['Cue']
data_aug['conspecific_cue'] <- data_aug['Cue']
data_aug['predator_cue'] <- data_aug['Cue']
data_aug['biofilm'] <- data_aug['Cue']
data_aug$Shell <- ifelse(grepl("conspecific shell", data_aug$Shell), TRUE, FALSE)
data_aug$conspecific_cue <- ifelse(grepl("conspecific cue", data_aug$conspecific_cue), TRUE, FALSE)
data_aug$predator_cue <- ifelse(grepl("predator cue", data_aug$predator_cue), TRUE, FALSE)
data_aug$biofilm <- ifelse(grepl("biofilm", data_aug$biofilm), TRUE, FALSE)
data_aug$biofilm <-sub("TRUE", "present", data_aug$biofilm)
data_aug$biofilm <-sub("FALSE", "absent", data_aug$biofilm)
data_aug$Shell <-sub("TRUE", "untreated", data_aug$Shell)
data_aug$Shell <-sub("FALSE", "sterilized", data_aug$Shell)
data_aug$predator_cue <-sub("TRUE", "present", data_aug$predator_cue)
data_aug$predator_cue <-sub("FALSE", "absent", data_aug$predator_cue)
data_aug$conspecific_cue <-sub("TRUE", "present", data_aug$conspecific_cue)
data_aug$conspecific_cue <-sub("FALSE", "absent", data_aug$conspecific_cue)

#making the models
#model is data from May
#data = data from may.  data_aug = data from aug

model <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + conspecific_cue:predator_cue + conspecific_cue:Shell + predator_cue:Shell + (1 | Larvae.batch), data = data, family = binomial)
modelx <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + (1 | Larvae.batch), data = data, family = binomial)


summary(model)
summary(modelx)


m<- ggpredict(modelx, terms = c('conspecific_cue', 'predator_cue'))
plot(m)
m11 <- ggpredict(modelx, terms = c('Shell', 'predator_cue'))
plot(m11)
m111<-ggpredict(modelx, terms = c('Shell', 'conspecific_cue'))
plot(m111) 

#post hoc with emmeans using predicted probabilities 

# Conduct post hoc tests with emmeans for the interaction
emm_interaction1 <- emmeans(modelx, ~ conspecific_cue * predator_cue, type = "response")
emm_interaction11 <- emmeans(modelx, ~ Shell * predator_cue, type = "response")
emm_interaction111 <- emmeans(modelx, ~ Shell * conspecific_cue, type = "response")

# Perform pairwise comparisons between the levels of the interaction
pairs(emm_interaction1)
pairs(emm_interaction11)
pairs(emm_interaction111)






#model data from May only without conspecific cues 
data1 = data[data$conspecific_cue %in% c("absent"),]
model1 <-glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1 | Larvae.batch), data = data1, family = binomial)
model1x <-glmer(Settled ~ predator_cue + Shell + (1 | Larvae.batch), data = data1, family = binomial)

summary(model1x)
summary(model1)
m1 <- ggpredict(model1, terms = c('predator_cue', 'Shell'))
plot(m1)

#model data from May only with conspecific cues
data2 = data[data$conspecific_cue %in% c("present"),]
model2 <-glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1 | Larvae.batch), data = data2, family = binomial)
model2x <-glmer(Settled ~ predator_cue + Shell + (1 | Larvae.batch), data = data2, family = binomial)

summary(model2x)
summary(model2)
m2 <- ggpredict(model2, terms = c('predator_cue', 'Shell'))
plot(m2)


#model with data from august
model3 <- glmer(Settled ~ conspecific_cue + predator_cue + Shell + biofilm + conspecific_cue:predator_cue + conspecific_cue:Shell + conspecific_cue:biofilm + predator_cue:Shell + predator_cue:biofilm + Shell:biofilm + (1 | Batch) + (1 | Age), data = data_aug, family = binomial)
model3x <- glmer(Settled ~ conspecific_cue + predator_cue + Shell + biofilm + (1 | Batch), data = data_aug, family = binomial)
summary(model3)
summary(model3x)

m3 <- ggpredict(model3x, terms = c('conspecific_cue', 'predator_cue', 'Shell', 'biofilm'))
plot(m3)

m33<- ggpredict(model3x, terms = c('conspecific_cue', 'predator_cue'))
plot(m33)
m333 <- ggpredict(model3x, terms = c('Shell', 'predator_cue'))
plot(m333)
m3333<-ggpredict(model3x, terms = c('Shell', 'conspecific_cue'))
plot(m3333)


#post hoc with emmeans using predicted probabilities 

# Conduct post hoc tests with emmeans for the interaction
emm_interaction2 <- emmeans(model3x, ~ conspecific_cue * predator_cue, type = "response")
emm_interaction22 <- emmeans(model3x, ~ Shell * predator_cue, type = "response")
emm_interaction222 <- emmeans(model3x, ~ Shell * conspecific_cue, type = "response")
emm_interaction2222 <- emmeans(model3x, ~ conspecific_cue * predator_cue * Shell * biofilm, type = "response")



# Perform pairwise comparisons between the levels of the interaction
pairs(emm_interaction2)
pairs(emm_interaction22)
pairs(emm_interaction222)
pairs(emm_interaction2222)



#model with data from aug without con cues
data_aug1 = data_aug[data_aug$conspecific_cue %in% c("absent"),]
model4 <- glmer(Settled ~ predator_cue + Shell +  biofilm + predator_cue:Shell + predator_cue:biofilm + Shell:biofilm + (1 | Batch) + (1 | Age), data = data_aug1, family = binomial)
summary(model4)
m4 <- ggpredict(model4, terms = c('predator_cue'))
plot(m4)


#model with data from aug only con cues
data_aug2 = data_aug[data_aug$conspecific_cue %in% c("present"),]
model5 <- glmer(Settled ~ predator_cue + Shell + biofilm + predator_cue:Shell + predator_cue:biofilm + Shell:biofilm + (1 | Batch) + (1 | Age), data = data_aug2, family = binomial)
model5x <- glmer(Settled ~ predator_cue + Shell + biofilm + (1 | Batch) + (1 | Age), data = data_aug2, family = binomial)


summary(model5)
summary(model5x)
m5 <- ggpredict(model5, terms = c('predator_cue'))
plot(m5)


#model with combined data 

data_aug_new = data_aug[,-1] #remove "Date.started" variable
colnames(data_aug_new) = c("Larvae.age", "Larvae.batch", "Tray.Number", "Cue", "Settled", "Shell", "conspecific_cue", "predator_cue", "biofilm")
colnames(data_aug_new)
data_aug_new$Larvae.batch = as.factor(data_aug_new$Larvae.batch)
exp = rep("aug", nrow(data_aug_new))
data_aug_new = cbind(data_aug_new, exp)

biofilm = rep("absent", nrow(data))
data_may_new = cbind(data, biofilm)
exp = rep("may", nrow(data_may_new))
data_may_new = cbind(data_may_new, exp)

levels(data_aug_new$Larvae.batch) = c("3", "4")

data_combined = rbind(data_may_new, data_aug_new)

# combined model
model6 <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + conspecific_cue:predator_cue + conspecific_cue:Shell + predator_cue:Shell + (1 | Larvae.age) + (1 | Larvae.batch) + (1 | biofilm) + (1 | exp), data = data_combined, family = binomial)
summary(model6)
m6 <- ggpredict(model6, terms = c('conspecific_cue', 'predator_cue'))
plot(m6)

#model with data combined without con cues
data_combined_1 = data_combined[data_combined$conspecific_cue %in% c("absent"),]
model7 <- glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1 | Larvae.batch) + (1 | Larvae.age) + (1|biofilm), data = data_combined_1, family = binomial)
summary(model7)
m7 <- ggpredict(model7, terms = c('predator_cue'))
plot(m7)


#model with data combined with con cues
data_combined_2 = data_combined[data_combined$conspecific_cue %in% c("present"),]
model8 <- glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1 | Larvae.batch) + (1 | Larvae.age) + (1|biofilm), data = data_combined_2, family = binomial)
summary(model8)
m8 <- ggpredict(model8, terms = c('predator_cue'))
plot(m8)


#removing biofilms
data_nobiofilm = data_combined[data_combined$biofilm %in% c("absent"),]
model9 <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + conspecific_cue:predator_cue + conspecific_cue:Shell + predator_cue:Shell + (1 | Larvae.age) + (1 | Larvae.batch) + (1 | exp), data = data_nobiofilm, family = binomial)
summary(model9)
m9 <- ggpredict(model9, terms = c('conspecific_cue', 'predator_cue'))
plot(m9)

data_nobiofilm_1 = data_nobiofilm[data_nobiofilm$conspecific_cue %in% c("present"),]
model10 <- glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1 | Larvae.batch) + (1 | Larvae.age), data = data_nobiofilm_1, family = binomial)
summary(model10)
m10 <- ggpredict(model10, terms = c('predator_cue'))
plot(m10)
