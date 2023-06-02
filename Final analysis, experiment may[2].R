#data from experiment 2
setwd("~/GitHub/")
data <- read.csv2(file="biofilm excel 04-22.csv", sep=",")
#deleting 10hr, 20hr data 
data <- select(data, -settled_10h, -unsettled_10h, -settled_20h, -unsettled_20h)
colnames(data)[5] <- 'Settled'
colnames(data)[6] <- 'Unattached'

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

data['Biofilm'] <- data['Cue']
data['conspecific_cue'] <- data['Cue']
data['predator_cue'] <- data['Cue']

data$Biofilm <- ifelse(grepl("Biofilm", data$Biofilm), TRUE, FALSE)
data$conspecific_cue <- ifelse(grepl("Conspecific", data$conspecific_cue), TRUE, FALSE)
data$predator_cue <- ifelse(grepl("Predator", data$predator_cue), TRUE, FALSE)

data$Biofilm <-sub("TRUE", "Biofilm_present", data$Biofilm)
data$Biofilm <-sub("FALSE", "Biofilm_absent", data$Biofilm)
data$predator_cue <-sub("TRUE", "predator_present", data$predator_cue)
data$predator_cue <-sub("FALSE", "predator_absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "conspecific_present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "conspecific_absent", data$conspecific_cue)

#making the models 

model <- glmer(Settled ~ conspecific_cue + predator_cue + conspecific_cue:predator_cue + (1|Biofilm) + (1 | larvae.batch), data = data, family = binomial)
model1 <- glmer(Settled ~ predator_cue + Biofilm + predator_cue:Biofilm + (1|conspecific_cue) + (1 | larvae.batch), data = data, family = binomial)
model2 <- glmer(Settled ~ conspecific_cue + Biofilm + conspecific_cue:Biofilm + (1|predator_cue) + (1 | larvae.batch), data = data, family = binomial)

summary(model)
summary(model1)
summary(model2)

m<-ggpredict(model, terms = c('conspecific_cue', 'predator_cue'))
plot(m)

m1 <-ggpredict(model1, terms = c('Biofilm','predator_cue'))
plot(m1)

m2<-ggpredict(model2, terms = c('Biofilm', 'conspecific_cue'))
plot(m2)

#post hoc
emm_interaction1 <- emmeans(model, ~ conspecific_cue + predator_cue,  adjust = "tukey", type = "response")
emm_interaction2 <- emmeans(model1, ~ Biofilm + predator_cue, adjust = "tukey", type = "response")
emm_interaction3 <- emmeans(model2, ~ Biofilm + conspecific_cue, adjust = "tukey", type = "response")

# Perform pairwise comparisons between the levels of the interaction
pairs(emm_interaction1)
pairs(emm_interaction2)
pairs(emm_interaction3)

