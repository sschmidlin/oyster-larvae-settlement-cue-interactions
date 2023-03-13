setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#all data
data <- read.csv2(file="cue interactions data.csv", sep=",")


#All data, cleaned for use in model
colnames(data)[4] <- 'Well_Number'
colnames(data)[2] <- 'Age'
colnames(data)[3] <- 'Batch'
colnames(data)[5] <- 'Cue'
colnames(data)[6] <- 'Settled'
colnames(data)[7] <- 'Unattached'
data[, 'Cue'] <- as.factor(data[, 'Cue'])

mult_s <- rep(1:nrow(data), data[, 'Settled'])
mult_u <- rep(1:nrow(data), data[, 'Unattached'])
data_s <- data[mult_s,]
data_s[, 'Settled'] <- 1
data_s <- data_s[, !(names(data_s) %in% c('Unattached'))]
data_u <- data[mult_u,]
data_u[, 'Unattached'] <- 0
data_u <- data_u[, !(names(data_u) %in% c('Settled'))]
colnames(data_u)[6] <- 'Settled'
data <- rbind(data_s, data_u)

data['Shell'] <- data['Cue']
data['conspecific_cue'] <- data['Cue']
data['predator_cue'] <- data['Cue']
data['biofilm'] <- data['Cue']

#this is the true/false way
data$Shell <- ifelse(grepl("conspecific shell", data$Shell), TRUE, FALSE)
data$conspecific_cue <- ifelse(grepl("conspecific cue", data$conspecific_cue), TRUE, FALSE)
data$predator_cue <- ifelse(grepl("predator cue", data$predator_cue), TRUE, FALSE)
data$biofilm <- ifelse(grepl("biofilm", data$biofilm), TRUE, FALSE)

data$biofilm <-sub("TRUE", "present", data$biofilm)
data$biofilm <-sub("FALSE", "absent", data$biofilm)
data$Shell <-sub("TRUE", "untreated", data$Shell)
data$Shell <-sub("FALSE", "sterilized", data$Shell)
data$predator_cue <-sub("TRUE", "present", data$predator_cue)
data$predator_cue <-sub("FALSE", "absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "absent", data$conspecific_cue)

data$Batch <- as.factor(data$Batch)

#model 
#without random effects
model <- glmer(Settled ~ conspecific_cue + predator_cue * conspecific_cue:predator_cue + Shell + (1 | Batch) + (1 | Age), data = data, family = binomial)

#with random effects
model <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data, family = binomial)
summary(model)

#visualizing
m <- ggpredict(model, terms = c("conspecific_cue", "predator_cue"))
plot(m)
m2 <- ggpredict(model, terms = c("Shell", "conspecific_cue"))
plot(m2)
m3 <-ggpredict(model, terms = c("Shell", "biofilm"))
plot(m3)
m4 <-ggpredict(model, terms = c("conspecific_cue", "biofilm"))
plot(m4)
m5 <-ggpredict(model, terms = c("Shell", "predator_cue"))
plot(m5)
m6 <-ggpredict(model, terms = c("biofilm", "predator_cue"))
plot(m6)





#Experiment day 27-08-22 same age larvae used from batch 2
data2 <-read.csv2(file="cue interactions 8-22 - 27th.csv", sep=",")



# data from day 27-08-22, cleaned for use in model
colnames(data2)[4] <- 'Well_Number'
colnames(data2)[2] <- 'Age'
colnames(data2)[3] <- 'Batch'
colnames(data2)[5] <- 'Cue'
colnames(data2)[6] <- 'Settled'
colnames(data2)[7] <- 'Unattached'
data2[, 'Cue'] <- as.factor(data2[, 'Cue'])


mult2_s <- rep(1:nrow(data2), data2[, 'Settled'])
mult2_u <- rep(1:nrow(data2), data2[, 'Unattached'])
data2_s <- data2[mult2_s,]
data2_s[, 'Settled'] <- 1
data2_s <- data2_s[, !(names(data2_s) %in% c('Unattached'))]
data2_u <- data2[mult2_u,]
data2_u[, 'Unattached'] <- 0
data2_u <- data2_u[, !(names(data2_u) %in% c('Settled'))]
colnames(data2_u)[6] <- 'Settled'
data2 <- rbind(data2_s, data2_u)

data2['Shell'] <- data2['Cue']
data2['conspecific_cue'] <- data2['Cue']
data2['predator_cue'] <- data2['Cue']
data2['biofilm'] <- data2['Cue']

data2$Shell <- ifelse(grepl("conspecific shell", data2$Shell), TRUE, FALSE)
data2$conspecific_cue <- ifelse(grepl("conspecific cue", data2$conspecific_cue), TRUE, FALSE)
data2$predator_cue <- ifelse(grepl("predator cue", data2$predator_cue), TRUE, FALSE)
data2$biofilm <- ifelse(grepl("biofilm", data2$biofilm), TRUE, FALSE)

data2$biofilm <-sub("TRUE", "present", data2$biofilm)
data2$biofilm <-sub("FALSE", "absent", data2$biofilm)
data2$Shell <-sub("TRUE", "untreated", data2$Shell)
data2$Shell <-sub("FALSE", "sterilized", data2$Shell)
data2$predator_cue <-sub("TRUE", "present", data2$predator_cue)
data2$predator_cue <-sub("FALSE", "absent", data2$predator_cue)
data2$conspecific_cue <-sub("TRUE", "present", data2$conspecific_cue)
data2$conspecific_cue <-sub("FALSE", "absent", data2$conspecific_cue)

data2$Batch <- as.factor(data2$Batch)

#model 
model2 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data2, family = binomial)
summary(model2)

#visualizing
m.2 <- ggpredict(model2, terms = c("conspecific_cue", "predator_cue"))
plot(m.2)
m2.2 <- ggpredict(model2, terms = c("Shell", "conspecific_cue"))
plot(m2.2)
m3.2 <-ggpredict(model2, terms = c("Shell", "biofilm"))
plot(m3.2)
m4.2 <-ggpredict(model2, terms = c("conspecific_cue", "biofilm"))
plot(m4.2)
m5.2 <-ggpredict(model2, terms = c("Shell", "predator_cue"))
plot(m5.2)
m6.2 <-ggpredict(model2, terms = c("biofilm", "predator_cue"))
plot(m6.2)






#Experiment day 30-08-22same age larvae used from batch 2 

data3 <-read.csv2(file="cue interactions 8-22 - 30th.csv", sep=",")


# data from day 27-08-22, cleaned for use in model
colnames(data3)[4] <- 'Well_Number'
colnames(data3)[2] <- 'Age'
colnames(data3)[3] <- 'Batch'
colnames(data3)[5] <- 'Cue'
colnames(data3)[6] <- 'Settled'
colnames(data3)[7] <- 'Unattached'
data3[, 'Cue'] <- as.factor(data3[, 'Cue'])


mult3_s <- rep(1:nrow(data3), data3[, 'Settled'])
mult3_u <- rep(1:nrow(data3), data3[, 'Unattached'])
data3_s <- data3[mult3_s,]
data3_s[, 'Settled'] <- 1
data3_s <- data3_s[, !(names(data3_s) %in% c('Unattached'))]
data3_u <- data3[mult3_u,]
data3_u[, 'Unattached'] <- 0
data3_u <- data3_u[, !(names(data3_u) %in% c('Settled'))]
colnames(data3_u)[6] <- 'Settled'
data3 <- rbind(data3_s, data3_u)

data3['Shell'] <- data3['Cue']
data3['conspecific_cue'] <- data3['Cue']
data3['predator_cue'] <- data3['Cue']
data3['biofilm'] <- data3['Cue']

data3$Shell <- ifelse(grepl("conspecific shell", data3$Shell), TRUE, FALSE)
data3$conspecific_cue <- ifelse(grepl("conspecific cue", data3$conspecific_cue), TRUE, FALSE)
data3$predator_cue <- ifelse(grepl("predator cue", data3$predator_cue), TRUE, FALSE)
data3$biofilm <- ifelse(grepl("biofilm", data3$biofilm), TRUE, FALSE)

data3$biofilm <-sub("TRUE", "present", data3$biofilm)
data3$biofilm <-sub("FALSE", "absent", data3$biofilm)
data3$Shell <-sub("TRUE", "untreated", data3$Shell)
data3$Shell <-sub("FALSE", "sterilized", data3$Shell)
data3$predator_cue <-sub("TRUE", "present", data3$predator_cue)
data3$predator_cue <-sub("FALSE", "absent", data3$predator_cue)
data3$conspecific_cue <-sub("TRUE", "present", data3$conspecific_cue)
data3$conspecific_cue <-sub("FALSE", "absent", data3$conspecific_cue)

data3$Batch <- as.factor(data3$Batch)

#model 
model3 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data3, family = binomial)
summary(model3)

#visualizing
m.3 <- ggpredict(model3, terms = c("conspecific_cue", "predator_cue"))
plot(m.3)
m2.3 <- ggpredict(model3, terms = c("Shell", "conspecific_cue"))
plot(m2.3)
m3.3 <-ggpredict(model3, terms = c("Shell", "biofilm"))
plot(m3.3)
m4.3 <-ggpredict(model3, terms = c("conspecific_cue", "biofilm"))
plot(m4.3)
m5.3 <-ggpredict(model3, terms = c("Shell", "predator_cue"))
plot(m5.3)
m6.3 <-ggpredict(model3, terms = c("biofilm", "predator_cue"))
plot(m6.3)




#Experiment day 27-08-22 larvae used from batch 1
data4 <-read.csv2(file="cue interactions 8-22 - 28th(1).csv", sep=",")

#All data, cleaned for use in model
colnames(data4)[4] <- 'Well_Number'
colnames(data4)[2] <- 'Age'
colnames(data4)[3] <- 'Batch'
colnames(data4)[5] <- 'Cue'
colnames(data4)[6] <- 'Settled'
colnames(data4)[7] <- 'Unattached'
data4[, 'Cue'] <- as.factor(data4[, 'Cue'])

mult4_s <- rep(1:nrow(data4), data4[, 'Settled'])
mult4_u <- rep(1:nrow(data4), data4[, 'Unattached'])
data4_s <- data4[mult4_s,]
data4_s[, 'Settled'] <- 1
data4_s <- data4_s[, !(names(data4_s) %in% c('Unattached'))]
data4_u <- data4[mult4_u,]
data4_u[, 'Unattached'] <- 0
data4_u <- data4_u[, !(names(data4_u) %in% c('Settled'))]
colnames(data4_u)[6] <- 'Settled'
data4 <- rbind(data4_s, data4_u)

data4['Shell'] <- data4['Cue']
data4['conspecific_cue'] <- data4['Cue']
data4['predator_cue'] <- data4['Cue']
data4['biofilm'] <- data4['Cue']

#this is the true/false way
data4$Shell <- ifelse(grepl("conspecific shell", data4$Shell), TRUE, FALSE)
data4$conspecific_cue <- ifelse(grepl("conspecific cue", data4$conspecific_cue), TRUE, FALSE)
data4$predator_cue <- ifelse(grepl("predator cue", data4$predator_cue), TRUE, FALSE)
data4$biofilm <- ifelse(grepl("biofilm", data4$biofilm), TRUE, FALSE)

data4$biofilm <-sub("TRUE", "present", data4$biofilm)
data4$biofilm <-sub("FALSE", "absent", data4$biofilm)
data4$Shell <-sub("TRUE", "untreated", data4$Shell)
data4$Shell <-sub("FALSE", "sterilized", data4$Shell)
data4$predator_cue <-sub("TRUE", "present", data4$predator_cue)
data4$predator_cue <-sub("FALSE", "absent", data4$predator_cue)
data4$conspecific_cue <-sub("TRUE", "present", data4$conspecific_cue)
data4$conspecific_cue <-sub("FALSE", "absent", data4$conspecific_cue)

data4$Batch <- as.factor(data4$Batch)

#model 

model4 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data4, family = binomial)
summary(model4)

#visualizing
m.4 <- ggpredict(model4, terms = c("conspecific_cue", "predator_cue"))
plot(m.4)
m2.4 <- ggpredict(model4, terms = c("Shell", "conspecific_cue"))
plot(m2.4)
m3.4 <-ggpredict(model4, terms = c("Shell", "biofilm"))
plot(m3.4)
m4.4 <-ggpredict(model4, terms = c("conspecific_cue", "biofilm"))
plot(m4.4)
m5.4 <-ggpredict(model, terms = c("Shell", "predator_cue"))
plot(m5.4)
m6.4 <-ggpredict(model, terms = c("biofilm", "predator_cue"))
plot(m6.4)

