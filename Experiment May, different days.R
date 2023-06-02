#data from study in May

setwd("~/GitHub/Conspecific-Predator-Settlement-Cue")

#with data from 10 hours 

data <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data <- select(data, -settled_20hr, -unattached_20hr, -settled_20hr, -unattached_20hr)
data <- select(data, -settled_30hr, -unattached_30hr, -settled_30hr, -unattached_30hr)
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


#with data from 20 hours 

data2 <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data2 <- select(data2, -settled_10hr, -unattached_10hr, -settled_10hr, -unattached_10hr)
data2 <- select(data2, -settled_30hr, -unattached_30hr, -settled_30hr, -unattached_30hr)
colnames(data2)[5] <- 'Settled'
colnames(data2)[6] <- 'Unattached'
data2$Larvae.batch = as.factor(data2$Larvae.batch)

mult_s2 <- rep(1:nrow(data2), data2[, 'Settled'])
mult_u2 <- rep(1:nrow(data2), data2[, 'Unattached'])
data_s2 <- data2[mult_s2,]
data_s2[, 'Settled'] <- 1
data_s2 <- data_s2[, !(names(data_s2) %in% c('Unattached'))]
data_u2 <- data2[mult_u2,]
data_u2[, 'Unattached'] <- 0
data_u2 <- data_u2[, !(names(data_u2) %in% c('Settled'))]
colnames(data_u2)[5] <- 'Settled'
data2 <- rbind(data_s2, data_u2)

data2['Shell'] <- data2['Cue']
data2['conspecific_cue'] <- data2['Cue']
data2['predator_cue'] <- data2['Cue']
data2$Shell <- ifelse(grepl("conspecific shell", data2$Shell), TRUE, FALSE)
data2$conspecific_cue <- ifelse(grepl("conspecific cue", data2$conspecific_cue), TRUE, FALSE)
data2$predator_cue <- ifelse(grepl("predator cue", data2$predator_cue), TRUE, FALSE)
data2$Shell <-sub("TRUE", "untreated", data2$Shell)
data2$Shell <-sub("FALSE", "sterilized", data2$Shell)
data2$predator_cue <-sub("TRUE", "present", data2$predator_cue)
data2$predator_cue <-sub("FALSE", "absent", data2$predator_cue)
data2$conspecific_cue <-sub("TRUE", "present", data2$conspecific_cue)
data2$conspecific_cue <-sub("FALSE", "absent", data2$conspecific_cue)


#with data from 30 hours 

data3 <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data3 <- select(data3, -settled_10hr, -unattached_10hr, -settled_10hr, -unattached_10hr)
data3 <- select(data3, -settled_20hr, -unattached_20hr, -settled_20hr, -unattached_20hr)
colnames(data3)[5] <- 'Settled'
colnames(data3)[6] <- 'Unattached'
data3$Larvae.batch = as.factor(data3$Larvae.batch)

mult_s3 <- rep(1:nrow(data3), data3[, 'Settled'])
mult_u3 <- rep(1:nrow(data3), data3[, 'Unattached'])
data_s3 <- data3[mult_s3,]
data_s3[, 'Settled'] <- 1
data_s3 <- data_s3[, !(names(data_s3) %in% c('Unattached'))]
data_u3 <- data3[mult_u3,]
data_u3[, 'Unattached'] <- 0
data_u3 <- data_u3[, !(names(data_u3) %in% c('Settled'))]
colnames(data_u3)[5] <- 'Settled'
data3 <- rbind(data_s3, data_u3)

data3['Shell'] <- data3['Cue']
data3['conspecific_cue'] <- data3['Cue']
data3['predator_cue'] <- data3['Cue']
data3$Shell <- ifelse(grepl("conspecific shell", data3$Shell), TRUE, FALSE)
data3$conspecific_cue <- ifelse(grepl("conspecific cue", data3$conspecific_cue), TRUE, FALSE)
data3$predator_cue <- ifelse(grepl("predator cue", data3$predator_cue), TRUE, FALSE)
data3$Shell <-sub("TRUE", "untreated", data3$Shell)
data3$Shell <-sub("FALSE", "sterilized", data3$Shell)
data3$predator_cue <-sub("TRUE", "present", data3$predator_cue)
data3$predator_cue <-sub("FALSE", "absent", data3$predator_cue)
data3$conspecific_cue <-sub("TRUE", "present", data3$conspecific_cue)
data3$conspecific_cue <-sub("FALSE", "absent", data3$conspecific_cue)



#model with 10 hrs 
model <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + (1 | Larvae.batch), data = data, family = binomial)
summary(model)

m <-ggpredict(model, terms = c('conspecific_cue', 'predator_cue', 'Shell')) 
plot(m)


#model with 20 hrs
model2 <- glmer(Settled ~ conspecific_cue + predator_cue + Shell + (1 | Larvae.batch), data = data2, family = binomial)
summary(model2)

m2 <-ggpredict(model2, terms = c('conspecific_cue', 'predator_cue', 'Shell')) 
plot(m2)

#model with 10 hrs 
model3 <-glmer(Settled ~ conspecific_cue + predator_cue + Shell + (1 | Larvae.batch), data = data3, family = binomial)
summary(model3)

m3 <-ggpredict(model3, terms = c('conspecific_cue', 'predator_cue', 'Shell')) 
plot(m3)

