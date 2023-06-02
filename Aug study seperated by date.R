data <- read.csv2(file="Cue interactions 08-2022.csv", sep=",")
colnames(data)[4] <- 'Well_Number'
colnames(data)[2] <- 'Age'
colnames(data)[3] <- 'Batch'
colnames(data)[5] <- 'Cue'
colnames(data)[6] <- 'Settled'
colnames(data)[7] <- 'Unattached'
data[, 'Cue'] <- as.factor(data[, 'Cue'])
#adding percent settled
data$larvae <- data$Settled + data$Unattached
data$proportion <- data$Settled / data$larvae

m<- violinplot()

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


data1 <- data[data$Date.started == "8/27/2022", ]

model1 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data1, family= binomial(link = "logit"))
summary(model)
m <- ggpredict(model1, terms= c('conspecific_cue', 'predator_cue'))
plot(m)

data2 <- data[data$Date.started == "8/28/2022", ]
model2 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data2, family= binomial(link = "logit"))
summary(model)
m <- ggpredict(model2, terms= c('conspecific_cue', 'predator_cue'))
plot(m)

data3 <- data[data$Date.started == "8/30/2022", ]
model3 <- glm(Settled ~ conspecific_cue + predator_cue + Shell + biofilm, data = data3, family= binomial(link = "logit"))
summary(model3)
m <- ggpredict(model, terms= c('conspecific_cue', 'predator_cue'))
plot(m)




#data repeat study 

data2 <- read.csv2(file="Experiment 1 NewData.csv", sep=",")
data2 <- select(data, -settled_20hr, -unattached_20hr, -settled_20hr, -unattached_20hr)
data2 <- select(data, -settled_30hr, -unattached_30hr, -settled_30hr, -unattached_30hr)
colnames(data)[5] <- 'Settled'
colnames(data)[6] <- 'Unattached'
