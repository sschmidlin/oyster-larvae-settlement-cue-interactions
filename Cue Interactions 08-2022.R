install.packages('lme4')
install.packages('Matrix')
install.packages('ggeffects')
install.packages('DHARMa')
install.packages("stringr")
require(lme4)
require(ggeffects)
require(DHARMa)
require(stringr)

setwd("~/GitHub/Conspecific-Predator-Settlement-Cue")
data <- read.csv2(file="Cue interactions 08-2022.csv", sep=",")


# Convert Cue and Tray Number to factor
colnames(data)[4] <- 'Well_Number'
colnames(data)[2] <- 'Age'
colnames(data)[3] <- 'Batch'
colnames(data)[5] <- 'Cue'
colnames(data)[6] <- 'Settled'
colnames(data)[7] <- 'Unattached'
data[, 'Cue'] <- as.factor(data[, 'Cue'])

# Type of model: generalized linear mixed-effect model; response variable is binary -> logistic regression
# Binary response variable: calculate per larva and save as "settlement"
# Predictor variables: shell, conspecific_cue, predator_cue, Biofilm 
# Random effect variables: Age, Batch 

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
colnames(data_u)[6] <- 'Settled'
data <- rbind(data_s, data_u)

# Make new binary predictor variables from multilevel factor
# shell: sterilized vs. untreated
# conspecific cue: present vs. absent
# predator cue: present vs. absent
# Biofilm: present vs. absent 
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


#the str_extract way

#data$biofilm <- str_extract(data$biofilm, "biofilm")
#data$biofilm[is.na(data$biofilm)] <- "absent"
#data$biofilm <-sub("biofilm", "present", data$biofilm)
#data$conspecific_cue <- str_extract(data$conspecific_cue, "conspecific cue")
#data$conspecific_cue[is.na(data$conspecific_cue)] <- 'absent'
#data$conspecific_cue <-sub("conspecific cue", "present", data$conspecific_cue)
#data$predator_cue <- str_extract(data$predator_cue, "predator cue")
#data$predator_cue[is.na(data$predator_cue)] <- 'absent'
#data$predator_cue <-sub("predator cue", "present", data$predator_cue)
#data$Shell <- str_extract(data$Shell, "conspecific shell")
#data$Shell[is.na(data$Shell)] <- 'sterilized'
#data$Shell <-sub("conspecific shell", "untreated", data$Shell)


# 3. Make a statistical model based on (conspecific vs predator cues)
model <- glmer(Settled ~ conspecific_cue * predator_cue + Shell +  biofilm + (1 | Batch) + (1 | Age), data = data, family = binomial)
model2 <- glmer(Settled ~ conspecific_cue * predator_cue + Shell *  biofilm + (1 | Batch) + (1 | Age), data = data, family = binomial)


# Testing model assumptions
testDispersion(model)
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
plot(simulationOutput)


# Test model
summary(model)
summary(model2)

# Visualize model predictions

m <- ggpredict(model, terms = c("conspecific_cue", "predator_cue"))
plot(m)
m2 <- ggpredict(model, terms = c("Shell", "conspecific_cue"))
plot(m2)
m3 <-ggpredict(model, terms = c("Shell", "biofilm"))
plot(m3)
m4 <-ggpredict(model, terms = c("biofilm", "conspecific_cue"))
plot(m4)
m5 <-ggpredict(model, terms = c("Shell", "predator_cue"))
plot(m5)
m6 <-ggpredict(model, terms = c("biofilm", "predator_cue"))
plot(m6)




#adjusting astestics
plot(m) + 
  labs(x = 'Conspecific Cue', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("dodgerblue3", "orangered4"))+
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,.5))  #to change the y axis limits
 
#theme(axis.text = element_text(face = 'bold', size = 10)) +

plot(m2) + 
  labs(x = 'Shell', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Conspecific Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Khaki2", "dodgerblue3"))+
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits


plot(m3) +
  labs(x = "Shell", 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = 'Biofilm')) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Khaki2", "chartreuse4")) +
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits


plot(m4) +
  labs(x = 'Conspecific Cue', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Biofilm")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("dodgerblue3", "chartreuse4")) +
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits

plot(m5) +
  labs(x = 'Shell', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("Khaki2", "orangered4"))+
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits



plot(m6) +
  labs(x = 'Biofilm', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("chartreuse4", "orangered4"))+
  theme(legend.title = element_text(size = 15),
        axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))  #to change the y axis limits



#visualizing whole data set 

levels(data$Cue) <- c("B-CC-CS", "B-CC-PC-CS", "B-CC-PC-SS", "B-CC-SS", "B-CS", "B-PC-CS", "B-PC-SS", "B-SS", "CC-CS", "CC-PC-CS", "CC-PC-SS", "CC-SS", "CS", "PC-CS", "PC-SS", "SS")
model2 <- glmer(Settled ~ Cue + (1 | Batch) + (1 | Age), data = data, family = binomial)
m0<- ggpredict(model2, terms = ('Cue'))
plot(m0)

car::Anova(model3, type=2)
marginal <-lsmeans(model3, ~ Cue)
pairs(marginal, adjust="tukey")

plot(m0) +
  labs(x = 'Cue', 
       y= 'Larvae Settled (%)',
       title = "") +
  scale_color_manual(breaks = c("absent", "present")
                     , labels= c("Absent", "Present"),
                     values = c("chartreuse4", "orangered4"))+
  theme(legend.title = element_text(size = 20),
        axis.title = element_text(size = 20), 
        axis.text = element_text(size = 20))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))
