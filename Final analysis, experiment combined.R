install.packages('lme4')
install.packages('Matrix')
install.packages('ggeffects')
install.packages('DHARMa')
install.packages("stringr")
require(lme4)
require(ggeffects)
require(DHARMa)
require(stringr)
require(lme4)
require(ggeffects)
require(DHARMa)
require(stringr)
require(ggeffects)
require(ggplot2)
require(emmeans)
require(dplyr)

#Combining data from MAY and AUG 


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
data$Shell <-sub("TRUE", "Untreated", data$Shell)
data$Shell <-sub("FALSE", "Sterilized", data$Shell)
data$predator_cue <-sub("TRUE", "Present", data$predator_cue)
data$predator_cue <-sub("FALSE", "Absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "Present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "Absent", data$conspecific_cue)

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
data_aug$biofilm <-sub("TRUE", "biofilm_present", data_aug$biofilm)
data_aug$biofilm <-sub("FALSE", "biofilm_absent", data_aug$biofilm)
data_aug$Shell <-sub("TRUE", "Untreated", data_aug$Shell)
data_aug$Shell <-sub("FALSE", "Sterilized", data_aug$Shell)
data_aug$predator_cue <-sub("TRUE", "Present", data_aug$predator_cue)
data_aug$predator_cue <-sub("FALSE", "Absent", data_aug$predator_cue)
data_aug$conspecific_cue <-sub("TRUE", "Present", data_aug$conspecific_cue)
data_aug$conspecific_cue <-sub("FALSE", "Absent", data_aug$conspecific_cue)


#make levels the same name 
data$Cue <- as.factor(data$Cue) 
levels(data$Cue)[levels(data$Cue) == "conspecific shell"] <- "conspecific shell_FSW"
levels(data$Cue)[levels(data$Cue) == "conspecific shell_conspecific cue"] <- "conspecific cue_conspecific shell"
levels(data$Cue)[levels(data$Cue) == "conspecific shell_predator cue"] <- "predator cue_conspecific shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell"] <- "sterilized shell_FSW"
levels(data$Cue)[levels(data$Cue) == "steralized shell_conspecific cue"] <- "conspecific cue_sterlized shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell_conspecific cue_predator cue"] <- "conspecific cue_predator cue_sterilized shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell_predator cue"] <- "predator cue_sterlized shell"


#combining data 
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
model <-glmer(Settled ~ conspecific_cue + predator_cue + conspecific_cue:predator_cue + (1|Shell) + (1 | biofilm) + (1 | Larvae.batch), data = data_combined, family = binomial)
model1 <-glmer(Settled ~ predator_cue + Shell + predator_cue:Shell + (1|conspecific_cue) + (1 | biofilm) + (1 | Larvae.batch), data = data_combined, family = binomial)
model2 <-glmer(Settled ~ conspecific_cue + Shell + conspecific_cue:Shell + (1|predator_cue) + (1 | biofilm) + (1 | Larvae.batch), data = data_combined, family = binomial)

summary(model)
summary(model1)
summary(model2)

m<-ggpredict(model, terms = c('conspecific_cue', 'predator_cue'))
plot(m)

m1 <-ggpredict(model1, terms = c('Shell','predator_cue'))
plot(m1)

m2<-ggpredict(model2, terms = c('Shell', 'conspecific_cue'))
plot(m2)

#Post Hoc
emm_interaction1 <- emmeans(model, ~ conspecific_cue + predator_cue,  adjust = "tukey", type = "response")
emm_interaction2 <- emmeans(model1, ~ Shell + predator_cue, adjust = "tukey", type = "response")
emm_interaction3 <- emmeans(model2, ~ Shell + conspecific_cue, adjust = "tukey", type = "response")


# Perform pairwise comparisons between the levels of the interaction
pairs(emm_interaction1)
pairs(emm_interaction2)
pairs(emm_interaction3)

#visuals 
plot(m) + 
  labs(x = 'Conspecific Cue (waterbourne)', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) + 
  scale_color_manual(values = c("dodgerblue3", "orangered4"))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))


plot(m1) + 
  labs(x = 'Conspecific Shell', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Predator Cue")) + 
  scale_color_manual(values = c("Khaki2", "orangered4"))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))


plot(m2) + 
  labs(x = 'Conspecific Shell', 
       y= 'Larvae Settled (%)',
       title = "") +
  guides(color = guide_legend(title = "Conspecific Cue (waterbourne)")) + 
  scale_color_manual(values = c("Khaki2","dodgerblue3"))+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12))+
  scale_y_continuous(labels= function(x) paste0(x*100), limits = c(0,1))


#figure out percent settled per treatment 

new_df <- data_combined[, c("Cue", "Settled")]
levels(new_df$Cue)
subset_data <- new_df[new_df$Cue == "predator cue_sterlized shell", ]
subset_data$Settled <- as.numeric(subset_data$Settled)
sum(subset_data$Settled)
level <- "0"
level_count <- sum(subset_data$Settled == level)
print(level_count)
(sum(subset_data$Settled))/(print(level_count))






################ extra code############################################
#######################################################################



#loading data from May

data <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data <- select(data, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
#use this if "select" is not working
#data <- dplyr::select(data, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
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
data$Shell <-sub("TRUE", "Untreated", data$Shell)
data$Shell <-sub("FALSE", "Sterilized", data$Shell)
data$predator_cue <-sub("TRUE", "Present", data$predator_cue)
data$predator_cue <-sub("FALSE", "Absent", data$predator_cue)
data$conspecific_cue <-sub("TRUE", "Present", data$conspecific_cue)
data$conspecific_cue <-sub("FALSE", "Absent", data$conspecific_cue)

###adding data from august
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
data_aug$biofilm <-sub("TRUE", "Present", data_aug$biofilm)
data_aug$biofilm <-sub("FALSE", "Absent", data_aug$biofilm)
data_aug$Shell <-sub("TRUE", "Untreated", data_aug$Shell)
data_aug$Shell <-sub("FALSE", "Sterilized", data_aug$Shell)
data_aug$predator_cue <-sub("TRUE", "Present", data_aug$predator_cue)
data_aug$predator_cue <-sub("FALSE", "Absent", data_aug$predator_cue)
data_aug$conspecific_cue <-sub("TRUE", "Present", data_aug$conspecific_cue)
data_aug$conspecific_cue <-sub("FALSE", "Absent", data_aug$conspecific_cue)


#make levels the same name 
data$Cue <- as.factor(data$Cue) 
levels(data$Cue)[levels(data$Cue) == "conspecific shell"] <- "conspecific shell_FSW"
levels(data$Cue)[levels(data$Cue) == "conspecific shell_conspecific cue"] <- "conspecific cue_conspecific shell"
levels(data$Cue)[levels(data$Cue) == "conspecific shell_predator cue"] <- "predator cue_conspecific shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell"] <- "sterilized shell_FSW"
levels(data$Cue)[levels(data$Cue) == "steralized shell_conspecific cue"] <- "conspecific cue_sterlized shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell_conspecific cue_predator cue"] <- "conspecific cue_predator cue_sterilized shell"
levels(data$Cue)[levels(data$Cue) == "steralized shell_predator cue"] <- "predator cue_sterlized shell"

#####model may
model1 <- glmer(Settled ~ conspecific_cue + predator_cue + Shell + conspecific_cue:predator_cue + conspecific_cue:Shell + predator_cue:Shell + Larvae.age + (1|Larvae.batch), data=data, family=binomial)
summary(model1)
#post hoc
emm_interaction <- emmeans(model1, ~ conspecific_cue + predator_cue, adjust = "tukey", type = "response")
pairs(emm_interaction)


####adding raw proportional data from may
data_raw <- read.csv2(file="Experiment 05-2022[repeat study].csv", sep=",")
data_raw <- select(data_raw, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
#use this if "select" is not working
#data_raw <- dplyr::select(data_raw, -settled_10hr, -unattached_10hr, -settled_20hr, -unattached_20hr)
colnames(data_raw)[5] <- 'Settled'
colnames(data_raw)[6] <- 'Unattached'
data_raw <- data_raw %>%
  mutate(pro_settled = Settled / (Settled + Unattached))
data_raw['Shell'] <- data_raw['Cue']
data_raw['conspecific_cue'] <- data_raw['Cue']
data_raw['predator_cue'] <- data_raw['Cue']

data_raw$Shell <- ifelse(grepl("conspecific shell", data_raw$Shell), TRUE, FALSE)
data_raw$conspecific_cue <- ifelse(grepl("conspecific cue", data_raw$conspecific_cue), TRUE, FALSE)
data_raw$predator_cue <- ifelse(grepl("predator cue", data_raw$predator_cue), TRUE, FALSE)

data_raw$Shell <- sub("TRUE", "Untreated", data_raw$Shell)
data_raw$Shell <- sub("FALSE", "Sterilized", data_raw$Shell)

data_raw$predator_cue <- sub("TRUE", "Present", data_raw$predator_cue)
data_raw$predator_cue <- sub("FALSE", "Absent", data_raw$predator_cue)

data_raw$conspecific_cue <- sub("TRUE", "Present", data_raw$conspecific_cue)
data_raw$conspecific_cue <- sub("FALSE", "Absent", data_raw$conspecific_cue)


#plotting raw data
ggplot(data_raw, aes(x = conspecific_cue, y = pro_settled, color = predator_cue)) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.6) +
  scale_y_continuous(labels = function(x) paste0(x*100), limits = c(0, 1)) +
  labs(x = "Conspecific Cue (waterborne)",
       y = "Larvae Settled (%)",
       color = "Predator Cue",
       title = "Raw settlement data") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("dodgerblue3", "orangered4"))


#figures for paper################################3
###############################################################
#####model predicted data with raw data in violin plots
##############################################################


# predictions 
m1 <- ggpredict(model1, terms = c("conspecific_cue","predator_cue"))
pred <- as.data.frame(m1) |>
  dplyr::mutate(
    x_num = as.integer(factor(as.character(x), levels = c("Absent","Present"))),
    group = factor(as.character(group), levels = c("Absent","Present")),
    # small left/right offset for the two groups
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# raw data (violins stay dodged)
raw_points <- data_raw |>
  dplyr::mutate(
    x_num        = as.integer(factor(as.character(conspecific_cue), levels = c("Absent","Present"))),
    predator_cue = factor(as.character(predator_cue), levels = c("Absent","Present"))
  )

pd <- position_dodge(width = 0.6)

p1 <- ggplot() +
  # RAW VIOLINS (dodged so they sit side-by-side)
  geom_violin(
    data = raw_points,
    aes(x = x_num, y = pro_settled, fill = predator_cue,
        group = interaction(x_num, predator_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # MODEL CIs + LINES + POINTS (use the offset; NO dodge)
  geom_errorbar(
    data = pred,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  scale_x_continuous(breaks = c(1, 2), labels = c("Absent","Present")) +
  scale_y_continuous(labels = function(y) paste0(y*100), limits = c(0,1)) +
  guides(color = guide_legend(title = "Predator Cue"),
         fill  = guide_legend(title = "Predator Cue")) +
  labs(x = "Conspecific Cue (waterborne)", y = "Larvae Settled (%)", title = " ") +
  theme(legend.position = "none", axis.text = element_text(size = 12), axis.title = element_text(size = 12))+
  # Absent letters near x=1 and x=2 (shifted left)
  annotate("text", x = 1 - 0.12, y = 0.25, label = "A", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.50, label = "B", size = 5) +
  # Present letters (shifted right)
  annotate("text", x = 1 + 0.12, y = 0.20, label = "A", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.35, label = "AB", size = 5)



##############################################
#Same plots for the other cue interactions
##predator X Shell


# --- predictions ---
m2 <- ggpredict(model1, terms = c("Shell","predator_cue"))
pred2 <- as.data.frame(m2) |>
  mutate(
    # Shell on x: Sterilized = 1, Untreated = 2
    x_num = as.integer(factor(as.character(x), levels = c("Sterilized","Untreated"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # predator cue groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points2 <- data_raw |>
  mutate(
    x_num        = as.integer(factor(as.character(Shell), levels = c("Sterilized","Untreated"))),
    predator_cue = factor(as.character(predator_cue), levels = c("Absent","Present"))
  )

pd <- position_dodge(width = 0.6)

p2 <- ggplot() +
  # RAW VIOLINS (dodged)
  geom_violin(
    data = raw_points2,
    aes(x = x_num, y = pro_settled, fill = predator_cue,
        group = interaction(x_num, predator_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred2,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred2,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred2,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Sterilized","Untreated")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Predator Cue"),
         fill  = guide_legend(title = "Predator Cue")) +
  labs(x = "Conspecific Shell", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  
  # (optional) significance letters — use same offsets
  annotate("text", x = 1 - 0.12, y = 0.25, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.175, label = "B", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.85, label = "C", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.91,  label = "C", size = 5)


##############################################
#Same plots for the other cue interactions
##Conspecific X Shell


# --- predictions ---
m3  <- ggpredict(model1, terms = c("Shell","conspecific_cue"))
pred3 <- as.data.frame(m3) |>
  mutate(
    # Shell on x: Sterilized = 1, Untreated = 2
    x_num = as.integer(factor(as.character(x), levels = c("Sterilized","Untreated"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # conspecific cue groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points3 <- data_raw |>
  filter(!is.na(Shell), !is.na(conspecific_cue), !is.na(pro_settled)) |>
  mutate(
    x_num           = as.integer(factor(as.character(Shell), levels = c("Sterilized","Untreated"))),
    conspecific_cue = factor(as.character(conspecific_cue), levels = c("Absent","Present"))
  )

pd <- position_dodge(width = 0.6)

p3 <- ggplot() +
  # RAW VIOLINS (dodged)
  geom_violin(
    data = raw_points3,
    aes(x = x_num, y = pro_settled, fill = conspecific_cue,
        group = interaction(x_num, conspecific_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "darkorange2", Present = "dodgerblue3")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred3,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred3,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred3,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "darkorange2", Present = "dodgerblue3")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Sterilized","Untreated")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Conspecific Cue"),
         fill  = guide_legend(title = "Conspecific Cue")) +
  labs(x = "Conspecific Shell", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  
  # (optional) significance letters — use the same offsets if you want per-group labels
  annotate("text", x = 1 - 0.12, y = 0.25, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.48, label = "B", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.86, label = "C", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.99, label = "D", size = 5)

 

library(patchwork)

combined <- (p1 | p2)
combined

p3

combined <- (p4 | p5 | p6) /
  (p7 | p8 | p9)

#############################
##model august ##################
model2 <- glmer(Settled ~ conspecific_cue +  Shell + biofilm + conspecific_cue:predator_cue + conspecific_cue:biofilm + Shell:biofilm + Age + (1|Batch), data=data_aug, family=binomial)

####adding raw proportional data from Aug
data_raw2 <- read.csv2(file="Cue interactions 08-2022.csv", sep=",")
colnames(data_raw2)[6] <- 'Settled'
colnames(data_raw2)[7] <- 'Unattached'
data_raw2 <- data_raw2 %>%
  mutate(pro_settled = Settled / (Settled + Unattached))
data_raw2['Shell'] <- data_raw2['Cue']
data_raw2['conspecific_cue'] <- data_raw2['Cue']
data_raw2['predator_cue'] <- data_raw2['Cue']
data_raw2['biofilm'] <- data_raw2['Cue']

data_raw2$Shell <- ifelse(grepl("conspecific shell", data_raw2$Shell), TRUE, FALSE)
data_raw2$conspecific_cue <- ifelse(grepl("conspecific cue", data_raw2$conspecific_cue), TRUE, FALSE)
data_raw2$predator_cue <- ifelse(grepl("predator cue", data_raw2$predator_cue), TRUE, FALSE)
data_raw2$biofilm <- ifelse(grepl("biofilm", data_raw2$biofilm), TRUE, FALSE)

data_raw2$Shell <- sub("TRUE", "Untreated", data_raw2$Shell)
data_raw2$Shell <- sub("FALSE", "Sterilized", data_raw2$Shell)

data_raw2$predator_cue <- sub("TRUE", "Present", data_raw2$predator_cue)
data_raw2$predator_cue <- sub("FALSE", "Absent", data_raw2$predator_cue)

data_raw2$conspecific_cue <- sub("TRUE", "Present", data_raw2$conspecific_cue)
data_raw2$conspecific_cue <- sub("FALSE", "Absent", data_raw2$conspecific_cue)

data_raw2$biofilm <-sub("TRUE", "Present", data_raw2$biofilm)
data_raw2$biofilm <-sub("FALSE", "Absent", data_raw2$biofilm)




#######Making figures######
#######
# --- predictions ---
m4 <- ggpredict(model2, terms = c("conspecific_cue","predator_cue"))
pred4 <- as.data.frame(m4) |>
  mutate(
    # Conspecific cue on x: Absent = 1, Present = 2
    x_num = as.integer(factor(as.character(x), levels = c("Absent","Present"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # predator cue groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points4 <- data_raw2 |>
  filter(!is.na(conspecific_cue), !is.na(predator_cue), !is.na(pro_settled)) |>
  mutate(
    x_num        = as.integer(factor(as.character(conspecific_cue), levels = c("Absent","Present"))),
    predator_cue = factor(as.character(predator_cue), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot (save as a ggplot object, e.g., p4) ---
p4 <- ggplot() +
  # RAW VIOLINS (dodged by predator cue)
  geom_violin(
    data = raw_points4,
    aes(x = x_num, y = pro_settled, fill = predator_cue,
        group = interaction(x_num, predator_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred4,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred4,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred4,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Absent","Present")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Predator Cue"),
         fill  = guide_legend(title = "Predator Cue")) +
  labs(x = "Conspecific Cue (waterborne)", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# (optional) significance letters using the same ±0.12 offset
p4 <- p4 +
  annotate("text", x = 1 - 0.12, y = 0.180, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.175, label = "A", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.45,  label = "B", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.38,  label = "B", size = 5)

# print
p4




##############################################
#Same plots for the other cue interactions
##predator X Shell

# --- predictions ---

m5 <- ggpredict(model2, terms = c("Shell","predator_cue"))
pred5 <- as.data.frame(m5) |>
  mutate(
    # Shell on x: Sterilized = 1, Untreated = 2
    x_num = as.integer(factor(as.character(x), levels = c("Sterilized","Untreated"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # predator cue groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points5 <- data_raw2 |>
  filter(!is.na(Shell), !is.na(predator_cue), !is.na(pro_settled)) |>
  mutate(
    x_num        = as.integer(factor(as.character(Shell), levels = c("Sterilized","Untreated"))),
    predator_cue = factor(as.character(predator_cue), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot ---
p5 <- ggplot() +
  # RAW VIOLINS (dodged by predator cue)
  geom_violin(
    data = raw_points5,
    aes(x = x_num, y = pro_settled, fill = predator_cue,
        group = interaction(x_num, predator_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred5,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred5,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred5,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "yellow3", Present = "orangered4")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Sterilized","Untreated")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Predator Cue"),
         fill  = guide_legend(title = "Predator Cue")) +
  labs(x = "Conspecific Shell", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# (optional) significance letters — use the same ±0.12 offsets
p5 <- p5 +
  annotate("text", x = 1 - 0.12, y = 0.180, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.175, label = "A", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.60,  label = "B", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.62,  label = "B", size = 5)

# preview
p5



##############################################
#Same plots for the other cue interactions
##Conspecific X Shell

# --- predictions ---
m6 <- ggpredict(model2, terms = c("Shell","conspecific_cue"))
pred6 <- as.data.frame(m6) |>
  mutate(
    # Shell on x: Sterilized = 1, Untreated = 2
    x_num = as.integer(factor(as.character(x), levels = c("Sterilized","Untreated"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # conspecific cue groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points6 <- data_raw2 |>
  filter(!is.na(Shell), !is.na(conspecific_cue), !is.na(pro_settled)) |>
  mutate(
    x_num           = as.integer(factor(as.character(Shell), levels = c("Sterilized","Untreated"))),
    conspecific_cue = factor(as.character(conspecific_cue), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot ---
p6 <- ggplot() +
  # RAW VIOLINS (dodged by conspecific cue)
  geom_violin(
    data = raw_points6,
    aes(x = x_num, y = pro_settled, fill = conspecific_cue,
        group = interaction(x_num, conspecific_cue)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "darkorange2", Present = "dodgerblue3")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred6,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred6,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred6,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "darkorange2", Present = "dodgerblue3")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Sterilized","Untreated")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Conspecific Cue"),
         fill  = guide_legend(title = "Conspecific Cue")) +
  labs(x = "Conspecific Shell", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# (optional) significance letters — use same ±0.12 offsets if per-group
p6 <- p6 +
  annotate("text", x = 1 - 0.12, y = 0.15, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.48, label = "B", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.57, label = "C", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.92, label = "D", size = 5)

# preview
p6


##############################################
#Same plots for the other cue interactions
##Conspecific cue X biofilm

# --- predictions ---
m7 <- ggpredict(model2, terms = c("conspecific_cue","biofilm"))
pred7 <- as.data.frame(m7) |>
  mutate(
    # Conspecific cue on x: Absent = 1, Present = 2
    x_num = as.integer(factor(as.character(x), levels = c("Absent","Present"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # biofilm groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points7 <- data_raw2 |>
  filter(!is.na(conspecific_cue), !is.na(biofilm), !is.na(pro_settled)) |>
  mutate(
    x_num   = as.integer(factor(as.character(conspecific_cue), levels = c("Absent","Present"))),
    biofilm = factor(as.character(biofilm), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot ---
p7 <- ggplot() +
  # RAW VIOLINS (dodged by biofilm)
  geom_violin(
    data = raw_points7,
    aes(x = x_num, y = pro_settled, fill = biofilm,
        group = interaction(x_num, biofilm)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred7,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred7,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred7,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Absent","Present")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Biofilm"),
         fill  = guide_legend(title = "Biofilm")) +
  labs(x = "Conspecific Cue (waterborne)", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# significance letters — use the same ±0.12 offsets if per-group
p7 <- p7 +
  annotate("text", x = 1 - 0.12, y = 0.17, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.32, label = "B", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.45, label = "C", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.63, label = "D", size = 5)

# preview
p7


##############################################
#Same plots for the other cue interactions
##Shell X biofilm


# --- predictions ---
m8 <- ggpredict(model2, terms = c("Shell","biofilm"))
pred8 <- as.data.frame(m8) |>
  mutate(
    # Shell on x: Sterilized = 1, Untreated = 2
    x_num = as.integer(factor(as.character(x), levels = c("Sterilized","Untreated"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # biofilm groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points8 <- data_raw2 |>
  filter(!is.na(Shell), !is.na(biofilm), !is.na(pro_settled)) |>
  mutate(
    x_num   = as.integer(factor(as.character(Shell), levels = c("Sterilized","Untreated"))),
    biofilm = factor(as.character(biofilm), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot ---
p8 <- ggplot() +
  # RAW VIOLINS (dodged by biofilm)
  geom_violin(
    data = raw_points8,
    aes(x = x_num, y = pro_settled, fill = biofilm,
        group = interaction(x_num, biofilm)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred8,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred8,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred8,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Sterilized","Untreated")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Biofilm"),
         fill  = guide_legend(title = "Biofilm")) +
  labs(x = "Conspecific Shell", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

# (optional) significance letters — use the same ±0.12 offsets if per-group
p8 <- p8 +
  annotate("text", x = 1 - 0.12, y = 0.16, label = "A", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.32, label = "B", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.57, label = "C", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.75, label = "D", size = 5)

# preview
p8


##############################################
#Same plots for the other cue interactions
##Predator X biofilm
library(ggeffects)
library(dplyr)
library(ggplot2)

# --- predictions ---
m9 <- ggpredict(model2, terms = c("predator_cue","biofilm"))
pred9 <- as.data.frame(m9) |>
  mutate(
    # Predator cue on x: Absent = 1, Present = 2
    x_num = as.integer(factor(as.character(x), levels = c("Absent","Present"))),
    group = factor(as.character(group), levels = c("Absent","Present")),  # biofilm groups
    # small left/right offset so groups don't overlap at each x
    x_off = x_num + ifelse(group == "Absent", -0.12, 0.12)
  )

# --- raw data for violins (dodged) ---
raw_points9 <- data_raw2 |>
  filter(!is.na(predator_cue), !is.na(biofilm), !is.na(pro_settled)) |>
  mutate(
    x_num   = as.integer(factor(as.character(predator_cue), levels = c("Absent","Present"))),
    biofilm = factor(as.character(biofilm), levels = c("Absent","Present"))
  )

# Dodge ONLY for violins
pd <- position_dodge(width = 0.6)

# --- build plot ---
p9 <- ggplot() +
  # RAW VIOLINS (dodged by biofilm)
  geom_violin(
    data = raw_points9,
    aes(x = x_num, y = pro_settled, fill = biofilm,
        group = interaction(x_num, biofilm)),
    position = pd, alpha = 0.3, color = NA
  ) +
  scale_fill_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # MODEL CI BARS + LINES + POINTS (offset; NO dodge)
  geom_errorbar(
    data = pred9,
    aes(x = x_off, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.06, linewidth = 0.8
  ) +
  geom_line(
    data = pred9,
    aes(x = x_off, y = predicted, color = group, group = group),
    linewidth = 1
  ) +
  geom_point(
    data = pred9,
    aes(x = x_off, y = predicted, color = group),
    size = 2
  ) +
  scale_color_manual(values = c(Absent = "darkslateblue", Present = "darkgreen")) +
  
  # axes / labels / theme
  scale_x_continuous(breaks = c(1, 2), labels = c("Absent","Present")) +
  scale_y_continuous(labels = function(y) paste0(y * 100), limits = c(0, 1)) +
  guides(color = guide_legend(title = "Biofilm"),
         fill  = guide_legend(title = "Biofilm")) +
  labs(x = "Predator Cue", y = "Larvae Settled (%)", title = " ") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
# significance letters — use the same ±0.12 offsets if per-group
p9 <- p9 +
  annotate("text", x = 1 - 0.12, y = 0.16, label = "B", size = 5) +
  annotate("text", x = 1 + 0.12, y = 0.32, label = "A", size = 5) +
  annotate("text", x = 2 - 0.12, y = 0.16, label = "B", size = 5) +
  annotate("text", x = 2 + 0.12, y = 0.34, label = "A", size = 5)

# preview
p9

#######################
##combined figure######
combined <- (p4 | p5 | p6) /
  (p7 | p8 | p9)

# Show it
combined












#######

theme(
  legend.title = element_text(size = 8),  # smaller legend titlehttp://127.0.0.1:19300/graphics/1d1b51a2-36c4-4fad-bb68-b351d869e42f.png
  legend.text  = element_text(size = 8),   # smaller legend labels
  legend.key.size = unit(0.5, "cm"),        # smaller boxes/keys
  legend.margin = margin(t = 0, r = 0, b = 0, l = 0),         # tighten inside spacing
  legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0)     # move legend closer to plot
)