
######################################################################################################################
####           LIST           ####
# what still needs to be done:


# check out if theres a batch difference
# possible re-formatting of the data for wells with missing larvae
#    --> if the larvae are missing in both 20 and 30h then scrap the well
#    --> if they have been found again at 30h then we keep and we attribute it to hiding
# 
# create some interesting bar and histogram charts (its difficult on R and i'm bad so maybe excel)

# 






##start

setwd("C:\\Users\\Owner\\Desktop\\2nd experiment")

#install.packages("lme4")
#install.packages("ggeffects")
#install.packages("stringr")
#require(lme4)
#require(ggeffects)
#require(ggplot2)
library(ggplot2)
library(dplyr)
library(stringr)
library(lme4)
library(ggeffects)




#loading data --> bdata (biofilm data)
#use if controls = steralised + conspecific cue
data <- read.csv2(file="biofilm excel.csv", check.names=FALSE, sep=",")

#use if controls = steralised + FSW
#data <- read.csv2(file="biofilm_control excel.csv", check.name=FALSE, sep=",")

# Convert dates from chr to Date-format
data[, 'Date_started'] <- as.Date(data[, 'Date_started'], "%d/%m/%Y")
data[, 'Fertilisation_date'] <- as.Date(data[, 'Fertilisation_date'], "%d/%m/%Y")

#help("as.Date")

# Convert Cue and Tray Number to factor
colnames(data)[4] <- 'Tray_Number_Well'
data[, 'Tray_Number_Well'] <- as.factor(data[, 'Tray_Number_Well'])
data[, 'Cue'] <- as.factor(data[, 'Cue'])
levels(data[,'Cue'])


# Calculate ages of larvae
data['age'] <- NA
for(i in 1:nrow(data)){
  data[i, 'age'] <- difftime(data[i, 'Date_started'], data[i, 'Fertilisation_date'], units = "days")
}
i
#help("difftime")



  #Does settlement differ between treatments?
  # Type of model: generalized linear mixed-effect model; response variable is binary -> logistic regression
  # Binary response variable: calculate per larva and save as "settlement"
  # Predictor variables: shell, conspecific_cue, predator_cue
  # Random effect variables: Crab, age, Tray_Number, Well


#remove empty rows
#data <- data[!apply(is.na(data) | data == "", 1, all),]
#is.na(data) <- 1


#1. Calculate response variable from input data
# Multiply rows by number of larvae per well for settled and unattached and concatenate the resulting dataframes




#larvae settlement
#takes all values not considered NA and places them in a new data
data<- data[!is.na(data[,'Settled_30h']),] 

#repeating a row for each larvae each time, settled/unsettled
mult_s1 <- rep(1:nrow(data), data[, 'Settled_10h'])
mult_u1 <- rep(1:nrow(data), data[, 'Unsettled_10h'])

mult_s2 <- rep(1:nrow(data), data[, 'Settled_20h'])
mult_u2 <- rep(1:nrow(data), data[, 'Unsettled_20h'])

mult_s3 <- rep(1:nrow(data), data[, 'Settled_30h'])
mult_u3 <- rep(1:nrow(data), data[, 'Unsettled_30h'])

#assigning each multi df into its own data df, will then cut out the other collumns and mash them together
data_s1 <- data[mult_s1, ]
data_s2 <- data[mult_s2, ]
data_s3 <- data[mult_s3, ]
data_u1 <- data[mult_u1, ]
data_u2 <- data[mult_u2, ]
data_u3 <- data[mult_u3, ]


#assigning the value 1 for each settled larve = for binary stats later
data_s1[, 'Settled_10h'] <- 1
data_s2[, "Settled_20h"] <- 1
data_s3[, "Settled_30h"] <- 1

data_u1[, 'Unsettled_10h'] <- 0
data_u2[, 'Unsettled_20h'] <- 0
data_u3[, 'Unsettled_30h'] <- 0

#taking away useless columns of data
data_s1 <- data_s1[, !(names(data_s1) %in% c("Unsettled_10h", "Settled_20h", "Unsettled_20h", "Settled_30h", "Unsettled_30h"))]
data_s2 <- data_s2[, !(names(data_s2) %in% c("Unsettled_10h", "Settled_10h", "Unsettled_20h", "Settled_30h", "Unsettled_30h"))]                                          
data_s3 <- data_s3[, !(names(data_s3) %in% c("Unsettled_10h", "Settled_20h", "Unsettled_20h", "Settled_10h", "Unsettled_30h"))]                                           
data_u1 <- data_u1[, !(names(data_u1) %in% c("Settled_10h", "Settled_20h", "Unsettled_20h", "Settled_30h", "Unsettled_30h"))]
data_u2 <- data_u2[, !(names(data_u2) %in% c("Unsettled_10h", "Settled_20h", "Settled_10h", "Settled_30h", "Unsettled_30h"))]
data_u3 <- data_u3[, !(names(data_u3) %in% c("Unsettled_10h", "Settled_20h", "Unsettled_20h", "Settled_30h", "Settled_10h"))]


#trying to find a way to bind all settled columns together in the same dataset
#testing if there are differences
identical(names(data_s1), names(data_s2)) 
#creating a new df to put them in
data_1 <- data

#need to change the names so they fit together, as they are binary we can have datasets for each timeframe 10, 20 30h
#is it needed to combine them togehter afterwards?
colnames(data_u1)[6] <- 'Settled_10h'

#trying to bind them
data_1 <- rbind(data_s1, data_u1 )

#do the same for time 20h 30h
data_2 <- data
data_3 <- data 
colnames(data_u2)[6] <- 'Settled_20h'
colnames(data_u3)[6] <- 'Settled_30h'
data_2 <- rbind(data_s2, data_u2 )
data_3 <- rbind(data_s3, data_u3 )

#Note: at 10h --> 1021 larvae     at 20h 1009larvae     then it goes back up because we find some hidden ones at 30h --> 1016 larvae
#normally should be around 1080 larvae at the start, some wells were lost and some larvae were hidden
# since the start till 10h= lost 59 larvae, at 30h lost 64 larvae
#represents about 5.9% of total larvae count

table(data_3[,'Settled_30h']) # just to see how many larvae settled
# about 2/3 didnt settle

#not working --> try organise by column
#data_s1 %>% arrange(Tray_Number_Well )
#data_u1 %>% arrange(Tray_Number_Well )

#####is there a way to link all 3 time dataframes together?
#data_s <- cbind(data_2, data_3, data_1 )





# 2. Make new binary predictor variables from multilevel factor
# shell: sterilized vs. biofilm
# conspecific cue: present vs. absent
# predator cue: present vs. absent

#giving the treatments for shells in data_1 --> dont know why they need to be replicated 3 and 4 times#####################
##levels(data_1$shell) <- c(rep('Biofilm', 3), rep('Sterilized', 4))

##data_1['Conspecific'] <- data_1['Cue']
##levels(data_1$Conspecific) <- c('absent', 'present', rep('absent', 2), rep('present', 2), 'absent')

##data_1['Predator'] <- data_1['Cue']
##levels(data_1$Predator) <- c(rep('absent', 2), 'present', rep('absent', 2), rep('present', 2))

#table(data_1[, c('shell', 'Conspecific', 'Predator')]) # check if experiment was balanced


########################################################################################################################################################
##          IF CONTROL CUES TURN OUT TO BE ONLY FSW AND STERALISED CHIPS
## splits all the cues to see the results of each larvae and indivudial cues (makes it binary)
## only works for 10h data, to change replace "data_1" with the other dfs
##
##

##                      Here is where you swap the data_1/data_2/data_3


data_3['shell'] <- data_3['Cue']
data_3['Conspecific'] <- data_3['Cue']
data_3['Predator'] <- data_3['Cue']

data_3$shell <- str_extract(data_3$Cue, "Biofilm")
data_3$shell[is.na(data_3$shell)] <- "Sterilized"

data_3$Conspecific <- str_extract(data_3$Cue, "Conspecific")
data_3$Conspecific <-sub("Conspecific", "present", data_3$Conspecific)
data_3$Conspecific[is.na(data_3$Conspecific)] <- "absent"

data_3$Predator <- str_extract(data_3$Predator, "Predator")
data_3$Predator <- sub("Predator", "present",data_3$Predator )
data_3$Predator[is.na(data_3$Predator)] <- "absent"

table(data_3[, c('shell', 'Conspecific', 'Predator')]) ######   we dont have any normal conspecific treatment (no predators no biofilm) so I dont know if its a problem.

##edit we are always going to be missing a Cue to complete the table, I dont know if thats critical or not

########################################################################################################################################################

##                  will need to replace Settled_10h with 20h and 30h



#getting rid of extra clutter
rm(data_s1, data_s2, data_s3, data_u1, data_u2, data_u3)

# 3. Make a statistical model
model <- glmer(Settled_20h ~ shell * Conspecific * Predator + (1 | age) + (1 | Tray_Number_Well), data = data_3, family = binomial)
plot(model)
##    model always fails to converge (?)
##    Experiment was unbalanced 
##    lacking data for sterallised conspecific cue with predators absent -> repeat analysis with only sterilized shells
##    -> repeat analysis with only biofilm shells (there must be a way, ask sarah/pascal --> should we include the control here?)


# MODEL 2   keep only biofilm cues
m_data1 <- data_3[data_3[, 'shell'] %in% 'Biofilm', ]
#make a table with 2 cues
table(m_data1[, c('Conspecific', 'Predator')])
model2 <- glmer(Settled_20h ~ Conspecific * Predator + (1 | age) +(1 | Tray_Number_Well) + (1 | Larvae_batch), data = data_3, family = binomial)

plot(model2)
summary(model2)
m2 <- ggpredict(model2, terms = c("Conspecific", "Predator"))
plot(m2)



# MODEL 3
# Repeat analysis once more without steralised cues
m_data2 <- data_3[data_3[, 'Conspecific'] %in% 'absent', ]
table(m_data2[, c('Predator', 'shell')])
model3 <- glmer(Settled_20h ~ Predator * shell + (1 | age) + (1 | Tray_Number_Well), data = data_3, family = binomial)
plot(model3)
summary(model3)
m3 <- ggpredict(model3, terms = c("Predator", "shell"))
plot(m3)


# Model with all data, but dropping unbalanced factor combinations
model4 <- glmer(Settled_30h ~ shell + Conspecific + Predator + shell:Conspecific + Conspecific:Predator + shell:Predator + (1 | age) + (1 | Tray_Number_Well) + (1 | Larvae_batch), data = data_3, family = binomial)
summary(model4)
plot(model4)
m4 <- ggpredict(model4, terms = c("shell","Predator" , "Conspecific"))
plot(m4)


#  Model with most data again, but looking if batch is a determining factor
#model5 <- glmer(Settled_30h ~ Larvae_batch + shell + Conspecific + Predator + (1 | age) + (1 | Tray_Number_Well), data = data_3, family = binomial)
#summary(model5)



###################################################################################################################











#other plots

##  creating df for the plots (individual cues, idk if its a good idea or not but will try)
data_p <- data[data$Cue=="Biofilm",]
 
## need to create 2 new columns, col1 = %settled, col2 = time of settlement (have 10, 20, 30) and maybe col3 = %unsettled
data_p <- cbind(data_p[1:5], stack(data_p[6:10 ]))


####data_p <- cbind(data_p[1:5], stack(data_p(6 : 8), drop(FALSE)


#data_p <- unstack(data_p$ind, select = "Unsettled_10h" )
data_p['Time'] <- data_p['ind'] 
table(data_p$Time)

data_p$Time <- as.character(data_p$Time)
data_p$Time<- as.numeric(gsub("Settled_10h", 10, gsub("Settled_20h", 20, gsub("Settled_30h", 30, data_p$Time))))




#1) 100% stacked bar charts with settlement % (settled/not settled) and time sampled

#2) cumulative proportion of larvae settled over the 30hs (0h, 10h, 20h, 30h)

#3) individual cues and how they induce larvae settlement
##    how do i get the results for the multi-cue experiments? ie: conspecific_predator...?




######################################## test area #####################################################
summary(data)
#tryouts

ggplot(data_p, aes(x= data_p$Time, y= data_p$values)) +geom_point()
ggplot(data = data_p, aes(y = values)) + geom_histogram()
ggplot (data_p, aes (x = Time, fill = Ind)) + geom_bar()



##R by Matty



data_t <- data
#larvae total in each well
data_p ["total"] <- 






new.col <- mean(df$oldcolumn)
cbind)df, new.col)

or

library (tidyverse)
mutate(iris %>%rowwise(),average_length_cols = rowMeans (cbind(Petal.Length, Sepal.Length)))




data_p["total"] <- data_p["values"]
data_p =
data_p$Time <- replace (data_p$Time, "Settled_10h" , 10)



data_p$Time["Settled_10h"] <- 10
data_p$Time <- replace (data_p$Time, Settled_10h , "10")




data_p$Time <- ( assign("Settled_10h", 10))
data_p$Time <- ( assign("Settled_20h", 20))
data_p$Time("Settled_10" := 10 )




data_p$Time["Settled_20h"] <- 20
data_p$Time["Settled_30h"] <- 30

#data_p$Time[, 'Unsettled_10h'] <- 0
#data_p$Time[, 'Unsettled_20h'] <- 0
#data_p$Time[, 'Unsettled_30h'] <- 0
#############################################################################################################






#looking if there was high variation between dates for the control cue (exemple)
install.packages("writexl")
library("writexl")
write_xlsx(data,"C:\\Users\\Owner\\Desktop\\2nd experiment\\databio.xlsx")

write_xlsx(data_p,"C:\\Users\\Owner\\Desktop\\2nd experiment\\bartest.xlsx")



