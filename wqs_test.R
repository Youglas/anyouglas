library(gWQS)
library(ggplot2)
library(epiDisplay)
data = read.csv(file = "~/douglas/R/wqsdata.csv")
data$d.low.muscle <- as.factor(data$d.low.muscle)
data$d.age <- as.numeric(data$d.age)
data$d.eth <- as.factor(data$d.eth)
data$d.sex <- as.factor(data$d.sex)
data$d.marital <- as.factor(data$d.marital)
data$d.edu <- as.factor(data$d.edu)
data$d.smoke <- as.factor(data$d.smoke)
data$d.alcohol.user <- as.factor(data$d.alcohol.user)
data$d.PA_total_MET <- as.numeric(data$d.PA_total_MET)
data_female <- subset(data,data$d.sex=="Female")
data_male <- subset(data,data$d.sex=="Male")
name2 <- names(data_female)[1:9]
name3 <- names(data_male)[1:9]
#female
results2=gwqs(d.low.muscle ~ wqs +d.age+d.eth+d.edu+d.marital+d.BMI_kg.m2+
                d.PIR+d.vitaminD+d.smoke+d.alcohol.user+d.PA_total_MET, 
              mix_name = name3, data = data_female, q = 4, validation = 0.6,b = 1000, 
              b1_pos = TRUE, b1_constr = FALSE, family = "binomial", seed = 2023)
