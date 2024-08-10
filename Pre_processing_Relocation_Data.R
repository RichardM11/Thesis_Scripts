################ DATA PROCESSING SCRIPT ###################
### BELO HORIZONTE MUNICIPAL DATA ON PLANNED RELOCATION ###
########## AUTHOR: RICHARD MOREIRA ########################

# Cleaning environment
rm(list=ls(all=TRUE))

# Setting directory
getwd()
setwd("C:/Users/rassi/OneDrive/Documentos/Project_PBH_PlannedRelocation")

# Libraries 
library(readxl)
library(xlsx)
library(dplyr)
library(dvmisc)
library(tidyverse)


# Entering data
relocation <- read_excel("relocation_pbh_2010_2020.xlsx", sheet = 1)

# Replacing string "NULL" and "NA" for Missing Values
relocation[relocation=="NULL"] <- NA
relocation[relocation=="NA"] <- NA

# Checking NA responses in some key variables
sum(is.na(relocation$"Empreendimento"))
sum(is.na(relocation$"N? do Selo"))
sum(is.na(relocation$"Regime de Ocupa??o da Benfeitoria"))
sum(is.na(relocation$"Uso do Domic?lio"))
sum(is.na(relocation$"N? do Morador"))
sum(is.na(relocation$"SIF"))
sum(is.na(relocation$"Estado Civil"))
sum(is.na(relocation$"Regional"))
sum(is.na(relocation$"Remocao"))
colSums(is.na(relocation))

# Subsetting database
data <- relocation[,-c(1:4,7:9,11:14,17:22,27,30:33,36,38,39,41:45)]

# Plotting a Data Map for Missing Data
install.packages("Amelia")
library(Amelia)
missmap(data, main = "Missing values vs observed")
sapply(data,function(x) sum(is.na(x)))

## Dealing with NAs exclusively for the logistic model
## Variable name in English (Variable name in the Prefecture Registers)

# Age (Idade): 2637 NAs - Replace NAs with Mean Age
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)
str(data)

## Checking Proportions

# Color/Race (Cor):
# 6 doesn't mean anything, so turn it into NA
prop.table(table(data$Cor))

# Other incomes (EOR):
prop.table(table(data$`Especifica??o de Outras rendas`))

# Education(GI):
prop.table(table(data$`Grau de Instru??o`))

# Marital Status (Estado Civil):
prop.table(table(data$`Estado Civil`))

# Situation of Work (SO):
prop.table(table(data$`Situa??o Ocupacional`))

# Work Contract (VE): take 0 out
prop.table(table(data$`V?nculo Empregat?cio`))

# Gender (Sexo):
prop.table(table(data$Sexo))

# Regional (Regional):
prop.table(table(data$Regional))

# Relocation (Remocao):
prop.table(table(data$Remocao))

## Renaming variables
names(data)[names(data) == "Cor"] <- "Color_Race"
names(data)[names(data) == "Especifica??o de Outras rendas"] <- "OIncomes"
names(data)[names(data) == "Grau de Instru??o"] <- "Education"
names(data)[names(data) == "Estado Civil"] <- "Marital_Status"
names(data)[names(data) == "Situa??o Ocupacional"] <- "Situation_ofWork"
names(data)[names(data) == "V?nculo Empregat?cio"] <- "Work_Contract"
names(data)[names(data) == "Sexo"] <- "Gender"
names(data)[names(data) == "Remocao"] <- "Relocation"


## Generating a new data frame
write.xlsx(as.data.frame(data), file = "Processed_relocation_pbh_2010_2020.xlsx", col.names = TRUE)

# 