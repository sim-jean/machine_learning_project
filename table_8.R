rm(list = ls())
library(readstata13)
library(dplyr)
library(lfe)
library(doBy)
setwd("C:/Users/Carlos Leon/Desktop/PSE/Second semester/Machine Learning/final_project")
table_8 <- read.dta13("./One_laptopfiles/data/data_table_8.dta")


####################################################### 
################   Table 8   ##########################
#######################################################

table_8 <- mutate(table_8, cfixeff_key = paste(table_8$codmod,
                                               table_8$grado_r1, table_8$seccion_r1,sep=""))
table_8 <- mutate(table_8, cfixeff=group_indices(table_8,cfixeff_key))
table_8 <- mutate(table_8, cfixeff=as.factor(cfixeff))
table_8 <- mutate(table_8, codmod=as.factor(codmod))

###### Table

dependent <- c("pchome", "pcuweek", "pcuyest", "stxo", "stpcotskill",
               "stpcsrskill", "straven", "friendh", "efforth", "eduexpt")

covariates <- c("male_r2", "age_r2", "sibling_r2", 
                "ysibling_r2","fathlivh_r2", "fathwout_r2", "mothwout_r2")

h1 <- c("t_male_r2", "t_female")
h2 <- c("t_g34", "t_g56")
h3 <- c("t_pchome_r1", "t_npc")
h4 <- c("t_inthome_r1", "t_nint")
h5 <- c("t_bmmath_r1", "t_ammath_r1")
h6 <- c("t_bmread_r1", "t_amread_r1")
h7 <- c("t_bmavmr_r1", "t_amavmr_r1")
Hetero_eff <- c("h1", "h2", "h3", "h4","h5","h6", "h7")

Table8_C3 <- matrix(NA, nrow = length(dependent)*2, ncol = length(Hetero_eff)*4)

for (j in 1:length(Hetero_eff)){
for (i in 1:length(dependent)){
    if (dependent[i]!="stxo"){
    aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                  paste(c(paste(dependent[i],"r1",sep="_"),get(Hetero_eff[j]),covariates),
                                        collapse = ' + '), sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  }
  if (dependent[i]=="stxo"){
    aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                  paste(c(get(Hetero_eff[j]),covariates), collapse = ' + '), 
                                  sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  }

  aux_reg <- felm(aux, table_8)
  if (i==1){
  if (dependent[i]!="stxo"){
  Table8_C3[i,4*j-3] <- paste(dependent[i], "coef", sep="_")
  Table8_C3[i+1,4*j-3] <- paste(dependent[i], "se", sep="_")
  Table8_C3[i,4*j-2] <- aux_reg[["coefficients"]][2]
  Table8_C3[i+1,4*j-2] <- aux_reg[["se"]][2]
  Table8_C3[i,4*j-1] <- aux_reg[["coefficients"]][3]
  Table8_C3[i+1,4*j-1] <- aux_reg[["se"]][3]
  Table8_C3[i,4*j] <- aux_reg[["N"]]
  }
  if (dependent[i]=="stxo"){
  Table8_C3[i,4*j-3] <- paste(dependent[i], "coef", sep="_")
  Table8_C3[i+1,4*j-3] <- paste(dependent[i], "se", sep="_")
  Table8_C3[i,4*j-2] <- aux_reg[["coefficients"]][1]
  Table8_C3[i+1,4*j-2] <- aux_reg[["se"]][1]
  Table8_C3[i,4*j-1] <- aux_reg[["coefficients"]][2]
  Table8_C3[i+1,4*j-1] <- aux_reg[["se"]][2]
  Table8_C3[i,4*j] <- aux_reg[["N"]]
  }
  }
  if (i>1){
  if (dependent[i]!="stxo"){
  Table8_C3[2*i-1,4*j-3] <- paste(dependent[i], "coef", sep="_")
  Table8_C3[2*i,4*j-3] <- paste(dependent[i], "se", sep="_")
  Table8_C3[2*i-1,4*j-2] <- aux_reg[["coefficients"]][2]
  Table8_C3[2*i,4*j-2] <- aux_reg[["se"]][2]
  Table8_C3[2*i-1,4*j-1] <- aux_reg[["coefficients"]][3]
  Table8_C3[2*i,4*j-1] <- aux_reg[["se"]][3]
  Table8_C3[2*i-1,4*j] <- aux_reg[["N"]]
  }
  if (dependent[i]=="stxo"){
  Table8_C3[2*i-1,4*j-3] <- paste(dependent[i], "coef", sep="_")
  Table8_C3[2*i,4*j-3] <- paste(dependent[i], "se", sep="_")
  Table8_C3[2*i-1,4*j-2] <- aux_reg[["coefficients"]][1]
  Table8_C3[2*i,4*j-2] <- aux_reg[["se"]][1]
  Table8_C3[2*i-1,4*j-1] <- aux_reg[["coefficients"]][2]
  Table8_C3[2*i,4*j-1] <- aux_reg[["se"]][2]
  Table8_C3[2*i-1,4*j] <- aux_reg[["N"]]
  }
}
  
}
}

colnames(Table8_C3) <- c("Dep.var.", "Males", "Females", "N",
                         "Dep.var.", "3-4 grade", "5-6 grade", "N",
                         "Dep.var.", "PC Home", "No PC Home", "N",
                         "Dep.var.", "Int. Home", "No Int. Home", "N",
                         "Dep.var.", "Math Below", "Math Above", "N",
                         "Dep.var.", "Read Below", "Read Above", "N",
                         "Dep.var.", "Academ. Below", "Academ. Above", "N")

