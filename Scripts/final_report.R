rm(list = ls())
list.of.packages=c('readstata13','dplyr','lfe','broom','doBy')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

lapply(list.of.packages, require, character.only = TRUE)


#library(readstata13)
#library(dplyr)
#library(lfe)
#library(doBy)
#setwd("C:/Users/Carlos Leon/Desktop/PSE/Second semester/Machine Learning/final_project")
tables_stlima <- read.dta13("data/Finales/tables_stlima.dta")

#######################################################
################   Table 2   ##########################
#######################################################

#Keep relevant observations
table_2 <- subset(tables_stlima, follow_up==1)
table_2 <- subset(table_2, grado_r1>=3 & grado_r1<=6)
table_2 <- subset(table_2, !is.na(male_r2) & !is.na(age_r2) & !is.na(sibling_r2) & 
                    !is.na(ysibling_r2) & !is.na(fathlivh_r2) & !is.na(fathwout_r2) &
                    !is.na(mothwout_r2))

table_2 <- subset(table_2, !is.na(male_r2) & !is.na(age_r2) & 
                    !is.na(sibling_r2) & !is.na(ysibling_r2) & 
                    !is.na(fathlivh_r2) & !is.na(fathwout_r2) &
                    !is.na(mothwout_r2))

table_2 <- subset(table_2, participated_in_lottery==1)
table_2 <- subset(table_2, !is.na(pchome_r1) & !is.na(pchome_r2))


table_2 <- mutate(table_2, tcol= ifelse(participated_in_lottery==1 & won_lottery==1,1,0))


##############################
##############################
# This line creates class fixed effects key
# Codigo modular : random number identifying a school in Peru
# Grado : school level eg 6e, 5e...
# Seccion : class in a certain level eg A,B,C

table_2 <- mutate(table_2, cfixeff_key = paste(table_2$codmod,
                                               table_2$grado_r1, table_2$seccion_r1,sep=""))
# This creates an index instead of the pasted number for readability
table_2 <- mutate(table_2, cfixeff=group_indices(table_2,cfixeff_key))
# Creates an index for the fixed effect and then creates a factor variable. 
table_2 <- mutate(table_2, cfixeff=as.factor(cfixeff))
table_2 <- mutate(table_2, codmod=as.factor(codmod))

#Column 3

dependent <- c("pchome", "pcuweek", "pcuyest", "pcuwsch", "pcuwhome", 
               "pcuwcafe", "pcuwfri", "pcuytwsch", "pcuytwhome", 
               "pcuytwcafe", "pcuytwfri", "pcthwork", "pctgame", 
               "pctmusic", "pctvideo", "inthome", "intuweek")

covariates <- c("tcol","male_r2", "age_r2", "sibling_r2", "ysibling_r2",
                "fathlivh_r2", "fathwout_r2","mothwout_r2")


Table2_C3 <- matrix(NA, nrow = length(dependent), ncol = 4)

for (i in 1:length(dependent)){
  
  aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                paste(c(paste(dependent[i],"r1",sep="_"),covariates), collapse = ' + '), 
                                sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  
  aux_reg <- felm(aux, table_2)
  Table2_C3[i,1] <- dependent[i]
  Table2_C3[i,2] <- aux_reg[["coefficients"]][2]
  Table2_C3[i,3] <- coef(summary(aux_reg))[1,2]
  Table2_C3[i,4] <- aux_reg[["N"]]
}

print(Table2_C3)



#######################################################
################   Table 6   ##########################
#######################################################

#Keep relevant observations
table_6 <- subset(tables_stlima, follow_up==1)
table_6 <- subset(table_6, grado_r1>=3 & grado_r1<=6)
table_6 <- subset(table_6, !is.na(male_r2) & !is.na(age_r2) & !is.na(sibling_r2) & 
                    !is.na(ysibling_r2) & !is.na(fathlivh_r2) & !is.na(fathwout_r2) &
                    !is.na(mothwout_r2) & !is.na(wfrndtotplott_r2))

table_6 <- subset(table_6, participated_in_lottery==1 & won_lottery==1)
table_6 <- subset(table_6, !is.na(pchome_r1) & !is.na(pchome_r2))

table_6 <- mutate(table_6, winwfnd=ifelse(
  (participated_in_lottery==1 & won_lottery==1) & 
    (wfrndtotwlott_r1>=1 & !is.na(wfrndtotwlott_r1)),1,0))

#Generate standardized scores including stxo
table_6 <- mutate(table_6, stpcotskill_r1=(pcotskill_r1-mean(pcotskill_r1))/sd(pcotskill_r1))
table_6 <- mutate(table_6, stpcsrskill_r1=(pcsrskill_r1-mean(pcsrskill_r1))/sd(pcsrskill_r1))
table_6 <- mutate(table_6, straven_r1=(raven_r1-mean(raven_r1,na.rm=T))/sd(raven_r1, na.rm=T))
table_6 <- mutate(table_6, stpcotskill_r2=(pcotskill_r2-mean(pcotskill_r2))/sd(pcotskill_r2))
table_6 <- mutate(table_6, stpcsrskill_r2=(pcsrskill_r2-mean(pcsrskill_r2))/sd(pcsrskill_r2))
table_6 <- mutate(table_6, straven_r2=(raven_r2-mean(raven_r2,na.rm=T))/sd(raven_r2,na.rm=T))
table_6 <- mutate(table_6, stxo_r2=(xo_r2-mean(xo_r2,na.rm=T))/sd(xo_r2,na.rm=T))


##############################
##############################

table_6 <- mutate(table_6, cfixeff_key = paste(table_6$codmod,
                                               table_6$grado_r1, table_6$seccion_r1,sep=""))
table_6 <- mutate(table_6, cfixeff=group_indices(table_6,cfixeff_key))
table_6 <- mutate(table_6, cfixeff=as.factor(cfixeff))
table_6 <- mutate(table_6, codmod=as.factor(codmod))



#Column 3

dependent <- c("pchome", "pcuweek", "pcuyest", "stpcotskill","stxo","straven",
               "stpcsrskill", "friendh", "efforth", "eduexpt")

covariates <- c("winwfnd","wfrndtotplott_r1", "male_r2", "age_r2", 
                "sibling_r2", "ysibling_r2","fathlivh_r2", "fathwout_r2",
                "mothwout_r2")

Table6_C3 <- matrix(NA, nrow = length(dependent), ncol = 4)

for (i in 1:length(dependent)){

  if (dependent[i]!="stxo"){
    aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                  paste(c(paste(dependent[i],"r1",sep="_"),covariates), collapse = ' + '), 
                                  sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  }
  if (dependent[i]=="stxo"){
    aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                  paste(covariates, collapse = ' + '), 
                                  sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  }
  

aux_reg <- felm(aux, table_6)
Table6_C3[i,1] <- dependent[i]
Table6_C3[i,2] <- aux_reg[["coefficients"]][2]
Table6_C3[i,3] <- coef(summary(aux_reg))[1,2]
Table6_C3[i,4] <- aux_reg[["N"]]
}

# note: remember that variables related to test were not available for the analysis
# specifically: Objective OLPC test (Digital skills), and Raven's progressive matrices (Cognitive skills)

print(Table6_C3)

#######################################################
################   Table 7   ##########################
#######################################################

table_7 <- subset(tables_stlima, follow_up==1)
table_7 <- subset(table_7, grado_r1>=3 & grado_r1<=6)
table_7 <- subset(table_7, !is.na(pchome_r1) & !is.na(pchome_r2))

#Generate standardized scores
table_7 <- mutate(table_7, stpcotskill_r1=(pcotskill_r1-mean(pcotskill_r1))/sd(pcotskill_r1))
table_7 <- mutate(table_7, stpcsrskill_r1=(pcsrskill_r1-mean(pcsrskill_r1))/sd(pcsrskill_r1))
table_7 <- mutate(table_7, straven_r1=(raven_r1-mean(raven_r1,na.rm=T))/sd(raven_r1,na.rm=T))
table_7 <- mutate(table_7, stpcotskill_r2=(pcotskill_r2-mean(pcotskill_r2))/sd(pcotskill_r2))
table_7 <- mutate(table_7, stpcsrskill_r2=(pcsrskill_r2-mean(pcsrskill_r2))/sd(pcsrskill_r2))
table_7 <- mutate(table_7, straven_r2=(raven_r2-mean(raven_r2,na.rm=T))/sd(raven_r2,na.rm=T))
table_7 <- mutate(table_7, stxo_r2=(xo_r2-mean(xo_r2, na.rm=T))/sd(xo_r2, na.rm=T))


#Generate weights for spillovers on classmates

dep_t7 <- c("pchome", "pcuweek", "pcuyest", "stpcotskill", "stpcsrskill", "straven", "friendh",
            "efforth", "eduexpt")

table_7 <- mutate(table_7, class_key = paste(table_7$codmod,
                                               table_7$grado_r1, table_7$seccion_r1,sep=""))
table_7 <- mutate(table_7, class=group_indices(table_7,class_key))
table_7 <- mutate(table_7, factor_pchome=ifelse(participated_in_lottery==0,1,0))
table_7 <- table_7 %>% group_by(class) %>% mutate(part_pchome = sum(participated_in_lottery)) #drop
table_7 <- table_7 %>% group_by(class) %>% mutate(obt_pchome = sum(won_lottery)) #drop
table_7 <- mutate(table_7, no_obt_pchome = part_pchome - obt_pchome) #drop
table_7 <- mutate(table_7, lost_pchome = part_pchome/no_obt_pchome) #drop
table_7 <- mutate(table_7, factor_pchome=ifelse(participated_in_lottery==1,lost_pchome,0))


#NOTE: variable "round" not available - cannot reproduce the table 7

# Table 7 uses an index variable: post = 1(round==2) 
# I didn't find the variable round, which determines the kids that were interview at the post-treatment period
# Although a variable named (reg_`x') is constructed to determine the response in each period for the different dependent variables
# I guess we cannot assume to infer the round variable based on the reg_`x` because non-answered questions, doesn't imply attrition, or non-compliance in the post treatment period.


