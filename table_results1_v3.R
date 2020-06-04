rm(list = ls())

# Installs packages if not already installed, then loads packages 
list.of.packages <- c("glmnet", "rpart.plot", "randomForest", "devtools", "tidyverse", "knitr", "SuperLearner", "caret", "xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))
library(sjlabelled)
library(dummies)
library(base)
library(devtools) 
library(rpart.plot)
library(githubinstall)
library(causalTree)
library(readstata13)
library(dplyr)
library(lfe)
library(doBy)

tables_stlima <- read.dta13("C:/Users/Carlos Leon/Desktop/PSE/Second semester/Machine Learning/final_project/tables_stlima.dta")


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

table_2 <- mutate(table_2, cfixeff_key = paste(table_2$codmod,
                                               table_2$grado_r1, table_2$seccion_r1,sep=""))
table_2 <- mutate(table_2, cfixeff=group_indices(table_2,cfixeff_key))
table_2 <- mutate(table_2, cfixeff=as.factor(cfixeff))
table_2 <- mutate(table_2, codmod=as.factor(codmod))

#Column 3

dependent <- c("pchome", "pcuweek", "pcuyest", "pcuwsch", "pcuwhome", 
               "pcuwcafe", "pcuwfri", "pcuytwsch", "pcuytwhome", 
               "pcuytwcafe", "pcuytwfri", "pcthwork", "pctgame", 
               "pctmusic", "pctvideo", "inthome", "intuweek")

covariates <- c("tcol","male_r2", "age_r2", "sibling_r2", "ysibling_r2",
                "fathlivh_r2", "fathwout_r2","mothwout_r2")


Table2_C3 <- matrix(NA, nrow = length(dependent), ncol = 3)

for (i in 1:length(dependent)){
  
  aux <- as.formula(paste(paste(paste(dependent[i],"r2",sep="_"), 
                                paste(c(paste(dependent[i],"r1",sep="_"),covariates), collapse = ' + '), 
                                sep = " ~ "),"cfixeff",0,"codmod",sep = " | "))
  
  aux_reg <- felm(aux, table_2)
  Table2_C3[i,1] <- dependent[i]
  Table2_C3[i,2] <- aux_reg[["coefficients"]][2]
  Table2_C3[i,3] <- aux_reg[["N"]]
}


colnames(Table2_C3) <- c("Dep.var.", "Adj. Diff.", "N")


print(Table2_C3)


####################################################### 
################   Table 8   ##########################
#######################################################
table_8 <- read.dta13("C:/Users/Carlos Leon/Desktop/PSE/Second semester/Machine Learning/final_project/One_laptopfiles/data/data_table_8.dta")
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

Table8 <- matrix(NA, nrow = length(dependent)*2, ncol = length(Hetero_eff)*4)

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
        Table8[i,4*j-3] <- paste(dependent[i], "coef", sep="_")
        Table8[i+1,4*j-3] <- paste(dependent[i], "se", sep="_")
        Table8[i,4*j-2] <- aux_reg[["coefficients"]][2]
        Table8[i+1,4*j-2] <- aux_reg[["se"]][2]
        Table8[i,4*j-1] <- aux_reg[["coefficients"]][3]
        Table8[i+1,4*j-1] <- aux_reg[["se"]][3]
        Table8[i,4*j] <- aux_reg[["N"]]
      }
      if (dependent[i]=="stxo"){
        Table8[i,4*j-3] <- paste(dependent[i], "coef", sep="_")
        Table8[i+1,4*j-3] <- paste(dependent[i], "se", sep="_")
        Table8[i,4*j-2] <- aux_reg[["coefficients"]][1]
        Table8[i+1,4*j-2] <- aux_reg[["se"]][1]
        Table8[i,4*j-1] <- aux_reg[["coefficients"]][2]
        Table8_C3[i+1,4*j-1] <- aux_reg[["se"]][2]
        Table8[i,4*j] <- aux_reg[["N"]]
      }
    }
    if (i>1){
      if (dependent[i]!="stxo"){
        Table8[2*i-1,4*j-3] <- paste(dependent[i], "coef", sep="_")
        Table8[2*i,4*j-3] <- paste(dependent[i], "se", sep="_")
        Table8[2*i-1,4*j-2] <- aux_reg[["coefficients"]][2]
        Table8[2*i,4*j-2] <- aux_reg[["se"]][2]
        Table8[2*i-1,4*j-1] <- aux_reg[["coefficients"]][3]
        Table8[2*i,4*j-1] <- aux_reg[["se"]][3]
        Table8[2*i-1,4*j] <- aux_reg[["N"]]
      }
      if (dependent[i]=="stxo"){
        Table8[2*i-1,4*j-3] <- paste(dependent[i], "coef", sep="_")
        Table8[2*i,4*j-3] <- paste(dependent[i], "se", sep="_")
        Table8[2*i-1,4*j-2] <- aux_reg[["coefficients"]][1]
        Table8[2*i,4*j-2] <- aux_reg[["se"]][1]
        Table8[2*i-1,4*j-1] <- aux_reg[["coefficients"]][2]
        Table8[2*i,4*j-1] <- aux_reg[["se"]][2]
        Table8[2*i-1,4*j] <- aux_reg[["N"]]
      }
    }
    
  }
}

colnames(Table8) <- c("Dep.var.", "Males", "Females", "N",
                         "Dep.var.", "3-4 grade", "5-6 grade", "N",
                         "Dep.var.", "PC Home", "No PC Home", "N",
                         "Dep.var.", "Int. Home", "No Int. Home", "N",
                         "Dep.var.", "Math Below", "Math Above", "N",
                         "Dep.var.", "Read Below", "Read Above", "N",
                         "Dep.var.", "Academ. Below", "Academ. Above", "N")

print(Table8)

####################################################### 
################   Causal Trees   #####################
#######################################################
CT_dat <- read.dta13("C:/Users/Carlos Leon/Desktop/PSE/Second semester/Machine Learning/final_project/One_laptopfiles/data/data_table_8.dta")

dependent1 <- c("stxo", "stpcotskill", "stpcsrskill", "straven")

for (i in 1:length(dependent1)){
set.seed(1234)
folds = createFolds(1:nrow(CT_dat), k=2)
y <- CT_dat[, names(CT_dat) %in% paste(dependent1[i],"r2",sep = "_")]
x <- CT_dat[, names(CT_dat) %in% "tcol"]
w <- CT_dat[, names(CT_dat) %in% 
              grep("r1",names(CT_dat),fixed = TRUE, value = TRUE)]
#additional variables
w$g34  <- CT_dat$g34
w$male <- ifelse(CT_dat$female!=1,1,0)


Y1 <- y[folds[[1]]]
Y2 <- y[folds[[2]]]

W1 <- w[folds[[1]],]
W2 <- w[folds[[2]],]

X1 <- x[folds[[1]]]
X2 <- x[folds[[2]]]

if (dependent1[i]!="stxo"){
CT_fml <- as.formula(paste(paste("Y", 
           paste(c(paste(dependent1[i],"r1",sep="_"),names(w)),
           collapse = ' + '), sep = " ~ "),sep = ""))
}

if (dependent1[i]=="stxo"){
  CT_fml <- as.formula(paste(paste("Y", 
           paste(names(w),
           collapse = ' + '), sep = " ~ "),sep = ""))
}

honest_tree <- honest.causalTree(formula = CT_fml,
                                 data = data.frame(Y=Y1,W1),
                                 treatment = X1,
                                 est_data = data.frame(Y=Y2,W2),
                                 est_treatment = X2,
                                 split.alpha = 0.5,
                                 split.Rule = "CT",
                                 split.Honest = T,
                                 cv.alpha = 0.5,
                                 cv.option = "CT",
                                 cv.Honest = T,
                                 split.Bucket = T,
                                 bucketNum = 5,
                                 bucketMax = 100, # maximum number of buckets
                                 minsize = 25) # number of observations in treatment and control on leaf

                                 
opcpid <- which.min(honest_tree$cp[, 4]) 
opcp <- honest_tree$cp[opcpid, 1]
honest_tree_prune <- prune(honest_tree, cp = opcp)
rpart.plot(honest_tree_prune, roundint = F)

leaf2 <- as.factor(round(predict(honest_tree_prune,
                                 newdata = data.frame(Y=Y2, W2),
                                 type = "vector"), 4))

# OLS treatment effect and SE
honest_ols_2 <- lm( Y ~ leaf + X * leaf - X -1, data = data.frame(Y=Y2, X=X2, leaf=leaf2, W2))
summary(honest_ols_2) 

#CATE HT
cate_honesttree <- predict(honest_tree_prune, newdata = data.frame(Y=Y2, W2), type = "vector")



####################################################### 
################   Causal Forest  #####################
#######################################################

causalforest <- causalForest(CT_fml,
                             data=data.frame(Y=Y1, W1), 
                             treatment=X1, 
                             split.Rule="CT", 
                             split.Honest=T,  
                             split.Bucket=T, 
                             bucketNum = 5,
                             bucketMax = 100, 
                             cv.option="CT", 
                             cv.Honest=T, 
                             minsize = 25, 
                             split.alpha = 0.5, 
                             cv.alpha = 0.5,
                             sample.size.total = floor(nrow(W1) / 2), 
                             sample.size.train.frac = .5,
                             mtry = ceiling(ncol(W1)/3),
                             nodesize = 5, 
                             num.trees = 10, 
                             ncov_sample = ncol(W1), 
                             ncolx = ncol(W1))

#CATE 
cate_causalforest <- predict(causalforest, newdata=data.frame(Y=Y2, W2), type="vector")

#####################################################################
########################   Graphs   #################################
#####################################################################

##############  Histograms Causal Trees / Forest  ###################
#####################################################################

#Plot in R
par(mfrow = c(1, 2))
hist(cate_honesttree, main = paste("HCT",dependent1[i],sep = " - "), 
     col="black")
hist(cate_causalforest, main = paste("HCF",dependent1[i],sep = " - "),
     col="blue")

#Save the plot in working directory

png(paste(dependent1[i],"CTCF.png",sep = "_"))
par(mfrow = c(1, 2))
hist(cate_honesttree, main = paste("HCT",dependent1[i],sep = " - "), 
     col="black")
hist(cate_causalforest, main = paste("HCF",dependent1[i],sep = " - "),
     col="blue")
dev.off()

#Reset Plot conf.
par(mfrow = c(1, 1))


##################  CATE Quantile Causal Forest   ###################
#####################################################################

cf_quintile <- W2 %>%
  mutate(cate = cate_causalforest) %>%
  mutate(cate_quintile = ntile(cate, n=5))

m <- cf_quintile %>% 
  group_by(cate_quintile) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  gather(key="variable", value = "value", -cate_quintile)

s <- cf_quintile %>% 
  group_by(cate_quintile) %>% 
  summarize_all(~sqrt(var(.,na.rm = TRUE)/length(.)), na.rm = TRUE) %>% 
  gather(key="variable", value = "value", -cate_quintile)

##### Variables reported in the paper
########################################

m <- as.data.frame(m)
var_paper  <- c("male","g34","pchome_r1", "bmavmr_r1", "cate")

m_paper <- m[m$variable %in% var_paper ,]
s_paper <- s[s$variable %in% var_paper ,]
limits_paper <- aes(ymax = m_paper$value+2*s_paper$value, 
                    ymin = m_paper$value-2*s_paper$value)

m_paper %>% ggplot(aes(x=factor(cate_quintile), y=value)) +
  facet_wrap(~variable, nrow = 3) + 
  geom_bar(aes(fill=factor(cate_quintile)), stat="identity") +
  geom_errorbar(limits_paper, width=.1) + 
  ggtitle(paste("Covariates across predicted treatment",dependent1[i],sep=" - ")) 

ggsave(paste(dependent1[i],"report_paper.png",sep="_"), device="png",  plot = last_plot(),
       path = "./")

##### Variables not reported in the paper
#########################################

m_notpaper <- m[!m$variable %in% var_paper ,]
s_notpaper <- s[!s$variable %in% var_paper ,]
limits_notpaper <- aes(ymax = m_notpaper$value+2*s_notpaper$value, 
                    ymin = m_notpaper$value-2*s_notpaper$value)

for (j in 1:7){
  m_aux <- m_notpaper[(j*60-59):(j*60),]
  
  m_aux %>% ggplot(aes(x=factor(cate_quintile), y=value)) +
  facet_wrap(~variable, nrow = 4) + 
  geom_bar(aes(fill=factor(cate_quintile)), stat="identity") +
  ggtitle(paste("Covariates across predicted treatment",dependent1[i],sep=" - "))

ggsave(paste(dependent1[i],j,"not_report_paper.png",sep="_"), device="png",  plot = last_plot(),
      path = "./")
}

par(mfrow = c(1, 1))


####################################################### 
################      GATES       #####################
#######################################################



}


 