list.of.packages=c('readstata13','dplyr','lfe','broom','doBy', "SuperLearner", "clusterGeneration", "mvtnorm", "xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)


###############
## DOUBLE ML ##
###############

tables_stlima <- read.dta13("/Users/raphaelhuleux/Documents/Cours/3. Master/2019-2020/2. Second Semester/Machine Learning/machine_learning_project/Data/Finales/tables_stlima.dta")

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
table_2 <- mutate(table_2, cfixeff_key = paste(table_2$codmod,
                                               table_2$grado_r1, table_2$seccion_r1,sep=""))
table_2 <- mutate(table_2, cfixeff=group_indices(table_2,cfixeff_key))
table_2 <- mutate(table_2, cfixeff=as.factor(cfixeff))
table_2 <- mutate(table_2, codmod=as.factor(codmod))

table_2 <- subset(table_2, select = -c(pcuytwothr_r2, pcuwothr_r2))

table_2 %>% summarise_all(funs(sum(is.na(.))))

table_2 <- na.omit(table_2) # We omit the observations with missing variables as they don't work with SL

#Column 3

table_2 %>% summarise_all(funs(sum(is.na(.))))

dependent <- c("pchome_r2", "pcuweek_r2", "pcuyest_r2", "pcuwsch_r2", "pcuwhome_r2", 
               "pcuwcafe_r2", "pcuwfri_r2", "pcuytwsch_r2", "pcuytwhome_r2", 
               "pcuytwcafe_r2", "pcuytwfri_r2", "pcthwork_r2", "pctgame_r2", 
               "pctmusic_r2", "pctvideo_r2", "inthome_r2", "intuweek_r2")

covariates <- c("tcol","male_r2", "age_r2", "sibling_r2", "ysibling_r2",
                "fathlivh_r2", "fathwout_r2","mothwout_r2")

X <- table_2$won_lottery
W <- table_2[, !names(table_2) %in% dependent]
Y <- table_2$pchome_r2

ridge = create.Learner("SL.glmnet", params = list(alpha = 0), name_prefix="ridge")
lasso = create.Learner("SL.glmnet", params = list(alpha = 1), name_prefix="lasso")
en05 = create.Learner("SL.glmnet", params = list(alpha = 0.5), name_prefix="en05")

ridge_results <- k2ml(X = X, W = W, Y = Y, K = 5, SL.library.X = ridge$names, SL.library.Y = ridge$names, family.X = gaussian(), family.Y = gaussian())
en05_results <- k2ml(X = X, W = W, Y = Y, K = 5, SL.library.X = en05$names, SL.library.Y = en05$names, family.X = gaussian(), family.Y = gaussian())
xgboost <- k2ml(X = X, W = W, Y = Y, K = 2, SL.library.X = "SL.xgboost", SL.library.Y = "SL.xgboost", family.X = gaussian(), family.Y = gaussian())

xgboost[1]
