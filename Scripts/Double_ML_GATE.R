list.of.packages=c("purr", "caret", "rpart.plot","devtools",'readstata13','dplyr','lfe','broom','doBy', "SuperLearner", "clusterGeneration", "mvtnorm", "xgboost", "glmnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)

install.packages("githubinstall")
library("githubinstall")

install_github("susanathey/causalTree")
library("causalTree")

#################
### DOUBLE ML ###
#################

df <- read.dta13("/Users/raphaelhuleux/Documents/Cours/3. Master/2019-2020/2. Second Semester/Machine Learning/machine_learning_project/Data/Finales/tables_stlima.dta")

#######################################################
################   Table 2   ##########################
#######################################################

#Keep relevant observations
df <- subset(df, follow_up==1)
df <- subset(df, grado_r1>=3 & grado_r1<=6)
df <- subset(df, !is.na(male_r2) & !is.na(age_r2) & !is.na(sibling_r2) & 
                    !is.na(ysibling_r2) & !is.na(fathlivh_r2) & !is.na(fathwout_r2) &
                    !is.na(mothwout_r2))
df <- subset(df, !is.na(male_r2) & !is.na(age_r2) & 
                    !is.na(sibling_r2) & !is.na(ysibling_r2) & 
                    !is.na(fathlivh_r2) & !is.na(fathwout_r2) &
                    !is.na(mothwout_r2))
df <- subset(df, participated_in_lottery==1)
df <- subset(df, !is.na(pchome_r1) & !is.na(pchome_r2))
df <- mutate(df, tcol= ifelse(participated_in_lottery==1 & won_lottery==1,1,0))
df <- mutate(df, cfixeff_key = paste(df$codmod,
                                               df$grado_r1, df$seccion_r1,sep=""))
df <- mutate(df, cfixeff=group_indices(df,cfixeff_key))
df <- mutate(df, cfixeff=as.factor(cfixeff))
df <- mutate(df, codmod=as.factor(codmod))


df %>% summarise_all(funs(sum(is.na(.))))

df <- subset(df, select = -c(pcuytwothr_r2, pcuwothr_r2, codest, codmod, tcol, cfixeff_key, nombr_es))
# df <- df[,71:157]
df <- na.omit(df) # We omit the observations with missing variables as they don't work with SL

#Column 3

df %>% summarise_all(funs(sum(is.na(.))))

dependent <- c("pchome_r2", "pcuweek_r2", "pcuyest_r2", "pcuwsch_r2", "pcuwhome_r2", 
               "pcuwcafe_r2", "pcuwfri_r2", "pcuytwsch_r2", "pcuytwhome_r2", 
               "pcuytwcafe_r2", "pcuytwfri_r2", "pcthwork_r2", "pctgame_r2", 
               "pctmusic_r2", "pctvideo_r2", "inthome_r2", "intuweek_r2")

covariates <- c("tcol","male_r2", "age_r2", "sibling_r2", "ysibling_r2",
                "fathlivh_r2", "fathwout_r2","mothwout_r2")
ridge = create.Learner("SL.glmnet", params = list(alpha = 0), name_prefix="ridge")
lasso = create.Learner("SL.glmnet", params = list(alpha = 1), name_prefix="lasso")

K = 5
X <- as.numeric(df$won_lottery)
W <- df[, !names(df) %in% dependent]
Y <- df$pchome_r2

for (i in 1:length(dependent)) {
  lasso_results <- k2ml(X = X, W = W, Y = , K = 5, SL.library.X = lasso$names, 
                                                    SL.library.Y = lasso$names, 
                                                    family.X = gaussian(), 
                                                    family.Y = gaussian())
}


#############
### GATES ###
#############

gates <- function(Y, W, X, Q=4, prop_scores=F) {
  
  ### STEP 1: split the dataset into two sets, 1 and 2 (50/50)
  split <- createFolds(1:length(Y), k=2)[[1]]
  
  Ya = Y[split]
  Yb = Y[-split]
  
  Xa = X[split]
  Xb = X[-split]
  
  Wa = W[split, ]
  Wb = W[-split, ]
  
  ### STEP 2a: (Propensity score) On set A, train a model to predict X using W. Predict on set B.
  if (prop_scores==T) {
    sl_w1 = SuperLearner(Y = Xa, 
                         X = Wa, 
                         newX = Wb, 
                         family = binomial(), 
                         SL.library = "SL.xgboost", 
                         cvControl = list(V=0))
    
    p <- sl_w1$SL.predict
  } else {
    p <- rep(mean(Xa), nrow(Wb))
  }
  
  ### STEP 2b let D = W(set B) - propensity score.
  D <- Xb-p
  
  ### STEP 3a: Get CATE (for example using causal forests) on set A. Predict on set B.
  tree_fml <- as.formula(paste("Y", paste(names(Wa), collapse = ' + '), sep = " ~ "))
  cf <- causalForest(tree_fml,
                     data=data.frame(Y=Ya, Wa), 
                     treatment=Xa, 
                     split.Rule="CT", 
                     split.Honest=T,  
                     split.Bucket=T, 
                     bucketNum = 5,
                     bucketMax = 100, 
                     cv.option="CT", 
                     cv.Honest=T, 
                     minsize = 2, 
                     split.alpha = 0.5, 
                     cv.alpha = 0.5,
                     sample.size.total = floor(nrow(Wa) / 2), 
                     sample.size.train.frac = .5,
                     mtry = ceiling(ncol(W1)/3), 
                     nodesize = 5, 
                     num.trees = 10, 
                     ncov_sample = ncol(Wa), 
                     ncolx = ncol(Wa))
  
  cate_cf <- predict(cf, newdata = Wb, type="vector")
  
  ### STEP 3b: divide the cate estimates into Q tiles, and call this object G. 
  # Divide observations into n tiles
  G <- data.frame(cate_cf) %>% # replace cate_cf with the name of your predictions object
    ntile(Q) %>%  # Divide observations into Q-tiles
    factor()
  
  ### STEP 4: Create a dataframe with Y, W (set B), D and G. Regress Y on group membership variables and covariates. 
  df <- data.frame(Y=Yb, Wb, D, G)
  
  Wnames <- paste(colnames(Wb), collapse="+")
  fml <- paste("Y ~",Wnames,"+ D:G")
  model <- lm(fml, df, weights = 1/(p*(1-p))) 
  
  return(model) 
}

table_from_gates <-function(model) {
  thetahat <- model%>% 
    .$coefficients %>%
    .[c("D:G1","D:G2","D:G3","D:G4")]
  
  # Confidence intervals
  cihat <- confint(model)[c("D:G1","D:G2","D:G3","D:G4"),]
  
  res <- tibble(coefficient = c("gamma1","gamma2","gamma3","gamma4"),
                estimates = thetahat,
                ci_lower_90 = cihat[,1],
                ci_upper_90 = cihat[,2])
  
  return(res)
}

output <- rerun(10, table_from_gates(gates(Y, W, X))) %>% # Increase reruns in practice! 
  bind_rows %>%
  group_by(coefficient) %>%
  summarize_all(median)

output

