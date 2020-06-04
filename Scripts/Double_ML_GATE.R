list.of.packages=c("causalTree", "purrr", "caret", "rpart.plot","devtools",'readstata13','dplyr','lfe','broom','doBy', "SuperLearner", "clusterGeneration", "mvtnorm", "xgboost", "glmnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)

df <- read.dta13("/Users/raphaelhuleux/Documents/Cours/3. Master/2019-2020/2. Second Semester/Machine Learning/machine_learning_project/Data/data_table_8.dta")

#######################################################
################   Table 8   ##########################
#######################################################

df <- mutate(df, cfixeff_key = paste(df$codmod,
                                               df$grado_r1, df$seccion_r1,sep=""))
df <- mutate(df, cfixeff=group_indices(df,cfixeff_key))
df <- mutate(df, cfixeff=as.factor(cfixeff))
df <- mutate(df, codmod=as.factor(codmod))

dependent <- c("pchome_r2", "pcuweek_r2", "pcuyest_r2", "stxo_r2", "stpcotskill_r2",
               "stpcsrskill_r2", "straven_r2", "friendh_r2", "efforth_r2", "eduexpt_r2")

covariates <- c("male_r2", "age_r2", "sibling_r2", 
                "ysibling_r2","fathlivh_r2", "fathwout_r2", "mothwout_r2")

df <- subset(df, select = -c(pcuytwothr_r2, pcuwothr_r2, codest, codmod, tcol, cfixeff_key, nombr_es))
df %>% summarise_all(funs(sum(is.na(.))))

df <- na.omit(df) # We omit the observations with missing variables as they don't work with SL

#############
### GATES ###
#############

X <- as.numeric(df$won_lottery)

W <- df[,1:73]
W <- W[, !names(df) %in% dependent]
Y <- df[, names(df) %in% dependent]


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
    p <- rep(mean(Xa), length(Xb))
  }
  
  ### STEP 2b let D = W(set 😎 - propensity score.
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
  df <- data.frame(Y=Yb, Wb, D, G, p)
  
  Wnames <- paste(colnames(Wb), collapse="+")
  fml <- paste("Y ~",Wnames,"+ D:G")
  model <- lm(fml, df, weights = 1/(p*(1-p))) 
  
  return(model) 
}

a <- gates(Y,W,X,Q=4, prop_scores = F)

table_from_gates(a)
#Turn the GATES into a nice table:

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

#Repeat for inference:

output <- list()
for (i in 1:length(dependent)){
  output[[i]] <- rerun(10, table_from_gates(gates(Y[,i], W, X))) %>% # Increase reruns in practice!
    bind_rows %>%
    group_by(coefficient) %>%
    summarize_all(median)
}
output
