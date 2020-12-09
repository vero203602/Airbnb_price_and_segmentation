##############################################################################
############################# PRICE PREDICTION ###############################
##############################################################################

library(ggplot2)
library(gridExtra)
library(corrplot)
library(plyr)      # mapvalues
library(dplyr)
library(MLmetrics) # RMSE
library(rpart)
library(rpart.plot)
library(rgr)       # where.na
library(randomForest)
library(ranger)    # faster implementation of RF
library(gbm) 
library(pdp)       # for dependency plots
library(caret)     # dummyVars for one hot encoding
library(xgboost)   # faster implementation of GB
library(glmnet)    # lasso shrinkage

setwd('/Users/veronicacipriani/Desktop/tesi/dati/listing_2019/listing_complete/price_pred')
jan <- read.csv('january_listing.csv',  sep=',', stringsAsFactors = TRUE)[, -c(1)]

##############################################################################
# DATS PRE-PROCESSING --> spostare in listing_rev_gennaio
# First of all, since .csv do not read factor levels, i need to recode them.
df_month %>%
  select_if(is.factor)%>%
  head(0)

df_month$host_response_time <- factor(df_month$host_response_time,
                                      levels=c("within an hour",
                                               "within a few hours",
                                               "within a day",
                                               "a few days or more",
                                               "no info"),
                                      ordered = TRUE)
df_month$cancellation_policy <- factor(df_month$cancellation_policy,
                                       levels=c('flexible_new',
                                                'moderate_new',
                                                'strict_new',
                                                'super_strict_30_new',
                                                'super_strict_60_new'),
                                       ordered = TRUE)
df_month$bed_type <- factor(df_month$bed_type,
                            levels=c("Real Bed",
                                     "Pull-out Sofa",
                                     "Futon",
                                     "Couch",
                                     "Airbed"))
df_month$room_type <- factor(df_month$room_type,
                             levels=c("Shared room",
                                      "Private room",
                                      "Entire home/apt",
                                      "Hotel room"
                             ))
df_month$month <- factor(df_month$month,
                         levels=month.abb)
df_month$neighbourhood <- as.factor(df_month$neighbourhood)

# correlation
subset_num <- df_month[, -1] %>%
  select_if(is.numeric)

corrmat <- cor(subset_num)
round(corrmat, 2)
corrplot(corrmat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#hist(df_month$review_scores_rating)
range(df_month$review_scores_rating)

df_month <- subset(df_month, select = -c(accommodates, # guests meaning
                                         # people included
                                         beds,
                                         review_scores_accuracy,
                                         review_scores_checkin,
                                         review_scores_cleanliness,
                                         review_scores_communication,
                                         review_scores_location,
                                         review_scores_value,
                                         availability_60, 
                                         availability_90,
                                         availability_365,
                                         days_first_review,
                                         occupancyrate_list,
                                         CBD
))

subset_num <- df_month[, -1] %>%
  select_if(is.numeric)
corrmat <- cor(subset_num)
round(corrmat, 2)
corrplot(corrmat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
df_month <- subset(df_month, select = -c(diff_from_avg))
# posso anche togliere availability_30
# sono corr tra loro a -1 circa

subset_num <- df_month[, -1] %>%
  select_if(is.numeric)
corrmat <- cor(subset_num)
round(corrmat, 2)
corrplot(corrmat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# subset di df_month per provare codice 
#set.seed(11)
#idx      <- sample(1:nrow(df_month),
#                  .1*nrow(df_month)) 
#df_month <- df_month[ idx, ]

# price per month:
df_month %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg = mean(price),
                   sd  = sd(price))

# split the dataset in train/test 50-50, given the high dimensionality
set.seed(11)
index_train <- sample( 1:nrow(df_month),
                       .5*nrow(df_month)) 
trainset    <- df_month[ index_train, -1] # Create the training data 
testset     <- df_month[-index_train, -1] # Create the test data

# check for missing NA
where.na(trainset)
where.na(testset)

# first some considerations: what would be the error if i'd use the expected
# value?
mean(df_month$price)
sd(df_month$price)

avg_p    <- rep(mean(df_month$price), nrow(df_month))
mean_err <- round(RMSE(avg_p, df_month$price), 3)

# clean environment
rm(col, corrmat, property_type, prova1_out, subset_bat, subset_bedapt,
   subset_bedpri, subset_num, subset_prova1, subset_prova1bat,
   subset2)

##############################################################################
############################ LINEAR REGRESSION ###############################
# As the price model of the company takes its root in hedonic pricing model,
# the first step is to preform a linear regression.
# I assusme that the relationship between the price and the rest of predictors 
# is somehow linear (I will test this assumption with the diagnostic plots).
lm_total <- lm(log(price) ~., data = trainset)
summary(lm_total)

par(mfrow = c(2, 2))
plot(lm_total)
par(mfrow = c(1, 1))

# how to decide on important variables?
# BACKWARD SELECTION
# names(jan_lm)
lm_1 <- lm(
  log(price) ~ 
    host_response_time + host_response_rate + 
    host_total_listings_count+ property_type + room_type + 
    bathrooms + bedrooms + bed_type + security_deposit + 
    guests_included + availability_30 + review_scores_rating +
    instant_bookable + cancellation_policy + 
    month + days_host_since + days_last_review  + 
    number_amenities + amen_wifi + amen_kitchen + amen_hair_dryer + 
    amen_free_parking + amen_bedlinens + amen_pets + 
    amen_smoke + number_pois + avg_distance,
  data = trainset
)
summary(lm_1)
par(mfrow = c(2, 2))
plot(lm_1)           # normal Q-Q: clear violation of the 
#             normality of errors
par(mfrow = c(1, 1))
# check for autocorr of errors
res      <- lm_1$res 
n        <- length(res) 
acdetect <- lm(res[-n] ~ res[-1]) 
summary(acdetect) # non significant slope

pred <- exp((predict(lm_1, newdata = testset))) 
plot(pred, testset$price) # some severe overpriced
abline(0, 1)  
text(pred, testset$price, labels = testset$id)

RMSE(pred, testset$price)

#predictions <- as.data.frame(cbind(testset$id,
#                                   testset$price,
#                                   pred))
#underdf    <- predictions[predictions$V1 > predictions$pred+100,]
#overdf     <- predictions[predictions$V1 < predictions$pred-100,]
#check_over <- df_month[rownames(over)  ,]
#check_und  <- df_month[rownames(under) ,]
under <- testset[testset$price > pred+100,]
over  <- testset[testset$price < pred-100,]
# the number of underestimated price is way larger than the
# number of underestimated price --> base price
# the 3 very large errors are due to the unusual number of bedrooms and 
# bathrooms (great influence)

##############################################################################
################################## LASSO #####################################
# the aim is to retain only a part of the predictors and discard the rest
# to create a subset that possibly produce a lower prediction error.
# PENALIZED MODEL: add penalty to do regularization in the model

# prepare the data starting from the lm dataset
# first of all, standardize data
not_all_na   <- function(x) any(!is.na(x))
xtrain_lasso <- trainset %>%
  select_if(!colnames(trainset) %in% c("id", "price")) %>%
  mutate_if(is.numeric, scale) %>% 
  select_if(not_all_na)
ytrain_lasso <- trainset$price  

xtest_lasso  <- testset %>%
  select_if(!colnames(testset) %in% c("id", "price")) %>%
  mutate_if(is.numeric, scale) %>% 
  select_if(not_all_na)
ytest_lasso  <- testset$price  

# dummify factorial variables: train and test
dmm_train_lasso  <- dummyVars(" ~ .", 
                              data = xtrain_lasso)
xtrain_lasso_dmm <- data.frame(predict(dmm_train_lasso, 
                                       newdata = xtrain_lasso))
dmm_test_lasso   <- dummyVars(" ~ .", 
                              data = xtest_lasso)
xtest_lasso_dmm  <- data.frame(predict(dmm_test_lasso, 
                                       newdata = xtest_lasso))

# matrix format for glmnet
xtrain_lasso_fn <- as.matrix(xtrain_lasso_dmm)
ytrain_lasso_fn <- as.matrix(ytrain_lasso)
xtest_lasso_fn  <- as.matrix(xtest_lasso_dmm)
ytest_lasso_fn  <- as.matrix(ytest_lasso)

cv_lasso <- cv.glmnet(xtrain_lasso_fn, 
                      ytrain_lasso_fn, 
                      alpha        = 1, 
                      type.measure = "mse", 
                      nfolds       = 5)
plot(cv_lasso$glmnet.fit, xvar="lambda", label=TRUE)
coef(cv_lasso)

lasso_model <- glmnet(xtrain_lasso_fn, 
                      ytrain_lasso_fn, 
                      alpha  = 1, 
                      lambda = cv_lasso$lambda.min)
coef(lasso_model)
preds_lasso     <- predict(lasso_model, 
                           xtest_lasso_fn)
lasso_RMSE_test <- round(RMSE(preds_lasso, 
                              ytest_lasso_fn), 3)

sprintf('The train error for the lasso regression is %f', 
        lasso_RMSE_test)

##############################################################################
############################# REGRESSION TREES ###############################
set.seed(11)

# Default model
start.time <- Sys.time()

m1def <- rpart(
  price ~ .,
  trainset,
  method = 'anova'
)

end.time <- Sys.time()
end.time - start.time

m1def$cptable
# printcp(m1def) 
# I can see the actually used variables and the best performing tree 
# (8th, with 7 split).
rpart.plot(m1def, faclen = 3) 
# clearly, the first split is by far the most informative
# one, while some are no-informative
trainpredm1   <- predict(m1def, trainset)
m1_RMSE_train <- round(RMSE(trainpredm1, trainset$price), 3)
testpredm1    <- predict(m1def, testset)
m1_RMSE_test  <- round(RMSE(testpredm1, testset$price), 3)
sprintf('The train error for the default 0.01 cp tree is %f, the test error is %f', 
        m1_RMSE_train, m1_RMSE_test)

# Force smaller complexity to grow a bigger tree
start.time <- Sys.time()

m2 <- rpart(
  price ~ .,
  trainset,
  method = 'anova',
  cp     = 0.001
)
end.time <- Sys.time()
end.time - start.time

printcp(m2)
bestcp <- m2$cptable[which.min(m2$cptable[,'xerror']),'CP']

trainpredm2   <- predict(m2, trainset)
m2_RMSE_train <- round(RMSE(trainpredm2, trainset$price), 3)
testpredm2    <- predict(m2, testset)
m2_RMSE_test  <- round(RMSE(testpredm2, testset$price), 3)
sprintf('The train error for the 0.001 cp tree is %f, the test error is %f', 
        m2_RMSE_train, m2_RMSE_test)

# prune the tree
m2_pruned <- prune(m2, cp=bestcp)
printcp(m2_pruned)
rpart.plot(m2_pruned) 
# errors for the pruned tree
trainpredm2p   <- predict(m2_pruned, trainset)
m2p_RMSE_train <- round(RMSE(trainpredm2p, trainset$price), 3)
testpredm2p    <- predict(m2_pruned, testset)
m2p_RMSE_test  <- round(RMSE(testpredm2p, testset$price), 3)
sprintf('The train error for the pruned tree is %f, the test error is %f', 
        m2p_RMSE_train, m2p_RMSE_test)
# m2 and m2_pruned are the same, as the bestcp is the last displayed in the
# cp table
# test error tree VS pruned
# pruned and single tree m2
plot(testpredm2p, testset$price, pty="s") # i do not have anymore great pred
# like lm_1 (error > 2000)
abline(0,1)

# The pruned model seems to give better results, as the overfitting of the tree
# is not present anymore.
# Instead, tuning is done with a loop over a grip of parameters. 
gridTree <- expand.grid(
  maxdepth   = seq(6, 8, 1),
  cp         = seq(0.00025, 0.001, 0.00025),
  minsplit   = seq(30, 60, 10),            # fine tuning after running code on
  # on a sample
  err_tuning = 0
)

gridTree2 <- gridTree

start.time <- Sys.time()
for (i in 1:nrow(gridTree)) {
  
  set.seed(11)
  
  m <- rpart(
    formula = price ~ .,
    data    = trainset,
    method  = "anova",
    control = list(cp       = gridTree$cp[i], 
                   maxdepth = gridTree$maxdepth[i],
                   minsplit = gridTree$minsplit[i])
  )
  
  index                  <- which.min(m$cptable[, "xerror"])
  gridTree$err_tuning[i] <- m$cptable[index, "xerror"] 
  
}

end.time <- Sys.time()
end.time - start.time

set.seed(11)
m_tuning <- rpart(
  formula = price ~ .,
  data    = trainset,
  method  = "anova",
  control = list(cp       = gridTree$cp[which.min(gridTree$err_tuning)], 
                 maxdepth = gridTree$maxdepth[which.min(gridTree$err_tuning)],
                 minsplit = gridTree$minsplit[which.min(gridTree$err_tuning)])
  # best cp      : .0001
  # best maxdepth: 8
  # best minsplit: 30 more than default given the dimensions.
)

m_tuning$cptable

trainpredm_tune <- predict(m_tuning)
mtun_RMSE_train <- round(RMSE(trainpredm_tune, trainset$price), 3)
testpredm_tune  <- predict(m_tuning, testset)
mtun_RMSE_test  <- round(RMSE(testpredm_tune, testset$price), 3)
cat(sprintf('The train error for the tree after tuning is %f, the test error is %f.\nParameters are cp: %f maxdepth: %i and minsplit: %i', 
            mtun_RMSE_train, mtun_RMSE_test, 
            gridTree$cp[which.min(gridTree$err_tuning)],
            gridTree$maxdepth[which.min(gridTree$err_tuning)],
            gridTree$minsplit[which.min(gridTree$err_tuning)]))

# Other models are therefore tested: 
# * bagging
# * random Forest
# * boosting.

# Get variable importance from the selected model
m1_impo  <- names(m1def$variable.importance) 
m2p_impo <- names(m2_pruned$variable.importance)
names(m_tuning$variable.importance)
as.data.frame(m_tuning$variable.importance)

##############################################################################
############################### BAGGING ######################################
# the function RF allows to perform bagging by simply selecting mtry = p, when 
# p = num. predictors
# nrow(trainset[is.na(trainset$host_response_time) == TRUE , ])

start.time <- Sys.time()
bagm1 <- randomForest(
  price ~ .,
  trainset,
  mtry       = ncol(trainset) - 1, 
  #importance = TRUE,
  ntree      = 50
)
end.time <- Sys.time()
end.time - start.time

plot(bagm1) # average error across more trees 
# the error is quite stable with 150 trees or more
#which.min(bagm1$mse)

# train and test error
trainpredbagm1   <- predict(bagm1, trainset)
bagm1_RMSE_train <- round(RMSE(trainpredbagm1, trainset$price), 3)
testpredbagm1    <- predict(bagm1, testset)
bagm1_RMSE_test  <- round(RMSE(testpredbagm1, testset$price), 3)
sprintf('The train error for the bagged tree is %f, the test error is %f', 
        bagm1_RMSE_train, bagm1_RMSE_test)


# plot importance
randomForest::importance(bagm1)
varImpPlot(bagm1) # coherent with first results

##############################################################################
############################## RANDOM FOREST #################################
start.time <- Sys.time()
start.time
randRm1 <- randomForest(
  price ~ .,
  trainset,
  mtry       = round(ncol(trainset)/3),
  importance = TRUE
)
end.time <- Sys.time()
end.time - start.time

plot(randRm1)
#which.min(randRm1$mse)
trainpredrandRm1   <- predict(randRm1)
randRm1_RMSE_train <- round(RMSE(trainpredrandRm1, trainset$price), 3)
testpredrandRm1    <- predict(randRm1, testset)
randRm1_RMSE_test  <- round(RMSE(testpredrandRm1, testset$price), 3)
sprintf('The train error for the tree after tuning is %f, the test error is %f', 
        randRm1_RMSE_train, randRm1_RMSE_test)

# tuning parameters
set.seed(11)

start.time <- Sys.time()
m2 <- tuneRF(
  x          = trainset[, -14],
  y          = trainset$price,
  ntreeTry   = 200,
  mtryStart  = 10,
  stepFactor = 1.15, # not testing all the combination
  improve    = 0.01,
  trace      = FALSE       
)
end.time <- Sys.time()
end.time - start.time


# loop on the number of variable
error_RF <- rep(0, 11)

for (i in (1:length(error_RF))){
  
  set.seed(11)
  
  RF <- randomForest(
    price  ~ .,
    trainset,
    mtry       = i + 11,
    ntree      = 150,
    importance = TRUE
  )
  
  pred_RF      <- predict(RF, testset)
  error_RF[i]  <- round(RMSE(pred_RF, testset$price), 3)
  
}

min.inx <- which.min(error_RF)

randRm2 <- randomForest(
  price ~ .,
  trainset,
  mtry       = (min.inx + 11), # best mtry: 16
  ntree      = 150,
  importance = TRUE
)

trainpredrandRm2   <- predict(randRm2)
randRm2_RMSE_train <- round(RMSE(trainpredrandRm2, trainset$price), 3)
testpredrandRm2    <- predict(randRm2, testset)
randRm2_RMSE_test  <- round(RMSE(testpredrandRm2, testset$price), 3)
sprintf('The train error for the random forest after tuning is %f, the test error is %f', 
        randRm2_RMSE_train, randRm2_RMSE_test)


##############################################################################
############################## RANGER RFOREST ################################
# BOTH THE TUNING PROCESS ARE HIGHLY TIME-CONSUMING
# since the package random forest tends to slow down a lot in large dataset, 
# we use here ranger (6x faster, source https://uc-r.github.io/random_forests)
set.seed(11)

start.time <- Sys.time()
rangerm1 <- ranger(
  price     ~ .,
  trainset,
  mtry      = round(ncol(trainset)/3), # m = p/3 
  num.trees = 500
)
end.time <- Sys.time()
end.time - start.time

ranger1_RMSE_train <- sqrt(rangerm1$prediction.error)
testpredrand_rang1 <- predict(rangerm1, testset)
ranger1_RMSE_test  <- round(RMSE(testpredrand_rang1$predictions, 
                                 testset$price), 3)
sprintf('The train error for the random forest with ranger is %f, the test error is %f', 
        ranger1_RMSE_train, ranger1_RMSE_test)


# tuning number of variables, min number of obs per node, fraction of obs in 
# each resample
ranger_grid2 <- expand.grid(
  mtry       = seq(12, 18, by = 1),
  node_size  = c(15, 25),
  sampe_size = c(.6, .8),
  OOB        = 0,
  RMSE       = 0
)

ranger_grid2 <- expand.grid(
  mtry       = seq(12, 18, by = 1),
  node_size  = c(15, 20),
  sampe_size = .8,
  OOB        = 0,
  RMSE       = 0
)

start.time <- Sys.time()
for(i in 1:nrow(ranger_grid2)) {
  
  set.seed(11)
  
  model <- ranger(
    formula         = price ~ ., 
    data            = trainset, 
    num.trees       = 500,
    mtry            = ranger_grid2$mtry[i],
    min.node.size   = ranger_grid2$node_size[i],
    sample.fraction = ranger_grid2$sampe_size[i],
  )
  
  predRanger  <- predict(model, data = testset)
  predR       <- predRanger$predictions
  RMSEranger1 <- round(RMSE(predR, testset$price), 3) # compute RMSE
  # in order to mantain continuity with previous method implemented
  
  ranger_grid2$OOB[i]  <- sqrt(model$prediction.error)
  ranger_grid2$RMSE[i] <- RMSEranger1
  
}

end.time <- Sys.time()
end.time - start.time

ranger_grid2 %>% 
  dplyr::arrange(RMSE) %>%
  head(10)

####################### BEST PARAMETERS ##########################
# -------------------  COMPLETE DATA SET  -----------------------#
# ---------------------------------------------------------------#
#             mtry |        min.node.size |      sample.fraction #
# ---------------------------------------------------------------#
#               18 |                   15 |                   .8 #               
# ---------------------------------------------------------------#
# default:                                                       #
#   round(ncol/3) |                    5  |                    1 #

# final model
rangerm2 <- ranger(
  formula         = price ~ ., 
  data            = trainset, 
  num.trees       = 500,
  mtry            = ranger_grid2$mtry[which.min(ranger_grid2$RMSE)],       # 18
  min.node.size   = ranger_grid2$node_size[which.min(ranger_grid2$RMSE)],  # 15
  sample.fraction = ranger_grid2$sampe_size[which.min(ranger_grid2$RMSE)], # .8
  importance      = "impurity",
)

rangerm2 <- ranger(
  formula         = price ~ ., 
  data            = trainset, 
  num.trees       = 500,
  mtry            = 18,
  min.node.size   = 15,
  sample.fraction = .8,
  importance      = "impurity",
)
ranger2_RMSE_train  <- sqrt(rangerm2$prediction.error)
testpredrand_rang2  <- predict(rangerm2, testset)
ranger2_RMSE_test   <- round(RMSE(testpredrand_rang2$predictions, 
                                  testset$price), 3)
sprintf('The train error for the random forest with ranger is %f, the test error is %f', 
        ranger2_RMSE_train, ranger2_RMSE_test)

as.data.frame(rangerm2$variable.importance)
#pred <- as.data.frame(cbind(testpredrand_rang2$predictions,testset$price))
#plot(testpredrand_rang2$predictions,testset$price) 
# the model tends to overestimate low prices and 
# underestimate high prices
#abline(0,1, col="red", lwd=2, lty=2)

impo <- as.data.frame(rangerm2$variable.importance) %>% 
  dplyr::arrange(desc(rangerm2$variable.importance)) %>%
  head(16) 
# check most important variable
impo$names     <- rownames(impo)
rownames(impo) <- seq(1, nrow(impo), 1)
colnames(impo) <- c('importance', 'variable')
impo$importance<- impo$importance/1000000
impo$categories<- ifelse(impo$variable == 'number_amenities', 
                         'services',
                         ifelse(impo$variable %in% c('avg_distance',
                                                     'neighbourhood',
                                                     'number_pois'), 
                                'location',
                                ifelse(impo$variable %in% c('reviews_per_month',
                                                            'review_scores_rating',
                                                            'days_host_since',
                                                            'days_last_review',
                                                            'host_total_listings_count',
                                                            'host_verifications'), 
                                       'trust',
                                       ifelse(impo$variable %in% c('bathrooms',
                                                                   'bedrooms',
                                                                   'room_type'), 
                                              'structure',
                                              'other'))))


ggplot(impo, aes(x=reorder(variable, importance), y=importance, color = categories)) + 
  geom_point() +
  geom_segment(aes(x=variable,xend=variable,y=0,yend=importance)) +
  scale_color_manual(values = c("structure" = "#cf547d",
                                "other"     = "#8fe367",
                                "services"  = "#bf94e4",
                                "trust"     = "#4c91d1",
                                "location"  = "#ffc300")) +
  ylab("importance (mln)") +
  xlab("Variable Name") +
  theme_light() +
  labs(title="Random Forest - ranger") + 
  coord_flip()

##############################################################################
############################ GRADIENT BOOSTING ###############################
# RF: each tree is built from a bootstrap sample
# GB: trees are sequentially grown
set.seed(11)

start.time <- Sys.time()
gb_m1 <- gbm(
  formula           = price ~ .,
  distribution      = "gaussian",
  data              = trainset,
  n.trees           = 5000,
  interaction.depth = 1,
  shrinkage         = 0.001,
  cv.folds          = 5,
  n.cores           = 4, 
  verbose           = FALSE
)  

end.time <- Sys.time()
end.time - start.time

sqrt(min(gb_m1$cv.error))
sqrt(min(gb_m1$train.error))
min(gb_m1$cv.fitted)
max(gb_m1$cv.fitted)

gbm.perf(gb_m1, method = "cv")

# variable impotance
def <- par('mar')
par(mar = c(5, 9, 1, 1))
summary(
  gb_m1, 
  cBars  = 10,
  method = relative.influence, 
  las    = 2
)
par(mar = def)

# gbm parameters tuning
# -           n.trees: higher number of trees reduce the train error,
#                      but might lead to overfitting; 
#                      for tuning we will be using a fixed value of 6000.
# - interaction.depth: 1 implies an additive model with tree stumps;
#                      value of max 5 give a good fit over very complex
#                      model already, so no need for higher values.
# -         shrinkage: learning rate.
# -    train.fraction: portion of the dataset used for training; the remaining
#                      used for validation
# -    n.minobsinnode: minimum number of observation in each final node; 
#                      10 is default, but since there is just a small fraction 
#                      prices with frequency < 10, I will tune also 15 and 20
# -      bag.fraction: value smaller than 1 implies STOCASTIC GRADIENT, default
#                      is 0.5.

gbm_tuning <- expand.grid(
  shrinkage         = c(.05, .1),
  interaction.depth = 7,
  n.minobsinnode    = c(15, 20),
  bag.fraction      = c(.75, .9), 
  optimal_trees     = 0,               
  min_RMSE          = 0                   
)

start.time <- Sys.time()
for(i in 1:nrow(gbm_tuning)) {  
  
  set.seed(11)
  
  # train model
  gbmtuning <- gbm(
    formula           = price ~ .,
    distribution      = "gaussian",
    data              = trainset,
    n.trees           = 1500,
    interaction.depth = gbm_tuning$interaction.depth[i],
    shrinkage         = gbm_tuning$shrinkage[i],        
    n.minobsinnode    = gbm_tuning$n.minobsinnode[i],    
    bag.fraction      = gbm_tuning$bag.fraction[i],      
    train.fraction    = .7,
    n.cores           =  4,
    verbose           = FALSE
  )                   # from the resulting grid i can see that the 
  # lowest learning rate might require higher number 
  # number of trees, in general
  
  gbm_tuning$optimal_trees[i] <- which.min(gbmtuning$valid.error)
  gbm_tuning$min_RMSE[i]      <- sqrt(min(gbmtuning$valid.error))
}

end.time <- Sys.time()
end.time - start.time


gbm_tuning %>% 
  dplyr::arrange(min_RMSE)%>%
  head(5)

####################### BEST PARAMETERS ##########################
# ----------------  Subset .1*nrow(df_month)  -------------------#
# ---------------------------------------------------------------#
# shrinkage  | interaction.depth | bag.fraction | n.minobsinnode #
# ---------------------------------------------------------------#
#        .05 |                 7 |           .9 |             20 #
# ---------------------------------------------------------------#
# best n.trees = 1319

start.time <- Sys.time()
gb_mfinal  <- gbm( # final model with tuned parameters
  formula           = price ~ .,
  distribution      = "gaussian",
  data              = trainset,
  n.trees           = gbm_tuning$optimal_trees[which.min(gbm_tuning$min_RMSE)],
  interaction.depth = gbm_tuning$interaction.depth[which.min(gbm_tuning$min_RMSE)],
  bag.fraction      = gbm_tuning$bag.fraction[which.min(gbm_tuning$min_RMSE)],
  shrinkage         = gbm_tuning$shrinkage[which.min(gbm_tuning$min_RMSE)],
  n.minobsinnode    = gbm_tuning$n.minobsinnode[which.min(gbm_tuning$min_RMSE)],
  cv.folds          = 5,
  n.cores           = 4, 
  verbose           = FALSE
)  
end.time <- Sys.time()
end.time - start.time

gbm.perf(gb_mfinal, method = "cv")
gb_mfinal_RMSE_train <- sqrt(min(gb_mfinal$cv.error)) 
predtest_gb_mfinal   <- predict(gb_mfinal, n.trees = gb_mfinal$n.trees, testset)
gb_mfinal_RMSE_test  <- round(RMSE(predtest_gb_mfinal, 
                                   testset$price), 3)
sprintf('The train error for the GB is %f, the test error is %f', 
        gb_mfinal_RMSE_train, gb_mfinal_RMSE_test)

# check for the attributes that influence the price the most
summary(gb_mfinal, 
        method = relative.influence,   # variable relative influence
        las    = 2)
plot.gbm(gb_mfinal, i.var = c(12, 36)) # see relationship between bedrooms and 
# avg distance from the city center

# first I want to see the average effect of different variables
# that proved to be significant
gb_mfinal %>%
  partial(                        # check for the partial dependence of
    pred.var = "bedrooms",        # a single predictor on the target
    plot     = TRUE,
    n.trees  = gb_mfinal$n.trees,
    ice = TRUE                   # to check for all the individual curves
  ) %>%
  autoplot(center = TRUE) # NON VA https://cran.r-project.org/web/packages/pdp/pdp.pdf

gb_mfinal %>%
  partial(                        
    pred.var = "neighbourhood",     
    plot     = TRUE,
    n.trees  = gb_mfinal$n.trees,
    #ice = TRUE                 
  )

##############################################################################
####################### GRADIENT BOOSTING WITH XGBOOST #######################
# this package is 10x faster
# this algorithm uses numeric matrix 
# one-hot encoding
# dummify train: the dummyVars function recognizes factors and character and
# change them to dummies
dmm_onehottrain <- dummyVars(" ~ .", data = subset(trainset, 
                                                   select = -c(price))) 
# fullrank = FALSE --> i want each realization of the variable to 
#                      have its own column
train_onehot    <- data.frame(predict(dmm_onehottrain, newdata = trainset))
train_respvar   <- trainset$price
# dummify test
dmm_onehottest  <- dummyVars(" ~ .", data = subset(testset, 
                                                   select = -c(price)))
test_onehot     <- data.frame(predict(dmm_onehottest, newdata = testset))
test_respvar    <- testset$price
# convert to a Dmatrix
train_matrix   <- xgb.DMatrix(as.matrix(train_onehot), label = train_respvar) 
test_matrix    <- xgb.DMatrix(as.matrix(test_onehot), label = test_respvar) 

set.seed(11)

xgb1 <- xgb.cv(
  data                  = train_matrix,
  eta                   = .01,
  nrounds               = 1000,
  nfold                 = 5,
  objective             = "reg:squarederror",
  verbose               = 0,
  early_stopping_rounds = 100,
  subsample             = .5
)

ggplot(xgb1$evaluation_log) + 
  geom_line(aes(iter, train_rmse_mean, color = "blue") ) +
  geom_line(aes(iter, test_rmse_mean, color = "red"))  +
  scale_colour_manual('',
                      values = c("blue","red"), 
                      labels = c('train', 'test')) +
  labs(title=" for train and test set", 
       subtitle="Objective: reg:squarederror.",
       caption = 'Source: Airbnb data',
       x = 'number of trees',
       y = 'RMSE error') 


xgb1$evaluation_log %>%
  dplyr::summarise(
    ntrees.test         = which.min(test_rmse_mean),
    RMSE_train_error    = min(test_rmse_mean),
  )

which.min(xgb1$evaluation_log$test_rmse_mean)
# results
# serach grid to tune parameters
# after first tuning on sample = 0.1*nrow(df_month)
xgb_tune <- expand.grid(
  eta              = c(.05, .1), # shrinkage = learning rate    
  max_depth        = 7,    # controls the depth of the tree and 
  # therefore complexity
  subsample        = c(.65, .8), # number of sample observation to model 
  # the tree
  min_child_weight = c(5, 10),   # min number of entries per final node
  colsample_bytree = c(.75, .9), # number of features (variables) 
  # supplied to a tree
  trees_best       = 0,          # best number of tree per parameters
  RMSE             = 0           # error
)

for(i in 1:nrow(xgb_tune)) {  
  
  set.seed(11)
  start.time <- Sys.time()
  print(start.time)
  xgbtuning <- xgb.cv(
    params             = list(
      eta              = xgb_tune$eta[i],
      max_depth        = xgb_tune$max_depth[i],
      min_child_weight = xgb_tune$min_child_weight[i],
      subsample        = xgb_tune$subsample[i],
      colsample_bytree = xgb_tune$colsample_bytree[i]
    ),
    data                  = train_matrix,
    nrounds               = 1500,
    nfold                 = 5,
    objective             = "reg:squarederror", 
    verbose               = 0,              
    early_stopping_rounds = 50 
  )
  
  xgb_tune$trees_best[i] <- which.min(xgbtuning$evaluation_log$test_rmse_mean)
  xgb_tune$RMSE[i]       <- min(xgbtuning$evaluation_log$test_rmse_mean)
  end.time <- Sys.time()
  stop_t   <- end.time - start.time
  print(stop_t)
}


xgb_tune %>% 
  dplyr::arrange(RMSE)%>%
  head(5)

best_parameters <- xgb_tune %>%
  summarise(
    min_RMSE         = min(RMSE),
    eta              = xgb_tune$eta[which.min(RMSE)],
    max_depth        = xgb_tune$max_depth[which.min(RMSE)],    
    subsample        = xgb_tune$subsample[which.min(RMSE)],    
    min_child_weight = xgb_tune$min_child_weight[which.min(RMSE)],    
    colsample_bytree = xgb_tune$colsample_bytree[which.min(RMSE)]    
  )
xgb_tune$eta[which.min(RMSE)]
xgb_tune$max_depth[which.min(RMSE)]    
xgb_tune$subsample[which.min(RMSE)]    
xgb_tune$min_child_weight[which.min(RMSE)]
xgb_tune$colsample_bytree[which.min(RMSE)]

########################## BEST PARAMETERS ########################### 
# ------------------  Subset .1*nrow(df_month)  ---------------------#
# -------------------------------------------------------------------#
# eta  | max_depth | subsample | min_child_weight | colsample_bytree #
# -------------------------------------------------------------------#
#   .1 |         7 |        .8 |                5 |               .9 #
# -------------------------------------------------------------------#

# which.min(RMSE)
start.time <- Sys.time()
xgbfinal   <- xgboost(          # train final model with tuned parameters
  params             = list(
    eta              = best_parameters$eta,               #
    max_depth        = best_parameters$max_depth,         #
    min_child_weight = best_parameters$min_child_weight,  #
    subsample        = best_parameters$subsample,         #
    colsample_bytree = best_parameters$colsample_bytree   #
  ),
  data                  = train_matrix,
  nrounds               = 1500,                           
  objective             = "reg:squarederror",
  verbose               = 0              
)

end.time <- Sys.time()
end.time - start.time

xgbfinal$evaluation_log
best_trees <- which.min(xgbfinal$evaluation_log$train_rmse)

xgbfinal_RMSE_train <- min(xgbfinal$evaluation_log$train_rmse)
predtest_xgbfinal   <- predict(xgbfinal, test_matrix)
xgbfinal_RMSE_test  <- round(RMSE(predtest_xgbfinal, 
                                  test_respvar), 3)
sprintf('The train error for the GB with xgboost is %f, the test error is %f',
        xgbfinal_RMSE_train, xgbfinal_RMSE_test)

# importance of variables
# xgboost has built-in function to explore variable importance
importance_matrix <- xgb.importance(model = xgbfinal)

impoxgb <- importance_matrix %>%
  dplyr::select(Feature, Gain) %>%
  dplyr::arrange(desc(Gain)) %>%
  head(16)

impoxgb$categories<- ifelse(impoxgb$Feature == 'number_amenities', 
                            'services',
                            ifelse(impoxgb$Feature %in% c('avg_distance',
                                                          'neighbourhood',
                                                          'number_pois'), 
                                   'location',
                                   ifelse(impoxgb$Feature %in% c('reviews_per_month',
                                                                 'review_scores_rating',
                                                                 'days_host_since',
                                                                 'days_last_review',
                                                                 'host_total_listings_count',
                                                                 'host_verifications'), 
                                          'trust',
                                          ifelse(impoxgb$Feature %in% c('bathrooms',
                                                                        'bedrooms',
                                                                        'room_type.Entire.home.apt'), 
                                                 'structure',
                                                 'other'))))


ggplot(impoxgb, aes(x=reorder(Feature, Gain), y=Gain, color = categories)) + 
  geom_point() +
  geom_segment(aes(x=Feature,xend=Feature,y=0,yend=Gain)) +
  scale_color_manual(values = c("structure" = "#cf547d",
                                "other"     = "#8fe367",
                                "services"  = "#bf94e4",
                                "trust"     = "#4c91d1",
                                "location"  = "#ffc300")) +
  ylab("importance (Gain)") +
  xlab("Variable Name") +
  theme_light() +
  labs(title="Gradient Boosting - XGboost") + 
  coord_flip()


xgb.plot.importance(importance_matrix, 
                    top_n = 10, 
                    measure = "Gain") 
# measure: - GAIN: the relative contribution of the feature
#          (relative influence);
#          - COVER: the number of observations related to 
#          this feature (percentage of leaf nodes decided
#          by the feature);
#          - FREQUENCY: the relative number of times a 
#          feature occurs in the trees of the model

# partial dependency plots (code from gbm)
xgbfinal %>%
  partial(                        
    pred.var = "",     
    plot     = TRUE,
    n.trees  = xgbfinal$n.trees,
    #ice = TRUE                 
  )


### Accuracy of each model - WHOLE DATA SET
# If I'd use the average as indicator, the average error would be 91.973

# Tested model             | Train accuracy | TEST ACCURACY #
# ----------------------------------------------------------#
# Linear Regression         | /              | 72.71         #
# LASSO                     | /              | 70.58         #
# Regression Tree           | 79.55          | 79.01         #
# Pruned Tree               | 69.56          | 70.62         #  
# Tuned parameters Tree     | 66.44          | 68.07         #  
# Bagging                   |           |          #
# Random Forest             |           |          #
# Random Forest (ranger)    | 33.53          | 32.98         #
# Random Forest (ranger)    | 39.21          | 38.73         #
# Gradient Boosting         |           |          #
# Gradient Boosting (tuned) |        |           |          #
# Grad. Boost. (xgboost)    |           |          #
# Grad.                     |                |               #
# Boost. (xgboost) tuned    |           |          #


##############################################################################
##############################################################################
##############################################################################
### REFERENCES:

### LINEAR REGRESSION ###
# ISLR
# http://uc-r.github.io/linear_regression
# https://stats.stackexchange.com/questions/14914/how-to-test-the-autocorrelation-of-the-residuals

### LASSO ###
# ISLR
# ESLII
# Applied Predictive Modeling
# https://www.r-bloggers.com/2017/05/ridge-regression-and-the-lasso/
# https://rpubs.com/bbroto06/ridge_lasso_regression

### REGR. TREES ###
# ISLR laboratories
# https://www.kaggle.com/myonin/prediction-of-house-prices-3-methods
# https://uc-r.github.io/regression_trees

### BAGGING ###
# ISLR laboratories

### RANDOM FOREST ###
# ISLR laboratories
# https://uc-r.github.io/random_forests

### GRADIENT BOOSTING ###
# ISLR laboratories
# ESLII
# http://uc-r.github.io/gbm_regression
# https://cran.r-project.org/web/packages/gbm/gbm.pdf
# https://www.rdocumentation.org/packages/pdp/versions/0.7.0/topics/partial
# https://bgreenwell.github.io/pdp/articles/pdp.html
# https://github.com/dmlc/xgboost/tree/master/R-package 
# https://www.slideshare.net/ShangxuanZhang/xgboost-55872323
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
