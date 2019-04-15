
#############################################################################################################
# compute transition probability matrices associated with each batting outcome (i.e. 1B, BB, HR, etc.)
# this is a precursor to a simulation model that will simulate the outcome for the batter/pitcher matchup
# based on multinomial probabilities and then 'translate' that into a new simulated state
# the code below does not contemplate baserunning, balks, wild pitches, or other things that advance runners
# during a plate appearance that the batter is not involved in
#############################################################################################################

library(MASS)
library(tidyverse)
library(devtools)
library(car)
library(knitr)
library(tictoc)
library(caret)
library(randomForest)
library(ROCR)

###############################################################################
# 1) define function to calculate base-out states and run expectancy
###############################################################################

# original function script can be downloaded from URL below (need to load devtools package)
# however, have copied full script here
# also made two modification, as noted in code below

# source_gist("https://gist.github.com/bayesball/8892999",filename="compute.runs.expectancy.R")

compute.runs.expectancy = function(season){
  # changed -- plyr function replaced with dplyr
  # (increases speed from 114 to 30 sec for 2013 data)
  
  # assume that files "allseason.csv" and "fields.csv"
  # are in current working folder
  # for example, if season = 1961, all1961.csv should be
  # available
  
  # returns play-by-play matrix with new variables
  # RUNS.ROI - runs scored in remainder of inning
  # STATE - current runners/outs state
  # NEW.STATE - new runners/outs state (after play)
  # RUNS.STATE - runs value of current runners/outs state
  # RUNS.NEW.STATE - runs value of new runners/outs state
  # RUNS.VALUE - runs value of play event
  
  data.file <- paste("all", season, ".csv", sep="")
  data <- read.csv(data.file, header=FALSE)
  #  fields <- read.csv("fields.csv")
  #  fields <- read.csv("http://personal.bgsu.edu/~albert/baseball/fields.csv")
  fields <- read.csv("http://www-math.bgsu.edu/~albert/baseball/fields.csv")
  names(data) <- fields[, "Header"]
  
  data$RUNS <- with(data, AWAY_SCORE_CT + HOME_SCORE_CT)
  data$HALF.INNING <- with(data, 
                           paste(GAME_ID, INN_CT, BAT_HOME_ID))
  
  data$RUNS.SCORED <- with(data, (BAT_DEST_ID > 3) +
                             (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
  RUNS.SCORED.INNING <- aggregate(data$RUNS.SCORED, 
                                  list(HALF.INNING = data$HALF.INNING), sum)
  
  RUNS.SCORED.START <- aggregate(data$RUNS, 
                                 list(HALF.INNING = data$HALF.INNING), "[", 1)
  
  MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
  MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
  data <- merge(data, MAX)
  N <- ncol(data)
  names(data)[N] <- "MAX.RUNS"
  
  data$RUNS.ROI <- data$MAX.RUNS - data$RUNS
  
  get.state <- function(runner1, runner2, runner3, outs){
    runners <- paste(runner1, runner2, runner3, sep="")
    paste(runners, outs)                      
  }
  
  RUNNER1 <- ifelse(as.character(data[,"BASE1_RUN_ID"])=="", 0, 1)
  RUNNER2 <- ifelse(as.character(data[,"BASE2_RUN_ID"])=="", 0, 1)
  RUNNER3 <- ifelse(as.character(data[,"BASE3_RUN_ID"])=="", 0, 1)
  data$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data$OUTS_CT)
  
  NRUNNER1 <- with(data, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
  NRUNNER2 <- with(data, as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2))
  NRUNNER3 <- with(data, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                      RUN3_DEST_ID==3 | BAT_DEST_ID==3))
  NOUTS <- with(data, OUTS_CT + EVENT_OUTS_CT)
  
  data$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)
  
  data <- subset(data, (STATE!=NEW.STATE) | (RUNS.SCORED>0))
  
  #  require(plyr)
  #  data.outs <- ddply(data, .(HALF.INNING), summarize,
  #                     Outs.Inning = sum(EVENT_OUTS_CT))
  #  data <- merge(data, data.outs)
  
  require(dplyr)
  data.outs <- summarize(group_by(data, HALF.INNING),
                         Outs.Inning = sum(EVENT_OUTS_CT))
  data <- merge(data, data.outs)
  
  # for expected runs computation, only consider complete innings
  dataC <- subset(data, Outs.Inning == 3)
  
  RUNS <- summarize(group_by(dataC, STATE), Mean=mean(RUNS.ROI))
  RUNS$Outs <- substr(RUNS$STATE, 5, 5)
  RUNS <- RUNS[order(RUNS$Outs), ]
  
  RUNS.POTENTIAL <- matrix(c(RUNS$Mean, rep(0, 8)), 32, 1)
  dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$STATE, "000 3","001 3",
                                     "010 3","011 3","100 3","101 3","110 3","111 3") 
  data$RUNS.STATE <- RUNS.POTENTIAL[data$STATE, ]
  data$RUNS.NEW.STATE <- RUNS.POTENTIAL[data$NEW.STATE, ]
  data$RUNS.VALUE <- data$RUNS.NEW.STATE - data$RUNS.STATE + 
    data$RUNS.SCORED
  
  data
  
}

###################################################################
# 2) define function to adjust result of compute.runs.expectancy 
###################################################################

adjust.df = function(df){
  df$NEW.STATE = recode(df$NEW.STATE, "c('000 3','001 3','010 3','011 3',
                                            '100 3','101 3','110 3','111 3')='3'")
  
  df
  #df = subset(df, BAT_EVENT_FL == TRUE)
}

###################################################################
# 3) apply functions to target season(s)
###################################################################

# unzipped folder should have target season csv (allXXXX.csv) and fields.csv
setwd(choose.dir())
 
d2018 = compute.runs.expectancy(2018)
d2018 = adjust.df(d2018)

###################################################################
# 4) add fields to facilitate pitcher data calcs
###################################################################

# remove non-batting events....pitcher model only based on events at the plate
d2018 = subset(d2018, BAT_EVENT_FL == TRUE)

# arrange so as to group teams within each game
d2018 = d2018 %>% arrange(GAME_ID, BAT_HOME_ID)

# create table of starters by game and team....only interested in modeling longevity of starters
starters = d2018 %>% 
            select(GAME_ID, BAT_HOME_ID, PIT_ID, INN_CT) %>%
            filter(INN_CT == 1)

starters$GAME_TEAM = with(starters, paste(GAME_ID, BAT_HOME_ID, sep="_"))
starters = starters[!duplicated(starters$GAME_TEAM),]
starters = starters %>% select(-INN_CT, -GAME_TEAM)
names(starters)[3] = "STARTER"

# set starter flag and remove non-starters
d2018 = left_join(d2018, starters, by=c("GAME_ID", "BAT_HOME_ID"))
d2018 = subset(d2018, PIT_ID == STARTER)

# create unique identifier for each gameID/starter combo
d2018$GAMEID_STARTER = with(d2018, paste(GAME_ID, STARTER))

# add batters faced (BF), H/BB/earned runs/unearned runs
d2018$BF = with(d2018, data.frame(BF=ave(GAMEID_STARTER==GAMEID_STARTER, GAMEID_STARTER, FUN=cumsum)))$BF
d2018$H_ALLOW = with(d2018, data.frame(H_ALLOW=ave(GAMEID_STARTER==GAMEID_STARTER & H_FL>0, GAMEID_STARTER, FUN=cumsum)))$H_ALLOW
d2018$BB_ALLOW = with(d2018, data.frame(BB_ALLOW=ave(GAMEID_STARTER==GAMEID_STARTER &
                                                    EVENT_CD >= 12 & EVENT_CD <=14,   # incl BB/IBB/HBP
                                                    GAMEID_STARTER, FUN=cumsum)))$BB_ALLOW

d2018$EARNED_RUNS = with(d2018, (BAT_DEST_ID == 4 | BAT_DEST_ID == 6) +    # incremental ER allowed
                                (RUN1_DEST_ID == 4 | RUN1_DEST_ID == 6) +
                                (RUN2_DEST_ID == 4 | RUN2_DEST_ID == 6) +
                                (RUN3_DEST_ID == 4 | RUN3_DEST_ID == 6))
d2018$ER_ALLOW = with(d2018, data.frame(ER_ALLOW=ave(EARNED_RUNS, GAMEID_STARTER, FUN=cumsum)))$ER_ALLOW  # running total ER allowed

d2018$UNEARNED_RUNS = with(d2018, (BAT_DEST_ID == 5) + (RUN1_DEST_ID == 5) + (RUN2_DEST_ID == 5) + (RUN3_DEST_ID == 5))   # incremental unearned runs allowed
d2018$UER_ALLOW = with(d2018, data.frame(UER_ALLOW=ave(UNEARNED_RUNS, GAMEID_STARTER, FUN=cumsum)))$UER_ALLOW  # running total unearned runs allowed

# add pitch counts field and rolling total pitch counts
d2018$PITCH_CT = nchar(gsub("[+*.123>]", "", d2018$PITCH_SEQ_TX))   # need to remove codes indicating something other than a pitch being thrown
d2018$TOT_PITCH_CT = with(d2018, data.frame(TOT_PITCH_CT=ave(PITCH_CT, GAMEID_STARTER, FUN=cumsum)))$TOT_PITCH_CT

# add IP field and flag end of inning (substitutions more likely to occur between half innings, esp in NL?)
# flags for end of 7th (setup time?) and end of 8th (closer time?)
d2018 = d2018 %>% mutate(IP = ave(EVENT_OUTS_CT, GAMEID_STARTER, FUN=cumsum)/3,
                        END_INNING = ifelse(EVENT_OUTS_CT > 0 & IP %% 1 == 0, 1, 0),
                        INN8_FLAG = ifelse(IP == 7 & END_INNING == 1, 1, 0),
                        INN9_FLAG = ifelse(IP == 8 & END_INNING == 1, 1, 0))

# establish response variable (i.e. lifted after facing batter)
FINAL_PITCH_CT = d2018 %>%
                  group_by(GAMEID_STARTER) %>%
                  summarise(FINAL_PITCH_CT = max(TOT_PITCH_CT))

d2018 = left_join(d2018, FINAL_PITCH_CT, by="GAMEID_STARTER")

d2018$FINAL_BATTER_FL = with(d2018, ifelse(TOT_PITCH_CT == FINAL_PITCH_CT, 1, 0))


# take subset of relevant fields for further analysis / modeling
data = d2018 %>% select(FINAL_BATTER_FL, BF, TOT_PITCH_CT, H_ALLOW, BB_ALLOW, ER_ALLOW, UER_ALLOW,
                        IP, END_INNING, INN8_FLAG, INN9_FLAG)

# exploratory modeling indicates that no hits & no runs allowed flags might be meaningful
data$NO_HITS = ifelse(data$H_ALLOW == 0, 1, 0)
data$NO_RUNS = ifelse((data$ER_ALLOW + data$UER_ALLOW) == 0, 1, 0)

# convert response to factor 
data$FINAL_BATTER_FL = as.factor(data$FINAL_BATTER_FL)

###################################################################
# 4b) modifications to work with simulation model
###################################################################

# eliminate pitch count...pitch count not tracked in sim model
data = data %>% select(-TOT_PITCH_CT)

# combine earned and unearned runs...these are not separately tracked, and original model assigns almost same coef to both anyway
data = data %>% mutate(RUNS_ALLOW = ER_ALLOW + UER_ALLOW) %>% select(-ER_ALLOW, -UER_ALLOW)

###################################################################
# 5) EDA
###################################################################
summary(data)

boxplot(BF ~ FINAL_BATTER_FL, data=data)
#boxplot(TOT_PITCH_CT ~ FINAL_BATTER_FL, data=data)
boxplot(H_ALLOW ~ FINAL_BATTER_FL, data=data)
boxplot(BB_ALLOW ~ FINAL_BATTER_FL, data=data)
#boxplot(ER_ALLOW ~ FINAL_BATTER_FL, data=data)
#boxplot(UER_ALLOW ~ FINAL_BATTER_FL, data=data)

###################################################################
# 6) Modeling
###################################################################
train.pct = 0.75
set.seed(123)
train.index = sample(1:nrow(data), nrow(data)*train.pct, replace=FALSE)

data.train = data[train.index,]
data.valid = data[-train.index,]

mod.null = glm(FINAL_BATTER_FL ~ 1, data=data.train, family=binomial)
mod.full = glm(FINAL_BATTER_FL ~ ., data=data.train, family=binomial)
mod.full2 = glm(FINAL_BATTER_FL ~ .*., data=data.train, family=binomial)


mod.AIC = stepAIC(mod.full, scope = list(lower=mod.null, upper=mod.full), direction='both', k=2)
summary(mod.AIC)
vif(mod.AIC)

mod.BIC = stepAIC(mod.full, scope = list(lower=mod.null, upper=mod.full), direction='both', k=log(nrow(data.train)))
summary(mod.BIC)
vif(mod.BIC)

mod.AIC2 = stepAIC(mod.null, scope = list(lower=mod.null, upper=mod.full2), direction='both', k=2)
summary(mod.AIC2)

mod.BIC2 = stepAIC(mod.null, scope = list(lower=mod.null, upper=mod.full2), direction='both', k=log(nrow(data.train)))
summary(mod.BIC2)


###################################################################
# 7) Testing
###################################################################

LogLossBinary = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps)
  - (sum(as.numeric(as.character(actual)) * log(predicted) + (1 - as.numeric(as.character(actual))) * log(1 - predicted))) / length(actual)  
}

mod.summary = function(my.mod){     
  mod.type = class(my.mod)[1]
  train.preds = if(mod.type=='glm'){
    predict(my.mod, newdata = data.train, type='response')
  } else {
    predict(my.mod, newdata = data.train, type='prob')[,2]    # if not glm, function assumes random forest
  }
  
  valid.preds = if(mod.type=='glm'){
    predict(my.mod, newdata = data.valid, type='response')
  } else {
    predict(my.mod, newdata = data.valid, type='prob')[,2]    # if not glm, function assumes random forest
  }
  
  pred.train = prediction(train.preds, data.train$FINAL_BATTER_FL)
  auc.train = performance(pred.train, measure = "auc")
  auc.train = auc.train@y.values[[1]]
  
  pred.valid = prediction(valid.preds, data.valid$FINAL_BATTER_FL)
  auc.valid = performance(pred.valid, measure = "auc")
  auc.valid = auc.valid@y.values[[1]]
  
  confusion.train = prop.table(table(train.preds > 0.50, data.train$FINAL_BATTER_FL), margin=1)
  falseneg.train = confusion.train[1,2]
  falsepos.train = confusion.train[2,1]
  
  confusion.valid = prop.table(table(valid.preds >  0.50, data.valid$FINAL_BATTER_FL), margin=1)
  falseneg.valid = confusion.valid[1,2]
  falsepos.valid = confusion.valid[2,1]
  
  logloss.train = LogLossBinary(data.train$FINAL_BATTER_FL, train.preds)
  logloss.valid = LogLossBinary(data.valid$FINAL_BATTER_FL, valid.preds)
  
  results = data.frame(matrix(NA, nrow=4, ncol=2))
  row.names(results) = c('AUC', 'FalseNeg', 'FalsePos', 'LogLoss')
  colnames(results) = c('train', 'valid')
  results$train = c(auc.train, falseneg.train, falsepos.train, logloss.train)
  results$valid = c(auc.valid, falseneg.valid, falsepos.valid, logloss.valid)
  round(results, 4)
}


mod.summary(mod.AIC)
mod.summary(mod.BIC)
mod.summary(mod.AIC2)
mod.summary(mod.BIC2)
#mod.summary(mod.rf)

logloss.by.band = function(my.mod){     
  mod.type = class(my.mod)[1]
  train.preds = if(mod.type=='glm'){
    predict(my.mod, newdata = data.train, type='response')
  } else {
    predict(my.mod, newdata = data.train, type='prob')[,2]    # if not glm, function assumes random forest
  }
  
  valid.preds = if(mod.type=='glm'){
    predict(my.mod, newdata = data.valid, type='response')
  } else {
    predict(my.mod, newdata = data.valid, type='prob')[,2]    # if not glm, function assumes random forest
  }

  results = data.frame(matrix(NA, nrow=10, ncol=4))
  row.names(results) = c('0-10%', '10-20%', '20-30%', '30-40%', '40-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%')
  colnames(results) = c('t.n', 'train', 'v.n', 'valid')
    
  for (i in 1:10){
    t.index = train.preds <= i/10 & train.preds > i/10-0.10
    v.index = valid.preds <= i/10 & valid.preds > i/10-0.10
    
    logloss.train = LogLossBinary(data.train$FINAL_BATTER_FL[t.index], train.preds[t.index])
    logloss.valid = LogLossBinary(data.valid$FINAL_BATTER_FL[v.index], valid.preds[v.index])
    
    results[i, 't.n'] = sum(t.index) 
    results[i, 'train'] = logloss.train
    results[i, 'v.n'] = sum(v.index) 
    results[i, 'valid'] = logloss.valid
  }
  round(results, 4) 
}

logloss.by.band(mod.AIC)
logloss.by.band(mod.BIC)
logloss.by.band(mod.AIC2)
logloss.by.band(mod.BIC2)

# ad hoc testing
test.conditions = data.frame(IP = 8,
                             BF = 25,
                             TOT_PITCH_CT = 86,
                             H_ALLOW = 4,
                             BB_ALLOW = 2,
                             RUNS_ALLOW = 2,
                             END_INNING = 0,
                             INN8_FLAG = 0,
                             INN9_FLAG = 0,
                             NO_HITS = 0,
                             NO_RUNS = 0)

predict(mod.AIC, newdata = test.conditions, type = 'response')
predict(mod.BIC, newdata = test.conditions, type = 'response')
predict(mod.AIC2, newdata = test.conditions, type = 'response')
predict(mod.BIC2, newdata = test.conditions, type = 'response')

###################################################################
# 8) Save model
###################################################################

target.dir = choose.dir()
mod.pitcher.survival = mod.BIC2
save(mod.pitcher.survival, file = paste0(target.dir, 'pitcher_survival_model.rda'))


