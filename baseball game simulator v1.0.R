
library(tidyverse)
library(devtools)
library(car)
library(knitr)
library(tictoc)


###############################################################################
# 1) load retrosheet event data and player ID file
###############################################################################

setwd(choose.dir())
d2018 = read_csv("2018 event data.csv")

playerID = as.data.frame(read_csv("PlayerIDs.csv"))
row.names(playerID) = playerID$ID

############################################################################
# 2) calculate transition matrices for each play result
############################################################################

# calculate transition matrices     this code adapted from Marchi/Albert section 9.2.3
T.matrix = with(d2018, table(STATE, NEW.STATE, EVENT_CD))
P.matrix = prop.table(T.matrix, margin = c(1,3))

############################################################################
# 3) calculate play probabilities, given opening state
############################################################################

play.probs = with(d2018, table(STATE, EVENT_CD))
play.probs = prop.table(play.probs, margin=1)

############################################################################
# 4) calculate batter and runner destinations and associated RBI counts
############################################################################

d2018$DESTINATIONS = with(d2018, paste0(BAT_DEST_ID, RUN1_DEST_ID, RUN2_DEST_ID, RUN3_DEST_ID))

dest.matrix = with(d2018, table(STATE, NEW.STATE, EVENT_CD, DESTINATIONS))
dest.probs = prop.table(dest.matrix, margin = c(1,2,3))

RBI.matrix = with(d2018, table(STATE, NEW.STATE, EVENT_CD, DESTINATIONS, RBI_CT))
RBI.probs = prop.table(RBI.matrix, margin = c(1,2,3,4))
rm(list='RBI.matrix')

############################################################################
# 5) associate runs scored with state changes
############################################################################
# adapted from code in Marchi & Albert section 9.2.4

count.runners.outs <- function(s)
  sum(as.numeric(strsplit(s,"")[[1]]), na.rm = TRUE)
runners.outs <- sapply(dimnames(T.matrix)[[1]], count.runners.outs)[-25]

R <- outer(runners.outs + 1, runners.outs, FUN="-")
R <- cbind(R, rep(0, 24))
dimnames(R)[[2]] <- dimnames(T.matrix)[[2]]

############################################################################
# 6) calculate MLB average hitting, baserunning, and pitching event probs
############################################################################

# batting event probabilities, GIVEN that a batting event occurs
bat.events = d2018 %>% filter(BAT_EVENT_FL == TRUE)
bat.probs.by.state = with(bat.events, table(STATE, EVENT_CD))
bat.probs.by.state = prop.table(bat.probs.by.state, margin=1)   # this will give probs GIVEN that a batting event has occurred
bat.probs = with(bat.events, table(EVENT_CD))
bat.probs = as.data.frame(prop.table(bat.probs))
row.names(bat.probs) = bat.probs$EVENT_CD
bat.probs = bat.probs %>% select(-EVENT_CD)


# baserunning event probabilities, GIVEN 1+ baserunners
# probabilities must be conditional on men on base, since player BR event probs are given that they are on base
baserunning.events = c('4','5','6','8','12')
baserunning.probs.by.state = with(d2018, table(STATE, EVENT_CD))
baserunning.probs.by.state = prop.table(baserunning.probs.by.state, margin = 1)
baserunning.probs.by.state = as.data.frame(baserunning.probs.by.state)
baserunning.probs.by.state = spread(baserunning.probs.by.state, EVENT_CD, Freq)
row.names(baserunning.probs.by.state) = baserunning.probs.by.state$STATE
baserunning.probs.by.state = baserunning.probs.by.state %>% select(-STATE)
baserunning.probs.by.state = baserunning.probs.by.state[, baserunning.events]

baserunner.flag = with(d2018, ifelse(BASE1_RUN_ID == '' & BASE2_RUN_ID == '' & BASE3_RUN_ID == '', FALSE, TRUE))   # flag for at least 1 runner on base
baserunning.prob = prop.table(table(d2018[baserunner.flag,'EVENT_CD']))    # prob of each event, given at least 1 runner on

# MLB average prob of a baserunning event, given at least 1 runner on
# to serve as comparison to player-specific of baserunning event occurring while they are on base
baserunning.event.prob = sum(baserunning.prob[baserunning.events])      


# pitching event probabilities
pitching.events = c('9','10','11')
pitch.event.probs.by.state = with(d2018, table(STATE, EVENT_CD))
pitch.event.probs.by.state = prop.table(pitch.event.probs.by.state, margin = 1)
pitch.event.probs.by.state = as.data.frame(pitch.event.probs.by.state)
pitch.event.probs.by.state = spread(pitch.event.probs.by.state, EVENT_CD, Freq)
row.names(pitch.event.probs.by.state) = pitch.event.probs.by.state$STATE
pitch.event.probs.by.state = pitch.event.probs.by.state %>% select(-STATE)
pitch.event.probs.by.state = pitch.event.probs.by.state[, pitching.events]

pitching.event.prob = sum(prop.table(table(d2018$EVENT_CD))[pitching.events])

############################################################################
# 7) load batter and pitcher event probabilities
############################################################################

bat.probs.by.player = as.data.frame(read_csv("batting event probs by player.csv"))
row.names(bat.probs.by.player) = bat.probs.by.player$BAT_ID
bat.probs.by.player = bat.probs.by.player %>% select(-BAT_ID)
bat.probs.by.player = rbind(bat.probs.by.player, t(bat.probs))   # add MLB average to prob table...can insert MLB average players in lineup if needed
rownames(bat.probs.by.player)[nrow(bat.probs.by.player)] = "MLB"

bat.probs.by.pitcher = as.data.frame(read_csv("batting event probs by pitcher.csv"))
row.names(bat.probs.by.pitcher) = bat.probs.by.pitcher$PIT_ID
bat.probs.by.pitcher = bat.probs.by.pitcher %>% select(-PIT_ID)
bat.probs.by.pitcher = rbind(bat.probs.by.pitcher, t(bat.probs))   # add MLB average to prob table...can insert MLB average players in lineup if needed
rownames(bat.probs.by.pitcher)[nrow(bat.probs.by.pitcher)] = "MLB"

BR.probs.by.player = as.data.frame(read_csv("baserunning event probs by player.csv"))
row.names(BR.probs.by.player) = BR.probs.by.player$PlayerID
BR.probs.by.player = BR.probs.by.player %>% select(-PlayerID)
BR.probs.by.player = rbind(BR.probs.by.player, baserunning.event.prob)   # add MLB average to prob table...can insert MLB average players in lineup if needed
rownames(BR.probs.by.player)[nrow(BR.probs.by.player)] = "MLB"

PE.probs.by.pitcher = as.data.frame(read_csv("pitching event probs by pitcher.csv"))
row.names(PE.probs.by.pitcher) = PE.probs.by.pitcher$PlayerID
PE.probs.by.pitcher = PE.probs.by.pitcher %>% select(-PlayerID)
PE.probs.by.pitcher = rbind(PE.probs.by.pitcher, pitching.event.prob)   # add MLB average to prob table...can insert MLB average players in lineup if needed
rownames(PE.probs.by.pitcher)[nrow(PE.probs.by.pitcher)] = "MLB"

############################################################################
# 8) load pitcher survival model
############################################################################

load("pitcher_survival_model.rda")      # mod.pitcher.survival

############################################################################
# 8b) establish log5 function
############################################################################
# function estimates the probability of an event in a batter-pitcher matchup
# given the batter overall event probability (B), the pitcher overall event
# probability (P), and the league-wide event probability (L)

log5 = function(B,P,L){
  res = (B*P/L) / ( (B*P/L) + (1-B)*(1-P)/(1-L))
  res[is.na(res)] = 0
  res
}

############################################################################
# 9) function to simulate half inning
############################################################################

simulate.half.inning.P <- function(first.up, lineup, starterID = "MLB", starter = TRUE, pitcher.stats, inning = 0, diagnostics = FALSE){    
  state.record = "000 0"        # only needed for diagnostics
  play.record = NULL            # only needed for diagnostics
  dest.record = NULL            # only needed for diagnostics
  runs = 0                      # initiate runs scored in inning
  bat.pos = first.up            # initiate order position of opening batter
  state = 1                     # index of opening state
  states = colnames(P.matrix)
  outcomes = colnames(play.probs)
  destinations = names(dest.probs[1,1,1,])
  nonbat.events = c("4", "5", "6", "8", "9", "10", "11", "12")  # can easily see which are which by running: with(d2018, table(EVENT_CD, BAT_EVENT_FL))
  
  lineup.bat.probs = bat.probs.by.player[lineup,]
  lineup.BR.probs = BR.probs.by.player[lineup, 'Prob']
  pitcher.PE.prob = NA
  
  player.stats = data.frame(matrix(0, nrow=9, ncol=length(outcomes)+3))   # standard events pluls 3 custom stats to be tracked
  colnames(player.stats) = c(outcomes, "R", "RBI", "SB")
  opp.pitcher.stats = data.frame(matrix(0, nrow=2, ncol=length(outcomes)+3))   # standard events pluls 3 custom stats to be tracked
  colnames(opp.pitcher.stats) = c(outcomes, "BF", "R", "Outs")
  
  runners.start = c(first.up,0,0,0)      # initiate baserunner tracker
  runners.end = c(0,0,0,0)
  credit.start = c(ifelse(starter, 1,2), 0, 0, 0)    # initiate tracker for crediting baserunners to correct pitcher
  credit.end = c(0,0,0,0)
  names(runners.start) = c("AB", "1B", "2B", "3B")
  names(runners.end) = c("AB", "1B", "2B", "3B")
  names(credit.start) = c("AB", "1B", "2B", "3B")
  names(credit.end) = c("AB", "1B", "2B", "3B")
  runner.tracker = t(data.frame(runners.end))     # runner.tracker is only for diagnostic purposes
  row.names(runner.tracker) = NULL
  
  while(state < 25){
    runner.tracker = rbind(runner.tracker, runners.start)
    p.index = ifelse(starter, 1, 2)

    # estimate event probability factors (baserunning event, pitching event, batting event) given state and players involved
    BR.probs.runners = lineup.BR.probs[runners.start[2:4]]      # for runners on base, these are the probabilities they will be involved in a baserunning event (given that they've reached base)
    BR.prob.select = ifelse(length(BR.probs.runners) == 0, 0, mean(BR.probs.runners))    # chose to use average probability, due to significant understatement of steal events for most
    BR.prob.MLB = baserunning.event.prob
    BR.prob.factor = BR.prob.select / BR.prob.MLB  
    
    PE.prob.pitcher = pitching.event.prob
    PE.prob.MLB = pitching.event.prob
    PE.prob.factor = PE.prob.pitcher / PE.prob.MLB
    
    bat.prob.MLB = as.vector(t(bat.probs))
    bat.prob.player = as.vector(t(lineup.bat.probs[bat.pos,]))   
    bat.prob.pitcher = if(starter) {as.vector(t(bat.probs.by.pitcher[starterID,]))} else {bat.prob.MLB}   # if reliever, set to MLB average...this will result in batter probs being used as-is
    bat.prob.select = log5(B=bat.prob.player, P=bat.prob.pitcher, L=bat.prob.MLB)   # can't apply log5 calc directly to state-specific probabilities, which are not comparable (e.g. hitter and pitcher could both have high BB rates, but if state implies VERY high MLB BB rate, that rate would be dialed back rather than up)
    bat.prob.factors = bat.prob.select / bat.prob.MLB
    
    # adjust outcome probablities
    prob.outcome = play.probs[state,]       # outcome probabilities given state, initially set to MLB averages
    prob.outcome[baserunning.events] = prob.outcome[baserunning.events] * BR.prob.factor    # adjust baserunning event probabilities
    prob.outcome[pitching.events] = prob.outcome[pitching.events] * PE.prob.factor          # adjust pitching event probabilities
    prob.hitting.event = 1 - sum(prob.outcome[c(baserunning.events, pitching.events)])      # calc probability of NO baserunning or pitching event (therefore: hitting event)
    adj.hitting.probs.by.state = prob.outcome[row.names(bat.probs)] * bat.prob.factors    # MLB batting event probs for state * matchup-specific bat probs factors (implicitly taken across all states)
    adj.hitting.probs.by.state = adj.hitting.probs.by.state / sum(adj.hitting.probs.by.state, na.rm=TRUE)  # normalize to sum to 1
    prob.outcome[row.names(bat.probs)] = adj.hitting.probs.by.state * prob.hitting.event  # multiply by probability of any hitting event

    # simulate play
    sim.outcome = sample(outcomes, 1, prob = prob.outcome)
    nonbat.flag = sim.outcome %in% nonbat.events
    
    # record batting and pitching stats
    if(!nonbat.flag) {
      player.stats[bat.pos, sim.outcome] = player.stats[bat.pos, sim.outcome] + 1
      opp.pitcher.stats[p.index, sim.outcome] = opp.pitcher.stats[p.index, sim.outcome] + 1
      opp.pitcher.stats[p.index, "BF"] = opp.pitcher.stats[p.index, "BF"] + 1
    }
    
    # given play, simulate new state
    prob.state = P.matrix[state,,sim.outcome]
    state.new = sample(1:25, 1, prob = prob.state)
    
    # given old and new state, record outs recorded by pitcher
    outs.start = as.numeric(substr(states[state], 5, 5))
    state.nchar = nchar(states[state.new])
    outs.end = as.numeric(substr(states[state.new], state.nchar, state.nchar))    # final state is "3" and different length than "000 0" et al
    opp.pitcher.stats[p.index, "Outs"] = opp.pitcher.stats[p.index, "Outs"] + (outs.end - outs.start)
    
    # given play and new state, simulate runner destinations
    prob.dest = dest.probs[state, state.new, sim.outcome,]
    sim.dest = sample(destinations, 1, prob = prob.dest)
    sim.dest.index = as.numeric(strsplit(sim.dest, "")[[1]])
    
    # advance each runner as required / record runs scored
    if(runners.start[1] > 0) {                                              # if there is a runner in this position
      if(sim.dest.index[1] > 0 & sim.dest.index[1] < 4) {                   # if batter/runner ends up on 1B/2B/3B
        runners.end[sim.dest.index[1]+1] = runners.start[1]                 # assign his batting order number to that base
        credit.end[sim.dest.index[1]+1] = credit.start[1]                   # assign pitcher credit to that base (1=starter, 2=pen)
      } else if(sim.dest.index[1] >= 4) {                                   # if batter/runner ends up at home
        player.stats[runners.start[1], "R"] = player.stats[runners.start[1], "R"] + 1     # increase his run total by 1
        opp.pitcher.stats[credit.start[1], "R"] = opp.pitcher.stats[credit.start[1], "R"] + 1           # increase pitcher runs allowed by 1
      } 
    }

    if(runners.start[2] > 0) {                                              # if there is a runner in this position
      if(sim.dest.index[2] > 0 & sim.dest.index[2] < 4) {                   # if batter/runner ends up on 1B/2B/3B
        runners.end[sim.dest.index[2]+1] = runners.start[2]                 # assign his batting order number to that base
        credit.end[sim.dest.index[2]+1] = credit.start[2]                   # assign pitcher credit to that base (1=starter, 2=pen)
      } else if(sim.dest.index[2] >= 4) {                                   # if batter/runner ends up at home
        player.stats[runners.start[2], "R"] = player.stats[runners.start[2], "R"] + 1     # increase his run total by 1
        opp.pitcher.stats[credit.start[2], "R"] = opp.pitcher.stats[credit.start[2], "R"] + 1           # increase pitcher runs allowed by 1
      }   
    }
    
    if(runners.start[3] > 0) {                                              # if there is a runner in this position
      if(sim.dest.index[3] > 0 & sim.dest.index[3] < 4) {                   # if batter/runner ends up on 1B/2B/3B
        runners.end[sim.dest.index[3]+1] = runners.start[3]                 # assign his batting order number to that base
        credit.end[sim.dest.index[3]+1] = credit.start[3]                   # assign pitcher credit to that base (1=starter, 2=pen)
      } else if(sim.dest.index[3] >= 4) {                                   # if batter/runner ends up at home
        player.stats[runners.start[3], "R"] = player.stats[runners.start[3], "R"] + 1     # increase his run total by 1
        opp.pitcher.stats[credit.start[3], "R"] = opp.pitcher.stats[credit.start[3], "R"] + 1           # increase pitcher runs allowed by 1
      }   
    }
    
    if(runners.start[4] > 0) {                                              # if there is a runner in this position
      if(sim.dest.index[4] > 0 & sim.dest.index[4] < 4) {                   # if batter/runner ends up on 1B/2B/3B
        runners.end[sim.dest.index[4]+1] = runners.start[4]                 # assign his batting order number to that base
        credit.end[sim.dest.index[4]+1] = credit.start[4]                   # assign pitcher credit to that base (1=starter, 2=pen)
      } else if(sim.dest.index[4] >= 4) {                                   # if batter/runner ends up at home
        player.stats[runners.start[4], "R"] = player.stats[runners.start[4], "R"] + 1     # increase his run total by 1
        opp.pitcher.stats[credit.start[4], "R"] = opp.pitcher.stats[credit.start[4], "R"] + 1           # increase pitcher runs allowed by 1
      }   
    }

    # record SBs and CSs
    if(sim.outcome == '4'){
      for (i in 2:4){
        if(runners.end[i] != runners.start[i]){
          player.stats[runners.start[i], '4'] = player.stats[runners.start[i], '4'] + 1
        }
      }
    }
    
    if(sim.outcome == '6'){
      for (i in 2:4){
        if(runners.end[i] != runners.start[i]){
          player.stats[runners.start[i], '6'] = player.stats[runners.start[i], '6'] + 1
        }
      }
    }  

    # record RBIs
    prob.RBI = RBI.probs[state, state.new, sim.outcome, sim.dest,]
    sim.RBI = sample(0:4, 1, prob = prob.RBI)
    player.stats[bat.pos, "RBI"] = player.stats[bat.pos, "RBI"] + sim.RBI
    
    # update records for diagnostics
    state.record = c(state.record, states[state.new])   
    play.record = c(play.record, sim.outcome)        
    dest.record = c(dest.record, sim.dest)
    
    # if starter still in, check for survival
    if(starter){
      df = data.frame(BF = pitcher.stats[p.index, "BF"],
                      END_INNING = ifelse(state.new == '3', 1, 0),
                      RUNS_ALLOW = pitcher.stats[p.index, "R"],
                      BB_ALLOW = pitcher.stats[p.index, '14'] + pitcher.stats[p.index, '15'],
                      INN8_FLAG = ifelse(state.new == '3' & inning == 7, 1, 0),
                      NO_RUNS = ifelse(pitcher.stats[p.index, "R"] == 0, 1, 0))
      prob.relieved = predict(mod.pitcher.survival, newdata = df, type="response")
      starter = runif(1) > prob.relieved
      starter = ifelse(state.new == '3' & inning == 9, FALSE, starter)   # override relief calc and remove pitcher at end of 9 regardless
    }
    
    # update runs and roll game forward
    runs = runs + R[state, state.new] + 
                  ifelse(nonbat.flag & state.new != 25, -1, 0)  # R matrix assumes a batting event, so must subtract 1 when is non-batting (i.e. batter remains at home) UNLESS play results in 3rd out
    state = state.new
    bat.pos = ifelse(nonbat.flag, bat.pos,
                  ifelse(bat.pos == 9, 1, bat.pos + 1))
    runners.start = runners.end
    runners.end = c(0,0,0,0)
    runners.start[1] = bat.pos
    
    credit.start = credit.end
    credit.end = c(0,0,0,0)
    credit.start[1] = ifelse(starter, 1, 2)
  }

  if(diagnostics) {
    print(data.frame(plays = c("",play.record), states = state.record, dests = c("", dest.record)))   
    print(runner.tracker)
  }
  
  list(runs = runs, 
       bat.pos = bat.pos, 
       player.stats = player.stats, 
       opp.pitcher.stats = opp.pitcher.stats,
       starter = starter)
}


############################################################################
# 7) function to simulate a full game
############################################################################

simulate.game = function(innings = 9, lineup.A, lineup.H, diagnostics = FALSE){
  runs.A = NULL
  runs.H = NULL
  
  bat.A = 1
  bat.H = 1
  
  inning = 1
  game.over = FALSE
  outcomes = colnames(play.probs)
  
  player.stats.A = data.frame(matrix(0, nrow=9, ncol=length(outcomes)+3))
  player.stats.H = data.frame(matrix(0, nrow=9, ncol=length(outcomes)+3))
  colnames(player.stats.A) = c(colnames(play.probs), "R", "RBI", "SB")
  colnames(player.stats.H) = c(colnames(play.probs), "R", "RBI", "SB")
  row.names(player.stats.A) = lineup.A
  row.names(player.stats.H) = lineup.H
  
  pitcher.stats.H = data.frame(matrix(0, nrow=2, ncol=length(outcomes)+3))
  pitcher.stats.A = data.frame(matrix(0, nrow=2, ncol=length(outcomes)+3))
  colnames(pitcher.stats.H) = c(colnames(play.probs), "BF", "R", "Outs")
  colnames(pitcher.stats.A) = c(colnames(play.probs), "BF", "R", "Outs")
  
  starter.A = TRUE      # initialize both flags to indicate starting pitchers are in
  starter.H = TRUE
  
  while(game.over == FALSE) {
    outcome.A = simulate.half.inning.P(first.up = bat.A, lineup = lineup.A, starter = starter.H, pitcher.stats = pitcher.stats.H, inning = inning, diagnostics = diagnostics)
    outcome.H = simulate.half.inning.P(first.up = bat.H, lineup = lineup.H, starter = starter.A, pitcher.stats = pitcher.stats.A, inning = inning, diagnostics = diagnostics)
    
    starter.H = outcome.A$starter     # update starter flags based on whether they survived half inning
    starter.A = outcome.H$starter     # nothing to do with this here, but need to pass back to subsequent half inning calls
    
    runs.A = c(runs.A, outcome.A$runs)
    player.stats.A = player.stats.A + outcome.A$player.stats
    pitcher.stats.H = pitcher.stats.H + outcome.A$opp.pitcher.stats
    
    if(inning == innings & sum(runs.H) > sum(runs.A)){         # check in last inning whether bottom needs to be played
      runs.H = c(runs.H, NA)
    } else {
      runs.H = c(runs.H, outcome.H$runs)
      player.stats.H = player.stats.H + outcome.H$player.stats
      pitcher.stats.A = pitcher.stats.A + outcome.H$opp.pitcher.stats
    }
    
    if(inning >= innings){        # check to see whether game should end
      if(sum(runs.H, na.rm=TRUE) != sum(runs.A)) {
        game.over = TRUE
      }
    }  
    
    bat.A = outcome.A$bat.pos
    bat.H = outcome.H$bat.pos
    
    inning = inning + 1
  }
  
  inning.scores = data.frame(A = runs.A, H = runs.H)
  final.score = data.frame(A = sum(runs.A), H = sum(runs.H, na.rm=TRUE))
  list(inning.scores = inning.scores, 
        final.score = final.score, 
        player.stats.A = player.stats.A, 
        player.stats.H = player.stats.H,
        pitcher.stats.A = pitcher.stats.A,
        pitcher.stats.H = pitcher.stats.H)
}

lineup.test = c('lindf001','branm003','ramij003','encae001','alony001','cabrm002','kipnj001','gomey001','alleg002')

tic()
simulate.game(lineup.A = lineup.test, lineup.H = lineup.test)
toc()

############################################################################
# 8) function to create boxscore format
############################################################################

boxscore = function(sim.res){
  inning.scores = sim.res$inning.scores
  final.score = sim.res$final.score
  player.stats.A = sim.res$player.stats.A
  player.stats.H = sim.res$player.stats.H
  pitcher.stats.A = sim.res$pitcher.stats.A
  pitcher.stats.H = sim.res$pitcher.stats.H
  
  line.score = matrix(t(inning.scores),nrow=2)
  line.score = cbind(line.score, t(final.score))
  colnames(line.score) = c(1:nrow(inning.scores), 'F')

  PA.types = c('2','3','18','19','20','21','22','23','14','15','16','17')  
  AB.types = c('2','3','18','19','20','21','22','23')
  H.types = c('20','21','22','23')
  
  player.stats.A$PA = rowSums(player.stats.A[,PA.types])
  player.stats.A$AB = rowSums(player.stats.A[,AB.types])
  player.stats.A$H = rowSums(player.stats.A[,H.types])
  player.stats.H$PA = rowSums(player.stats.H[,PA.types])
  player.stats.H$AB = rowSums(player.stats.H[,AB.types])
  player.stats.H$H = rowSums(player.stats.H[,H.types])
  
  pitcher.stats.A$H = rowSums(pitcher.stats.A[,H.types])
  pitcher.stats.H$H = rowSums(pitcher.stats.H[,H.types])
  
  box.score.A = data.frame(Player = playerID[row.names(player.stats.A), "Last"],
                           AB = player.stats.A$AB,
                           R = player.stats.A$R,
                           H = player.stats.A$H,
                           RBI = player.stats.A$RBI,
                           BB = player.stats.A$'14'+player.stats.A$'15',
                           SO = player.stats.A$'3')
  totals.A = c(NA, colSums(box.score.A[,-1]))
  box.score.A = rbind(box.score.A, totals.A)

  pitcher.line.A = data.frame(Player =  c('starter', 'bullpen'),
                              IP = round(pitcher.stats.A$Outs / 3, 2),
                              H = pitcher.stats.A$H,
                              R = pitcher.stats.A$R,
                              ER = pitcher.stats.A$R,
                              BB = pitcher.stats.A$'14'+pitcher.stats.A$'15',
                              K = pitcher.stats.A$'3')
  
  box.score.H = data.frame(Player = playerID[row.names(player.stats.H), "Last"],
                           AB = player.stats.H$AB,
                           R = player.stats.H$R,
                           H = player.stats.H$H,
                           RBI = player.stats.H$RBI,
                           BB = player.stats.H$'14'+player.stats.H$'15',
                           SO = player.stats.H$'3')
  
  totals.H = c(NA, colSums(box.score.H[,-1]))
  box.score.H = rbind(box.score.H, totals.H)
  
  pitcher.line.H = data.frame(Player = c('starter', 'bullpen'),
                              IP = round(pitcher.stats.H$Outs / 3, 2),
                              H = pitcher.stats.H$H,
                              R = pitcher.stats.H$R,
                              ER = pitcher.stats.H$R,
                              BB = pitcher.stats.H$'14'+pitcher.stats.H$'15',
                              K = pitcher.stats.H$'3')
  
  print(kable(line.score), format='pandoc')
  print(kable(box.score.A, format='pandoc', caption='Away team'))
  print(kable(pitcher.line.A, format='pandoc'))
  print(kable(box.score.H, format='pandoc', caption='Home team'))
  print(kable(pitcher.line.H, format='pandoc'))
}

lineup.CLE2018 = c('lindf001','branm003','ramij003','encae001','alony001','cabrm002','kipnj001','gomey001','alleg002')
lineup.PHI2018 = c('hernc005','hoskr001','herro001','santc002','willn001','kings001','alfaj002','franm004','altha001')

boxscore(simulate.game(lineup.A = lineup.PHI2018, lineup.H = lineup.CLE2018))


############################################################################
# 9) function to apply DraftKings scoring to simulated stats
############################################################################

DraftKings = function(sim.res){
  player.stats.A = sim.res$player.stats.A
  player.stats.H = sim.res$player.stats.H
  pitcher.stats.A = sim.res$pitcher.stats.A
  pitcher.stats.H = sim.res$pitcher.stats.H
  
  inning.scores = sim.res$inning.scores
  inning.scores[is.na(inning.scores)] = 0     # cumsum has no na.rm, so need to insert 0 for bottom 9th if home team did not bat
  game.length = nrow(inning.scores)
  running.scores = cumsum(inning.scores)
  running.scores = rbind(running.scores, running.scores[game.length,])   # add extra row to avoid erroring out when performing lead checks below
  final.score = sim.res$final.score

  win.check.H = FALSE
  win.check.A = FALSE
  
  DK.batting = c(x1B = 3, x2B = 5, x3B = 8, HR = 10, RBI = 2, R = 2, BB = 2, HBP = 2, SB = 5)
  DK.pitching = c(IP = 2.25, K = 2, W = 4, ER = -2, H = -0.6, BB = -0.6, HBP = -0.6, CG = 2.5, SHO = 2.5, NH = 5)
                 
  # determine whether either starter should be credited with a win
  if(final.score[1,'H'] > final.score[1,'A']){
    final.inning = max(floor(pitcher.stats.H[1,'Outs']/3), 1)    # min of 1 doesn't impact calc but avoids using 0 as array index, which can cause error
    lead.when.exit = running.scores[final.inning, 'H'] > running.scores[final.inning, 'A']    # for home pitcher, what matters is he has lead at end of inning (e.g. could leave after 6 losing, but if team takes (and maintains) lead in bottom of 6th, he will get credit for W)
    lead.after.exit = all(running.scores[final.inning:game.length, 'H'] > running.scores[(final.inning+1):(game.length+1), 'A'])  # compares score after top of each subsequent inning to home score after bottom of previous, returns TRUE when home team has lead
    at.least.5IP = pitcher.stats.H[1, 'Outs']/3 >= 5
    win.check.H = lead.when.exit & lead.after.exit & at.least.5IP
  }
  
  if(final.score[1,'A'] > final.score[1,'H']){
    final.inning = max(floor(pitcher.stats.A[1,'Outs']/3), 1)    # min of 1 doesn't impact calc but avoids using 0 as array index, which can cause error
    lead.when.exit = running.scores[final.inning+1, 'A'] > running.scores[final.inning, 'H']    # for away pitcher, what matters is he has lead at middle of next inning (e.g. could leave after 6 losing, but if team takes (and maintains) lead in top of 7th, he will get credit for W)
    lead.after.exit = all(running.scores[(final.inning+1):game.length, 'A'] > running.scores[(final.inning+1):game.length, 'H'])  # compares scores after bottom of each subsequent inning, returns TRUE when away team has lead
    at.least.5IP = pitcher.stats.A[1, 'Outs']/3 >= 5
    win.check.A = lead.when.exit & lead.after.exit & at.least.5IP
  }
  
  player.DFS.A = data.frame(x1B = player.stats.A[,'20'],
                            x2B = player.stats.A[,'21'],
                            x3B = player.stats.A[,'22'],
                            HR = player.stats.A[,'23'],
                            RBI = player.stats.A[,'RBI'],
                            R = player.stats.A[,'R'],
                            BB = player.stats.A[,'14'] + player.stats.A[,'15'],
                            HBP = player.stats.A[,'16'],
                            SB = player.stats.A[,4])
  
  player.DFS.H = data.frame(x1B = player.stats.H[,'20'],
                            x2B = player.stats.H[,'21'],
                            x3B = player.stats.H[,'22'],
                            HR = player.stats.H[,'23'],
                            RBI = player.stats.H[,'RBI'],
                            R = player.stats.H[,'R'],
                            BB = player.stats.H[,'14'] + player.stats.H[,'15'],
                            HBP = player.stats.H[,'16'],
                            SB = player.stats.H[,4])
  
  pitcher.DFS.A = data.frame(IP = pitcher.stats.A[1,'Outs'] / 3,
                               K = pitcher.stats.A[1,'3'],
                               W = ifelse(win.check.A, 1, 0),
                               ER = pitcher.stats.A[1,'R'],
                               H = pitcher.stats.A[1,'20']+pitcher.stats.A[1,'21']+pitcher.stats.A[1,'22']+pitcher.stats.A[1,'23'],
                               BB = pitcher.stats.A[1,'14']+pitcher.stats.A[1,'15'],
                               HBP = pitcher.stats.A[1,'16'])
  
  pitcher.DFS.A = pitcher.DFS.A %>% mutate(CG = ifelse(IP == 9 & W == 1, 1, 0),    # does not contemplate the possibility that away pitcher throws an 8IP CG loss (https://www.baseball-reference.com/bullpen/Complete_game)
                                           SHO = ifelse(CG == 1 & ER == 0, 1, 0),
                                           NH = ifelse(CG == 1 & H == 0, 1, 0))
  
  pitcher.DFS.H = data.frame(IP = pitcher.stats.H[1,'Outs'] / 3,
                               K = pitcher.stats.H[1,'3'],
                               W = ifelse(win.check.H, 1, 0),
                               ER = pitcher.stats.H[1,'R'],
                               H = pitcher.stats.H[1,'20']+pitcher.stats.H[1,'21']+pitcher.stats.H[1,'22']+pitcher.stats.H[1,'23'],
                               BB = pitcher.stats.H[1,'14']+pitcher.stats.H[1,'15'],
                               HBP = pitcher.stats.H[1,'16'])
  
  pitcher.DFS.H = pitcher.DFS.H %>% mutate(CG = ifelse(IP == 9 & W == 1, 1, 0),    
                                           SHO = ifelse(CG == 1 & ER == 0, 1, 0),
                                           NH = ifelse(CG == 1 & H == 0, 1, 0))
  
  player.DFS.A = as.matrix(player.DFS.A) %*% as.matrix(DK.batting)
  player.DFS.H = as.matrix(player.DFS.H) %*% as.matrix(DK.batting)
  pitcher.DFS.A = as.matrix(pitcher.DFS.A) %*% as.matrix(DK.pitching)
  pitcher.DFS.H = as.matrix(pitcher.DFS.H) %*% as.matrix(DK.pitching)
  
  list(batting.A = player.DFS.A, batting.H = player.DFS.H, pitching.A = pitcher.DFS.A, pitching.H = pitcher.DFS.H)
  
}

tic()
DraftKings(simulate.game(lineup.A = lineup.PHI2018, lineup.H = lineup.CLE2018))
toc()

