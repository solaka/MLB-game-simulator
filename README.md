# MLB-game-simulator
<img src='https://i.sportsbookreview.com/5759896c722866065c5e19fa/original-savermets.jpg?d=950x950'>

## Introduction
If you've played any daily fantasy sports (DFS) contests, you probably know there are a few different contest formats, each of which demands a different lineup strategy. (If you're NOT familiar with DFS, here's a [primer](https://www.dailyfantasysports101.com/basics/).) In a "double-up" format, for example, the ~44% of entrants with the highest scores double their money. The strategy here is relatively straightforward: draft a lineup that maximizes the expected points scored, given salary and position constraints. (Technically, the right strategy would be 'draft a lineup that maximizes the probability that the final score will be in the top 44% of entrants, but in most cases that's probably not too different and more complicated to model.)

In a GPP contest, only those entries scoring in the top 20% or so receive any payout, and the REAL money is concentrated among the top few entries out of possibly thousands. This complicates things significantly, since now our strategy must consider not only expected points scored by player, but also:
* the volatility (e.g. variance) of each player's point distribution
* the relationships (e.g. correlation) between pairs of players' point distributions
* the payout structure
* ideally, the frequency with which each player is found on our competitors' rosters

There are a few papers out there that address GPP roster optimization. [Hunter, Vielma, and Zaman (2019)](https://arxiv.org/abs/1604.01455) show how to use integer programming to optimize NHL and MLB rosters for GPP contests, and also gave a [related presentation](http://www.sloansportsconference.com/content/winning-at-daily-fantasy-sports-using-analytics/) at the 2016 Sloan Sports Analytics Conference. The paper provides a good framework for solving this problem, but ultimately they take some serious shortcuts. First and foremost, the objective is to maximize expected points scored, subject to a number of heuristic constraints. There's no explicity contemplation of variability or correlations. [Newell (2017)](http://krex.k-state.edu/dspace/handle/2097/35393) wrote her thesis on the topic, and impressively was able to incorporate the actual payout structure into a stochastic integer program. However, she also assumes that player scores are entirely independent.

Recognizing the need to get my arms around the variance and covariance of player scores if I was going to optimize GPP lineups, I built this simulation model for MLB games.  It's hardly the first of its kind, though I'm not aware of one that facilitates an analysis of DFS scoring in this way.  The fact that the focus is on DFS permits a few shortcuts (e.g. ABs are slightly misstated because I don't track sacrifice flies), but at some point I may go back and address those, if only to produce even more realistic-looking boxscores.

You might ask: why bother with a simulation model, when you could just look up actual DFS scores for these players and calculate variances and covariances?  You could, but there would be a lot of noise, and a small sample size.  Batters come and go, move around in the lineup, and face different types and levels of pitchers.  And even if things were completely stable, you'd only have 162 observations to work with.  A good simulation model -- one that correctly reflects event probabilities and the mechanics of a baseball game -- allows us to look at any lineup configuration with any set of players, and to examine the results of thousands of trials.

## Data
#### Data acquisition
The R file “download and parse retrosheet play-by-play files.R”, adapted from code from the previously mentioned blog posts by Jim Albert, downloads and unzips event files from retrosheet.org and converts them to csv files.  For this study, I used only data from the 2018 regular season.

The R file “process retrosheet data and write to csv.R” uses code from Jim Albert’s GitHub site  (with some small modifications) to add base-out states and runs scored to the Retrosheet event data frame, then saves them as csv files.

I also use data on an ad hoc basis from baseball-reference.com in order to validate model output, look at the most common lineups by team, etc.

#### Data prep
Before simulating, I preprocessed some of the data in a separate R script (“player specific play probabilities.R”), which derives the player-specific event probabilities that are used by the simulation model.  I restricted the analysis to batters with at least 250 plate appearances (PA) in 2018, and pitchers with at least 80 innings pitched (IP).

For each batter meeting the PA threshold, I calculated:
* the probability of each type of batting event, given that a batting event occurs
* the probability of a baserunning event (e.g. stolen base, pickoff) of any type being the next event to occur, given that the player is on base

For each pitcher meeting the IP threshold, I calculated:
* the probability of each type of batting event, given that a batting event occurs
* the probability of a pitching event (e.g. wild pitch, balk) of any type being the next event to occur

In the simulation model itself, I also calculate the MLB averages that parallel each of these player-specific probabilities.  These are league averages are ultimately used to adjust event probabilities by opening base-out state, or alternately to plug generic MLB players into the lineup.

In all cases, probabilities are based on the empirical event frequencies.  Events are categorized by type (batting, baserunning, pitching) because the ways in which each set of probabilities are used in the simulation model differs somewhat.

## Modeling
Games are simulated and DFS points calculated in “baseball game simulator v1.0.R”.  At the core of the simulation model is a function that simulates half-innings of the game.  Subsequent functions call this function repeatedly to simulate a full game, and then assign DFS scores to players based on the stats they’ve accrued.

#### Half-inning simulation
First, the initial conditions are established.  The opening base-out state is obviously “no outs, none on”, expressed as “000 0”, but in addition several parameters are passed: lineup slot leading off, player IDs of the lineup, starting pitcher player ID, the pitcher’s accumulated stats for the game, and the inning number.  The utility of these parameters will become clear as the model is described.

#### Event probabilities
Before simulating which event occurs, conditional event probabilities (given the current state) must be established.  The process differs slightly for each of the three event categories.  The main idea, however, is that we need a transition probability matrix that has as its dimensions opening state, closing state, and event.  Deriving such a matrix for each player would use too small a sample size, so instead I compare the frequency of higher-level events (e.g. any baserunning event, any double, etc.) to MLB averages, and use that factor to adjust state-specific probabilities.

#### Pitching events
The “raw” probability of a pitching event (wild pitch, balk, etc.) occurring is assumed to depend on the pitcher and the current state.  P(EPi) is probability that pitcher i creates any type of pitching event (EP) as the next event, and P(EPMLB) is the MLB average probability.  The ratio of these two represents how more or less likely a given pitcher is to create a pitching event than the MLB average, and is multiplied by the MLB average probability of each type of pitching event (Ep ∈ EP) given the current state s.

<img src='https://github.com/solaka/MLB-game-simulator/blob/master/equations/Eq1.gif'>

#### Baserunning events
The “raw” probability of a baserunning event occurring is assumed to be equal to the average of the player-specific probabilities for current baserunners.  Let m be the set of runners on base.  Where P(EBi) is the probability of any baserunning event (implicitly, across all base-out states) occurring given player i is on base:

<img src='https://github.com/solaka/MLB-game-simulator/blob/master/equations/Eq2.gif'>

Per the average MLB transition matrix, the probability of baserunning event b occurring given state s is P(EbsMLB).  Then the same probability given baserunner set m is estimated to be:

<img src='https://github.com/solaka/MLB-game-simulator/blob/master/equations/Eq3.gif'>

#### Batting events
Every batter has a vector of probabilities representing the rates at which each type of batting event occurred while he was at the plate.  Every pitcher has a similar vector of probabilities representing the rates at which they allowed or caused each type of batting event to occur.  A first step is to estimate event probabilities given this matchup using the log5  formula.  Let B be the rate of batting event Eb for batter i, P be the rate for pitcher j, and L be the league average rate.  The log5 formula estimates the probability of Eb in this matchup as:

<img src='https://github.com/solaka/MLB-game-simulator/blob/master/equations/Eq4.gif'>

Similar to before, the ratio of this probability to the MLB overall average probability for the same event (across all states) is used to adjust the probability of b occurring given state s:

<img src='https://github.com/solaka/MLB-game-simulator/blob/master/equations/Eq5.gif'>

The resulting vector of conditional hitting event probabilities does not necessarily sum to 1, so it is forced to do so by dividing each value by the sum of all values.

Finally, the baserunning, pitching, and batting event probabilities are combined into a single vector.  In doing so, the batting probabilities are scaled down so that their sum equals one minus the sum of the baserunning and pitching event probabilities .

#### Event simulation
Given the complete event probability vector, simulating the event (double, pick-off, wild pitch, etc.) to occur is straightforward.  Ensuring that the impact of that event is appropriately recorded, however, is somewhat more difficult.

First, the base-out state must be updated.  While some events result in a known result (e.g. a home run always results in the batter and all runners scoring), many others do not.  For example, the table to the right shows the ending state probabilities (based on the 2018 MLB season) starting from a runner on second with no outs (i.e. “010 0”), given the batter singles.  The runner might score, advance to third, stay on second, or be put out.  The batter could reach first, advance further on a throw on an error, or be caught out trying to advance.  For this reason, the ending state is simulated based on a vector of such conditional probabilities from the MLB average transition matrix, given the opening state and the event simulated to have occurred.

While this step gives us the new state, it doesn’t necessarily tell us what happened to the batter and each runner.  For example, from the opening state of bases loaded and no outs -- “111 0” –assume a “generic out” is recorded, resulting in a new state of “111 1”.  From this, it’s not clear if the lead runner was forced out at home or if the batter was retired on a line drive or similar.  Fortunately, the Retrosheet data tracks runners on the basepaths, so I was able to create a multi-dimensional contingency table (opening state, closing state, event ID, runner destinations) that yields the observed probabilities necessary to record this.  In this example, the batter was retired about 69.7% of the time and the lead runner was forced out about 30.3% of the time over the 2018 MLB season.

#### Game statistics
Crediting some statistics to players is straightforward.  For example, when a double is simulated to have occurred, it’s easy to credit the batter with a double and the pitcher with a hit allowed.  Runs, stolen bases, etc. are not too difficult, as they end up being a product of the baserunner tracking discussed previously.

RBIs are more problematic.  For example, from an opening state of “001 0”, the batter records a generic out, moving the state to “000 1”.  Was the batter credited with an RBI?  Over the 2018 season, he was 98.7% of the time, but 1.3% of the time he was not…perhaps because the runner on third scored on an error that occurred after the batter was retired.  For this reason, another multidimensional contingency table is constructed that yields probabilities of the possible RBI counts credited to the batter given opening state, closing state, event, and runner destinations.  These probabilities are then used to stochastically determine the RBI assigned to the batter, given the specifics of the situation.

#### Pitcher survival
For DFS purposes, we care most about the starting pitchers since relievers are rarely (if ever) worth drafting.  The starting pitcher’s statistics (and DFS score) depend heavily on how long he remains in the game, particularly when DFS points are credited for innings pitched.  As such, an important feature to incorporate is a probabilistic survival model, where after each batter faced there is assumed to be a chance that the pitcher will be relieved.

I started by using the following variables as predictors, where the numeric features are rolling totals of statistics accrued by the pitcher during the game:
 
* Batters faced
* Innings pitched (i.e. outs recorded/3)
* Hits allowed
* Walks allowed
* Runs allowed
* End of inning flag
* Inning 8 flag
* Inning 9 flag
* No hits allowed flag
* No runs allowed flag
 
The target variable is a factor indicating whether or not the pitcher survives the previous batter to pitch to another, making this is a classification model.  However, I was most interested in the estimated probability of survival.  I looked at a random forest model, but felt there were too many cases where the model had the pitcher leaving with 100% certainty, simply because that’s what happened in the limited instances over the 2018 season.  Instead, I chose a logistic model.  Splitting the data into training and test sets with a 75/25 proportion, I used step-wise selection (both directions) to find candidate models.  I repeated the process both including and excluding all possible 2-way interactions, and also using both AIC and BIC as criteria.

The model that yielded the best results (per test MSE) is summarized below.

<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/equations/pitcher%20survival%20model%20summary.gif'>

The model is fit and documented in the file “pitcher duration model.R”.

In the simulation model, when the starting pitcher is determined to have left the game, he is relieved by a generic, MLB-average replacement.  Although in reality that reliever may himself be relieved, no effort is made to model that here, since the assumption is made that all relievers are at the MLB average level. 

#### Full game simulation
Most of the heavy lifting is performed by the half-inning simulation function.  A complete game is simulated by another function, which repeatedly calls the half-inning function until the game is deemed to be complete.  The full game function performs the following roles:

* Accumulate game statistics
* Track which batter leads off the next inning
* Track whether the starting pitcher is still in the game
* Track scores by inning
* Determine when the game is over

#### DFS scoring
Finally, given the cumulative simulated game statistics, DFS scores must be assigned to players, which is performed by another function.  Assigning points is straightforward for most stats, for example multiplying total singles by DFS points awarded per single.  However, the function also checks to see whether either pitcher should be credited with a win, which for the purpose of this model occurs if the starter was still in immediately before his team permanently took the lead, and pitched at least five innings.  The function also checks whether points should be awarded for a complete game, shutout, or no-hitter.  The current code uses DraftKings scoring, but another site's system could be easily substituted.

## Validation
To ensure that the model produces reasonable results, I first simulated 50,000 games between two teams comprised of generic MLB-average hitters and pitchers.  If the model works as intended, these players should (in total) accumulate statistics at a rate comparable to the overall league-wide average.  The table below compares simulated per game averages to the observed 2018 MLB average totals per game.
 
<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/tables/validation%20table%201.gif'>

Most of the differences are small and explainable.  One isn't, and that is explained below.

* Plate appearances (PA) are slightly higher than actual, probably because the simulation matches two identical teams, meaning that games in this test are more likely to go to extra innings than in reality.  Note that most other events that are simulated directly (e.g. H, HR, BB) are high by a similar degree.
* At bats (AB) are a little higher than this, which is probably attributable to the fact that the model does not attempt to distinguish between sacrifice events (flies and bunts) with regards to crediting ABs, so the simulated total is slightly inflated.  This has no bearing on DFS point totals, however.
* Simulated triples (3B) occur 3.1% more often than in reality.  Given how well-behaved other stats are and that triples are simulated in the exact same way, this seems likely due to their scarcity, and therefore simulation error.
* Runs (R) and RBIs are higher by 1.7%.  While doubles, HRs, etc. are directly simulated from observed frequencies, runs and RBIs are a product of the simulated mechanics of a baseball game.  Besides the additional uncertainty inherent with that, it may be that a perfectly balanced lineup comprised of all MLB-average players is very slightly more productive than a lineup consisting of good and bad players, who are average in the aggregate.

Stolen bases (SB) and caught stealing (CS) show the largest discrepancies.  The reason for the differences is that in the Retrosheet data, when a SB or CS occurs on the same play as a strikeout or a walk, the play is recorded as the latter (i.e. the batting event).  Therefore, the SB and CS event probabilities used in the current model are understated.  Fortunately, the understatement on SB, while a concern, is not fatally large.  CS is not a point scoring event in DraftKings scoring.  Also, note that the base-out state transition probabilities do contemplate the possibility of these concurrent events.  Said differently, it’s not that these types of event aren’t occurring at an appropriate rate in the mode, only that runners are not being credited with them properly when they do occur.  That said, it's an issue that will be corrected in the next version of the model.

## Variability
To set a baseline, the table below shows the mean, standard deviation, and coefficient of variation (CoV) of DK points for nine identical, MLB-average players across 50,000 simulated games.

<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/tables/variability%20table%201.gif'>

Mean points decrease fairly linearly as we move down the lineup for an obvious reason: the higher a player appears in the lineup, the more likely he is to receive an extra plate appearance.  Conversely, the coefficient of variation increases as we move lower in the lineup.  However, the reason is the same: more plate appearances leads to lower relative volatility since the sample size is larger.

Next, let's look at the 2018 Cleveland Indians.  I chose this team because their 2018 lineup was *relatively* consistent throughout the season, so distortions from players moving around in the order should be minimized.  The table below shows the mean, standard deviation, and coefficient of variation of actual and simulated outcomes of DraftKings points for players in the primary 2018 lineup. 

<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/tables/variability%20table%202.gif'>

Some differences are expected due to the randomness of the 2018 season, and largely that appears to be responsible for the differences here.  Simulated means are off by an average of -1.3%.  The only concerning result is Allen, who is a very frequent basestealer.  Allen’s mean points scored are materially understated in the model, which is due to the aforementioned limitation around how SBs that are coincidental with a batting event are registered.

Also note that modeled CoVs *tend* to be (but are not always) lower than observed.  That's because the modeled results are based on pitting the 2018 Cleveland lineup against an average MLB pitcher, while during their actual season they faced many different pitchers with many different ability levels, leading to greater volatility.

What is clear is that there is a link between the degree of volatility in DFS score and the type of player.  The players' rates for all events (e.g. K, HR, etc.) play a role, but strikeout rate appears to be the strongest driver.  The graph below illustrates the link between the rate of strikeouts per PA and each players' CoV of DFS points.  Clearly, more experimentation is needed, but it appears that a players CoV can probably be pretty well estimated from their lineup position and their expected event rates.

<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/tables/CV%20vs%20K%20rate.gif'>

## Correlation
The table below shows the matrix of Pearson correlation coefficients of DK scores between all players in a matchup of two teams comprised of generic, MLB-average players.  (I looked at Spearman rank correlation also, but differences between the two were very slight.)

<img src = 'https://github.com/solaka/MLB-game-simulator/blob/master/tables/corr%20matrix%20-%20MLB%20avg.gif'>

Lots to unpack here!

* Correlations between batters on a given team are mildly positive, with the largest coming between adjacent batters and levels growing weaker as batters grow more distant in the lineup.  Not too surprising.
* Correlations between home team batters are slightly lower than between away team batters.  Since the home team doesn't play the bottom of the 9th inning if they're ahead, home batters receive slightly fewer plate appearances, on average.  Think of it as home batters having slightly fewer opportunities to correlate themselves.
* For a similar reason, batters higher in the lineup are more negatively correlated with opposing pitchers than those lower down.
* All hitters show a relatively strong negative correlation with the opposing pitcher, which is to be expected.  The magnitude of correlation appears to decrease slightly for players lower in the lineup, possibly because they tend to receive fewer at bats and so tend to contribute less to the opposing pitcher’s (mis)fortunes.
* Home and away batters are very slightly positively correlated.  That's likely because they occasionally *all* go to extra innings together and receive extra plate appearances.
* Away batters have a slight positive correlation with their own starter, and home batters have an even smaller positive correlation with theirs.  In the former case, a good performance by the away team's batters increase the likelihood that their pitcher will receive bonus points for a win.  That's true of the home team as well, *BUT* a strong performance by the pitcher means that the game is less likely to go to extra innings, taking potential plate appearances away from home team batters.  These factors seem to largely offset.
* The starting pitchers have a small negative correlation, likely because a strong performance by Pitcher A makes it less likely that Pitcher B will receive bonus points for the win, and vice versa.

Now, many of these correlations are small, but given the importance of maximizing total variance in a GPP strategy, they might be very meaningful.  Consider 5 players with mean DFS points of 7, CoV of 1.0, and correlation of zero.  Assuming each is normally distributed, the 99th percentile result for the group would be 71.4.  Now assume pairwise correlations are 0.10 across the board.  The 99th percentile becomes 78.1.  Ultimately, the strategy should balance gains made by selecting positively correlated players (and/or avoiding negatively correlated players) with the value that each presents with regards to salary vs. expected points scored.

## Known issues and future work
There's lots to do!  Here's a list of known limitations.
* State transition probabilities and baserunner assignments are invariant to the game situation.  For example, in later innings if the game is close, defenses are more likely to try to get the lead runner than if the game is a runaway.  Steals are more likely to occur in close games than otherwise.  This may not matter to long-term DFS point distributions, but might impact a single simulated box score.
* No algorithm for determining which runs are unearned (DK only deducts points for earned).  Currently all are counted as earned, but historically about 93% of runs are earned, so not too big a distortion.
* Runs that score on the same play on which the third out is recorded in a half inning may not be counted by the model.
* If a runner is retired as the third out of a half-inning, the algorithm does not return the current batter to the top of the lineup in the subsequent inning.
* The algorithm ends the half inning once three outs have been reached.  Therefore, it does not stop early for “walk off” wins, as it should.
* Pitcher survival model seems to relieve pitchers too frequently after 8.1 or 8.2 IP.  This is a subjective observation, and needs to be examined more closely.
* Sacrifice flies and sacrifice bunts are incorrectly credited as an at bat.  This has no bearing on DFS scoring, but does impact the box score.
* Bullpens are assumed to be comprised entirely of MLB-average pitchers.  In reality, not only do the quality of relievers vary significantly, but the quality of reliever that the manager elects to use depends on individual matchups and the game situation.  At a minimum, could consider using team-specific bullpen quality here.
* Some stolen bases and caught stealing are not credited to players appropriately.  The issue, for example, is that a SB that are concurrent with a strikeout is tagged in the Retrosheet data as a strikeout (EVENT_CD = 3) rather than a stolen base (EVENT_CD = 4).  The workaround will be to use the stolen base flag fields (e.g. RUN1_SB_FL, etc.) to identify basestealers, and possibly manually create new event flags that capture these concurrent events (e.g. 34 could capture strikeout + stolen base).
* Player-specific baserunning and pitching event probabilities could potentially be defined on a more granular basis.  Currently, the algorithm looks at the probability of a runner being involved in any baserunning event while on base, and uses this to adjust MLB-wide event probabilities given the opening state. However, this doesn’t capture varying success rates, allows players who were only involved in pickoff events during the season to be simulated as having stolen a base, etc.  Only danger will be that extremely rare events (e.g. defensive indifference) could cause distortions.  Might be best to have these always take on the MLB average probability, rather than try to adjust for player experience.
* The algorithm takes the average of runner probabilities to determine the (combined) likelihood of a baserunning event.  This is a difficult dynamic to get right; a slow runner on first might not prevent a fast runner on second from stealing, but a slow runner on second would prevent a fast runner on first from doing so.
* While log5 is perfectly adequate and well-vetted, Healey (2015) and Doo and Kim (2018) explore some possible enhancements.
* Currently the model is somewhat slow, taking in the neighborhood of 30 minutes to run 10,000 sims.
* The model is more descriptive than predictive, using empirical event frequencies from the 2018 season as a basis.  While this is fine for MLB averages and also for gaining insight into variance and covariance, it should not be used to predict individual player performance directly.  To do so, there should be a separate predictive model that establishes these probabilities based on recent and long-term player performance, opposing pitcher, and other factors.

## References
Doo, W. and Kim, H. (2018).  Modeling the probability of a batter/pitcher matchup event: A Bayesian approach.  PLoS One, 13(10).

Evans, B. A., Roush, J., Pitts, J. D., & Hornby, A. (2018). Evidence of Skill and Strategy in Daily Fantasy Basketball. Journal of Gambling Studies,34(3), 757-771.

Healey, G. (2015). Modeling the Probability of a Strikeout for a Batter/Pitcher Matchup. IEEE Transactions on Knowledge and Data Engineering, 27(9), 2415-2423. 

Hunter, D., Vielma, J.P., and Zaman, T. (2016).  Picking Winners in Daily Fantasy Sports Using Integer Programming.  Submitted to INFORMS Journal on Optimization.

Marchi, M. and Albert, J. 2014. Analyzing Baseball Data with R. Boca Raton, Fla.: CRC Press.

Newell, S., Easton, T. (2017).  Optimizing Tiered Daily Fantasy Sports – Mathematically Modeling DraftKings NFL Millionaire Maker Tournament.  Proceedings of the 2017 Industrial and Systems Engineering Conference.

Winston, W. 2012. Mathletics. Princeton, NJ.: Princeton Press.
