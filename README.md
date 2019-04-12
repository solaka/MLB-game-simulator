# MLB-game-simulator
<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/D7K_4966_Roy_Halladay.jpg/800px-D7K_4966_Roy_Halladay.jpg' width="520" height="800">

## Introduction
If you've played any daily fantasy sports (DFS) contests, you probably know there are a few different contest formats, each of which demands a different lineup strategy. (If you're NOT familiar with DFS, here's a [primer](https://www.dailyfantasysports101.com/basics/).) In a "double-up" format, for example, the ~44% of entrants with the highest scores double their money. The strategy here is relatively straightforward: draft a lineup that maximizes the expected points scored, given salary and position constraints. (Technically, the right strategy would be 'draft a lineup that maximizes the probability that the final score will be in the top 44% of entrants, but in most cases that's probably not too different and more complicated to model.)

In a GPP contest, only those entries scoring in the top 20% or so receive any payout, and the REAL money is concentrated among the top few entries out of possibly thousands. This complicates things significantly, since now our strategy must consider not only expected points scored by player, but also:
* the volatility (e.g. variance) of each player's point distribution
* the relationships (e.g. correlation) between pairs of players' point distributions
* the payout structure
* arguably, the frequency with which each player is found on our competitors' rosters

There are a few papers out there that address GPP roster optimization. [Hunter, Vielma, and Zaman (2019)](https://arxiv.org/abs/1604.01455) show how to use integer programming to optimize NHL and MLB rosters for GPP contests, and also gave a [related presentation](http://www.sloansportsconference.com/content/winning-at-daily-fantasy-sports-using-analytics/) at the 2016 Sloan Sports Analytics Conference. The paper provides a good framework for solving this problem, but ultimately they take some serious shortcuts. First and foremost, the objective is to maximize expected points scored, subject to a number of heuristic constraints. There's no explicity contemplation of variability or correlations. [Newell (2017)](http://krex.k-state.edu/dspace/handle/2097/35393) wrote her thesis on the topic, and impressively was able to incorporate the actual payout structure into a stochastic integer program. However, she also assumes that player scores are entirely independent.

Recognizing the need to get my arms around the variance and covariance of player scores if I was going to optimize GPP lineups, I built a simulation model for MLB games.

## Data
### Data acquisition
The R file “download and parse retrosheet play-by-play files.R”, adapted from code from the previously mentioned blog posts by Jim Albert, downloads and unzips event files from retrosheet.org and converts them to csv files.  For this study, I used only data from the 2018 regular season.

The R file “process retrosheet data and write to csv.R” uses code from Jim Albert’s GitHub site  (with some small modifications) to add base-out states and runs scored to the Retrosheet event data frame, then saves them as csv files.

I also use data on an ad hoc basis from baseball-reference.com in order to validate model output, look at the most common lineups by team, etc.

### Data prep
Before simulating, I preprocessed some of the data in a separate R script (“player specific play probabilities.R”), which derives the player-specific event probabilities that are used by the simulation model.  I restricted the analysis to batters with at least 250 plate appearances (PA) in 2018, and pitchers with at least 80 innings pitched (IP).

For each batter meeting the PA threshold, I calculated:
* the probability of each type of batting event, given that a batting event occurs
* the probability of a baserunning event (e.g. stolen base, pickoff) of any type being the next event to occur, given that the player is on base

For each pitcher meeting the IP threshold, I calculated:
* the probability of each type of batting event, given that a batting event occurs
* the probability of a pitching event (e.g. wild pitch, balk) of any type being the next event to occur

In the simulation model itself, I also calculate the MLB averages that parallel each of these player-specific probabilities.  These are league averages are ultimately used to adjust event probabilities by opening base-out state, or alternately to plug generic MLB players into the lineup.

In all cases, probabilities are based on the empirical event frequencies.  Events are categorized by type (batting, baserunning, pitching) because the ways in which each set of probabilities are used in the simulation model differs somewhat.  See Appendix 4 for a full list of event types provided in the Retrosheet data, and how I’ve categorized them for this purpose.
