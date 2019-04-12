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

Further, when I searched the web for DFS correlations for MLB, I came up empty. And to be honest, even if I'd found something, I'd be loathe to use those results in my own model without knowing a whole lot about how they were derived.

