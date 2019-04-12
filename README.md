# MLB-game-simulator
<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/b3/D7K_4966_Roy_Halladay.jpg/800px-D7K_4966_Roy_Halladay.jpg' width="520" height="800">

If you've played any daily fantasy sports (DFS) contests, you probably know there are a few different contest formats, each of which demands a different lineup strategy. (If you're NOT familiar with DFS, here's a [primer](https://www.dailyfantasysports101.com/basics/).) In a "double-up" format, for example, the ~44% of entrants with the highest scores double their money. The strategy here is relatively straightforward: draft a lineup that maximizes the expected points scored, given salary and position constraints. (Technically, the right strategy would be 'draft a lineup that maximizes the probability that the final score will be in the top 44% of entrants, but in most cases that's probably not too different and more complicated to model.)

In a GPP contest, only those entries scoring in the top 20% or so receive any payout, and the REAL money is concentrated among the top few entries out of possibly thousands. This complicates things significantly, since now our strategy must consider not only expected points scored by player, but also:
* the volatility (e.g. variance) of each player's point distribution
* the relationships (e.g. correlation) between pairs of players' point distributions
* the payout structure
* arguably, the frequency with which each player is found on our competitors' rosters

There are a few papers out there that address GPP roster optimization. 
