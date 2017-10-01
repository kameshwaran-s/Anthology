# Gambler's Ruin Functions for Simulation Analytical Models

# Generate bet sequence from stake sequence
GenerateBetSeq <- function(stakeSeq) {
  n <- length(stakeSeq)
  betSeq <- sapply(1:(n-1), function(x) stakeSeq[x+1]-stakeSeq[x])
  return(betSeq)
}


# Fixed Bet ------------------
# iniWealth: Initial Wealth
# wager: Wager
# p: Winning probability
GamblersRuinSim <- function(iniWealth, wager, p) {
  # if (iniWealth <= 0) {
  # }
  stake <- iniWealth
  stakeSeq <- c(stake)
  betSeq <- c(0)
  while (stake > 0) {
    bet <- sample(c(1, -1), 1, prob = c(p, (1-p)))
    stake <- stake + (bet * wager)
    stakeSeq <- c(stakeSeq, stake)
    betSeq <- c(betSeq, bet)
  }
  numBets <- length(betSeq) - 1
  numWins <- length(betSeq[betSeq == 1])
  numLoss <- numBets - numWins
  maxWealth <- max(stakeSeq)
  rleBets <- rle(betSeq[-1])
  winStreaks <- rleBets$lengths[(rleBets$values == 1) && (rleBets$lengths > 1)]
  lossStreaks <- rleBets$lengths[(rleBets$values == -1) && (rleBets$lengths > 1)]
  return(stakeSeq)
}

stakeSeq <- GamblersRuinSim(10, 1, 0.5)
betSeq <- GenerateBetSeq(stakeSeq)

# Frequeny of Wealth & Bet
wealthFreq <- table(stakeSeq)
betSeq <- table(betSeq)
