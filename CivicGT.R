# Simulating the Civic extensive form game to validate Nash equilibrium
# Jonjon Clark
# 30/07/2018


# simple environment 
# confirm nash equilibrium through reinforcement learning techniques

# net worth? Some function that both essentially want to maximise
Validator = 0
Requester = 0

# Rep my have some sort of function to do with propensity of other actors to engage with them.
validator_rep <-0
requestor_rep <-0

# contraints hypothesise
# CA > IF > 0 > IA > Pe
# CF > IA
# CF, IF <= |Pe|
# epsilon is small (obvious)

# Correct attestation
CA <- 2 # CA > 0 
# i) for requestor this is the net benefit of using the civic system over its own KYC
# ii) for the validator this is the net financial gain
# Question, should the CA payoff be different for requestor and validator?

# Incorrect attestation
IA <- 0 # IA < 0
# Encompassing the reputational, regulatory and practical risks and costs associated with an incorrect attestation

# Incorrect flag
IF <- 1 # IF > 0 
# since reward

# Correct flag
CF <- 1 # CF > 0 
# since reward

# Penalty
Pe <- 0 # Pe < 0
# Obvious since this is a penalty

# epsilon. v small obvious
epsilon <- 0.01





















