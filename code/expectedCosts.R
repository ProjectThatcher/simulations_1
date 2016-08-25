### Proj Thatcher Simulation - Computation of Drivers' Expected Costs and Inversion of Prices
### August, 2016
### SV
### 
### In this script, we detail a monte carlo estimator for the expected cost (for one period) 
### of a class of drivers with a fixed probability of accident (lambda) and a 
### fixed (IID) probability that an accident that has occured is a major accident
### 
### We then include a maximum likelihood estimator for lambda and pMaj to parametrically 
### estimate the accident distributions implied by prices used

# Clear function here for convenience
# rm(list = ls())


## Assuming this code loads up in Proj_Thatcher/Simulations/Code,
## this tries to set the pwd correctly on any computer + prints to make sure we know
setwd('~')
if (substr(getwd(),1,18) == "/Users/shoshievass") {
  setwd('/Volumes/Transcend/Dropbox/Project_Thatcher/Simulations/')
} else if (substr(getwd(),1,11) == "/Users/yjin"){
  setwd('/Users/yjin/Dropbox/Research/IO/Proj_Thatcher/Simulations')
} else{
  print("Could not recognize being in YJ or SV's computer. Need to set the directory manually")
}
print(sprintf("Set the wd to : %s", getwd()))


#To differentiate major and minor accident costs, make up some means here:
meanMj = 500
meanMn = 10

#Wrapper function for drawing the number of accidents given parameter lambda
#Currently, using a poisson distribution.
numAccidentsRV <- function(numDraws=1,lam){
  return(rpois(numDraws,lam))
}

# Wrapper function for random draws of the costs of accidents
getRawTotalCostOfAccidentsRV <- function(numSims=1,mean_cost_s){
  if(numSims>0){
    # Current Cost Distribution: Log-normal rv with mean, mean_cost, and std dev = 100
    costDrawsSim = rlnorm(n = numSims,meanlog = log(mean_cost_s),sdlog = 1)
    return(costDrawsSim)
  }
  else{
    return(0)
  }
}

# Wrapper function for random draws of the total [summed] costs of accidents, bounded from below by zero (doesn't do anything for lognormal dist but might in general)
getTotalCostOfAccidentsRV <- function(numDraws=1,mean_cost){
  if(numDraws>0){
    costDraws = getRawTotalCostOfAccidentsRV(numSims = numDraws,mean_cost_s = mean_cost)
    posCostDraws = pmax(costDraws,rep(0,numDraws))
    totalCost = sum(posCostDraws)
    return(totalCost)
  }
  else{
    return(0)
  }
}

#Vectorized form of getTotalCostOfAccidentsMinusDeductibleRV; Used in getMonteCarloCostDraws to speed things up
vectorized_getTotalCostOfAccidentsRV <- Vectorize(getTotalCostOfAccidentsRV)

# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
getExpectedCostOfDriverClass <- function(lambda_mc, pMaj_mc){
  EV = 0
  numSimulations = 10^4
  
  #draw the number of accidents according to Poisson(lambda_mc)
  numAccidentDraws = numAccidentsRV(numSimulations,lambda_mc)
  
  numMaj = rbinom(rep(1,numSimulations),numAccidentDraws,rep(pMaj_mc,numSimulations))
  numMin = (numAccidentDraws - numMaj)
  
  majCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMaj , mean_cost=meanMj)
  minCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMin , mean_cost=meanMn)
  costDraw = majCostDraws + minCostDraws
  avgCost = round(mean(costDraw),2)
  
  return(avgCost)
}

#Example
#getExpectedCostOfDriverClass(.4,.2)