### Proj Thatcher Simulation - Single Agent Optimization Simulation
### August, 2016
### SV

### In this script, we detail a sample driver's optimal choice of contract 
### across a path of accidents given a fixed exogenous accident probability

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

# Run pricing_contractChoice.r if it has not been run
if(!exists("ranPricingCode")) {source("./code/pricing_contractchoice.R")}

# Start out with 1 driver with uniform probs:
lambda_i = round(runif(1,max=.3),2)
pMaj_i = round(runif(1,max=.5),3)


#To differentiate major and minor accident costs, make up some means here:
meanMj = 700
meanMn = 20

# Assume finite time lines no longer than T are considered; We can relax the
# uniformity of T later
T = 20

# Initially assume all drivers have the same risk aversion parameter;
# expand this later
gamma_ra = .5
alpha_ra = -gamma_ra/2000

# Assume homogeneous time discount factor
beta = .95

# Function Wrapper for the random variable generating the cost of an accident; 
# Use the wrapper in future code and modify the distribution HERE ONLY
require(truncdist)

costOfAccidentRV <- function(numDraws=1,mean_cost){
  # Current Cost Distribution: Log-normal rv with mean, mean_cost, and std dev = 1
  costDraw = rlnorm(n = numDraws,meanlog = log(mean_cost),sdlog = 1)
  return(costDraw)
}

# Wrapper function for random draws of the total costs of accidents (minus deductible)
getTotalCostOfAccidentsMinusDeductibleRV <- function(numDraws=1,mean_cost,u_l_var){
  if(numDraws>0){
  # Current Cost Distribution: Log-normal rv with mean, mean_cost, and std dev = 100
  costDraws = rlnorm(n = numDraws,meanlog = log(mean_cost),sdlog = 1) - rep(u_l_var,numDraws)
  posCostDraws = pmax(costDraws,rep(0,numDraws))
  totalCost = sum(posCostDraws)
  return(totalCost)
  }
  else{
    return(0)
  }
}

vectorized_getTotalCostOfAccidentsRV <- Vectorize(getTotalCostOfAccidentsMinusDeductibleRV)



numAccidentsRV <- function(numDraws=1,lam){
  return(rpois(numDraws,lam))
}

# Simulate the number of major and minor accidents in one period according to the distribution used in the Monte Carlo simulation
simulateAccidentsOnePeriod <- function(lambda_var=lambda_i,pMaj_var=pMaj_i){
  numAccidentDraws = numAccidentsRV(1,lambda_var)
  numMaj = rbinom(1,numAccidentDraws,pMaj_var)
  return(list(nMaj = numMaj, nMin = numAccidentDraws-numMaj))
}


getExpCostAboveCoverage <-function(mean_cost,coverage){
  return(extrunc(spec="lnorm",a=coverage,b=Inf,meanlog = log(mean_cost), sdlog = 1))
}

# Risk-averse Utility function
getUtility <- function(alpha=alpha_ra,cost){
  utility = -cost - alpha*cost^2
  
  #scale so numbers aren't huuuuuge
  scaled_utility = utility/10^2
  return(utility)
}

# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
# Major and minor accidents are assumed to be independent of each other, each governed by a Poisson dist with its own parameter
getMonteCarloCostDraws <- function(lambda_mc, pMaj_mc, u_l_000_mc){
  EV = 0
  numSimulations = 10^4
  
  #draw the number of accidents according to Poisson(lambda_mc)
  numAccidentDraws = numAccidentsRV(numSimulations,lambda_mc)
  
  numMaj = rbinom(rep(1,numSimulations),numAccidentDraws,rep(pMaj_mc,numSimulations))
  numMin = (numAccidentDraws - numMaj)
  
  majCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMaj , mean_cost=meanMj, u_l_var=u_l_000_mc)
  minCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMin , mean_cost=meanMn, u_l_var=u_l_000_mc)
  costDraw = majCostDraws + minCostDraws
  
  return(costDraw)
}

# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
# using an ALREADY drawn set of monte carlo accident cost draws. We can do this because in model 1.0, 
# accident probability is fixed and independent of the num of accidents that have occurred
getExpectedUtilityMonteCarloFixedDraws <- function(costMCDraws, geo_string_mc="C", majA_mc, minA_mc, female_mc=1, married_mc=0, spouse_rated_mc=0, age_mc=24, u_l_000_mc, m_mc, m_tier_mc=0.9, t_mc=1){
  EV = 0
  numSimulations = 10^4
  
  # premium = Pricing_l(geo_string, majA, minA, female, married, spouse_rated, age, u_l_000, m_choice, m_tier, t)
  premium = Pricing_l(geo_string_var = geo_string_mc, majA_var = majA_mc, minA_var = minA_mc, female_var = female_mc, married_var = married_mc, spouse_rated_var = spouse_rated_mc, age_var = age_mc, u_l_var_000 = u_l_000_mc, m_var = m_mc, m_tier_var = m_tier_mc, t_var = t_mc)

  totalCostDraws = rep(premium,numSimulations) + costMCDraws
  
  EV = EV + sum(getUtility(cost = totalCostDraws))
  
  # print(sprintf("the final EV is %f",EV/numSimulations))
  return(round(EV/numSimulations,2))
}

# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
# Major and minor accidents are assumed to be independent of each other, each governed by a Poisson dist with its own parameter
getExpectedUtilityMonteCarlo <- function(lambda_mc, pMaj_mc, geo_string_mc="C", majA_mc, minA_mc, female_mc=1, married_mc=0, spouse_rated_mc=0, age_mc=24, u_l_000_mc, m_mc, m_tier_mc=0.9, t_mc=1){
  EV = 0
  numSimulations = 10^3
  
  # premium = Pricing_l(geo_string, majA, minA, female, married, spouse_rated, age, u_l_000, m_choice, m_tier, t)
  premium = Pricing_l(geo_string_var = geo_string_mc, majA_var = majA_mc, minA_var = minA_mc, female_var = female_mc, married_var = married_mc, spouse_rated_var = spouse_rated_mc, age_var = age_mc, u_l_var_000 = u_l_000_mc, m_var = m_mc, m_tier_var = m_tier_mc, t_var = t_mc)
  #draw the number of accidents according to Poisson(lambda_mc)
  numAccidentDraws = numAccidentsRV(numSimulations,lambda_mc)

    numMaj = rbinom(rep(1,numSimulations),numAccidentDraws,rep(pMaj_mc,numSimulations))
    numMin = (numAccidentDraws - numMaj)
    
    majCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMaj , mean_cost=meanMj, u_l_var=u_l_000_mc)
    minCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMin , mean_cost=meanMn, u_l_var=u_l_000_mc)
    costDraw = rep(premium,numSimulations) + majCostDraws + minCostDraws
        
    EV = EV + sum(getUtility(cost = costDraw))
    
  # print(sprintf("the final EV is %f",EV/numSimulations))
  return(round(EV/numSimulations,2))
}


#TODO: Vectorize this!!
#
# Finds the optimal choice of y_l and m given the # major and minor accidents and other relavent factors
# example:
# getOptimalCoverage(majA=2, minA=2)
getOptimalCoverage <- function(lam_v,pMaj_v,geo_string_v="C", majA_v, minA_v, female_v=1, married_v=0, spouse_rated_v=0, age_v=24,m_tier_v=0.9, t_v=1){
  optimumPlanKey = ""
  optimalValue = -99999999999
  
  for(i in 1:nrow(y_l)){
    u_choice = y_l$u_l[i]
    m_choice = m[i]

    newVal = getExpectedUtilityMonteCarlo(lam_v,pMaj_v,geo_string_v, majA_v, minA_v, female_v, married_v, spouse_rated_v, age_v, u_choice, m_choice, m_tier_v, t_v)
    # print(sprintf("The val being considered is %f",newVal))
    if(newVal > optimalValue){
      # print(sprintf("The u_choice being considered are %d",u_choice))
      # print(sprintf("The m_choice being considered are %d",m_choice))
      optimumPlanKey = as.vector(paste(u_choice, m_choice, sep = ","))
      # print("Updated optimum")
      # print(list(optplan = optimumPlanKey, curVal = newVal))
      optimalValue = newVal
    }
    if(optimumPlanKey == ""){print(sprintf("Shit broke with uchoice: %s and val: %f",u_choice,newVal))}
  }
  # print("Final answer")
  return(list(optimalPlan = optimumPlanKey,optValue = optimalValue))
}

getOptimalCoverageFixedDraws <- function(costMCDraws_set,geo_string_v="C", majA_v, minA_v, female_v=1, married_v=0, spouse_rated_v=0, age_v=24, m_tier_v=0.9, t_v=0){
  optimumPlanKey = ""
  optimalValue = -99999999999
  
  for(m_chosen in c(0,1)){
    for(i in 1:nrow(y_l)){
      u_choice = y_l$u_l[i]
      
      #costMCDraws_set is a hash table with draws for the different u_choices;
      #Here we select the right one for comparison. This is going to be the most 
      #efficient implimentation for this particular simulation though the code is kind of gross.
      costMCDraws_v = costMCDraws_set[[as.character(u_choice)]]
      
      newVal = getExpectedUtilityMonteCarloFixedDraws(costMCDraws_v,geo_string_v, majA_v, minA_v, female_v, married_v, spouse_rated_v, age_v, u_choice, m_chosen, m_tier_v, t_v)
      # print(sprintf("The val being considered is %f",newVal))
      if(newVal > optimalValue){
        # print(sprintf("The u_choice being considered are %d",u_choice))
        optimumPlanKey = as.vector(paste(u_choice, m_chosen, sep = ","))
        # print("Updated optimum")
        # print(list(optplan = optimumPlanKey, curVal = newVal))
        optimalValue = newVal
      }
      if(optimumPlanKey == ""){print(sprintf("Shit broke with uchoice: %s and val: %f",u_choice,newVal))}
    }
  }
  # print("Final answer")
  return(list(optimalPlan = optimumPlanKey,optValue = optimalValue))
}


#For some reason, this isn't being instantiated inside the function. GRRRRR
optCoverageHash = hash()

# Hash solutions to optimal coverage for all possible states of Amaj,Amin and t
# example:
# hashOptimCovByNumAccAndAge(lambda_var = lambda_i,pMaj_var = pMaj_i,start_age_var = 25)
hashOptimCovByNumAccAndAge <- function(lambda_var, pMaj_var, T_var = T,geo_string_var="C", female_var=1, married_var=0, spouse_rated_var=0, start_age_var, m_tier_var=0.9){
  # Create the hash table if it does not yet exist
  if (!exists("optCoverageHash")) {
    print("initiating optimal coverage hash table")
    optCoverageHash = hash()
  }
  
  costMCDraws_hash = hash()
  for(u_l_i in y_l$u_l){
    costDraws = getMonteCarloCostDraws(lambda_var, pMaj_var, u_l_i)
    costMCDraws_hash[[as.character((u_l_i))]] = costDraws
  }
  
  
  #Consider T possible periods forward
  age_seq = seq(start_age_var,start_age_var+T_var)
  for(acc_hist_ind in seq(A$majA)){
    for(age_i in age_seq){
      majA_i = A$majA[acc_hist_ind]
      minA_i = A$minA[acc_hist_ind]
      
      t_i = ifelse(age_i == start_age_var, 1, 0)
      
      #Currently only allow for heterogeneity in lambda and pMaj - can change this easily in the future
      key_i = paste(lambda_var, pMaj_var, majA_i,minA_i,age_i, sep = ",")
      optChoice = getOptimalCoverageFixedDraws(costMCDraws_hash,geo_string_var,majA_i, minA_i, female_var, married_var, spouse_rated_var, age_i,m_tier_var, t_i)
      
      optCoverageHash[[key_i]] = list(optPlan = optChoice$optimalPlan,optVal = optChoice$optValue)
      
    }
  }
}

#just for fun while we figure out the right data structure for this
simPathHash = hash()
simulateOptCovPath <-function(){
   # Simulates optimal path given a random starting point
  sim_path = data.frame(matrix(ncol = 3, nrow =T))
  names(sim_path) = c("age","optPlan","optVal")
  
  start_age = sample(18:40,1)
  # sim_path$age = seq(start_age,start_age+T-1)
  lambda_i = round(runif(1,max=.4),2)
  pMaj_i = round(runif(1,max=.15),3)
  sim_path$age = rep(paste(lambda_i,pMaj_i,start_age,","),T)
  
  # Draw a sample of what the monitoring 
  m_tier_draw = sample(keys(P_M_hash),1)
  
  hashOptimCovByNumAccAndAge(lambda_var = lambda_i,pMaj_var = pMaj_i,start_age_var = start_age, m_tier_var = m_tier_draw)
  
  init_maj = sample(1:3,1)
  init_min = sample(1:6,1)
  
  majA_record = c(init_maj,0,0)
  minA_record = c(init_min,0,0)
  age_i = start_age
  for(t_i in seq(T)){
    majA_i = sum(majA_record)
    minA_i = sum(minA_record)
    
    if(majA_i>3 | minA_i > 6){
      # print("dropped out")
      sim_path$optPlan[t_i] = "tooManyAccidents"
      sim_path$optVal[t_i] = 0
    }
    else{
      key_t = paste(lambda_i, pMaj_i, majA_i,minA_i,age_i, sep = ",")
      # print(sprintf("they key is: %s and t_i is: %d",key_t,t_i))
      optChoice_t = optCoverageHash[[key_t]]
      sim_path$optPlan[t_i] = optChoice_t$optPlan
      sim_path$optVal[t_i] = optChoice_t$optVal
      
      simAcc = simulateAccidentsOnePeriod(lambda_var=lambda_i,pMaj_var=pMaj_i)
      
      majA_record[t_i%%3 + 1] = simAcc$nMaj
      minA_record[t_i%%3 + 1] = simAcc$nMin
      
      age_i = age_i + 1
    }
    
  }
  return(sim_path)
}

#Simulate a population and hash the results
numSims = 100
for (i in seq(numSims)) {
  ptm <- proc.time()
  sim_i = simulateOptCovPath()
  print(proc.time() - ptm)
  simPathHash[[as.character(i)]] = sim_i
}

simPaths = data.frame()
for(key in keys(simPathHash)){
  # print(key)
  keypath = simPathHash[[as.character(i)]]
  row = t(keypath$optPlan)
  simPaths = rbind(simPaths,row)
}
