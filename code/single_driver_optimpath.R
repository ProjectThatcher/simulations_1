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

#To differentiate major and minor accident costs, make up some means here:
meanMj = 700
meanMn = 20

# Assume finite time lines no longer than T are considered; We can relax the
# uniformity of T later
T = 20

# Initially assume all drivers have the same risk aversion parameter;
# expand this later
# gamma_ra = .5
# alpha_ra = -gamma_ra/2000
alpha_ra = 9^10^(-5)

# Assume homogeneous time discount factor
beta = .95

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
  

# Wrapper function for random draws of the total [summed] costs of accidents minus the deductible, bounded from below by zero
getTotalCostOfAccidentsMinusDeductibleRV <- function(numDraws=1,mean_cost,u_l_var){
  if(numDraws>0){
  costDraws = getRawTotalCostOfAccidentsRV(numSims = numDraws,mean_cost_s = mean_cost) - rep(u_l_var,numDraws)
  posCostDraws = pmax(costDraws,rep(0,numDraws))
  totalCost = sum(posCostDraws)
  return(totalCost)
  }
  else{
    return(0)
  }
}

#Vectorized form of getTotalCostOfAccidentsMinusDeductibleRV; Used in getMonteCarloCostDraws to speed things up
vectorized_getTotalCostOfAccidentsMinusDeductibleRV <- Vectorize(getTotalCostOfAccidentsMinusDeductibleRV)

#Wrapper function for drawing the number of accidents given parameter lambda
#Currently, using a poisson distribution.
numAccidentsRV <- function(numDraws=1,lam){
  return(rpois(numDraws,lam))
}

# Simulate the number of major and minor accidents in one period according to the distribution used in the Monte Carlo simulation
simulateAccidentsOnePeriod <- function(lambda_var=lambda_i,pMaj_var=pMaj_i){
  numAccidentDraws = numAccidentsRV(1,lambda_var)
  numMaj = rbinom(1,numAccidentDraws,pMaj_var)
  return(list(nMaj = numMaj, nMin = numAccidentDraws-numMaj))
}

# Risk-averse Utility function
getUtility <- function(alpha=alpha_ra,cost){
  utility = -cost - alpha*cost^2
  
  #scale so numbers aren't huuuuuge
  scaled_utility = utility/10^3
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
  
  majCostDraws = vectorized_getTotalCostOfAccidentsMinusDeductibleRV(numDraws = numMaj , mean_cost=meanMj, u_l_var=u_l_000_mc)
  minCostDraws = vectorized_getTotalCostOfAccidentsMinusDeductibleRV(numDraws = numMin , mean_cost=meanMn, u_l_var=u_l_000_mc)
  costDraw = majCostDraws + minCostDraws
  
  return(costDraw)
}

# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
# using an ALREADY drawn set of monte carlo accident cost draws. We can do this because in model 1.0, 
# accident probability is fixed and independent of the num of accidents that have occurred
getExpectedUtilityMonteCarloFixedDraws <- function(costMCDraws, geo_string_mc="C", majA_mc, minA_mc, female_mc=1, married_mc=0, spouse_rated_mc=0, age_mc=24, u_l_000_mc, m_mc, m_tier_mc=0.9, t_mc=1){
  numSimulations = 10^4
  
  premium = Pricing_l(geo_string_var = geo_string_mc, majA_var = majA_mc, minA_var = minA_mc, female_var = female_mc, married_var = married_mc, spouse_rated_var = spouse_rated_mc, age_var = age_mc, u_l_var_000 = u_l_000_mc, m_var = m_mc, m_tier_var = m_tier_mc, t_var = t_mc)

  totalCostDraws = rep(premium,numSimulations) + costMCDraws
  
  EV = mean(getUtility(cost = totalCostDraws))
  
  # print(sprintf("the final EV is %f",EV/numSimulations))
  return(round(EV,2))
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

#Finds the best coverage choice to optimize flow utility for given a fixed monitoring choice and monitoring tier
getOptimalCoverageGivenMFixedDraws <- function(costMCDraws_set,geo_string_v="C", majA_v, minA_v, female_v=1, married_v=0, spouse_rated_v=0, age_v=24, m_v, m_tier_v,t_v){
  optimumPlanKey = ""
  optimalValue = -99999999999
  
    for(i in 1:nrow(y_l)){
      u_choice = y_l$u_l[i]
      
      #costMCDraws_set is a hash table with draws for the different u_choices;
      #Here we select the right one for comparison. This is going to be the most 
      #efficient implimentation for this particular simulation though the code is kind of gross.
      costMCDraws_v = costMCDraws_set[[as.character(u_choice)]]
      
      newVal = getExpectedUtilityMonteCarloFixedDraws(costMCDraws_v,geo_string_v, majA_v, minA_v, female_v, married_v, spouse_rated_v, age_v, u_choice, m_v, m_tier_v, t_v)
      # print(sprintf("The val being considered is %f",newVal))
      if(newVal > optimalValue){
        # print(sprintf("The u_choice being considered are %d",u_choice))
        optimumPlanKey = as.vector(paste(u_choice, m_v, sep = ","))
        # print("Updated optimum")
        # print(list(optplan = optimumPlanKey, curVal = newVal))
        optimalValue = newVal
      }
      if(optimumPlanKey == ""){print(sprintf("Shit broke with uchoice: %s and val: %f",u_choice,newVal))}
    }
  # print("Final answer")
  return(list(optimalPlan = optimumPlanKey,optValue = optimalValue))
}


#For some reason, this isn't being instantiated inside the function. GRRRRR
optCoverageHash = hash()

# Hash solutions to optimal coverage for all possible states of Amaj,Amin and m
# example:
# hashOptimCovByNumAccAndAge(lambda_var = lambda_i,pMaj_var = pMaj_i,start_age_var = 25)
hashOptimCovByNumAccAndAge <- function(lambda_var, pMaj_var, T_var = T,geo_string_var="C", female_var=1, married_var=0, spouse_rated_var=0, start_age_var, m_tier_var=0.9){
  # Create the hash table if it does not yet exist
  if (!exists("optCoverageHash")) {
    print("initiating optimal coverage hash table")
    optCoverageHash = hash()
  }
  
  #Make numsims = 10^4 monte carlo draws once
  costMCDraws_hash = hash()
  for(u_l_i in y_l$u_l){
    costDraws = getMonteCarloCostDraws(lambda_var, pMaj_var, u_l_i)
    costMCDraws_hash[[as.character((u_l_i))]] = costDraws
  }
  
  #Consider T possible periods forward
  #Look at periods 2 to T first; then look at period 1 separately
  age_seq = seq(start_age_var,start_age_var+T_var)
  for(acc_hist_ind in seq(A$majA)){
    for(age_i in age_seq){
      for(m_i in c(0,1)){
        majA_i = A$majA[acc_hist_ind]
        minA_i = A$minA[acc_hist_ind]
        
        t_i = ifelse(age_i == start_age_var,0,1)
        
        #Currently only allow for heterogeneity in lambda and pMaj - can change this easily in the future
        key_i = paste(lambda_var, pMaj_var, majA_i,minA_i,age_i,m_i, sep = ",")
        optChoice = getOptimalCoverageGivenMFixedDraws(costMCDraws_hash,geo_string_var,majA_i, minA_i, female_var, married_var, spouse_rated_var, age_i,m_i,m_tier_var,t_i)
        
        optCoverageHash[[key_i]] = list(optPlan = optChoice$optimalPlan,optVal = optChoice$optValue)
      }
    }
  }
  
  #Now look at period 1
  # maxVal = -999
  for(acc_hist_ind in seq(A$majA)){
    majA_i = A$majA[acc_hist_ind]
    minA_i = A$minA[acc_hist_ind]
    
    opt_m_val = -9999999999999
    opt_m = -1
    for(m_i in c(0,1)){
      # print(start_age_var)
      future_value = getEVofMonitoringMonteCarlo(optCoverageHash,lambda_var, pMaj_var, m_i,m_tier_var,majA_i,minA_i,start_age_var,T_var = T)
      key_i = paste(lambda_var, pMaj_var, majA_i,minA_i,start_age_var,m_i, sep = ",")
      optFlow = optCoverageHash[[key_i]]
      
      totalVal = future_value + optFlow$optVal
      print(sprintf("total value is %f",totalVal))
      print(sprintf("opt_m_val is %f", opt_m_val))
      if(totalVal>opt_m_val){
        print(sprintf("changed opt_m to %d", m_i))
        opt_m = m_i
        opt_m_val = totalVal
      }
    }
    key_i_over_m = paste("opt_mchoice",paste(lambda_var, pMaj_var, majA_i,minA_i,start_age_var, sep = ","), sep = "_")
    optCoverageHash[[key_i_over_m]] = opt_m
  }
  return(optCoverageHash)
}

getHashedCoverageByKeyForMCSimulation <- function(optimPathHash,lambda_i, pMaj_i, majA_i,minA_i,age_i,m_i){
  opt_keys = paste(lambda_i, pMaj_i, majA_i,minA_i,age_i,m_i, sep = ",")
  hasHash = (majA_i < 4 & minA_i <7)
  # print(hasHash)
  # print(opt_keys)

  optValues = vector(length = length(opt_keys))
  #Going to do this in a loop for now and try to vectorize later
  for(k_ind in seq(opt_keys)){
    if(hasHash[k_ind]){
      key_t = opt_keys[k_ind]
      hash_val = optimPathHash[[key_t]]
      # print(hash_val)
      optValues[k_ind] = hash_val$optVal
    }
    else{
      optValues[k_ind] = getUtility(cost=10000) #This number is made up and needs to change
    }
  }
  return(optValues)
}


#need to add discount factor
getEVofMonitoringMonteCarlo <-function(optimPathHash,lambda_var,pMaj_var,m_i,m_tier_i,majA_init, minA_init,age_init,T_var = T){
  # Simulates optimal path given a random starting point

  numSimulations = 10
  majA_state = rep(majA_init,numSimulations)
  minA_state = rep(minA_init,numSimulations)
  lambdas = rep(lambda_var,numSimulations)
  pMajs = rep(pMaj_var,numSimulations)
  age_is = rep(age_init,numSimulations)
  m_is = rep(m_i,numSimulations)
  ones = rep(1,numSimulations)
  
  EV = rep(0,numSimulations)
  
  majA_record = rbind(majA_state,rep(0,numSimulations),rep(0,numSimulations))
  minA_record = rbind(minA_state,rep(0,numSimulations),rep(0,numSimulations))
  for(t_i in seq(T-1)){
    
    numAccidentDraws = numAccidentsRV(numSimulations,lambda_var)
    
    numMaj = rbinom(rep(1,numSimulations),numAccidentDraws,rep(pMaj_var,numSimulations))
    numMin = (numAccidentDraws - numMaj)
    
    majA_record[t_i%%3 + 1,] = numMaj
    minA_record[t_i%%3 + 1,] = numMin
    
    majA_state = colSums(majA_record)
    minA_state = colSums(minA_record)
    
    age_is = age_is + ones
    
    #Need a better model of what happens if the num of accidents is too high
    #Currently this is just given a big number, given in the getHashedCoverage... function
    addition = getHashedCoverageByKeyForMCSimulation(optimPathHash,lambda_i=lambdas, pMaj_i=pMajs, majA_i=majA_state,minA_i=minA_state,age_i=age_is,m_i=m_is)
    discountFactor = rep(beta^t_i,numSimulations)
    EV = EV + addition*discountFactor
    }
  return(round(mean(EV),2))
}


#just for fun while we figure out the right data structure for this
simPathHash = hash()
simulateOptCovPath <-function(){
   # Simulates optimal path given a random starting point
  sim_path = data.frame(matrix(ncol = 5, nrow =T))
  names(sim_path) = c("key","optPlan","optVal","numMajAcc","numMinAcc")
  
  start_age_draw = sample(18:40,1)
  # sim_path$age = seq(start_age_draw,start_age_draw+T-1)
  lambda_i = round(runif(1,max=.4),2)
  pMaj_i = round(runif(1,max=.15),3)
  
  # Draw a sample of what the monitoring 
  m_tier_draw = sample(keys(P_M_hash),1)
  
  sim_path$key = rep(paste(lambda_i,pMaj_i,start_age_draw,m_tier_draw,sep=","),T)
  
  
  optCoverageHash = hashOptimCovByNumAccAndAge(lambda_var = lambda_i,pMaj_var = pMaj_i,start_age_var = start_age_draw, m_tier_var = m_tier_draw)
  
  #Don't start people at max accidents
  init_maj = sample(1:2,1)
  init_min = sample(1:5,1)
  
  majA_record = c(init_maj,0,0)
  minA_record = c(init_min,0,0)
  age_i = start_age_draw
  for(t_i in seq(T)){
    majA_i = sum(majA_record)
    minA_i = sum(minA_record)
    
    sim_path$numMajAcc[t_i] = majA_i
    sim_path$numMinAcc[t_i] = minA_i
    
    if(majA_i>3 | minA_i > 6){
      # print("dropped out")
      sim_path$optPlan[t_i] = "tooManyAccidents"
      sim_path$optVal[t_i] = 0
    }
    else{
      if(t_i == 1){
        key_m = paste("opt_mchoice",paste(lambda_i, pMaj_i, majA_i,minA_i,age_i, sep = ","),sep = "_")
        print(sprintf("they key is: %s and t_i is: %d",key_m,t_i))
        optMon = optCoverageHash[[key_m]]
      }
      
      key_t = paste(lambda_i, pMaj_i, majA_i,minA_i,age_i,optMon, sep = ",")
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
numSims = 1
for (i in seq(numSims)) {
  ptm <- proc.time()
  sim_i = simulateOptCovPath()
  print(proc.time() - ptm)
  simPathHash[[as.character(i)]] = sim_i
}

simPaths = data.frame()
simKeys = data.frame()
simMaj = data.frame()
simMin = data.frame()
for(key in keys(simPathHash)){
  print(key)
  keypath = simPathHash[[as.character(key)]]
  row = t(keypath$optPlan)
  simPaths = rbind(simPaths,row)
  
  row = t(keypath$key)
  simKeys = rbind(simKeys,row)
  
  row = t(keypath$numMajAcc)
  simMaj = rbind(simMaj,row)
  
  row = t(keypath$numMinAcc)
  simMin = rbind(simMin,row)
}

# write.table(simPaths, "simPaths_it1b", sep=",")
# 
# For testing:
a=grep("opt_mchoice_*",keys(optCoverageHash), value=TRUE)
optCoverageHash[a]

#TODO:
# (1) Correlate X,lambda and m
# (2) Write dynamic program to get optimal choice of m
# (3) Clear out code for expected cost simulation (that's in a separate file now)
