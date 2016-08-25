# This is a code archive for functions previously written and discarded
# The purpose of this file is to have them on hand for reference in an easy to find place
# 
# 
### Archived because for model 1.0, we only need to make the monte carlo draws once (fixed cost draws)!
# Function to compute expected cost based on state (age + accident history) and contract choice (and prob of accident/acc type)
# Major and minor accidents are assumed to be independent of each other, each governed by a Poisson dist with its own parameter
# getExpectedUtilityMonteCarlo <- function(lambda_mc, pMaj_mc, geo_string_mc="C", majA_mc, minA_mc, female_mc=1, married_mc=0, spouse_rated_mc=0, age_mc=24, u_l_000_mc, m_mc, m_tier_mc=0.9, t_mc=1){
#   EV = 0
#   numSimulations = 10^3

#   # premium = Pricing_l(geo_string, majA, minA, female, married, spouse_rated, age, u_l_000, m_choice, m_tier, t)
#   premium = Pricing_l(geo_string_var = geo_string_mc, majA_var = majA_mc, minA_var = minA_mc, female_var = female_mc, married_var = married_mc, spouse_rated_var = spouse_rated_mc, age_var = age_mc, u_l_var_000 = u_l_000_mc, m_var = m_mc, m_tier_var = m_tier_mc, t_var = t_mc)
#   #draw the number of accidents according to Poisson(lambda_mc)
#   numAccidentDraws = numAccidentsRV(numSimulations,lambda_mc)

#     numMaj = rbinom(rep(1,numSimulations),numAccidentDraws,rep(pMaj_mc,numSimulations))
#     numMin = (numAccidentDraws - numMaj)

#     majCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMaj , mean_cost=meanMj, u_l_var=u_l_000_mc)
#     minCostDraws = vectorized_getTotalCostOfAccidentsRV(numDraws = numMin , mean_cost=meanMn, u_l_var=u_l_000_mc)
#     costDraw = rep(premium,numSimulations) + majCostDraws + minCostDraws

#     EV = EV + sum(getUtility(cost = costDraw))

#   # print(sprintf("the final EV is %f",EV/numSimulations))
#   return(round(EV/numSimulations,2))
# }

# 
# 
# #ARCHIVED: 
# Old function for expected utility of monte carlo trial
# Discarded because not vectorized and does not use fixed cost draws (done for speed in newer version)
# getExpectedUtilityMonteCarlo <- function(lam=lambda_i,pMaj=pMaj_i,geo_string_var="C", majA_var, minA_var, female_var=1, married_var=0, spouse_rated_var=0, age_var=24, u_l_000_var, m_var, m_tier_var=0.9, t_var=1){
#   EV = 0
#   numSimulations = 10^3

#   premium = Pricing_l(geo_string_var, majA_var, minA_var, female_var, married_var, spouse_rated_var, age_var, u_l_000_var, m_var, m_tier_var, t_var)
#   #draw the number of accidents according to Poisson(lam)
#   numAccidentDraws = numAccidentsRV(numSimulations,lam)
#   for(draw in numAccidentDraws){
#     for(acc in seq(draw)){
#       #for each accident, draw if minor or major
#       isMaj = rbinom(1,1,pMaj)
#       if(isMaj){
#         costDraw = premium + max(0,(costOfAccidentRV(mean_cost=meanMj) - u_l_000_var))
#         print(sprintf("the total cost drawn is %f",costDraw))
#         print(sprintf("the utility of the cost drawn is %f",getUtility(cost=costDraw)))
#         EV = EV + getUtility(cost=costDraw)
#       }
#       else{
#         costDraw = premium + max(0,(costOfAccidentRV(mean_cost=meanMn) - u_l_000_var))
#         print(sprintf("the total cost drawn is %f",costDraw))
#         print(sprintf("the utility of the cost drawn is %f",getUtility(cost=costDraw)))
#         EV = EV + getUtility(cost=costDraw)
#       }
#     }
#   }
#   print(sprintf("the final EV is %f",EV/numSimulations))
#   return(round(EV/numSimulations,2))
# }

# #Example
# getExpectedUtilityMonteCarlo(majA_var=2,minA_var=2,u_l_000_var=30,m_var=1)