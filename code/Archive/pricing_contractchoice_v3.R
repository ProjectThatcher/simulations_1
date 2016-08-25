### Proj Thatcher Simulation - Pricing and contract choice table
### August, 2016
### YJ

### In this script, I create a pricing function "Pricing" that takes 
### y, X, A as inputs and returns pricing in dollars

## As intermediate output, pricing factors & base pricing are summarized
## in matrices called "P_xxx". For example, P_A represents the pricing
## factor associated with the number of major and minor accidents during
## the past three years, in which I have columns "majA", "minA" and the 
## pricing factor column "A_factor".

## For ease of lookup, I created hash tables for each pricing matrix,
## named P_xxx_hash. To retrive the pricing factor, simply use 
## P_xxx_hash$"yyy", where yyy is the vector of keys inputed 
## without any parentheses but with commas and quotation marks

## xxx can be y_l, y_c, X, A

## In creating each pricing factor tables, the ones that come from csv
## files directly are based on actual pricing tables, while the ones 
## I hardcoded with "expand.grid" are extrapolated for simplicity and 
## they will be replaced in latter versions.

#NOTE: We will only want a 'clear' function in the 'main' script file, so this will likely be moved.
rm(list = ls())

## Assuming this code loads up in Proj_Thatcher/Simulations/Code,
## this tries to set the pwd correctly on any computer + prints to make sure we know
setwd('~')
if (substr(getwd(),1,18) == "/Users/shoshievass") {
  setwd('/Volumes/Transcend/Dropbox/Project_Thatcher/Simulations/')
} else if (substr(getwd(),1,11) == "/Users/yjin"){
  setwd('/Users/yjin/Dropbox/Research/IO/Proj_Thatcher/Simulations')
} else if (substr(getwd(),1,11) == "/Users/ejoe"){
  setwd('/Users/ejoe/Dropbox/Research/IO/Proj_Thatcher/Simulations')
} else{
  print("Could not recognize being in YJ or SV's computer. Need to set the directory manually")
}
print(sprintf("Set the wd to : %s", getwd()))

source("./code/helper.R")

#### Contract Choice Pricing Matrix P_y ####
require(utils)

## liability
y_l = expand.grid(u_l = c(30, 100))
y_l$keys = y_l$u_l

factor_y_l = expand.grid(factor_u_l = c(1.0, 1.54))
factor_y_l$y_l_factor = apply(factor_y_l, 1, prod)

P_y_l = cbind(y_l[c("keys")], factor_y_l[c("y_l_factor")])

## comprehensive
y_c = expand.grid(u_c = c(0, 10)
                  , d_c = c(500, 2000))
y_c$keys = as.vector(paste(y_c$u_c, y_c$d_c, sep = ","))

factor_y_c = expand.grid(factor_u_c = c(0.0, 1.0)
                         , factor_d_c = c(1.0, 0.65))
factor_y_c$y_c_factor = apply(factor_y_c, 1, prod)

P_y_c = cbind(y_c[c("keys")], factor_y_c[c("y_c_factor")])

#### Observables Pricing Matrix P_X ####
P_X_hh = read.csv("./raw/X_hh_factor.csv", header=T)
P_X_hh$keys = as.vector(paste(P_X_hh$female, P_X_hh$married, P_X_hh$spouse_rated, P_X_hh$age, sep = ","))
P_X_hh$X_hh_factor_liab = sqrt(P_X_hh$X_factor_bi * P_X_hh$X_factor_pd)
P_X_hh$X_hh_factor_comp = sqrt(P_X_hh$X_factor_comp * P_X_hh$X_factor_coll)
P_X_hh = P_X_hh[c("keys", "X_hh_factor_liab", "X_hh_factor_comp")]

X_car = expand.grid(make_symbol = c("low", "med", "high"),
                    model_symbol = c("low", "med", "high"),
                    style_symbol = c("low", "med", "high"),
                    car_age = seq(0,20, by = 2))
X_car$keys = as.vector(paste(X_car$make_symbol, X_car$model_symbol, X_car$style_symbol, X_car$car_age, sep = ","))

factor_X_car_liab = expand.grid(factor_make = c(0.8,1.0,1.1),
                               factor_model = c(0.8,1.0,1.3),
                               factor_style = c(0.9,1.0,1.1),
                               factor_car_age = seq(0.8,1.3, by = 0.05))
factor_X_car_liab$X_car_factor_liab = apply(factor_X_car_liab, 1, prod)

factor_X_car_comp = expand.grid(factor_make = c(0.5,1.0,1.5),
                                factor_model = c(0.7,1.0,1.3),
                                factor_style = c(0.8,1.0,1.5),
                                factor_car_age = seq(0.8,1.8, by = 0.1))
factor_X_car_comp$X_car_factor_comp = apply(factor_X_car_comp, 1, prod)
P_X_car = cbind(X_car[c("keys")], factor_X_car_liab[c("X_car_factor_liab")], factor_X_car_comp[c("X_car_factor_comp")])

#### Accident History Pricing Matrix P_A ####
## A = (SUM(majorA_t-1, ... , majorA_t-6), SUM(minorA_t-1, ... , minorA_t-6))
## tier includes many types of violations, I use major as at fault accident, minor as speeding
require(dplyr)
A = expand.grid(majA = seq(0, 3), 
                minA = seq(0, 6))
A$keys = as.vector(paste(A$majA, A$minA, sep = ","))

point_A = expand.grid(P_majA = c(0, 4, 7, 16),
                      P_minA = seq(0, 12, by=2))
point_A$point = rowSums(point_A)

factor_point_A = read.csv("./raw/tier_factor_bi_tbl.csv", header=T)

P_A = merge(x = point_A[c("point")], y = factor_point_A, by.x = "point", by.y = "tier_pt_bi")
P_A = cbind(A[c("keys")], P_A[c("tier_factor")])
P_A = rename(P_A, A_factor = tier_factor)

### Age Matrix ###
Ages = expand.grid(age = seq(22,42))

#### Monitoring Tier (0.5x observable risk to 1.5x) Pricing Matrix P_M ####

m = c(0,1)
m_factor = c(1,0.9)
P_m = as.data.frame(cbind(m, m_factor))
P_M = read.csv("./raw/m_factor.csv", header=T)

#### Base Pricing
P_base = read.csv("./raw/base_rate.csv", header=T)
P_base = P_base[rowSums(!is.na(P_base[, 2:9])) > 0,]
P_base = rename(P_base, geo_type = URBAN.IND)
P_base$liab = P_base$BI + P_base$PD
P_base$comp = P_base$COMP + P_base$COLL + P_base$MED + P_base$PIP
P_l_base = P_base[c("geo_type", "liab")]
P_c_base = P_base[c("geo_type", "comp")]

#### Simulate Pricing ####
## It's much more efficient to separately calculate each factor
## than searching over the entire pricing table
## P = P(X, A, y)
## For each coverage type, P = Base rate by geo type * tier factor * coverage factor 
## * hh_risk factor * monitoring tier (if period > 0 and m=1) * car factor
require(hash)
P_l_base_hash = hash(keys = P_l_base$geo_type, values = P_l_base$liab)
P_c_base_hash = hash(keys = P_c_base$geo_type, values = P_c_base$comp)
P_X_hh_l_hash = hash(keys = P_X_hh$keys, values = P_X_hh$X_hh_factor_liab)
P_X_hh_c_hash = hash(keys = P_X_hh$keys, values = P_X_hh$X_hh_factor_comp)
P_X_car_l_hash = hash(keys = P_X_car$keys, values = P_X_car$X_car_factor_liab)
P_X_car_c_hash = hash(keys = P_X_car$keys, values = P_X_car$X_car_factor_comp)
P_y_l_hash = hash(keys = P_y_l$keys, values = P_y_l$y_l_factor) 
P_y_c_hash = hash(keys = P_y_c$keys, values = P_y_c$y_c_factor)
P_A_hash = hash(keys = P_A$keys, values = P_A$A_factor)
P_m_hash = hash(keys = P_m$m, values = P_m$m_factor)
P_M_hash = hash(keys = P_M$m_result, values = P_M$m_factor)

Pricing <- function(geo_string_var, majA_var, minA_var, female_var, married_var, spouse_rated_var, age_var, make_var, model_var, style_var, car_age_var, u_l_var_000, u_c_var_000, d_c_var, m_var, m_tier_var, t_var){
  # the actual age schedule is not continuous and have no real patterns
  age_adj = ifelse(age_var < 15, 1, ifelse(age_var > 91, 91, ifelse(age_var>75, ceiling(age_var/2)*2 - 1, ifelse(age_var>40 & age_var<50, 40, ifelse(age_var>25 & age_var<60, floor(age_var/5)*5, age_var)))))
  # car age cutoffs are my assumption
  car_age_adj = ifelse(car_age_var > 20, 20, floor(car_age_var/2)*2)
  p_l = as.numeric(values(P_l_base_hash, keys = geo_string_var))
  p_c = as.numeric(values(P_c_base_hash, keys = geo_string_var))
  p_A = as.numeric(values(P_A_hash, keys = paste(majA_var, minA_var, sep=",")))
  p_X_hh_l = as.numeric(values(P_X_hh_l_hash, keys = paste(female_var, married_var, spouse_rated_var, age_adj, sep=",")))
  p_X_hh_c = as.numeric(values(P_X_hh_c_hash, keys = paste(female_var, married_var, spouse_rated_var, age_adj, sep=",")))
  p_X_car_l = as.numeric(values(P_X_car_l_hash, keys = paste(make_var, model_var, style_var, car_age_adj, sep=",")))
  p_X_car_c = as.numeric(values(P_X_car_c_hash, keys = paste(make_var, model_var, style_var, car_age_adj, sep=",")))
  p_y_l = as.numeric(values(P_y_l_hash, keys = u_l_var_000))
  p_y_c = as.numeric(values(P_y_c_hash, keys = paste(u_c_var_000, d_c_var, sep=",")))
  p_m = as.numeric(values(P_m_hash, keys = m_var))
  p_M = as.numeric(values(P_M_hash, keys = m_tier_var))
  p_y_X = p_l * p_y_l * p_X_hh_l * p_X_car_l + p_c * p_y_c * p_X_hh_c * p_X_car_c
  if(t_var == 0){P =  p_y_X * p_A * p_m}
  else{P = p_y_X * p_A * ((1 - m_var) * 1 + m_var * p_M)}
  return(P)
}

Pricing_l <- function(geo_string_var, majA_var, minA_var, female_var, married_var, spouse_rated_var, age_var, u_l_var_000, m_var, m_tier_var, t_var){
  age_adj = ifelse(age_var < 15, 1, ifelse(age_var > 91, 91, ifelse(age_var>75, ceiling(age_var/2)*2 - 1, ifelse(age_var>40 & age_var<50, 40, ifelse(age_var>25 & age_var<60, floor(age_var/5)*5, age_var)))))
  p_l = as.numeric(values(P_l_base_hash, keys = geo_string_var))
  p_c = as.numeric(values(P_c_base_hash, keys = geo_string_var))
  p_A = as.numeric(values(P_A_hash, keys = paste(majA_var, minA_var, sep=",")))
  p_X_hh_l = as.numeric(values(P_X_hh_l_hash, keys = paste(female_var, married_var, spouse_rated_var, age_adj, sep=",")))
  p_y_l = as.numeric(values(P_y_l_hash, keys = u_l_var_000))
  p_m = as.numeric(values(P_m_hash, keys = m_var))
  p_M = as.numeric(values(P_M_hash, keys = m_tier_var))
  if(t_var == 0){P = p_l * p_y_l * p_A * p_X_hh_l * p_m}
  else{P = p_l * p_y_l * p_A * p_X_hh_l * ((1 - m_var) * 1 + m_var * p_M)}
  return(P)
}

## For someone with geo_type = C, A = (0,2), X = (2,2), y_l = (100000,1), y_c = (10000,2000,1)
Pricing("C", 0, 2, 1, 0, 0, 24, "med", "high", "low", 4, 100, 10, 2000, 1, 0.9, 1)
Pricing_l("C", 0, 2, 1, 0, 0, 24, 100, 1, 0.9, 1)

#### Monitoring Pricing ####
require(tidyr)
P_M$fair_factor = P_M$m_result
df_P_M <- gather(P_M, variable, value, -m_result)

require(ggplot2)
require(scales)
gg_m_factor <-
  ggplot(df_P_M, aes(x=m_result, y=value, group=variable, colour=variable)) + 
  geom_line() +
  #xlim(1985,2014) +
  fte_theme() + 
  scale_colour_brewer(palette = "Set2") +
  theme(legend.position="bottom") +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) +
  xlab("monitoring result (compared to premonitoring cost estimates)") +
  ylab("monitoring pricing factor (compare to premonitoring pricing)") +
  ggtitle("Monitoring Discount/Surcharge")
ggsave("output/figures/gg_m_factor.png")

ranPricingCode = TRUE