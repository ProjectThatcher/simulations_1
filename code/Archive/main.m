% This will be converted to a function of X later

% This code simulates the optimal path of contracting choices for drivers
% with inherent probabilities of accident that are to be generated

% Start out with 1 driver with a uniform prob:
poa = rand(1);

% Assume finite time lines no longer than T are considered; We can relax the
% uniformity of T later
T = 20;

% Contract choices are tuples y = (u_l, delta_c, u_c): limits and
% deductibles on liability and comprehensive claims
% Y is an array of contract choices y_i
Y_options = {y_1,y_2,y_3};

% For each contracting choice, there is a TxT matrix of premiums that
% determines a driver i's premium after t periods (row t) given that i has
% had t' accidents (column t') up till then.
P_options = {P_1,P_2,P_3};

% Initially assume all drivers have the same risk aversion parameter;
% expand this later
alpha_ra = .5;

% Assume homogeneous time discount factor
beta = .95;

% Given probability of accident and a contract choice, can compute the
% following period's expected cost of participating/driving
% NOTE: This will need to take in a distribution of costs given an accident
pi_t = @(poa,contract_index,num_acc,t)(-getPrice(Y_options,P_options,contract_index,num_acc,t) - getPeriodExpectedCost(Y_options,P_options,poa,contract_index,num_acc));

% Poisson probability of claim
% LogNormal distribution of costs

% This is the period (flow) utility as a function of prob of accident,
% contract choice and accident history at the end of the period
v_t = @(poa,contract_index,num_acc,t)(pi_t(poa,contract_index,num_acc,t) - alpha_ra.*(pi_t(poa,contract_index,num_acc,t)).^2);

% This is the value function going forward from time t assuming an optimal
% path going forward.
% NOTE: THIS ASSUMES YOU CAN ONLY GET INTO AT MOST ONE ACCIDENT IN ONE
% PERIOD!!! [EASY FOR NOW BUT CAN BE CHANGED LATER]
V_t = @(poa, contract_index, t, v_plus1_A, v_plus1_Aplus)(v_t(poa,contract_index,num_acc,t) + beta.*((1-poa).*v_plus1_A + poa.* v_plus1_Aplus));

Y_path = zeros(T,T);
V = zeros(T,T);

for i=1:T
    max_y = fmincon(@(contract_choice)(-v_t(poa,contract_choice,i,T)),0,[],[],[],[],0,1); 
    Y_path(i,T) = max_y;
    V(i,T) = v_t(poa,max_y,i,T);
end

for t_inc = 1:1:T-1
    t = T-t_inc;

    for i=1:t
        vp = V(i,t+1);
        vp_plus = V(i+1,t+1);
        max_y = fmincon(@(contract_choice)(-V_t(poa,contract_choice,i,vp,vp_plus)),0,[],[],[],[],0,1); 
        Y_path(i,t) = max_y;
        V(i,t) = V_t(poa,max_y,i,t,vp,vp_plus);
    end
    
end 
