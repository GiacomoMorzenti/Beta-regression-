data {
	int<lower=1> N; // number of observations
	int<lower=1> J; // number of groups
	vector[N]    y; // outcomes
	int<lower=1> K; // number of covariates+intercept
	matrix[N,K]    X; // covariate
	real sd_prior;  // Prior standard deviation
	real<lower=0> g;
	int<lower=0> subject[N]; // subject ID

}


parameters {
	vector[K] beta;
	real<lower=0> phi;
	real<lower=0> sigma_u;
	  
	real U[J];
}


transformed parameters {
	vector<lower=0,upper=1>[N]  mu;
	vector<lower=0>[N] b;
	vector<lower=0>[N] a; 

	
	for (i in 1:N) {
		mu[i] = inv_logit(X[i,]* beta+ U[subject[i]]);
	}
	
	b = (1-mu)*phi;
	a = mu*phi;

}

model {  
//priors
	for (l in 1:K) {
		beta[l] ~ normal(0, sd_prior);
	}
	
	phi ~  inv_gamma(g, g);
	sigma_u ~ inv_gamma(g, g);
	
// construct random effects
	U ~ normal(0, sigma_u);
 
// likelihood of log(y)
	for(i in 1:N)
	target += beta_lpdf(y[i] | a[i],b[i]); 
}

generated quantities{
	vector[N] log_lik;
	for(i in 1:N){
		log_lik[i] = beta_lpdf(y[i] | a[i],b[i]); 
	}
}   
