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
	real<lower=0,upper=1> w;
	real<lower=0,upper=1> p;
	real<lower=0> phi;
	real<lower=0> sigma_u;
	  
	real U[J];
}


transformed parameters {
	vector<lower=0,upper=1>[N]  mu;
	vector<lower=0,upper=1>[N]  lambda1;
	vector<lower=0,upper=1>[N]  lambda2;
	vector<lower=0>[N] b1;
	vector<lower=0>[N] b2;
	vector<lower=0>[N] a1; 
	vector<lower=0>[N] a2;


	
	for (i in 1:N) {
		mu[i] = inv_logit(X[i,]* beta+ U[subject[i]]);
		  
		lambda1[i] = mu[i]+(1-p)*w*fmin(mu[i]/p,(1-mu[i])/(1-p));
		lambda2[i] = mu[i]-p*w*fmin(mu[i]/p,(1-mu[i])/(1-p));
	}
	
	//con probabilità p
	b2 = (1-lambda1)*phi;
	a2 = lambda1*phi;
	//con probabilità 1-p
	b1 = (1-lambda2)*phi;
	a1 = lambda2*phi;
}

model {  
//priors
	for (l in 1:K) {
		beta[l] ~ normal(0, sd_prior);
	}
	
	phi ~  inv_gamma(g, g);
	sigma_u ~ inv_gamma(g, g);
	
	p ~ uniform(0,1);
	w ~ uniform(0,1);
	
	// construct random effects
	U ~ normal(0, sigma_u);
 
// likelihood of log(y)
	for(i in 1:N)
	target += log_mix(p, beta_lpdf(y[i] | a2[i],b2[i]),
                         beta_lpdf(y[i] | a1[i], b1[i])); 
}

generated quantities{
	vector[N] log_lik;
	for(i in 1:N){
		log_lik[i] = log_mix(p, beta_lpdf(y[i] | a2[i],b2[i]),
                         beta_lpdf(y[i] | a1[i], b1[i]));
	}
}   
