data {
  int<lower=1> n;  
  int<lower=1>k;   
  int y[n];       
  matrix[n,k] x;  
}


parameters {
  real intercept;
  vector[k] coeff;  
}


model {
  vector[n] p;
  p= inv_logit(intercept + x*coeff[1:k]);  
  y~bernoulli(p);
  intercept~lognormal(0,1);  
  coeff[1] ~normal(0,1);
  coeff[2] ~ exponential(coeff[2])
  for(i in 3:k)
    coeff[i] ~lognormal(0,1);
  target += if_else(coeff[1] > coeff[9], 1, 0);
}






