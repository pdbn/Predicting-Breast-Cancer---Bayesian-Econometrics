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
  intercept~normal(0,1);
  coeff[1]~normal(0,1);
  coeff[2]~lognormal(0,1);
  for(i in 3:k)
  coeff[i]~normal(0,1);      
  
}






