data {
  int m;  //numero de tiendas
  int T; // máximo numero de semanas
  int N; // total de observaciones
  int n[m]; //numero de semanas de observaciones por tienda
  int ind[m, T]; // indice para tienda i y semana t
  int y[N], // volumen de ventas por semana
  real acv_disp[N]; //acv de exhibición por tienda y semana
  real precio[N];´
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta_0;
  real gamma_0;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma;
  real<upper=0> beta[m];
  real<lower=0> gamma[m];
  real alpha[m];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  y ~ normal(mu, sigma);
}

