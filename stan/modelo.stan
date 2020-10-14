data {
  int m;  //numero de tiendas
  int N; //numero de datos
  int tienda_id[N]; // indice para tienda i
  int semana_id[N]; // indice para semana t
  real acv_disp[N]; //acv de exhibición por tienda y semana
  real precio[N];
  real beta_pars[3];
  real gamma_pars[3];
  real sigma_par;
  real alpha_pars[2];
  real y[N]; //ventas obervadas
}

transformed data{
  real precio_prom;
  precio_prom = mean(precio);
}
parameters {
  real alpha[m];
  real beta[m];
  real gamma[m];
  real beta_0;
  real gamma_0;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma;
}

model {
  // poblacion
  beta_0 ~  normal(beta_pars[1], beta_pars[2]);
  sigma_beta ~ normal(0, beta_pars[3]);
  gamma_0 ~ normal(gamma_pars[1], gamma_pars[2]);
  sigma_gamma ~ normal(0, gamma_pars[3]);
  sigma ~ normal(0, sigma_par);
  // tiendas
  alpha ~ normal(alpha_pars[1], alpha_pars[2]);
  beta ~ normal(beta_0, sigma_beta);
  gamma ~ normal(gamma_0, sigma_gamma);
  //observaciones
  for(i in 1:N){
    int tda = tienda_id[i];
    real log_y_pred = alpha[tda] + beta[tda] * log(precio[i] / precio_prom) + gamma[tda] * acv_disp[i];
    y[i] ~ lognormal(log_y_pred, sigma);
  }
}

