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
  real sigma_alpha;
}


// parameters {
//   real beta_0;
//   real gamma_0;
//   real<lower=0> sigma_beta;
//   real<lower=0> sigma_gamma;
//   real<lower=0> sigma;
//   real<upper=0> beta[m];
//   real<lower=0> gamma[m];
//   real alpha[m];
// }

generated quantities {
  real<lower=0> y[N];
  real alpha[m];
  real beta[m];
  real gamma[m];
  real beta_0 = normal_rng(beta_pars[1], beta_pars[2]);
  real sigma_beta = fabs(normal_rng(0, beta_pars[3]));
  real gamma_0 = normal_rng(gamma_pars[1], gamma_pars[2]);
  real sigma_gamma = fabs(normal_rng(0, gamma_pars[3]));
  real sigma = fabs(normal_rng(0, sigma_par));
  real precio_prom = mean(precio);
  for(i in 1:m){
    alpha[i] = fabs(normal_rng(0, sigma_alpha));
    beta[i] = normal_rng(beta_0, sigma_beta);
    gamma[i] = normal_rng(gamma_0, sigma_gamma);
  }
  for(i in 1:N){
    real epsilon = normal_rng(0, sigma);
    int tda = tienda_id[i];
    real log_y = alpha[tda] + beta[tda] * log(precio[i] / precio_prom) + gamma[tda] * acv_disp[i] + epsilon;
    y[i] = exp(log_y);
  }
}
