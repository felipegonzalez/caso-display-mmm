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
  real gamma_0 = fabs(normal_rng(gamma_pars[1], gamma_pars[2]));
  real sigma_gamma = fabs(normal_rng(0, gamma_pars[3]));
  real sigma = fabs(normal_rng(0, sigma_par));
  real precio_prom = mean(precio);
  real incremental_disp[m];
  real retorno_disp[m];
  real retorno_disp_pct[m];
  real y_total[m];
  for(i in 1:m){
    alpha[i] = fabs(normal_rng(alpha_pars[1], alpha_pars[2]));
    beta[i] = normal_rng(beta_0, sigma_beta);
    gamma[i] = normal_rng(gamma_0, sigma_gamma);
    incremental_disp[i] = 0.0;
    retorno_disp[i] = 0.0;
    y_total[i] = 0.0;
  }
  for(i in 1:N){
    real epsilon = normal_rng(0, sigma);
    int tda = tienda_id[i];
    real log_y = alpha[tda] + beta[tda] * log(precio[i] / precio_prom) + gamma[tda] * acv_disp[i];
    incremental_disp[tda]+= exp(gamma[tda] * acv_disp[i]);
    retorno_disp[tda]+= (exp(gamma[tda] * acv_disp[i]) - 1.0) * exp(alpha[tda]);
    y[i] = exp(log_y + epsilon);
    y_total[tda]+= y[i];
  }
  for(j in 1:m){
    // contribución a precio promedio
    retorno_disp_pct[j] = 100.0 * retorno_disp[j] / y_total[j] ;
  }
}
