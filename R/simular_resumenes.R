resumir_precio <- function(ajuste, sim_ensemble_datos){
  beta_sim_tbl <- ajuste$draws("beta") %>%
    as_draws_df() %>%
    as_tibble() %>%
    select(-.chain, -.iteration, -.draw) %>%
    ungroup() %>%
    mutate(n_sim = row_number()) %>%
    pivot_longer(cols = -n_sim, names_to = c("a", "tienda_id", "b"),
                 values_to = "beta", names_sep = "[\\[\\]]") %>%
    select(-a, -b)
  #f <- c(0.10, 0.50, 0.90)
  #beta_sim_tbl %>% group_by(tienda_id) %>%
  #  summarise(cuantiles = quantile(beta, f), f = f) %>%
  #  ungroup() %>%
  #  pivot_wider(names_from = f, values_from = cuantiles)
  beta_sim_tbl
}

contribucion_disp <- function(ajuste, ensemble_datos){
  datos_tbl <- tibble(tienda_id = ensemble_datos$tienda_id,
                      disp = ensemble_datos$acv_disp) %>%
    group_by(tienda_id) %>%
    summarise(disp_media = mean(disp))
  retorno_tbl <- ajuste$draws(c("retorno_disp", "retorno_disp_pct",
                                "incremental_disp", "gamma", "y_total")) %>%
    as_draws_df %>%
    as_tibble() %>%
    select(-.chain, -.iteration, -.draw) %>%
    mutate(n_sim = row_number()) %>%
    pivot_longer(cols = -n_sim, names_to = c("variable", "tienda_id", "b"),
                 values_to = "valor", names_sep = "[\\[\\]]") %>%
    select(-b) %>%
    mutate(tienda_id = as.integer(tienda_id)) %>%
    left_join(datos_tbl)
  retorno_tbl
}

simular_ensemble <- function(modelo, sim_datos, R = 1000){
  # simular
  ensemble_1 <- modelo$sample(
    data = sim_datos,
    iter_sampling = R, iter_warmup = 0,
    chains = 1,
    refresh = R, seed = 432,
    fixed_param = TRUE
  )
  ensemble_1
}

ajustar_modelo <- function(modelo, datos, beta, iter_sampling = 2000, iter_warmup = 2000){

  ajuste <- modelo$sample(data = datos,
                          seed = 2210,
                          iter_sampling = iter_sampling, iter_warmup = iter_sampling,
                          refresh = 0,
                          show_messages = FALSE)
  ajuste
}

ajustar_diagnosticos <- function(rep, modelo, datos, params,
                                 iter_sampling=2000, iter_warmup = 2000){

  ajuste <- ajustar_modelo(modelo, datos, iter_sampling = iter_sampling, iter_warmup = iter_warmup)
  suppressMessages(diagnostico <- ajuste$cmdstan_diagnose())
  suppressMessages(resumen <- ajuste$summary())

  # diagnosticar parámetros
  sims_tbl <- ajuste$draws(names(params)) %>% as_draws_df() %>% as_tibble()
  sbc_tbl <- sbc_rank(params, sims_tbl)
  tibble(rep = rep, params = list(params), sbc_rank = list(sbc_tbl),
         resumen = list(resumen), diagnosticos = list(diagnostico))
}

sbc_rank <- function(params_tbl, sims_tbl){
  params_nom <- names(params_tbl)
  sims_tbl_larga <- sims_tbl %>%
    filter((row_number() %% 10) == 0) %>% # adelgazar la cadena
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor")
  params_tbl_larga <- params_tbl %>%
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor_real")
  sbc_tbl <- sims_tbl_larga %>%
    left_join(params_tbl_larga, by = "parametro") %>%
    group_by(parametro) %>%
    summarise(sbc_rank = mean(valor_real < valor))
  sbc_tbl %>% pivot_wider( names_from = "parametro", values_from ="sbc_rank")
}

calcular_post_check <- function(ajuste, datos){

}
