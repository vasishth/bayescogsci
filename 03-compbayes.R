## ---- echo = FALSE, message=FALSE------------------------------------------
lst_cloze_data <- list(k = 80, N = 100)
# Fit the model with the default values of number of chains and iterations
# chains = 4,    iter = 2000
fit_cloze <- stan(
  file = "stan_models/binomial_cloze.stan", data = lst_cloze_data,
  warmup = 1000,
  iter = 21000
)


## ---- echo = FALSE---------------------------------------------------------
samples_theta <- rstan::extract(fit_cloze)$theta
some_samples <- toString(round(head(samples_theta, 20), 3))
df_fit_cloze <- data.frame(theta = samples_theta)
diff_means <- format(mean(samples_theta) - 84 / (84 + 24), digits =1)
diff_var <- format(sd(samples_theta)^2 - 84 * 24 / ((84 + 24)^2 * (84 + 24 + 1)), digits=1)


## ----betapost, fig.cap="(ref:betapost)",echo = FALSE-----------------------
ggplot(df_fit_cloze, aes(theta)) +
  geom_histogram(binwidth = .01, colour = "gray", alpha = .5, aes(y = ..density..)) +
  stat_function(fun = dbeta, color = "red", args = list(
    shape1 = 84,
    shape2 = 24
  ))


## ---- reading_noreading, message = FALSE-----------------------------------
df_noreading_data <- read_csv("./data/button_press.csv")
df_noreading_data


## ----m1visualize, fig.cap="Visualizing the data"---------------------------
ggplot(df_noreading_data, aes(rt)) +
  geom_density() +
  ggtitle("Button-press data")


## ---- message = FALSE, cache = TRUE----------------------------------------
fit_press <- brm(rt ~ 1,
  data = df_noreading_data,
  family = gaussian(),
  prior = c(
    prior(uniform(0, 60000), class = Intercept),
    prior(uniform(0, 2000), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000
)


## ----warmup, fig.cap = "(ref:warmup)", echo = FALSE------------------------
plot_all <- function(fit, xlab=500, ylab =100){
  chains <- as.mcmc(fit, inc_warmup = TRUE) %>%
    map_dfr(~ as.data.frame(.x) %>%
              as_tibble() %>%
              mutate(iter = 1:n()), .id = "chain") %>%
    rename(mu = b_Intercept) %>%
    dplyr::select(-`lp__`) %>%
    tidyr::gather("parameter", "value", -iter, -chain)

  ggplot(chains, aes(x = iter, y = value, color = chain)) + geom_line() +
    facet_wrap(~parameter, ncol = 1) +
    geom_vline(xintercept = 1000, linetype = "dashed") +
    xlab("Iteration number") +
    ylab("Sample value")+
    annotate("text", x=xlab, y =ylab, color= "black", label="Warm-up", size =5.2)
}

plot_all(fit_press)


## ----warmup2, fig.cap = "(ref:warmup2)", echo = FALSE, message = FALSE, warning = FALSE----
data_mm <- tibble(rt = rnorm(500, c(5,3000), c(5,5)))
fit_press_bad <- brm(rt ~ 1,
  data = data_mm,
  family = gaussian(),
  prior = c(
    prior(uniform(0, 60000), class = Intercept),
    prior(uniform(0, 2000), class = sigma)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000
  )

plot_all(fit_press_bad, ylab = 1000)


## --------------------------------------------------------------------------
posterior_samples(fit_press) %>% str() 


## --------------------------------------------------------------------------
plot(fit_press)


## ---- results = "hold"-----------------------------------------------------
fit_press
# posterior_summary(fit_press) is also useful


## --------------------------------------------------------------------------
posterior_samples(fit_press)$b_Intercept %>% mean()
posterior_samples(fit_press)$b_Intercept %>% quantile(c(0.025, .975))


## --------------------------------------------------------------------------
normal_predictive_distribution <- function(mu_samples, sigma_samples, N_obs) {
  # empty data frame with headers:
  df_pred <- tibble(trialn = numeric(0),
                    rt_pred = numeric(0),
                    iter = numeric(0))
  # i iterates from 1 to the length of mu_samples,
  # which we assume is identical to 
  # the length of the sigma_samples:
  for (i in seq_along(mu_samples)) {
    mu <- mu_samples[i]
    sigma <- sigma_samples[i]
    df_pred <- bind_rows(
      df_pred,
      tibble(
        trialn = seq_len(N_obs), #1, 2,... N_obs
        rt_pred = rnorm(N_obs, mu, sigma),
        iter = i
      )
    )
  }
  df_pred
}


## --------------------------------------------------------------------------
tic()
N_samples <- 1000
N_obs <- nrow(df_noreading_data)
mu_samples <- runif(N_samples, 0, 60000)
sigma_samples <- runif(N_samples, 0, 2000)

normal_predictive_distribution(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs = N_obs
)
toc()


## --------------------------------------------------------------------------
normal_predictive_distribution_fast <- function(mu_samples,
                                                sigma_samples,
                                                N_obs) {
  # map_dfr works similarly to lapply, it essentially runs
  # a for-loop, and builds a dataframe with the output.
  # We iterate over the values of mu_samples and sigma_samples
  # simultaneously, and in each iteration we bind a new
  # data frame with N_obs observations.
  map2_dfr(mu_samples, sigma_samples, function(mu, sigma) {
    tibble(
      trialn = seq_len(N_obs),
      rt_pred = rnorm(N_obs, mu, sigma)
    )
  }, .id = "iter") %>%
    # .id is always a string and needs to be converted to a number
    mutate(iter = as.numeric(iter))
}

tic()
(prior_pred <- normal_predictive_distribution_fast(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs))
toc()


## ----priorpred-simple, fig.cap = "(ref:priorpred-simple)", message = FALSE----
prior_pred %>%
  filter(iter <= 18) %>%
  ggplot(aes(rt_pred)) +
  geom_histogram() +
  facet_wrap(~iter, ncol = 3)


## ----priorpred-stats,fig.cap="(ref:priorpred-stats)", message = FALSE------
prior_pred %>%
  group_by(iter) %>%
  summarize(
    min_rt = min(rt_pred),
    max_rt = max(rt_pred),
    average_rt = mean(rt_pred)
  ) %>%
  # we convert the previous data frame to a long one,
  # where min_rt, max_rt, average_rt are possible values
  # of the columns "stat"
  pivot_longer(cols = ends_with("rt"),
               names_to = "stat",
               values_to = "rt") %>%
  ggplot(aes(rt)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~stat, ncol = 1)


## to-do: I guess this section could be completed. We should do a bit more justice to people advocating for uninformative priors, and also refer to this idea of uninformative priors that are invariant to transformations.

## SV: I think more discussion would help beginners. We need to test this section out with readers who are complete beginners.


## to-do: SV: could we show the posteriors from the last and this model side by side using ridge plots?


## ---- message = FALSE, cache = TRUE----------------------------------------
# We fit the model with the default setting of the sampler:
# 4 chains, 2000 iterations with half of them as warmup.
fit_press_unif <- brm(rt ~ 1,
  data = df_noreading_data,
  family = gaussian(),
  prior = c(
    prior(uniform(-10^10, 10^10), class = Intercept),
    prior(uniform(0, 10^10), class = sigma)
  )
)


## --------------------------------------------------------------------------
fit_press_unif


## ---- message = FALSE, cache = TRUE----------------------------------------
fit_press_inf <- brm(rt ~ 1,
  data = df_noreading_data,
  family = gaussian(),
  prior = c(
    prior(normal(400, 10), class = Intercept),
    # brms knows that SD needs to be bounded by zero:
    prior(normal(100, 10), class = sigma)
  )
)


## --------------------------------------------------------------------------
fit_press_inf


## --------------------------------------------------------------------------
N_obs <- nrow(df_noreading_data)
mu_samples <- posterior_samples(fit_press)$b_Intercept
sigma_samples <- posterior_samples(fit_press)$sigma
normal_predictive_distribution_fast(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs
)


## ----normalppc, fig.cap = "(ref:normalppc)", message = FALSE---------------
pp_check(fit_press, nsamples = 11, type = "hist")


## ----normalppc2, fig.cap = "(ref:normalppc2)" , message = FALSE------------
pp_check(fit_press, nsamples = 100)


## ----logndemo,out.width="48%",fig.cap="Two log-normal distributions with the same parameters generated by either generating samples from a log-normal distribution or exponentiating samples from a normal distribution.", message=FALSE, fig.show = "hold", fig.width=3----

mu <- 6
sigma <- 0.5
N <- 500000
# Generate N random samples from a log-normal distribution
sl <- rlnorm(N, mu, sigma)
ggplot(tibble(samples = sl), aes(samples)) +
  geom_histogram(binwidth = 50) +
  ggtitle("Log-normal distribution\n") +
  coord_cartesian(ylim = c(0, 70000), xlim = c(0, 2000))
# Generate N random samples from a normal distribution,
# and then exponentiate them
sn <- exp(rnorm(N, mu, sigma))
ggplot(tibble(samples = sn), aes(samples)) +
  geom_histogram(binwidth = 50) +
  ggtitle("Exponentiated samples of\na normal distribution") +
    coord_cartesian(ylim = c(0, 70000), xlim = c(0, 2000))


## --------------------------------------------------------------------------
N_samples <- 1000
N_obs <- nrow(df_noreading_data)
mu_samples <- runif(N_samples, 0, 8)
sigma_samples <- runif(N_samples, 0, 1)
prior_pred_ln <- exp(normal_predictive_distribution_fast(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs
))


## ----priorpredlogunif,fig.cap="(ref:priorpredlogunif)", message = FALSE----
prior_pred_ln %>%
  group_by(iter) %>%
  summarize(
    min_rt = min(rt_pred),
    max_rt = max(rt_pred),
    average_rt = mean(rt_pred),
    median_rt = median(rt_pred)
  ) %>%
  pivot_longer(cols = ends_with("rt"), names_to = "stat", values_to = "rt") %>%
  ggplot(aes(rt)) +
  scale_x_continuous("Reaction times in ms",
    trans = "log", breaks = c(0.001, 1, 10, 100, 1000, 10000, 100000)
  )+
  geom_histogram() +
  facet_wrap(~stat, ncol = 1)


## to-do: We should explain more the meaning of location of the prior of sigma being 0. Maybe using rtnorm(0,1,a=0)


## --------------------------------------------------------------------------
c(lower = exp(6 - 2 * 1.5),
  higher = exp(6 + 2 * 1.5))


## ----priorpredlognorm,fig.cap="(ref:priorpredlognorm)", message = FALSE, tidy=FALSE----

N_samples <- 1000
N_obs <- nrow(df_noreading_data)
mu_samples <- rnorm(N_samples, 6, 1.5)
sigma_samples <- rtnorm(N_samples, 0, 1, a = 0)
prior_pred_ln_better <- exp(normal_predictive_distribution_fast(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs
))
prior_pred_ln_better %>%
  group_by(iter) %>%
  summarize(
    min_rt = min(rt_pred),
    max_rt = max(rt_pred),
    average_rt = mean(rt_pred),
    median_rt = median(rt_pred)
  ) %>%
 pivot_longer(cols = ends_with("rt"),
              names_to = "stat", values_to = "rt") %>%
  ggplot(aes(rt)) +
  scale_x_continuous(trans = "log", breaks = c(0.001, 1, 100, 1000, 10000, 100000)) +
  geom_histogram() +
  facet_wrap(~stat, ncol = 1) +
  coord_cartesian(xlim = c(0.001, 300000))


## ---- message = FALSE, cache = TRUE----------------------------------------
fit_press_ln <- brm(rt ~ 1,
  data = df_noreading_data,
  family = lognormal(),
  prior = c(
    prior(normal(6, 1.5), class = Intercept),
    prior(normal(0, 1), class = sigma)
  )
)


## --------------------------------------------------------------------------
fit_press_ln


## ---- message = FALSE------------------------------------------------------
estimate_ms <- exp(posterior_samples(fit_press_ln)$b_Intercept)


## --------------------------------------------------------------------------
c(mean = mean(estimate_ms), quantile(estimate_ms, probs = c(.025, .975)))


## ----lognppc, message=FALSE, fig.cap="(ref:logppc)"------------------------
pp_check(fit_press_ln, nsamples = 100)


## ----ppcheckmin,fig.cap="Distribution of minimum values in a posterior predictive check. The minimum in the data is 110 ms.", fig.show="hold", message=FALSE, out.width = "45%", fig.width=4----
pp_check(fit_press, type = "stat", stat = "min") + ggtitle("Normal model")
pp_check(fit_press_ln, type = "stat", stat = "min") + ggtitle("Log-normal model")


## ----ppcheckmax,fig.cap="Distribution of maximum values in a posterior predictive check. The maximum in the data is 409 ms.", fig.show="hold", message=FALSE, out.width = "45%", fig.width=4----
pp_check(fit_press, type = "stat", stat = "max") + ggtitle("Normal model")
pp_check(fit_press_ln, type = "stat", stat = "max") + ggtitle("Log-normal model")


## ---- eval = FALSE---------------------------------------------------------
## fit_press_prior <- brm(rt ~ 1,
##   family = gaussian(),
##   prior = c(
##     prior(uniform(0, 60000), class = Intercept),
##     prior(uniform(0, 2000), class = sigma)
##   ),
##   sample_prior = "only",
##   warmup = 0,
##   chains = 1,
##   iter = 1000,
##   data = df_noreading_data
## )
## 
## # predict with summary = FALSE returns a N_samples * N_obs array:
## prior_pred <- predict(fit_press_prior, summary = FALSE) %>%
##   # array_banch converts it in a list of N_samples elements,
##   # and in each element a dataset of N_obs:
##   array_branch(1) %>%
##   # map_dfr loops over the N_sample elements of the list and builds a data frame
##   map_dfr(~ tibble(rt_pred = .x, trialn = seq_along(.x)), .id = "sample_n") %>%
##   # .id is always a string and needs to be converted
##   mutate(sample_n = as.numeric(sample_n))

