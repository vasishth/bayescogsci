## to-do: This could be expanded much more.


## ----prepare-pupil, echo = FALSE, eval = !file.exists("./data/pupil_pilot.csv") | !file.exists("./data/pupil.csv")----
## df_pupil_full_control <- read_csv("./data/PLOS_raw_pupildata_controlexperiment.csv") %>%
##     rename(p_size = Pupilsize, load = Attentionalload, trial = Trial, time = Time)
## # "Pilot" data
## df_pupil_pilot <- df_pupil_full_control %>%
##     filter(Subnum==701, load ==0,time <100, trial ==5) %>%
##     select(time, p_size, load)
## write_csv(df_pupil_pilot, "./data/pupil_pilot.csv")
## # Averaged by trial data
## df_pupil <- df_pupil_full_control %>%
##     filter(Subnum==701, time > 100) %>%
##     group_by(trial, load) %>%
##     summarize(p_size = mean(p_size, na.rm= TRUE))
## write_csv(df_pupil, "./data/pupil.csv")


## ----mot, fig.cap = "(ref:mot)", out.width = "80%", echo = FALSE, fig.align = "center"----
knitr::include_graphics("cc_figure/MOT.png", dpi =1000)


## ---- message = FALSE-------------------------------
df_pupil_pilot <- read_csv("./data/pupil_pilot.csv")
df_pupil_pilot$p_size %>% summary()


## ---------------------------------------------------
c(qnorm(.025, 1000,500), qnorm(.975, 1000, 500))


## ---------------------------------------------------
c(qtnorm(.025, 0, 1000, a = 0), qtnorm(.975, 70,1000, a = 0))


## ---------------------------------------------------
c(qnorm(.025, 0,100), qnorm(.975, 0,100))


## to-do: maybe prior predictive distributions here??


## ---- message = FALSE-------------------------------
df_pupil_data <- read_csv("data/pupil.csv")
df_pupil_data <- df_pupil_data %>%
    mutate(c_load = load - mean(load))
df_pupil_data


## ----fitpupil, message = FALSE, results = "hide"----
fit_pupil <- brm(p_size ~ 1 + c_load,
                 data = df_pupil_data,
                 family = gaussian(),
                 prior = c(
                     prior(normal(1000, 500), class = Intercept),
                     prior(normal(0, 1000), class = sigma),
                     prior(normal(0, 100), class = b, coef = c_load)
                 )) 


## ---------------------------------------------------
plot(fit_pupil)
fit_pupil


## **Intercepts in `brms`**


## ----fitpupil2, message = FALSE, results = "hide"----
fit_pupil_non_centered <- brm(p_size ~ 0 + intercept + load,
                 data = df_pupil_data,
                 family = gaussian(),
                 prior = c(
                     prior(normal(800, 200), class = b, coef = intercept),
                     prior(normal(0, 1000), class = sigma),
                     prior(normal(0, 100), class = b, coef = load)
                 ))


## ---------------------------------------------------
posterior_summary(fit_pupil_non_centered)


## ---- eval = FALSE----------------------------------
## fit_pupil_wrong <- brm(p_size ~ 1 + load,
##                  data = df_pupil_data,
##                  family = gaussian(),
##                  prior = c(
##                      prior(normal(800, 100), class = Intercept),
##                      prior(normal(0, 1000), class = sigma),
##                      prior(normal(0, 100), class = b, coef = load)
##                  ))
## 


## ---- echo = FALSE----------------------------------
mean_load <- posterior_summary(fit_pupil)["b_c_load","Estimate"] %>%
    round(2)
load_l <- posterior_summary(fit_pupil)["b_c_load","Q2.5"]%>%
    round(2)
load_h <- posterior_summary(fit_pupil)["b_c_load","Q97.5"]%>%
    round(2)


## ---------------------------------------------------
mean(posterior_samples(fit_pupil)$b_c_load > 0)


## ---------------------------------------------------
# we start from an array of 1000 samples by 41 observations
df_pupil_pred <- posterior_predict(fit_pupil, nsamples = 1000) %>%
    # we convert it to a list of length 1000, with 41 observations in each element:
    array_branch(margin = 1) %>%
    # We iterate over the elements (the predicted distributions)
    # and we convert them into a long data frame similar to the data,
    # but with an extra column `iter` indicating from which iteration
    # the sample is coming from.
    map_dfr( function(yrep_iter) {
        df_pupil_data %>%
            mutate(p_size = yrep_iter)
    }, .id = "iter") %>%
    mutate(iter = as.numeric(iter))


## ----postpreddens, fig.cap ="(ref:postpreddens)" , message= FALSE, fig.height =9----
df_pupil_pred %>% filter(iter < 100) %>%
    ggplot(aes(p_size, group=iter)) +
    geom_line(alpha = .05, stat="density", color = "blue") +
    geom_density(data=df_pupil_data, aes(p_size),
                 inherit.aes = FALSE, size =1)+
    geom_point(data=df_pupil_data, aes(x=p_size, y = -0.001), alpha =.5,
               inherit.aes = FALSE)+
    coord_cartesian(ylim=c(-0.002, .01))+
    facet_grid(load ~ .) 


## ---------------------------------------------------
# predicted means:
df_pupil_pred_summary <- df_pupil_pred %>%
    group_by(iter, load) %>%
    summarize(av_p_size = mean(p_size))
# observed means:
df_pupil_summary <- df_pupil_data %>%
    group_by(load) %>%
    summarize(av_p_size = mean(p_size))


## ----postpredmean, fig.cap ="(ref:postpredmean)", message= FALSE----
ggplot(df_pupil_pred_summary, aes(av_p_size)) +
    geom_histogram(alpha=.5)+
    geom_vline(aes(xintercept= av_p_size),data= df_pupil_summary)+
    facet_grid(load ~ .)


## ---------------------------------------------------
lognormal_model_pred <- function(alpha_samples,
                                 beta_samples,
                                 sigma_samples,
                                 N_obs) {
    # pmap extends map2 (and map) for a list of lists:
    pmap_dfr(list(alpha_samples, beta_samples, sigma_samples),
             function(alpha, beta, sigma) {
                 tibble(
                     trialn = seq_len(N_obs),
                     # we center trial:
                     c_trial = trialn - mean(trialn),
                     # we change the likelihood: 
                     # Notice rlnorm and the use of alpha and beta
                     rt_pred = rlnorm(N_obs, alpha + c_trial * beta, sigma)
                 )
             }, .id = "iter") %>%
    # .id is always a string and needs to be converted to a number
        mutate(iter = as.numeric(iter))
}


## ---------------------------------------------------
N_obs = 361
alpha_samples <- rnorm(1000, 6, 1.5)
sigma_samples <- rtnorm(1000, 0, 1, a =0)
beta_samples <- rnorm(1000, 0, 1)

prior_pred <- lognormal_model_pred(
    alpha_samples = alpha_samples,
    beta_samples = beta_samples, 
    sigma_samples = sigma_samples,
    N_obs = N_obs)


## ---------------------------------------------------
median_effect <-
     prior_pred %>%
     group_by(iter) %>%
     mutate(diff = rt_pred - lag(rt_pred)) %>%
     summarize(
         median_rt = median(diff, na.rm = TRUE)
 )


## ----priorbeta,fig.cap="(ref:priorbeta)", message = FALSE----
median_effect %>%
    ggplot(aes(median_rt)) +
    geom_histogram()


## ---- echo = FALSE----------------------------------
beta_samples2 <- rnorm(1000, 0, .01)

prior_pred2 <- lognormal_model_pred(
    alpha_samples = alpha_samples,
    beta_samples = beta_samples2, 
    sigma_samples = sigma_samples,
    N_obs = N_obs)


## ----priorbeta2,fig.cap="(ref:priorbeta2)", message = FALSE, echo = FALSE----
    prior_pred2 %>%
    group_by(iter) %>%
    mutate(diff = rt_pred - lag(rt_pred)) %>%
    summarize(
        median_rt = median(diff, na.rm = TRUE)
    ) %>%
    ggplot(aes(median_rt))+
    geom_histogram()


## **Understanding the Log-normal likelihood**


## ----logexp, fig.cap="(ref:logexp)", echo = FALSE, fig.pos = "H", out.extra = ''----
df_a <- tibble(Intercept = seq(.1,15,.01),
             beta=.01,
           ms = exp(Intercept + beta) - exp(Intercept))
ggplot(df_a, aes(x = Intercept, y = ms)) +
  geom_line() +
  scale_y_continuous("Difference in reaction times between adjacent trials \n") +
  scale_x_continuous("Intercept value") +
  ylab("(A)")


## ----expgd, fig.cap="(ref:expgd)", echo = FALSE, fig.pos = "H", out.extra = ''----
N <- 300
df_dg <- tibble(
    n=rep(seq(1,N,5),2),
    Intercept = 6,
    beta = rep(c(-.01,.01),length(n)/2),
    ms = exp(Intercept + n* beta),
    type = rep(c("(A) Exponential decay", "(B) Exponential growth"),length(n)/2)
)


ggplot(df_dg, aes(x = n, y = ms)) +
    geom_point() +
    scale_x_continuous("Trial number") +
    scale_y_continuous("Reaction times  in milliseconds \n") +
    facet_grid(type ~.,scales = "free")



## ----expgd2, fig.cap="(ref:expgd2)", echo = FALSE, fig.pos = "H",out.extra =''----
N <- 300
df_dg <- tibble(
    n=rep(seq(1,N,5),2),
    Intercept = 6,
    beta = rep(c(-.01,.01),length(n)/2),
    ms = exp(Intercept + log(n)* beta),
    type = rep(c("(A) Exponential decay", "(B) Exponential growth"),length(n)/2)
)

ggplot(df_dg, aes(x = n, y = ms)) +
    geom_point() +
    scale_x_continuous("Trial number") +
    scale_y_continuous("Reaction times  in milliseconds \n") +
    facet_grid(type ~.,scales = "free")



## ----  message = FALSE------------------------------
df_noreading_data <- read_csv("./data/button_press.csv")
df_noreading_data <- df_noreading_data %>%
    mutate(c_trial = trialn - mean(trialn))

fit_press_trial <- brm(rt ~ 1 + c_trial,
  data = df_noreading_data,
  family = lognormal(),
  prior = c(
    prior(normal(6, 1.5), class = Intercept),
    prior(normal(0, 1), class = sigma),
    prior(normal(0, 1), class = b, coef = c_trial)
  )
)


## ---------------------------------------------------
## print posterior means and 95% credible intervals to four decimal places:
posterior_summary(fit_press_trial)[1:3,c(1,3,4)]


## ---------------------------------------------------
posterior_summary(fit_press_trial, pars = "b_c_trial")


## ---------------------------------------------------
plot(fit_press_trial)


## ---- echo=FALSE, results="hide"--------------------
alpha_samples<- posterior_samples(fit_press_trial)$b_Intercept
beta_samples<- posterior_samples(fit_press_trial)$b_c_trial

beta_ms<- exp(alpha_samples) - exp(alpha_samples-beta_samples)

beta_msmean <- round(mean(beta_ms),5)
beta_mslow <- round(quantile(beta_ms,prob=0.025),5)
beta_mshigh <- round(quantile(beta_ms,prob=0.975),5)

beta_mean <- round(mean(beta_samples),5) %>% format() 
beta_low <- round(quantile(beta_samples,prob=0.025),5) %>% format() 
beta_high <- round(quantile(beta_samples,prob=0.975),5) %>% format() 


## ---------------------------------------------------
alpha_samples<- posterior_samples(fit_press_trial)$b_Intercept
beta_samples<- posterior_samples(fit_press_trial)$b_c_trial
effect_middle_ms <- exp(alpha_samples) - exp(alpha_samples - 1* beta_samples)
## ms effect in the middle of the expt (mean trial vs. mean trial - 1 ) 
c(mean = mean(effect_middle_ms), quantile(effect_middle_ms, c(.025,.975)))


## ---------------------------------------------------
first_trial <- min(df_noreading_data$c_trial)
second_trial <- min(df_noreading_data$c_trial) +1
effect_beginning_ms <- exp(alpha_samples+  second_trial * beta_samples) -
    exp(alpha_samples+  first_trial * beta_samples)
## ms effect from first to second trial:
c(mean = mean(effect_beginning_ms), quantile(effect_beginning_ms, c(.025,.975)))


## ----oberauer, fig.cap = "(ref:oberauer)", fig.height =5, echo = FALSE----
knitr::include_graphics("cc_figure/fig1_oberauer_2019_modified.png")


## ----  message = FALSE, warning = FALSE-------------
df_recall_data <- read_table2("./data/PairsRSS1_all.dat",
                              col_names = c("subject", "session", "block",
                                            "trial", "set_size",
                                            "response_size_list",
                                            "response_size_new_words",
                                            "tested", "response",
                                            "response_category", "rt")) %>%
    # We ignore the type of incorrect responses (the focus of the paper)
    mutate(correct = if_else(response_category ==1, 1, 0)) %>%
    # and we only use the data from the free recall task:
    # (when there was no list of possible responses)
    filter(response_size_list + response_size_new_words == 0) %>%
    # We select one subject
    filter(subject == 10) %>%
    mutate(c_set_size = set_size - mean(set_size))
# we can ignore the warning from read_table

# Set sizes in the dataset:
df_recall_data$set_size %>%
    unique
# Trials by set size
df_recall_data %>%
    group_by(set_size) %>%
    count()


## ---------------------------------------------------
df_recall_data<-df_recall_data[,c(12,13)]
df_recall_data


## ---------------------------------------------------
as.numeric(rbernoulli(n=10,p=0.5))


## ----logisticfun, fig.cap = "The logit and inverse logit (logistic) function." ,echo = FALSE,warning=FALSE----
x<-seq(0.001,0.999,by=0.001)
y<-log(x/(1-x))
logistic_dat<-data.frame(theta=x,eta=y)

p1<-qplot(logistic_dat$theta,logistic_dat$eta,geom="line")+xlab(expression(theta))+ylab(expression(eta))+ggtitle("The logit link")+
  annotate('text', 
           x = 0.3, y = 4, 
        label = expression(paste(eta,"=",g(theta))),parse = TRUE,size=8)
  

p2<-qplot(logistic_dat$eta,logistic_dat$theta,geom="line")+xlab(expression(eta))+ylab(expression(theta))+ggtitle("The inverse logit link (logistic)")+annotate('text', 
           x = -3.5, y = 0.80, 
        label = expression(paste(theta,"=",g^-1, eta)),parse = TRUE,size=8)

gridExtra::grid.arrange(p1,p2,ncol=2)

#ggplot(tibble(x=c(-5, 5)), aes(x)) +
#    stat_function(fun = plogis) +
#    ylab("Probability")+
#    xlab("x (Log-odds)")


## ----echo=FALSE-------------------------------------
m_logit<-glm(correct~c_set_size,
    df_recall_data,family=binomial("logit"))
alpha_hat<-round(summary(m_logit)$coefficients[1,1],2)
beta_hat<-round(summary(m_logit)$coefficients[2,1],2)


## ----logoddspriorsf, fig.cap= "(ref:logoddspriorsf)",fig.show='hold', out.width = "45%", fig.width =3, fig.height =3----
samples_logodds <- tibble(alpha = rnorm(100000, 0, 4))
samples_prob <- tibble(p = plogis(rnorm(100000, 0, 4)))
ggplot(samples_logodds, aes(alpha)) +
    geom_density()
ggplot(samples_prob, aes(p)) +
    geom_density()


## ----logoddspriorsf2, fig.cap= "(ref:logoddspriorsf2)",fig.show='hold', echo=FALSE, out.width = "45%", fig.width =3, fig.height =3----
samples_logodds <- tibble(alpha = rnorm(100000, 0, 1.5))
samples_prob <- tibble(p = plogis(rnorm(100000, 0, 1.5)))
ggplot(samples_logodds, aes(alpha)) +
    geom_density()
ggplot(samples_prob, aes(p)) +
    geom_density()


## ---------------------------------------------------
logistic_model_pred <- function(alpha_samples,
                                beta_samples,
                                set_size,
                                 N_obs) {
    map2_dfr(alpha_samples, beta_samples,
             function(alpha, beta) {
                 tibble(
                     set_size = set_size,
                     # we center size:
                     c_set_size = set_size - mean(set_size),
                     # change the likelihood: 
                     # Notice the use of a link function for alpha and beta
                     theta = plogis(alpha + c_set_size * beta),
                     # There is no bernoulli in R, but we can just use
                     # binomial when the total number of trials is 1
                     correct_pred = rbinom(N_obs, size = 1, prob = theta)
                 )
             }, .id = "iter") %>%
    # .id is always a string and needs to be converted to a number
        mutate(iter = as.numeric(iter))
}


## ---------------------------------------------------
N_obs <- 800
set_size <- rep(c(2,4,6,8),200)


## ---------------------------------------------------
alpha_samples <- rnorm(1000, 0, 1.5)
sds_beta <- c(1, 0.5, 0.1,0.01, 0.001) 
prior_pred <- map_dfr(sds_beta, function(sd) {
    beta_samples <- rnorm(1000, 0, sd)
    logistic_model_pred(alpha_samples = alpha_samples,
                        beta_samples = beta_samples,
                        set_size = set_size,
                        N_obs = N_obs
                        ) %>%
        mutate(prior_beta_sd = sd)
})


## ---------------------------------------------------
mean_accuracy <-
     prior_pred %>%
     group_by(prior_beta_sd, iter, set_size) %>%
     summarize(accuracy = mean(correct_pred)) %>%
     mutate(prior = paste0("Normal(0, ",prior_beta_sd,")"))



## ----priors4beta,fig.cap="(ref:priors4beta)", message = FALSE----
mean_accuracy %>%
    ggplot(aes(accuracy)) +
    geom_histogram() +
    facet_grid(set_size~prior)


## ---------------------------------------------------
diff_accuracy <- mean_accuracy %>%
    arrange(set_size) %>%
    group_by(iter, prior_beta_sd) %>%
    mutate(diffaccuracy = accuracy - lag(accuracy) ) %>%
    mutate(diffsize = paste(set_size,"-",  lag(set_size))) %>%
    filter(set_size >2)



## ----priors4beta2,fig.cap="(ref:priors4beta2)", message = FALSE----
diff_accuracy %>%
    ggplot(aes(diffaccuracy)) +
    geom_histogram() +
    facet_grid(diffsize~prior)


## ----  message = FALSE, warning = FALSE-------------
df_recall_data <- read_table2("./data/PairsRSS1_all.dat",
                              col_names = c("subject", "session", "block",
                                            "trial", "set_size",
                                            "response_size_list",
                                            "response_size_new_words",
                                            "tested", "response",
                                            "response_category", "rt")) %>%
    # ignore the type of incorrect responses (the focus of the paper)
    mutate(correct = if_else(response_category ==1, 1, 0)) %>%
    # use only the data from the free recall task:
    # (when there was no list of possible responses)
    filter(response_size_list + response_size_new_words == 0) %>%
    # select one subject
    filter(subject == 10) %>%
    mutate(c_set_size = set_size - mean(set_size))
# ignore the warning from read_table

# Set sizes in the dataset:
df_recall_data$set_size %>%
    unique
# Trials by set size
df_recall_data %>%
    group_by(set_size) %>%
    count()

fit_recall <- brm(correct ~ 1 + c_set_size,
  data = df_recall_data,
  family = bernoulli(link = logit),
  prior = c(
    prior(normal(0, 1.5), class = Intercept),
    prior(normal(0, .5), class = b, coef = c_set_size)
  )
)


## ---------------------------------------------------
posterior_summary(fit_recall)[1:2,c(1,3,4)]


## ---------------------------------------------------
plot(fit_recall)


## ---- echo=FALSE, results="hide"--------------------
alpha_samples<- posterior_samples(fit_recall)$b_Intercept
beta_samples<- posterior_samples(fit_recall)$b_c_set_size
beta_mean <- round(mean(beta_samples),5)
beta_low <- round(quantile(beta_samples,prob=0.025),5)
beta_high <- round(quantile(beta_samples,prob=0.975),5)


## ---------------------------------------------------
alpha_samples<- posterior_samples(fit_recall)$b_Intercept
av_accuracy <- plogis(alpha_samples)
c(mean = mean(av_accuracy), quantile(av_accuracy, c(.025,.975)))


## ---------------------------------------------------
beta_samples<- posterior_samples(fit_recall)$b_c_set_size
effect_middle <- plogis(alpha_samples) - plogis(alpha_samples - beta_samples)
c(mean = mean(effect_middle), quantile(effect_middle, c(.025,.975)))


## ---------------------------------------------------
effect_4m2 <- plogis(alpha_samples+  (4 - mean(df_recall_data$set_size)) * beta_samples) -
    plogis(alpha_samples+  (2 - mean(df_recall_data$set_size)) * beta_samples)
c(mean = mean(effect_4m2), quantile(effect_4m2, c(.025,.975)))


## ---------------------------------------------------
df_recall_data_ext <- df_recall_data %>%
    bind_rows(tibble(set_size = rep(c(3,5,7),23),
                     c_set_size = set_size - mean(df_recall_data$set_size)))
df_recall_pred_ext <- posterior_predict(fit_recall,
                                 newdata = df_recall_data_ext,
                                 nsamples = 1000) %>%
    array_branch(margin = 1) %>%
    map_dfr( function(yrep_iter) {
        df_recall_data_ext %>%
            mutate(correct = yrep_iter)
    }, .id = "iter") %>%
    mutate(iter = as.numeric(iter))


## ----postpredsum2, fig.cap ="(ref:postpredsum2)", message = FALSE----
df_recall_pred_ext_summary <- df_recall_pred_ext %>%
    group_by(iter, set_size) %>%
    summarize(accuracy = mean(correct))
# observed means:
df_recall_summary<- df_recall_data %>%
    group_by(set_size) %>%
    summarize(accuracy = mean(correct))
ggplot(df_recall_pred_ext_summary, aes(accuracy)) +
    geom_histogram(alpha=.5)+
    geom_vline(aes(xintercept= accuracy),data= df_recall_summary)+
    facet_grid(set_size ~ .)


## ----prepshow, ref.label="prepare-pupil", eval=FALSE----
## NA

