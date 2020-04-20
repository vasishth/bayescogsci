## to-do: make this better


## ---- echo = FALSE---------------------------------------------------------
N400_c <- c("Cz", "CP1", "CP2", "P3", "Pz", "P4", "POz")


## ----N400noun, results = "hold",fig.height= 4.5, fig.cap =  "(ref:N400noun)", echo = FALSE, warning = FALSE----
noun_s <- readRDS("data/noun_s.RDS") %>%
  mutate(constraint = ifelse(cond %in% c(0, 1),
                             "Constraining", "Non-constraining"),
         completion = ifelse(cond %in% c(0, 2), "a", "b"),
         predictability = case_when(cond == 0 ~ "high",
                           cond == 1 ~ "low",
                           TRUE ~ NA_character_)
         ) %>%
  filter(channel == "neg", .time >= -1.6, .time <= 2.2 ,
         constraint == "Constraining")

noun_s %>%
  ggplot(aes(x=.time,y=mean_s, linetype = predictability)) +
  labs(linetype="Predictability",fill="", color ="", x = "Time (s)", y = "Amplitude (\u03BCV)")+
    scale_x_continuous(limits= c(-.1,.8)) +
    coord_cartesian(xlim=c(-.1,.8),clip="off")+
  geom_line(size = .5, na.rm=TRUE) +
  geom_vline(xintercept = .3, linetype = "dashed", color = "gray")+
  geom_vline(xintercept = .5, linetype = "dashed", color = "gray")



## ---- echo = FALSE---------------------------------------------------------
#hack:
select <- dplyr::select


## ---- message = FALSE------------------------------------------------------
df_eeg_data <- read_tsv("data/public_noun_data.txt") %>%
  filter(lab=="edin") %>%
  # choose only the relevant columns:
  select(subject, cloze, item, n400) %>%
  # we simplify the subjects id 
  mutate(subject = as.factor(subject) %>% as.numeric())
df_eeg_data
# Number of subjects
df_eeg_data %>%
    distinct(subject) %>%
    count()


## --------------------------------------------------------------------------
df_eeg_data  <- df_eeg_data %>%
    mutate(c_cloze= cloze/100 - mean(cloze/100) )
df_eeg_data$c_cloze %>% summary() 


## ---- fig.cap="Histogram of the N400 averages for every trial in gray; density plot of a normal distribution in black.", message=FALSE----
df_eeg_data %>% ggplot(aes(n400)) +
  geom_histogram(binwidth = 4, colour="gray", alpha = .5, aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_eeg_data$n400),
                                         sd = sd(df_eeg_data$n400))) +
  xlab("Average voltage in microvolts for the N400 spatiotemporal window")


## --------------------------------------------------------------------------
samples <- rtnorm(20000, 0, 50, a = 0)
c(mean = mean(samples), sd(samples))


## --------------------------------------------------------------------------
quantile(samples, c(0.025, .975))
# or c(qtnorm(.025, 0, 50, a = 0), qtnorm(.975, 0, 50, a = 0))


## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_cp <- brm(n400 ~ c_cloze,
  prior = 
    c(prior(normal(0, 10), class = Intercept),
      prior(normal(0, 10), class = b, coef = c_cloze),
      prior(normal(0, 50), class = sigma)),
  data = df_eeg_data
)


## --------------------------------------------------------------------------
posterior_summary(fit_N400_cp)
plot(fit_N400_cp)


## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_np <- brm(n400 ~ 0 + factor(subject) + c_cloze:factor(subject),
                 prior =
                     c(prior(normal(0, 10), class = b),
                       prior(normal(0, 50), class = sigma)),
                 data = df_eeg_data)


## --------------------------------------------------------------------------
# parameter name of beta by subject: 
ind_effects_np <- paste0("b_factorsubject",unique(df_eeg_data$subject), ":c_cloze")
beta_across_subj <- posterior_samples(fit_N400_np, pars=ind_effects_np)%>% rowMeans() 
# We calculate the average of these estimates
(grand_av_beta <- tibble(mean = mean(beta_across_subj),
                    lq = quantile(beta_across_subj, c(.025)),
                    hq = quantile(beta_across_subj, c(.975))))


## ----nopooling, fig.cap = "(ref:nopooling)", fig.height=11, message = FALSE----
# We make a table of beta by subject  
beta_by_subj <- posterior_summary(fit_N400_np, pars=ind_effects_np) %>%
  as_tibble() %>%
  mutate(subject = 1:n()) %>%
  ## reorder plot by magnitude of mean:
  arrange(Estimate) %>%
  mutate(subject = factor(subject, levels = subject))
# We plot: 
ggplot(beta_by_subj, aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = subject)) +
  geom_point() +
  geom_errorbarh() +
  geom_vline(xintercept = grand_av_beta$mean) +
  geom_vline(xintercept = grand_av_beta$lq, linetype = "dashed") +
  geom_vline(xintercept = grand_av_beta$hq, linetype = "dashed") +
  xlab("By-subject effect of cloze probability in microvolts")


## **Some important (and sometimes confusing) points:**


## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_v <- brm(n400 ~ c_cloze + (c_cloze || subject),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b, coef = c_cloze),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
                        prior(normal(0, 20), class = sd, coef = c_cloze, group = subject)
                        ),
              data = df_eeg_data)


## ----eval=FALSE------------------------------------------------------------
## fit_N400_v


## --------------------------------------------------------------------------
plot(fit_N400_v, N=6)


## ----partialpooling, fig.cap = "(ref:partialpooling)", fig.height=11,message=FALSE,warning=FALSE----
# We make a table of u_1s
ind_effects_v <- paste0("r_subject[",unique(df_eeg_data$subject), ",c_cloze]")
u_1_v <- posterior_summary(fit_N400_v)[ind_effects_v, ] %>%
  as_tibble() %>%
  mutate(subject = 1:n()) %>%
  ## reorder plot by magnitude of mean:
  arrange(Estimate) %>%
  mutate(subject = factor(subject, levels = subject))
# We plot:
ggplot(u_1_v, aes(x = Estimate, xmin = Q2.5, xmax = Q97.5, y = subject)) +
  geom_point() +
  geom_errorbarh() +
  xlab("By-subject adjustment to the slope in microvolts")


## ----comparison, message=FALSE, fig.height=11, fig.cap= "(ref:comparison)"----
# No pooling model
ind_effects_v <- paste0("r_subject[",unique(df_eeg_data$subject), ",c_cloze]")

par_np <- posterior_summary(fit_N400_np)[ind_effects_np,] %>%
    as_tibble() %>%
    mutate(model = "No pooling",
           subj = unique(df_eeg_data$subject))
# For the hierarchical model is more complicated,
# because we want the effect (beta) + adjustment:
par_h <- posterior_samples(fit_N400_v) %>%
    select(all_of(ind_effects_v))  %>%
    # We create a dataframe where each column is beta + u_{1,i}
    mutate_all( ~ . + posterior_samples(fit_N400_v)$b_c_cloze) %>%
    # We iterate over each column and create a dataframe with
    # estimate and the 95% CI of each iteration:
    map_dfr(~ tibble(Estimate = mean(.),
                Q2.5 = quantile(.,.025),
                Q97.5 = quantile(., .975))) %>%
    # We add a column to identify that the model,
    # and one with the subject labels:
    mutate(model = "Hierarchical",
           subj = unique(df_eeg_data$subject))
# The mean and 95% CI of both models in one dataframe:
by_subj_df <- bind_rows(par_h, par_np) %>%
  arrange(Estimate) %>%
  mutate(subj = factor(subj, levels= unique(.data$subj)))

ggplot(by_subj_df,
       aes(ymin = Q2.5, ymax = Q97.5,x=subj, y = Estimate, color=model,
           shape = model)) +
    geom_errorbar(position = position_dodge(1)) +
    geom_point(position = position_dodge(1)) +
    # We'll also add the mean and 95% CrI of the overall difference to the plot:
    geom_hline(yintercept = posterior_summary(fit_N400_v)["b_c_cloze","Estimate"]) +
    geom_hline(yintercept = posterior_summary(fit_N400_v)["b_c_cloze","Q2.5"],
               linetype = "dotted",size = .5)+
    geom_hline(yintercept = posterior_summary(fit_N400_v)["b_c_cloze","Q97.5"],
               linetype = "dotted",size = .5) +
    xlab("N400 effect of predictability") +
    coord_flip()


## **The variance-covariance matrix and the corresponding correlation matrix:**


## ----lkjviz,echo=FALSE, fig.cap ="(ref:lkjviz)", message= FALSE,warning=FALSE,results="asis",fig.height=11,cache=TRUE, fig.width =4, fig.height=3,fig.show='hold', out.width='48%'----
## https://github.com/rmcelreath/rethinking/blob/1def057174071beb212532d545bc2d8c559760a2/R/distributions.r
# onion method correlation matrix
dlkjcorr <- function( x , eta=1 , log=FALSE ) {
    ll <- det(x)^(eta-1)
    if ( log==FALSE ) ll <- exp(ll)
    return(ll)
}

#Simplified for a 2 x 2 matrix
dlkjcorr2 <- function(rho, eta = 1 ) {
    map_dbl(rho, ~ matrix(c(1, .x,.x,1),ncol=2) %>%  
                                 dlkjcorr(., eta))
}

ggplot(tibble(rho = c(-.99,.99)), aes(rho)) +
    stat_function(fun = dlkjcorr2,  geom = "line", args = list(eta = 1)) +
    ylab("density") +
    ggtitle("eta = 1")

ggplot(tibble(rho = c(-.99,.99)), aes(rho)) +
    stat_function(fun = dlkjcorr2,  geom = "line", args = list(eta = 2)) +
    ylab("density") +
    ggtitle("eta = 2")

ggplot(tibble(rho = c(-.99,.99)), aes(rho)) +
    stat_function(fun = dlkjcorr2,  geom = "line", args = list(eta = 4)) +
    ylab("density") +
    ggtitle("eta = 4")

ggplot(tibble(rho = c(-.99,.99)), aes(rho)) +
    stat_function(fun = dlkjcorr2,  geom = "line", args = list(eta = .9)) +
    ylab("density") +
    ggtitle("eta = .9")




## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_h <- brm(n400 ~ c_cloze + (c_cloze | subject),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b, coef = c_cloze),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
                        prior(normal(0, 20), class = sd, coef = c_cloze, group = subject),
                        prior(lkj(2), class = cor, group= subject)),
              data = df_eeg_data)


## --------------------------------------------------------------------------
plot(fit_N400_h, N=6)


## ---- eval = FALSE---------------------------------------------------------
## fit_N400_sih <- brm(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze | item),
##                   prior =
##                       c(prior(normal(0, 10), class = Intercept),
##                         prior(normal(0, 10), class = b, coef = c_cloze),
##                         prior(normal(0, 50), class = sigma),
##                         prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
##                         prior(normal(0, 20), class = sd, coef = c_cloze, group = subject),
##                         prior(lkj(2), class = cor, group = subject),
##                         prior(normal(0, 20), class = sd, coef = Intercept, group = item),
##                         prior(normal(0, 20), class = sd, coef = c_cloze, group = item),
##                         prior(lkj(2), class = cor, group = item)),
##                   data = df_eeg_data)


## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_sih <- brm(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze | item),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd),
                        prior(lkj(2), class =cor)),
                  data = df_eeg_data)


## ---- fig.height = 11------------------------------------------------------
fit_N400_sih
plot(fit_N400_sih, N=9)


## --------------------------------------------------------------------------
pp_check(fit_N400_sih, nsamples =50, type="dens_overlay")


## ----postpreddensbysubj, fig.cap ="(ref:postpreddensbysubj)" , message= FALSE,fig.height=11----
df_eeg_pred <- posterior_predict(fit_N400_sih,
                                 nsamples = 1000) %>%
    array_branch(margin = 1) %>%
    map_dfr( function(yrep_iter) {
        df_eeg_data %>%
            mutate(n400 = yrep_iter)
    }, .id = "iter") %>%
    mutate(iter = as.numeric(iter))

df_eeg_pred %>% filter(iter < 100) %>%
    ggplot(aes(n400, group=iter)) +
    geom_line(alpha = .05, stat="density", color = "blue") +
    geom_density(data=df_eeg_data, aes(n400),
                 inherit.aes = FALSE, size =1)+
    facet_wrap(subject ~ .) +
    xlab("Signal in the N400 spatiotemporal window")


## ----postpredsumbysubj, fig.cap ="(ref:postpredsumbysubj)", message= FALSE, fig.height=11----
# predicted subject:
df_eeg_pred_summary <- df_eeg_pred %>%
    group_by(iter, subject) %>%
    summarize(sd = sd(n400))
# observed means:
df_eeg_summary <- df_eeg_data %>%
    group_by(subject) %>%
    summarize(sd = sd(n400, na.rm= TRUE))
# plot
ggplot(df_eeg_pred_summary, aes(sd)) +
    geom_histogram(alpha=.5)+
    geom_vline(aes(xintercept= sd),data= df_eeg_summary)+
    facet_wrap(subject ~.)+
    xlab("Standard deviation")


## ---- message = FALSE, results = "hide"------------------------------------
fit_N400_s <- brm(bf(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze | item ),
                       sigma ~ 1 + (1 | subject)),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b),
                        prior(normal(0, 20), class = sd),
                        prior(lkj(2), class = cor),
                        prior(normal(0, log(50)), class = Intercept, dpar = sigma),
                        prior(normal(0, 5), class = sd, group = subject, 
                              dpar = sigma)
                        ),
                  data = df_eeg_data)


## --------------------------------------------------------------------------
posterior_summary(fit_N400_s)["b_c_cloze",]


## ----postpreddensbysubj2, fig.cap ="(ref:postpreddensbysubj2)" , message= FALSE,echo=FALSE, fig.height=11----
df_eeg_pred2 <- posterior_predict(fit_N400_s,
                                 nsamples = 1000) %>%
    array_branch(margin = 1) %>%
    map_dfr( function(yrep_iter) {
        df_eeg_data %>%
            mutate(n400 = yrep_iter)
    }, .id = "iter") %>%
    mutate(iter = as.numeric(iter))

df_eeg_pred2 %>% filter(iter < 100) %>% 
    ggplot(aes(n400, group=iter)) +
    geom_line(alpha = .05, stat="density", color = "blue") +
  geom_density(data=df_eeg_data, aes(n400),
                 inherit.aes = FALSE, size =1)+
    facet_wrap(subject ~ .) +
  xlab("Signal in the N400 spatiotemporal window")


## ----postpredsumbysubj2, fig.cap ="(ref:postpredsumbysubj2)", message= FALSE, fig.height=11, echo = FALSE----
# predicted subject:
df_eeg_pred_summary <- df_eeg_pred2 %>%
    group_by(iter, subject) %>%
    summarize(sd = sd(n400))
# observed means:
df_eeg_summary <- df_eeg_data %>%
    group_by(subject) %>%
    summarize(sd = sd(n400, na.rm= TRUE)) 
ggplot(df_eeg_pred_summary, aes(sd)) +
  geom_histogram(alpha=.5)+
    geom_vline(aes(xintercept= sd),data= df_eeg_summary)+
    facet_wrap(subject ~.)+
  xlab("Standard deviation") 


## ----open_grodneretal, message = FALSE-------------------------------------
gg05_data <- read_csv("data/GrodnerGibson2005E1.csv") %>%
    filter(item != 0) %>%
    mutate(word_positionnew = if_else(item != 15 & 
                                             word_position > 10, 
                                             word_position-1, 
                                             word_position))
#there is a mistake in the coding of word position,
#all items but 15 have regions 10 and higher coded
#as words 11 and higher

## get data from relative clause verb:
rc_data <- gg05_data %>%
  filter((condition == "objgap" & word_position == 6 ) |
            ( condition == "subjgap" & word_position == 4 ))


## --------------------------------------------------------------------------
rc_data <- rc_data %>%
  mutate(c_cond = if_else(condition == "objgap",  1, -1))

