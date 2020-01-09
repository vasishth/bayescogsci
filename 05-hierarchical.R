## to-do: make this better


## to-do: N400 plot here


## ---- message = FALSE-------------------------------
df_eeg_data <- read_tsv("data/public_noun_data.txt") %>%
    filter(lab=="edin")
df_eeg_data
# Number of subjects
df_eeg_data %>%
    distinct(subject) %>%
    count()
## choose only the relevant columns:
df_eeg_data<-df_eeg_data[,c(1,3,5,6)]
## strip edin prefix on subject names:
df_eeg_data$subject<-stringr::str_replace(df_eeg_data$subject,"edin","")
## save unique subject ids:
subj_levels<-sort(unique(as.numeric(as.character(df_eeg_data$subject))))
df_eeg_data$subject<-factor(df_eeg_data$subject,levels=subj_levels)


## ---------------------------------------------------
df_eeg_data  <- df_eeg_data %>%
    mutate(c_cloze= cloze/100 - mean(cloze/100) )
df_eeg_data$c_cloze %>% summary() 


## ---- fig.cap="Histogram of the N400 averages for every trial in gray; density plot of a normal distribution in black.", message=FALSE----
df_eeg_data %>% ggplot(aes(n400)) +
    geom_histogram(binwidth = 4, colour="gray", alpha = .5, aes(y = ..density..)) +
    stat_function(fun = dnorm, args = list(
                                                 mean = mean(df_eeg_data$n400),
                                                 sd = sd(df_eeg_data$n400))) +
    xlab("Average voltage in microvolts for the N400 spatiotemporal window")


## ---------------------------------------------------
samples <- rtnorm(1000,0 ,50, a=0)
c(mean = mean(samples), sd(samples))


## ---------------------------------------------------
quantile(samples, c(0.025,.975))


## ---- message = FALSE, results = "hide"-------------
fit_N400_cp <- brm(n400 ~ c_cloze,
  prior = 
    c(prior(normal(0, 10), class = Intercept),
      prior(normal(0, 10), class = b, coef = c_cloze),
      prior(normal(0, 50), class = sigma)),
  data = df_eeg_data
)


## ---------------------------------------------------
posterior_summary(fit_N400_cp)
plot(fit_N400_cp)


## ---- message = FALSE, results = "hide"-------------
fit_N400_np <- brm(n400 ~ 0 + subject + c_cloze:subject,
                 prior =
                     c(prior(normal(0, 10), class = b),
                       prior(normal(0, 50), class = sigma)),
                 data = df_eeg_data)


## ---------------------------------------------------
## choose parameters:
ind_effects_np <- paste0("b_subject",unique(df_eeg_data$subject), ":c_cloze")
average_beta_across_subj <- posterior_samples(fit_N400_np,
                                              pars=ind_effects_np) %>%
    rowMeans()

grandmean<-c(mean=mean(average_beta_across_subj),
  quantile(average_beta_across_subj, c(.025,.975)))


## ----nopooling, fig.cap = "(ref:nopooling)", fig.height=11, message = FALSE----

## reorder plot by magnitude of mean:
dat<-mcmc_intervals_data(fit_N400_np,point_est="mean")
dat<-dat[38:74,]
dat<-dat[order(dat$m),]
## strip unnecessary characters from subject ids:
dat$subj<-stringr::str_replace(dat$parameter,"b_subject","")
dat$subj<-stringr::str_replace(dat$subj,":c_cloze","")
dat$subj<-factor(dat$subj,
                 levels=unique(dat$subj))

mcmc_intervals(fit_N400_np, pars=as.character(dat$parameter),
               prob = 0.8,
               prob_outer = 0.95,  
               point_est = "mean") +
#theme(axis.text.y = element_blank())+
  ylab("subject")+
    scale_y_discrete(name="subjects",
               labels=as.character(unique(dat$subj)))+
  xlab("microvolts")+
geom_vline(xintercept = grandmean[2],colour="black")+
geom_vline(xintercept = grandmean[3],colour="black")


## **Some important (and sometimes confusing) points:**


## ---- message = FALSE, results = "hide"-------------
fit_N400_v <- brm(n400 ~ c_cloze + (c_cloze || subject),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b, coef = c_cloze),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
                        prior(normal(0, 20), class = sd, coef = c_cloze, group = subject)
                        ),
              data = df_eeg_data)


## ----eval=FALSE-------------------------------------
## posterior_summary(fit_N400_v)


## ---------------------------------------------------
plot(fit_N400_v, N=6)


## ----partialpooling, fig.cap = "(ref:partialpooling)", fig.height=11,message=FALSE,warning=FALSE----

## reorder plot by magnitude of mean:
dat_v<-mcmc_intervals_data(fit_N400_v,point_est="mean")
dat_v<-dat_v[43:79,]
dat_v<-dat_v[order(dat_v$m),]
## strip unnecessary characters from subject ids:
dat_v$subj<-stringr::str_replace(dat_v$parameter,"r_subject\\[","")
dat_v$subj<-stringr::str_replace(dat_v$subj,",c_cloze\\]","")
dat_v$subj<-factor(dat_v$subj,
                 levels=unique(dat_v$subj))

mcmc_intervals(fit_N400_v, pars=as.character(dat_v$parameter),
               prob = 0.8,
               prob_outer = 0.95,  
               point_est = "mean") +
#theme(axis.text.y = element_blank())+
  ylab("subject")+
    scale_y_discrete(name="subjects",
               labels=as.character(unique(dat_v$subj)))+
  xlab("microvolts")


## ----echo=FALSE,eval=FALSE--------------------------
## ind_effects_v <- paste0("r_subject[",unique(df_eeg_data$subject), ",c_cloze]")
## 
## mcmc_intervals(fit_N400_v, pars=ind_effects_v,
##                prob = 0.8,
##                prob_outer = 0.95,
##                point_est = "mean"
## )


## ----comparison, message=FALSE, fig.height=11, fig.cap= "(ref:comparison)"----
# We'll need to make the plot "manually"
# No pooling model
ind_effects_v <- paste0("r_subject[",unique(df_eeg_data$subject), ",c_cloze]")

par_np <- posterior_summary(fit_N400_np)[ind_effects_np,] %>%
    as_tibble() %>%
    mutate(model = "No pooling",
           subj = unique(df_eeg_data$subject))
# For the hierarchical model is more complicated,
# because we want the effect (beta) + adjustment:
par_h <- posterior_samples(fit_N400_v) %>%
    select(ind_effects_v)  %>%
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
by_subj_df <- bind_rows(par_h, par_np)

by_subj_df<-by_subj_df[order(by_subj_df$Estimate),]

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


## ----lkjviz,echo=FALSE, fig.cap ="(ref:lkjviz)", message= FALSE,warning=FALSE,results="asis",fig.height=11,cache=TRUE----
## code for visualizing lkj priors:
fake_data <- list(x = rnorm(30,0,1),
                  N = 30, R = 2) 

stancode <- "
data {
  int<lower=0> N; 
  real x[N]; 
  int R;
  }
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  x ~ normal(mu,sigma);  
}
generated quantities {
  corr_matrix[R] LKJ05;
  corr_matrix[R] LKJ1;
  corr_matrix[R] LKJ2;
  corr_matrix[R] LKJ4;
  LKJ05 = lkj_corr_rng(R,.5);
  LKJ1 = lkj_corr_rng(R,1);
  LKJ2 = lkj_corr_rng(R,2);
  LKJ4 = lkj_corr_rng(R,4);
}
"

fitfake <- stan(model_code = stancode, pars = c("LKJ05","LKJ1","LKJ2","LKJ4"),
                data = fake_data, chains = 4, 
                iter = 2000)

corrs<-extract(fitfake,pars=c("LKJ05[1,2]","LKJ1[1,2]","LKJ2[1,2]","LKJ4[1,2]"))

corrs_df<-as.data.frame(corrs)
colnames(corrs_df)<-c("LKJ0.5","LKJ1","LKJ2","LKJ4")

corrs_long<-gather(corrs_df, 
                   eta, 
                   prior, LKJ0.5:LKJ4, factor_key=FALSE)

p<-ggplot(corrs_long, aes(x=prior, 
                  color=eta)) +
  geom_density(adjust=5, position="stack")
p


## ---- message = FALSE, results = "hide"-------------
fit_N400_h <- brm(n400 ~ c_cloze + (c_cloze | subject),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b, coef = c_cloze),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
                        prior(normal(0, 20), class = sd, coef = c_cloze, group = subject),
                        prior(lkj(2), class = cor, group= subject)),
              data = df_eeg_data)


## ---------------------------------------------------
fit_N400_h
plot(fit_N400_h, N=6)


## ---- eval = FALSE----------------------------------
## fit_N400_sih <- brm(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze |item ),
##                   prior =
##                       c(prior(normal(0, 10), class = Intercept),
##                         prior(normal(0, 10), class = b, coef = c_cloze),
##                         prior(normal(0, 50), class = sigma),
##                         prior(normal(0, 20), class = sd, coef = Intercept, group = subject),
##                         prior(normal(0, 20), class = sd, coef = c_cloze, group = subject),
##                         prior(lkj(2), class =cor, group = subject),
##                         prior(normal(0, 20), class = sd, coef = Intercept, group = item),
##                         prior(normal(0, 20), class = sd, coef = c_cloze, group = item),
##                         prior(lkj(2), class =cor, group = item)),
##                   data = df_eeg_data)


## ---- message = FALSE, results = "hide"-------------
fit_N400_sih <- brm(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze |item ),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b),
                        prior(normal(0, 50), class = sigma),
                        prior(normal(0, 20), class = sd),
                        prior(lkj(2), class =cor)),
                  data = df_eeg_data)


## ---- fig.height = 11-------------------------------
fit_N400_sih
plot(fit_N400_sih, N=9)


## ---------------------------------------------------
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


## ---- message = FALSE, results = "hide"-------------
fit_N400_s <- brm(bf(n400 ~ c_cloze + (c_cloze | subject) + (c_cloze |item ),
                       sigma ~ 1+(1|subject)),
                  prior =
                      c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b),
                        prior(normal(0, 20), class = sd),
                        prior(lkj(2), class =cor),
                        prior(normal(0,50), class = Intercept, dpar = sigma),
                        prior(normal(0,10), class = sd, group = subject, 
                              dpar = sigma)
                        ),
                  data = df_eeg_data)


## ---------------------------------------------------
posterior_summary(fit_N400_s)[3,c(1,3,4)]


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



## ----open_grodneretal, message = FALSE--------------
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
rc_data <- gg05_data %>% filter((condition == "objgap" & word_position == 6 ) |
            ( condition == "subjgap" & word_position == 4 ))


## ---------------------------------------------------
rc_data <- rc_data %>% mutate(ccond = if_else(condition == "objgap",  1,-1))

