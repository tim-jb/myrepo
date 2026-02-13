## im Folgenden entsprechend der Anleitung aus Strobl (2024) S. 8 ff

## allgemeine Basis ist hier die Gleichung 
## y = ß_o + ß_1 * x + €
library(simstudy)

## Fehlervarianz 

err_10it <- rnorm(n=50, mean=0, sd=5)

# Kontrolle 
mean(err_10it)
sd(err_10it)
var(err_10it)

#> mean(err_10it)
#[1] 0.597845
#> sd(err_10it)
#[1] 4.687624
#> var(err_10it)
#[1] 21.97382



npers_10it <- 50



s_err_10it <- 0.2
changescore <-  0.6


# Datensatz erzeugen mit
N <-  c(20, 50)
mu1 <- 0
mu2 <- c(0.3, 0.6, 0.9)
sd1 <- 1
# var2 <- c(1, 2, 4, 8)
sd2 <- c(1, 2, 4)

rho <- c(0, 0.3, 0.5, 0.8)

p_mat <- expand.grid(N = N, 
            m1 = mu1,
            m2 = mu2, 
            s1 = sd1,
            s2 = sd2,
            r = rho)


dat <- genCorData(n = N,
                  mu = c(mu1, mu2),
                  sigma = c(sd1, sd2),
                  rho = 0.5,
                  corstr = "cs")

dat |> 
  psych::describe()

cor(dat$V1, dat$V2)

# und in einem Simulations-Durchgang

one_sim <- function(N, changescore, s_err_10it) {
  t0_10it <- rnorm(N, 3.3, 0.4)
  t1_10it <- t0_10it + changescore + rnorm(N, 0, s_err_10it)
  
#für MLM long format erstellen  
  dat_long_10it <- data.frame(
    x = rep(c(0, 1), each = N),
    y = c(t0_10it, t1_10it),
    id = 1:N
  )
# Berechnung paired t-test  
  a_10it <- t.test(t1_10it, t0_10it, paired = TRUE)
  t_est <- a_10it$estimate 
  
# Berechnung MLM  
  
  e_ri <- suppressWarnings(glmmTMB(y ~ 1 + x +(1| id),
                                     REML = TRUE,
                                     data = dat_long_10it))
  
  e_ris <- suppressWarnings(glmmTMB(y ~ 1 + x +(1 + x| id),
                                     REML = TRUE,
                                     data = dat_long_10it))
  
  mlm_est <- fixef(e_ris)$cond["x"]
  
  return(c(t_est = t_est, mlm_est = mlm_est))
}


# Simulation mit 10 Durchgängen 

niter <- 10

erg_10it <- matrix(NA, nrow=niter, ncol = 2)
colnames(erg_10it) <- c("tTest_Schätzung", "MLM_Schätzung")

for(i in 1:niter) {
  erg_10it[i,] <- one_sim(N = 50,
                          changescore = 0.6,
                          s_err_10it = 0.2)
}

# Analyse der Abweichungen/ bias, um anzuzeigen, wie weit vom wahren changescore-Wert abgewichen

sim_df <- as.data.frame(erg_10it)

sim_df$Abweichung_tTest <- sim_df$tTest_Schätzung - changescore
sim_df$Abweichung_MLM <- sim_df$MLM_Schätzung - changescore

sim_df <- sim_df |> 
  mutate(
    Abweichung_tTest = tTest_Schätzung - changescore,
    Abweichung_MLM = MLM_Schätzung - changescore
  ) 



print(sim_df)

sim_df |> 
  summarise(mean_MLM = mean(Abweichung_MLM))

