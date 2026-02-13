# Load packages for data handling and plotting
library(tidyverse)
library(patchwork)
library(broom)
library(lm.beta)
library(ggstatsplot)
library(nlme)
library(lmerTest)
library(ggplot2)
library(glmmTMB)
library(psych)
library(jtools)
library(sjPlot)
library(faux)
library(emmeans)
library(ez)
library(rstatix)
library(knitr)
library(kableExtra)
library(rockchalk)



# Datensatz erstellen 
N <- 35
t0 <- rnorm(N, 3.3, 0.4)
t1 <- t0 + rnorm(N, 0.6, 0.2)    ###t_0 ist der Teil, den beide Zeitpunkte gemeinsame haben  konnte nicht das zu "rnorm_fixed" zugehörige Paket finden, welches ist zu verwenden? ist es zwingend notwendig? 
diff <- t1-t0


D_tpaired <- data.frame(
  y = diff
)

D_tpaired_long <- data.frame(
  x = rep(c(0, 1), each = N),       # erstellt UV / Prädiktor - erzeugt dafür Liste mit je N mal 0 für t0 und 1 für t1 
  y = c(t0, t1),                    # Vektor der Messwerte erzeugen, erst t0 dann t1
  id = 1:N
)

############################################################################################
### der folgende Code dient der Erstellng einer Visualisierung, die dabei helfen soll die Parallelen zwischen dem paired t-test und der Herangehensweise mittels mlm zu erstellen 
#P_t1 <- ggplot(D_tpaired, aes(y = y, x = 0)) +        # darstellung d. diff auf y-achse
#  stat_summary(fun = mean,                            # automatische berechnung d. diff
#               geom = "errorbar",                     # "Fehlerbalken"
#               aes(ymax = after_stat(y), 
#                   ymin = after_stat(y), 
#                   color = "beta_0"), 
#               lwd = 2) +
#  scale_color_manual(name = NULL, 
#                     values = c("blue"), 
#                     labels = c(bquote(beta[0] * " (Intercept)"))) +
#  geom_text(aes(label = round(y, 1)), 
#            nudge_x = 0.2, 
#            size = 3, 
#            color = "pink") +
#  labs(title = "         Differenzen / t-Test")


#print(P_t1)



# Plot
#P_tpaired <- ggplot(D_tpaired_long, 
#                    aes(x = x, y = y)) +
#  geom_line(aes(group = id)) +
#  labs(title = "Paare")

#print(P_tpaired)

# Patchwork
#theme_axis(P_tpaired,                          ## hier gibt R einen Fehlercode aus. laut google gemini ist dies ein eigens erstellter befehl ?? 
#           ylim = c(min(D_tpaired_long$y), 
#                    max(D_tpaired_long$y))) + 
#  theme_axis(P_t1, 
#             ylim = c(min(D_tpaired_long$y), 
#                      max(D_tpaired_long$y)), 
#             legend.position = c(0.6, 0.1))

############################################################################################

## hier folgt der code für einen paired sampled t-test 

# Berechnung des paired sample t-test auf basis der definierten variablen t_0 und t_1, das Ergebnis wird als a abgelegt 

a <- t.test(t1, t0, paired = TRUE)
print(a)

#Paired t-test  erster run 

#data:  t1 and t0
#t = 104.23, df = 34, p-value < 2.2e-16
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  3.842749 3.995586
#sample estimates:
#  mean difference 
#3.919167 


# Paired t- test zweiter run 

#Paired t-test

#data:  t1 and t0
#t = 18.19, df = 34, p-value < 2.2e-16
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  0.5546307 0.6941509
#sample estimates:
#  mean difference 
#0.6243908 

# händische Berechnung von Cohens d_z (Fokus liegt auf Streuung der Veränderung)

# Mittelwert der Differenzen berechnen 
m_diff <- mean(diff)

# SD der Differenzen berechnen
sd_diff <- sd(diff)

# Cohens d_z berechnen 
d_z <- m_diff / sd_diff
print(d_z)

### 1.run d_z = 17.61725
### 2. run d_z = 3.074619

# händische Berechnung von Cohens d_av (Fokus liegt auf Rohwerten)

# SDs von t_0 und t_1 berechnen 
sd_t0 <- sd(t0)
sd_t1 <- sd(t1)

# durchschnitliche SD (kommt in den Nenner)
sd_av <- (sd_t0 + sd_t1) / 2

# Cohens d_av berechnen 
d_av <- m_diff / sd_av
print(d_av)

### 1. run d_av = 9.670992
### 2. run d_av = 1.428485


# als MLM durchführen 
e <- suppressWarnings(glmmTMB(y ~ 1 + x +(1 + x| id),
                              REML = TRUE,
                              data = D_tpaired_long))
print(e)

## 1. run
#Formula:          y ~ 1 + x + (1 + x | id)
#Data: D_tpaired_long
#AIC       BIC    logLik -2*log(L)  df.resid 
#40.73079  54.22176 -14.36539  28.73079        64 
#Random-effects (co)variances:
  
#  Conditional model:
#  Groups   Name        Std.Dev. Corr 
#id       (Intercept) 0.3458        
#         x           0.1544   0.40                 ### hier die verschiedenen Varianzen von t_0 und t_1
#Residual             0.1132                        ### und die Fehler-/Residualvarianz

#Number of obs: 70 / Conditional model: id, 35

#Dispersion estimate for gaussian family (sigma^2): 0.0128 

#Fixed Effects:
  
#  Conditional model:
#  (Intercept)        x  
#    3.212        3.919      


## der geschätzte Werte für t_0, also Intercept im obigen Output liegt nah an dem vorher festgelegten Wert von 3.3 
## der Wert für den mean difference aus der t-test berechnung weiter oben kommt zum selben Ergebnis wie die 
## MLM Berechnung und zwar einem Wert von 3.919


## 2. run 
#Formula:          y ~ 1 + x + (1 + x | id)
#Data: D_tpaired_long
#AIC       BIC    logLik -2*log(L)  df.resid 
#NA        NA        NA        NA        64 
#Random-effects (co)variances:
  
#  Conditional model:
#  Groups   Name        Std.Dev. Corr 
#id       (Intercept) 0.3767        
#x           0.1429   0.57 
#Residual             0.1020        

#Number of obs: 70 / Conditional model: id, 35

#Dispersion estimate for gaussian family (sigma^2): 0.0104 

#Fixed Effects:
  
#  Conditional model:
#  (Intercept)            x  
#         3.2922       0.6244  




## Effektstärke auf Basis des MLM Outputs berechnen 

# totale Varianz berechen 
var_tot <- (0.3767^2) + (0.57^2) + (0.1020^2)
sd_tot <- sqrt(var_tot)

vcm <- VarCorr(e)
vcm$cond

## ES 
d_mlm <- 0.6244 / sd_tot
print(d_mlm)

# 1. run d_mlm = 9.914968
# 2. run d_mlm = 0.9038774

### unerwarteterweise fällt die mittels mlm output berechnete ES beim ersten Durchgang höher aus, woran liegt das?
### berücksichtigung der unterschiedlichen Varianzquellen und damit auch der messfehler-varianz
### welche Rolle spielen hier Random Slopes? 


