
# Load Packages
library(MASS)
library(psych)
library(tidyverse)
library(lmerTest)





# Function to create correlated data
simulate_within_t <- function(N, d, r, sd2_sim, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Kovarianzmatrix für die beiden Bedingungen
  Sigma <- matrix(
    c(1, r,
      r, sd2_sim),
    nrow = 2
  )
  
  # Multivariat normalverteilte Daten simulieren
  data <- MASS::mvrnorm(
    n = N,
    mu = c(0, d),
    Sigma = Sigma
  )
  
  #Retrieve or set the row or column names of a matrix-like object.
  colnames(data) <- c("condition_A", "condition_B")
  
  
  as.data.frame(data)
}
# oder package


# Simulate Data
N <-  5000 # Sample Size
d <- 0.3 # Simulated true effect size 
sd2_sim <- 1 # Variance of the second measure. For Homogeneity == 1
r <-  0.1 # Coorelation between Measures

dat <- simulate_within_t(
  N = N,
  d = d,
  r = r,
  sd2_sim = sd2_sim
)

head(dat)



# Calculate Effect Sizes from basic data


# $ s actually an infix that takes two arguments, the values preceding and following it. 
#It is a convenience function designed that uses non-standard evaluation of its second argument. 
#It's called non-standard because the unquoted characters following $
#are first quoted before being used to extract a named element from the first argument.

m1 <- mean(dat$condition_A)
m2 <- mean(dat$condition_B)
m_dif <- abs(m1 - m2)

sd1 <- sd(dat$condition_A)
sd2 <- sd(dat$condition_B)

r <- cor(dat$condition_B, dat$condition_A)

s_dif <- sqrt(sd1^2 + sd2^2 - 2 * r * sd1 * sd2)
s_av <- sqrt((sd1^2 + sd2^2) / 2)


cohens_dz <- m_dif / s_dif
cohens_drm <- cohens_dz * sqrt(2 * (1 - r)) 
cohens_dav <- m_dif / s_av


# Calculate effect sizes from Multilevel Models
# We need Data in long format

dat_long <- dat |>
  transform(id = seq_len(nrow(dat))) |>
  pivot_longer(
    cols = c(condition_A, condition_B),
    names_to = "condition",
    values_to = "value"
  )


dat_long$condition <- factor(
  dat_long$condition,
  levels = c("condition_A", "condition_B"),
  labels = c("A", "B")
)

# Calculate ML Models
# Random-effects terms are distinguished by vertical bars (|) 
#separating expressions for design matrices from grouping factors. #
#By default, non-scalar random effects (where the design matrix has more than one column, 
#e.g. (1+x|f)) are fitted with unstructured (general positive semidefinite) covariance matrices.


# First Model with random intercept only
mlm.1 <- lmer(value ~ condition + (1 | id), 
              control=lmerControl(check.nobs.vs.nRE="ignore"),
              data = dat_long)
#Second Model with random intercepts and random slopes
mlm.2 <- lmer(value ~ condition + (1 + condition | id), 
              control=lmerControl(check.nobs.vs.nRE="ignore"),
              data = dat_long)

# Get Parameters from both models
vc1 <- VarCorr(mlm.1)
vc2 <- VarCorr(mlm.2)

SD_id1 <- sqrt(as.numeric(vc1$id[1,1]))
SD_id2 <- sqrt(as.numeric(vc2$id[1,1]))

SD_res1 <- sigma(mlm.1)
SD_res2 <- sigma(mlm.2)

mlm_diff1 <- as.numeric(fixef(mlm.1)["conditionB"])
mlm_diff2 <- as.numeric(fixef(mlm.2)["conditionB"])

# Calculate Effect Sizes from ML Model Data
d_mlm1 <- mlm_diff1 / sqrt( (SD_id1^2 + SD_res1^2) ) # with both variance sources: between and within
d_mlm1a <- mlm_diff1 / sqrt( (SD_res1^2) ) # only within variance source (between ids removed)

d_mlm2 <- mlm_diff2 / sqrt( (SD_id2^2 + SD_res2^2) ) # with both variance sources: between and within
d_mlm2a <- mlm_diff2 / sqrt( (SD_res2^2) ) # only within variance source (between ids removed)



cat(
  "\n", "Mean Differences ES for dependent samples with true  Effect of:", round(d, 4), "\n", "\n",
  "Cohen's dz = ", round(cohens_dz, 4), "\n", "----------------------------------------------------------------------------", "\n",
  "Cohen's dav = ", round(cohens_dav, 4), "\n", "----------------------------------------------------------------------------", "\n",
  "Cohen's drm = ", round(cohens_drm, 4), 
  "\n", "----------------------------------------------------------------------------", "\n",
  "\n", "Mean Differences ES from Multilevel Models:", "\n", "\n",
  "\n", "1) Random Intercepts only:", "\n", "\n",
  "Both Variances (within + between) = ", round(d_mlm1, 4), "\n", "----------------------------------------------------------------------------", "\n",
  "One Variance (only within) = ", round(d_mlm1a, 4), "\n", "----------------------------------------------------------------------------", "\n",
  "\n", "1) Random Intercepts and Slopes:", "\n", "\n",
  "Both Variances (within + between) = ", round(d_mlm2, 4), "\n", "----------------------------------------------------------------------------", "\n",
  "One Variance (only within) = ", round(d_mlm2a, 4), "\n", "----------------------------------------------------------------------------", "\n"
)

