N_houses  = 60 * 8
N_test = N_houses/2

col_residents = 3
col_subsidies = 9
col_income = 11
col_rent = 19
#DATA = read.table("/home/asta/Nonparametric-statistics/database.csv", header = FALSE, sep = ",")
DATA = read.table("/Users/ahavrius/Computer_statistics/sampling/database.csv", header = FALSE, sep = ",")

get_data = function(amount){
  index_selected  = sample(1:N_houses, amount, replace=F)
  data_selected = DATA[index_selected, c(1, 2, col_residents, col_subsidies)]
  names(data_selected) = c("block", "unit", "residents", "subsidies")
  data_selected
}

real_x = DATA[, col_residents]
real_y = DATA[, col_subsidies]
data_sample = get_data(N_test)
sample_x = data_sample$residents
sample_y = data_sample$subsidies

#estimation by ratio
ratio = sum(sample_y)/sum(sample_x)
estim_ratio = ratio * mean(real_x)

#linear regression estimation
model = lm(subsidies ~ residents, data = data.frame(residents = sample_x, subsidies = sample_y))
lm_coef = model$coefficients
estim_lm = lm_coef[1] + lm_coef[2] * mean(real_x)

#estim_lm 3204.101 
#estim_ratio 3230.333

plot(sample_x, sample_y, pch = 16, col = sample_y/5000 + 1, ylab = "sample subs", xlab = "sample residents", main = "Dependency")
abline(0, ratio)

norm_quantile = qnorm(0.975)
D_ratio = (1 - N_test/N_houses) * sd(sample_y - estim_ratio)^2 / N_test
D_lm = (1 - N_test/N_houses) * sd(model$residuals)^2 / N_test

left_ratio = estim_ratio - sqrt(D_ratio) * norm_quantile
right_ratio = estim_ratio + sqrt(D_ratio) * norm_quantile

left_lm = estim_lm - sqrt(D_lm) * norm_quantile
right_lm = estim_lm + sqrt(D_lm) * norm_quantile

#Confident intervals
library(ggplot2)
d = data.frame(x = c("real", "ratio", "linear regression"), y = c(mean(real_y), estim_ratio, estim_lm),
               ylo = c(max(left_ratio, left_lm), left_ratio, left_lm),
               yhi = c(min(right_ratio, right_lm), right_ratio, right_lm))
ggplot(d, aes(x = x, y = y)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = yhi, ymin = ylo))