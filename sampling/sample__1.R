N_houses  = 60 * 8
N_sample = 10
v_select<-sample(1:N_houses, N_sample)
M_select<-cbind(X %/% 8 + 1, X %% 8 + 1)

col_residents = 3
col_subsidies = 9
col_income = 11
col_rent = 19
DATA = read.table("/Users/ahavrius/Desktop/sampling/database.csv", header = FALSE, sep = ",")
select = names(DATA)[c(1, 2, col_residents, col_income, col_subsidies, col_rent)]
DATA_select = DATA[v_select, select]
test_sample = DATA_select
names(test_sample) = c("block", "unit", "residents", "income", "subsidies", "rent")

#Estimation of Total Household Government Transfer Payments received by the population
t_subsidies = N_houses * sum(test_sample$subsidies) / N_sample
#Estimation of Mean Total Household Income
m_income = mean(test_sample$income)
#Estimation of Proportion of families with rented household
p_rent = 1 - sum(test_sample$rent == '.') / N_sample
#print(c("mean income", "total subsidies", "proport rent"))
#print(c(m_income, t_subsidies, p_rent))

#Variance
# Estimation of variance for the total subsidies
mean_t_subsidies = mean(test_sample$subsidies)
var_t_subsidies = (N_houses^2)* sum((test_sample$subsidies - mean_t_subsidies)^2) / (N_sample - 1)
# Estimation of variance for the mean
var_income = sum((test_sample$income - m_income)^2) / (N_sample - 1)
# Estimation of variance for the proportion
var_p_rent = p_rent * (1 - p_rent) * N_sample / (N_sample - 1)
#Confidence amount of samples
alpha = 0.05
e = 0.1
x = qnorm(1 - alpha / 2)
cv = c(sqrt(var_t_subsidies) / t_subsidies, sqrt(var_income) / m_income, sqrt(var_p_rent) / p_rent)
N_conf = ceiling(x^2 * cv^2 / (e^2 + cv^2 * x^2 / N_houses))

DATA_conf = DATA[sample(1:N_houses, max(N_conf)), select]
names(DATA_conf) = c("block", "unit", "residents", "income", "subsidies", "rent")
n = max(N_conf)
N = 8 * 60
t<-N*sum(DATA_conf$subsidies)/n
m<-mean(DATA_conf$income)
tm<-sum(DATA_conf$subsidies)/n
p<-1 - sum(is.na(DATA_conf$rent))/n
St<-(N^2)*sum((DATA_conf$subsidies-tm)^2)/(n-1)
Sm<-sum((DATA_conf$income - m)^2)/(n-1)
Sp<-p*(1-p) * n/(n-1)
alpha<-0.05
z<-qnorm(1-alpha/2)
S<-c(St,Sm,Sp)
est<-c(t,m,p)
val1<-est-z*sqrt(S*(1/n-1/N))
val2<-est+z*sqrt(S*(1/n-1/N))

