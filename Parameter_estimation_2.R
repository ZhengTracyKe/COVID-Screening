# code for parameter estimation in Massachusetts (Method 2)

rm(list = ls())
set.seed(1)
setwd("/Users/Yaxuan/PKU/Research/Testing_Allocation/Code_updated")
source("data_preparation.R")
source("Active_Screening_Strategy_0906.R")
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(patchwork)


# parameter estimation --------

# fixed parameters
state <- "Massachusetts"
c <- 1 / 15
k <- 0.16
budget <- 0
d <- 0.01


Data <- data_preparation(state, d)
N <- Data[[4]]$population

# data preparation
ma_smooth = function(v){
  # 7-days moving average smoothing
  diff_v = c(v[1],diff(v))
  diff_v_s = stats::filter(diff_v, rep(1/7,7))
  index = which(is.na(diff_v_s))
  diff_v_s[index] = diff_v[index]
  v_s = cumsum(diff_v_s)
  return(v_s)
}

filename2 = paste0('/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/US_comfirmed/',
                   state,'_comfirmed.csv')
comfirmed_matrix = read.csv(filename2,stringsAsFactors = F) %>% 
  mutate(Date = mdy(Date))
for (i in 2:13){
  comfirmed_matrix[,i] = ma_smooth(comfirmed_matrix[,i]) #smoothing
}

# new_confirmed_matrix
new_C = comfirmed_matrix
for (i in 2:13){
  v = comfirmed_matrix[,i]
  new_C[,i] = c(NA,diff(v))
}

# hidden_case_matrix
H = new_C
for (i in 2:13){
  v = new_C[-1,i]
  H[,i] = c(v/k,NA)
}

# estimate delta(t)
delta = H
for (i in 2:13){
  v = H[,i]
  delta[,i] = c(diff(v),NA)/v
}

new_C_matrix = new_C[, -1]
delta_matrix = delta[, -1]
H_matrix = H[, -1]

hat_p <- function(i, t, alpha, W){
  tmp = 1
  for (j in 1:12){
    tmp = tmp*(1-alpha*W[i,j]*H_matrix[t,j]/N[j])
  }
  return(1-tmp)
}

  

opt_func_1 <- function(para){
  state <- "Massachusetts"
  c <- 1 / 15
  k <- 0.16
  TT <- 7
  Num <- 25
  d = para[1]
  alpha = para[2]
  
  Data <- data_preparation(state, d)
  W <- Data[[2]]
  
  Delta <- delta_matrix[(Num+1):(Num+7),]
  P = Delta
  for (t in 1:TT){
    for (j in 1:12){
      P[t, j] = hat_p(j, t+Num, alpha, W)
    }
  }
  
  loss = sum((Delta-P + k + c)^2)
  return(loss)
}

opt_func_1(c(0.01, 0.4))

# minimal distance
para_opt = optim(par = c(0.09,0.3), fn = opt_func_1,
      lower = c(0.001,0.2), upper = c(0.2,0.8))$par

d = para_opt[1]
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
N <- Data[[4]]$population
area <- Data[[4]]$population
Dens <- rep(1, length(nodes))
n <- length(nodes)
alpha = para_opt[2]*Dens

state <- "Massachusetts"
c <- 1 / 15
k <- 0.16
budget <- 0
# d <- 0.01
TT <- 7

# TT <- 14
Id <- as.numeric(comfirmed_matrix[Num, -1])
Ih <- as.numeric(comfirmed_matrix[Num, -1] - comfirmed_matrix[Num-1, -1])/k
R <- rep(0, n)

method = "No_Intervention"
simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, method, I_ac = NA)
Traj_sim <- simulation[[1]] %>% 
  filter(type == "Dcum", county != "Total") %>%
  select(-type) %>%
  pivot_wider(names_from = county, values_from = case)

for (i in 1:14){
  Traj_sim[i,2:13] = Traj_sim[i,2:13]+Id
}


fitting_data <- Traj_sim %>% filter(Time <= 7) %>%
  gather(-Time, key = "county", value = "case") %>%
  mutate(label = "fitting")
testing_data <- Traj_sim %>% filter(Time > 7) %>%
  gather(-Time, key = "county", value = "case") %>%
  mutate(label = "testing")
real_data <- comfirmed_matrix[(Num):(Num+17), ] %>%
  mutate(Time = 0:17) %>%
  gather(-c(Time,Date), key = "county", value = "case") %>%
  mutate(label = "real")

p = ggplot() +
  # geom_point(data = real_data, aes(x = Time, y = case, color = "real")) +
  geom_line(data = real_data, aes(x = Time, y = case, color = "real")) +
  geom_point(data = fitting_data, 
             aes(x = Time, y = case, color = "fitting"), 
             shape = 4, size = 1) +
  geom_point(data = testing_data, 
             aes(x = Time, y = case, color = "testing"), 
             shape = 4, size = 1) +
  scale_color_manual(name = "Label",
                     values = c(fitting = "red", testing = "blue", real = "black"),
                     labels = c("fitting data", "testing data",
                                "smoothed real data")) +
  facet_wrap(county~. , nrow = 2, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(p, filename = "estimation.pdf", height = 3.4, width = 8)
  

