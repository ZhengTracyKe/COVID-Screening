# code for simulations on Hubei

rm(list = ls())
set.seed(1)
setwd("/Users/Yaxuan/PKU/Research/Testing_Allocation/Code_updated")
source("data_preparation.R")
source("Active_Screening_Strategy_0906.R")
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(patchwork)

CompareMethods <- function(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac){
  M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[1],I_ac)
  Traj_sim <- simulation[[1]]
  Ra_sim <- simulation[[2]]
  plot_data1 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[1])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[2], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data2 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[2])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[3], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data3 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[3])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[4], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data4 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[4])
  
  plotdata <- bind_rows(plot_data1,plot_data2,plot_data3,plot_data4)
  
  return(plotdata)
}

# role of d -------
state <- "Wuhan"
d <- 0.02
Data <- data_preparation(state, d)
W <- as.data.frame(Data[[2]])
W_df <- W %>% mutate(County = colnames(W)) %>%
  select(County, Enshi:Yichang)


# test on real data: one network -------
state <- "Wuhan"
d <- 0.02
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
Comfirmed <- Data[[3]]
N <- Data[[4]]$population*10000
area <- Data[[4]]$population
Dens <- rep(1, length(nodes))
n <- length(nodes)
alpha <- 0.1 * Dens
c <- 1 / 15
k <- 0.16
TT <- 30
Num <- 10
budget <- 200000

Id <- as.numeric(Comfirmed[Num, -1])
Ih <- as.numeric(Comfirmed[Num, -1] - Comfirmed[Num - 1, -1])/k
I_ac <- (as.numeric(Comfirmed[Num-1, -1] - Comfirmed[Num-2, -1]))/k
R <- rep(0, n)
# Ih = as.numeric(Comfirmed[Num,-1])


tmp <- diag(W)
tmp_names <- colnames(W)

index <- sort.int(tmp, index.return = T)$ix
tmp <- tmp[index]
tmp_names <- tmp_names[index]
t(rbind(tmp_names, tmp))

W[2,]

Ih/N


# compare 4 methods (d) -------------
alpha <- 0.3 * Dens 

d = 0.01
Data <- data_preparation(state, d)
W <- Data[[2]]
plotdata1 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "lambda = 100")

d = 0.02
Data <- data_preparation(state, d)
W <- Data[[2]]
plotdata2 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "lambda = 50")

d = 0.05
Data <- data_preparation(state, d)
W <- Data[[2]]
plotdata3 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "lambda = 20")

d = 0.1
Data <- data_preparation(state, d)
W <- Data[[2]]
plotdata4 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "lambda = 10")

plotdata = bind_rows(plotdata1,plotdata3) %>%
  filter(Time >= 15)

p = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  facet_wrap(.~label, nrow = 2) +
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom")
# ggsave(p,filename = "plots/varing_d.png", width = 8, height = 3)

# compare 4 methods (alpha) -------------
alpha <- 0.1 * Dens 

d = 0.02
Data <- data_preparation(state, d)
W <- Data[[2]]

# plotdata1 = CompareMethods(1*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
#   mutate(label = "alpha = 0.1")
plotdata2 = CompareMethods(2*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.2")
# plotdata3 = CompareMethods(3*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
#   mutate(label = "alpha = 0.3")
plotdata4 = CompareMethods(5*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.5")

plotdata = bind_rows(plotdata2,plotdata4) %>%
  filter(Time >= 15)

p2 = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  facet_wrap(.~label, nrow = 2, scale = "free_y") +
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom")

# ggsave(p,filename = "plots/varing_alpha.png", width = 9, height = 3)


# compare 4 methods (budget) -------------
alpha <- 0.3 * Dens 

plotdata1 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 10000, I_ac) %>%
  mutate(label = "M = 50000")
plotdata2 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 50000, I_ac) %>%
  mutate(label = "M = 100000")
plotdata3 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 100000, I_ac) %>%
  mutate(label = "M = 200000")
plotdata4 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 200000, I_ac) %>%
  mutate(label = "M = 500000")

plotdata = bind_rows(plotdata2,plotdata4) %>%
  filter(Time >= 15)

p3 = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  facet_wrap(.~label, nrow = 2, scale = "free_y")+
  theme(legend.position = "bottom")
# ggsave(p,filename = "plots/varing_budget.png", width = 9, height = 3)

p_compare <- p + p2 + p3 + 
  plot_layout(guides = 'collect') &
  theme_bw() &
  theme(legend.position='bottom') 

ggsave(p_compare, filename = "compare_hubei.pdf", width = 6, height = 3.5)


# compare two strategies -----------

state <- "Wuhan"
d <- 0.02
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
Comfirmed <- Data[[3]]
N <- Data[[4]]$population*10000
area <- Data[[4]]$population
Dens <- rep(1, length(nodes))
n <- length(nodes)
alpha <- 0.3 * Dens
c <- 1 / 15
k <- 0.16
TT <- 30
Num <- 10
budget <- 500000

M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
method = M_list[4]
simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, method, I_ac)
Ra_sim_AS <- as.data.frame(simulation[[2]]) %>% 
  mutate(Time = 1:TT) %>%
  gather(key = "county", value = "rate", -Time)

method = M_list[3]
simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, method, I_ac)
Ra_sim_I <- as.data.frame(simulation[[2]]) %>%
  mutate(Time = 1:TT) %>%
  gather(key = "county", value = "rate", -Time)

method_comparison = Ra_sim_AS %>% 
  filter(rate > 0) %>%
  left_join(Ra_sim_I, by = c("Time", "county"), 
            suffix = c(".Active_Screening", ".By_Infection"))

library(ggrepel)
p = method_comparison %>%
  filter(Time %in% c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30)) %>%
  ggplot() +
  geom_point(aes(x = rate.By_Infection, y = rate.Active_Screening,
                 color = county), size = 3) +
  geom_text_repel(aes(x = rate.By_Infection, y = rate.Active_Screening,
                      label = county))+
  xlab("Testing Rate(Allocated by infection rate)") + 
  ylab("Testing Rate(Allocated by Active Screening)")+
  facet_wrap(Time~., scale = "free_y", nrow = 2) + theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.8))

ggsave(p, filename = "allocation_hubei_2.pdf", height = 4, width = 10)


p = method_comparison %>%
  # filter(Time %in% c(3, 6, 9, 12, 15, 18, 21, 24)) %>%
  ggplot() +
  geom_point(aes(x = rate.By_Infection, y = rate.Active_Screening,
                 color = county), size = 3) +
  geom_text_repel(aes(x = rate.By_Infection, y = rate.Active_Screening,
                      label = county))+
  xlab("Testing Rate(Allocated by infection rate)") + 
  ylab("Testing Rate(Allocated by Active Screening)")+
  facet_wrap(Time~., scale = "free_y", nrow = 6) + theme_bw() +
  theme(legend.position = "none")

ggsave(p, filename = "allocation_hubei_2_all.pdf", height = 9, width = 10)

# bar plot -------------

CompareMethods_time <- function(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac){
  M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[1], I_ac)
  Traj_sim <- simulation[[1]]
  Ra_sim <- simulation[[2]]
  plot_data1 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[1])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[2], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data2 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[2])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[3], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data3 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[3])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[4], I_ac)
  Traj_sim <- simulation[[1]]
  plot_data4 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[4])
  
  plotdata <- bind_rows(plot_data1,plot_data2,plot_data3,plot_data4) %>%
    filter(Time == 30)
  
  return(plotdata)
}

plotdata <- CompareMethods_time(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac)

library(RColorBrewer)
p_bar <- plotdata %>%
  filter(county != "Total") %>%
  ggplot() +
  geom_col(aes(x = county, y = case, 
               fill = method), position = "dodge") +
  theme_bw()+
  scale_fill_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1))+
  ggtitle("Cumulative Confirmed Cases of Counties at Time = 30")

ggsave(p_bar,filename = "barplot_hubei.pdf", width = 6, height = 3.5)
