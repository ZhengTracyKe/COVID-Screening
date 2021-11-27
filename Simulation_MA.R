# code for simulations on Massachusetts

rm(list = ls())
set.seed(1)
setwd()
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(patchwork)


# functions --------

source("data_preparation.R")
source("Active_Screening_Strategy_0906.R")

CompareMethods <- function(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac){
  
  # generate results for 4 different methods in same parameter settings
  
  M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[1],I_ac)
  Traj_sim <- simulation[[1]]
  Ra_sim <- simulation[[2]]
  plot_data1 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[1])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[2],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data2 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[2])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[3],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data3 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[3])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[4],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data4 <- Traj_sim %>% 
    filter(county == "Total", type == "Dcum") %>%
    mutate(method = M_list[4])
  
  plotdata <- bind_rows(plot_data1,plot_data2,plot_data3,plot_data4)
  
  return(plotdata)
}


# W = as.matrix(W)
# rowSums(W)
# 
# tmp <- Ih
# tmp_names <- colnames(W)
# 
# t(rbind(colnames(W), diag(W)))
# 
# index <- sort.int(tmp, index.return = T)$ix
# tmp <- tmp[index]
# tmp_names <- tmp_names[index]
# t(rbind(tmp_names, tmp))
# 
# W[2,]
# 
# Ih/N

# p = ggplot(W_df, aes(x = County, y = County2)) +
#   geom_tile(aes(fill = weight),colour = "white") +
#   coord_equal() + xlab("") + ylab("") + ggtitle("Network Weight (d = 0.05)")+
#   scale_fill_gradientn(low = brewer.pal(7,"Set1")[2],mid = "white",high = brewer.pal(7,"Set1")[1],
#                        midpoint = 0.6,breaks = c(0.1,0,0.9))+
#   labs(fill = "Weight")+
#   theme(axis.text.y = element_text(size=12),
#         axis.text.x = element_text(size=12),
#         plot.title = element_text(hjust = 0.5,size=14,face = "bold"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white"))
#   
# p


# test on real data: Massachusetts -------

state <- "Massachusetts"
TT <- 30 # simulation length
Num <- 20 # begin time
c <- 1 / 15
k <- 0.16
budget <- 100000
Dens <- rep(1,12) 

# compare 4 methods (d) -------------
alpha <- 0.3 * Dens

d = 0.01
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
n <- length(nodes)
Comfirmed <- Data[[3]]
N <- Data[[4]]$population*10000
area <- Data[[4]]$population
Id <- as.numeric(Comfirmed[Num, -1])
Ih <- (as.numeric(Comfirmed[Num, -1] - Comfirmed[Num - 1, -1]))/k
I_ac <- (as.numeric(Comfirmed[Num-1, -1] - Comfirmed[Num - 2, -1]))/k
R <- rep(0, n)
plotdata1 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget,I_ac) %>%
  mutate(label = "lambda = 100")

# d = 0.02
# Data <- data_preparation(state, d)
# W <- Data[[2]]
# plotdata2 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget,I_ac) %>%
#   mutate(label = "d = 0.02")

d = 0.05
Data <- data_preparation(state, d)
W <- Data[[2]]
plotdata3 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget,I_ac) %>%
  mutate(label = "lambda = 20")

# d = 0.1
# Data <- data_preparation(state, d)
# W <- Data[[2]]
# plotdata4 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
#   mutate(label = "d = 0.1")

# plotdata = bind_rows(plotdata1,plotdata2,plotdata3,plotdata4)
plotdata = bind_rows(plotdata1,plotdata3) %>%
  filter(Time >= 15)

p = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  facet_wrap(.~label, nrow = 2) +
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom")
p

# ggsave(p,filename = "plots/varing_d.png", width = 8, height = 3)

# compare 4 methods (alpha) -------------
budget <- 100000
alpha <- 0.1 * Dens

d = 0.01
Data <- data_preparation(state, d)
W <- Data[[2]]
R <- rep(0, n)

plotdata1 = CompareMethods(1*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.1")
plotdata2 = CompareMethods(2*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.2")
plotdata3 = CompareMethods(3*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.3")
plotdata4 = CompareMethods(4*alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac) %>%
  mutate(label = "alpha = 0.4")

plotdata = bind_rows(plotdata2, plotdata4) %>%
  filter(Time >= 15)

p2 = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  facet_wrap(.~label, nrow = 2, scale = "free_y")+
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom")
# ggsave(p,filename = "plots/varing_alpha.png", width = 9, height = 3)


# compare 4 methods (budget) -------------
alpha <- 0.3 * Dens 

plotdata1 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 10000, I_ac) %>%
  mutate(label = "M = 10000")
plotdata2 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 50000, I_ac) %>%
  mutate(label = "M = 50000")
plotdata3 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 100000, I_ac) %>%
  mutate(label = "M = 100000")
plotdata4 = CompareMethods(alpha, c, k, TT, W, N, Ih, Id, R, 200000, I_ac) %>%
  mutate(label = "M = 200000")

plotdata = bind_rows(plotdata1, plotdata4) %>%
  filter(Time >= 15)

p3 = ggplot(plotdata) + 
  geom_line(aes(x = Time, y = case, color = method))+
  facet_wrap(.~label, nrow = 2, scale = "free_y") +
  scale_color_manual(values = c("#fb8072","#bebada","#80b1d3","#fdb462"))+
  theme(legend.position = "bottom")
# ggsave(p,filename = "plots/varing_budget.png", width = 9, height = 3)

p_compare <- p + p2 + p3 + 
  plot_layout(guides = 'collect') &
  theme_bw() &
  theme(legend.position='bottom') 

ggsave(p_compare, filename = "compare_massachusetts.pdf", width = 6, height = 3.5)


# bar plot -------------

CompareMethods_time <- function(alpha, c, k, TT, W, N, Ih, Id, R, budget, I_ac){
  
  # generate data for bar plots
  
  M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[1],I_ac)
  Traj_sim <- simulation[[1]]
  Ra_sim <- simulation[[2]]
  plot_data1 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[1])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[2],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data2 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[2])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[3],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data3 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[3])
  
  simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, M_list[4],I_ac)
  Traj_sim <- simulation[[1]]
  plot_data4 <- Traj_sim %>% 
    filter(type == "Dcum") %>%
    mutate(method = M_list[4])
  
  plotdata <- bind_rows(plot_data1,plot_data2,plot_data3,plot_data4) %>%
    filter(Time == 30)
  
  return(plotdata)
}

state <- "Massachusetts"
d <- 0.01
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
Comfirmed <- Data[[3]]
N <- Data[[4]]$population
area <- Data[[4]]$population
Dens <- rep(1, length(nodes))
n <- length(nodes)
alpha <- 0.3 * Dens
c <- 1 / 15
k <- 0.16
TT <- 30
Num <- 40
budget <- 100000

plotdata <- CompareMethods_time(alpha, c, k, TT, W, N, Ih, Id, R, budget,I_ac)

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
        axis.text.x = element_text(angle = 15, hjust = 1))+
  ggtitle("Cumulative Confirmed Cases of Counties at Time = 30")

ggsave(p_bar,filename = "barplot_massachusetts.pdf", width = 6, height = 3.5)

# compare two strategies -----------

state <- "Massachusetts"
d <- 0.01
Data <- data_preparation(state, d)
W <- Data[[2]]
nodes <- colnames(W)
Comfirmed <- Data[[3]]
N <- Data[[4]]$population
area <- Data[[4]]$population
Dens <- rep(1, length(nodes))
n <- length(nodes)
alpha <- 0.3 * Dens
c <- 1 / 15
k <- 0.16
TT <- 30
Num <- 40
budget <- 100000

M_list <- c("No_Intervention", "By_population", "By_infection", "Active_Screening")
method = M_list[4]
simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, method,I_ac=NA)
Ra_sim_AS <- as.data.frame(simulation[[2]]) %>% 
  mutate(Time = 1:TT) %>%
  gather(key = "county", value = "rate", -Time)

method = M_list[3]
simulation <- TrajectoryGenerator(alpha, c, k, TT, W, N, Ih, Id, R, budget, method,I_ac)
Ra_sim_I <- as.data.frame(simulation[[2]]) %>%
  mutate(Time = 1:TT) %>%
  gather(key = "county", value = "rate", -Time)

method_comparison = Ra_sim_AS %>% 
  filter(rate > 0) %>%
  left_join(Ra_sim_I, by = c("Time", "county"), 
            suffix = c(".Active_Screening", ".By_Infection"))

method_comparison %>%
  group_by(county) %>%
  summarise(count = n()) %>%
  arrange(count)

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
  theme(legend.position = "none")

ggsave(p, filename = "allocation_massachusetts.pdf", height = 4, width = 10)

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

ggsave(p, filename = "allocation_massachusetts_all.pdf", height = 9, width = 10)
