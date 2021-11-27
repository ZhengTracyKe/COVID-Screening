# Yaxuan
# update at 2021/09/06

library(boot) # simplex algorithm

# parameters of Active_Screening() function:
#' @param alpha: infection rate;
#' @param c: natural recovery rate;
#' @param k: natural detection rate;
#' @param t: present time;
#' @param TT: total time;
#' @param W: weight matrix of the network;
#' @param N: population vector;
#' @param Ih: hiden case vector at time t;
#' @param Id: diagnosis case vector at time t;
#' @param R: recovered case vector at time t;
#' @param Ra: screening rate at time t;
#' @param budget: maximum number of total screening people;
#' @param Method: allocation strategy: "Active_Screening", "No_Intervention", "By_population", "By_infection";



# functions for Active_Screening model
{
  Active_Screening <- function(alpha, c, k, t, TT, W, N, Ih, Id, R, Ra, budget, Method,
                               I_ac) {
    n <- nrow(W) # number of nodes

    # i = as.numeric((Id/N/k)*(rep(1-k,n)-diag(Rat))) # estimate infection rate
    i <- as.numeric(Ih / N)
    i_ac <- as.numeric(I_ac / N)

    if (Method == "Active_Screening") {
      Ra_next <- Allocation(alpha, c, k, t, TT, W, N, i, budget)
      Ra_next <- diag(Ra_next)
    }
    if (Method == "No_Intervention") {
      Ra_next <- rep(0, n)
    }
    if (Method == "By_population") {
      r <- budget / sum(N)
      Ra_next <- rep(r, n)
    }
    if (Method == "By_infection") {
      # r <- budget / sum(i_ac)
      # Ra_next <- r * i_ac / N
      r <- budget / sum(i)
      Ra_next <- r * i / N
    }

    d_next <- rep(NA, n)
    da_next <- rep(NA, n)
    Id_next <- rep(NA, n)
    R_next <- rep(NA, n)
    Ih_next <- rep(NA, n)

    for (v in 1:n) {
      r <- Ra_next[v] # screening rate of node v
      tmp <- 1
      for (m in 1:n) {
        tmp <- tmp * (1 - alpha[v] * W[v, m] * i[m])
      }
      p <- 1 - tmp # infection probability of node i

      Ih_next[v] <- (N[v] - Ih[v] - Id[v] -R[v]) * p + 
        Ih[v] * (1 - c - k - r) # infection process
      Id_next[v] <- Id[v] + (k+r) * Ih[v] - c * Id[v]
      R_next[v] <- R[v] + c * (Id[v] + Ih[v])
      
      d_next[v] <- k * Ih[v] # diagnosis
      da_next[v] <- r * Ih[v] # screening
    }
    return(list(Ra_next, Ih_next, Id_next, R_next, d_next, da_next))
  }

  Allocation <- function(alpha, c, k, t, TT, W, N, i, budget) {
    # return active screening strategy
     
    n <- nrow(W)
    Alpha <- diag(alpha)
    U <- Alpha %*% W + (1 - c - k) * diag(rep(1, n))
    
    # i <-  U %*% i_ac
    
    Ra <- Ua <- Ra_new <- array(0, dim = c(n, n, TT))
    L <- 50
    for (l in 1:L) {
      gamma <- 2 / (l + 2)

      for (m in (t + 1):TT) {
        Ua[, , m] <- U - Ra[, , m]
      }

      for (time in (t + 1):TT) {
        if (time < TT) {
          tmp1 <- diag(rep(0, n))
          for (t_prime in (time + 1):TT) {
            tmp2 <- diag(rep(1, n))
            for (tau in (time + 1):t_prime) {
              tmp2 <- Ua[, , tau] %*% tmp2
            }
            tmp1 <- tmp1 + t(tmp2)
          }
        } else {
          tmp1 <- diag(rep(1, n))
        }

        if (time == t + 1) {
          tmp3 <- diag(rep(1, n))
        } else {
          tmp3 <- diag(rep(1, n))
          for (tao in (t + 1):(time - 1)) {
            tmp3 <- Ua[, , tao] %*% tmp3
          }
          tmp3 <- t(tmp3)
        }

        Delta <- -tmp1 %*% matrix(N, nrow = n, ncol = 1) %*% 
          matrix(i, nrow = 1, ncol = n) %*% tmp3 # %*% t(U)
        D <- diag(Delta)
        Ra_star <- simplex(a = t(D), A1 = diag(rep(1, n)), b1 = rep(1, n), 
                           A3 = t(N), b3 = budget)$soln

        Ra_new[, , time] <- gamma * diag(Ra_star) + (1 - gamma) * Ra[, , time]
      }
      Ra <- Ra_new
    }
    Ra_next <- Ra[, , t + 1]
    return(Ra_next)
  }
  
  TrajectoryGenerator <- function(alpha, c, k, TT, W, N, Ih, Id, R , budget, Method, I_ac) {
    # given parameters
    # return the trajectory and allocation strategy at each time
    
    Ra_T = Id_T = Ih_T = d_T = R_T = da_T = S_T =  matrix(0, nrow = TT, ncol = length(N))
    Ra = rep(0,n)
    for (t in 0:(TT-1)){
      result = Active_Screening(alpha, c, k, t, TT, W, N, Ih, Id, R, Ra, budget, Method, I_ac)
      # return(list(diag(Ra_next), Ih_next, Id_next, R_next, d_next, da_next))
      Ra = result[[1]]
      Ih = result[[2]]
      Id = result[[3]]
      R = result[[4]]
      d_next = result[[5]]
      da_next = result[[6]]
      
      t = t+1
      # print(t)
      Ra_T[t, ] = Ra
      Ih_T[t, ] = Ih
      Id_T[t, ] = Id
      R_T[t, ] = R
      S_T[t, ] = N-R-Ih-Id
      
      d_T[t, ] = d_next
      da_T[t, ] = da_next
    }
    colnames(Ra_T) = colnames(Ih_T) = colnames(Id_T) = colnames(S_T) =
      colnames(R_T) = colnames(d_T) = colnames(da_T) = nodes
    
    Ih_df = as.data.frame(Ih_T) %>%
      mutate(Total = rowSums(Ih_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "Ih")
    
    Id_df = as.data.frame(Id_T) %>%
      mutate(Total = rowSums(Id_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "Id")
    
    R_df = as.data.frame(R_T) %>%
      mutate(Total = rowSums(R_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "R")
    
    S_df = as.data.frame(S_T) %>%
      mutate(Total = rowSums(S_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "S")
    
    # ggplot(SIR, aes(x = Time, y = case)) +
    #   geom_line(aes(color = type)) +
    #   facet_wrap(.~county, scales = "free_y")
    
    D_T = d_T + da_T
    Dcum_T = apply(D_T, 2, cumsum)
    D_df = as.data.frame(D_T) %>%
      mutate(Total = rowSums(D_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "D")
    Dcum_df = as.data.frame(Dcum_T) %>%
      mutate(Total = rowSums(Dcum_T), Time = 1:TT) %>%
      gather(key = "county", value = "case", -Time) %>%
      mutate(type = "Dcum")
    
    Trajectory = bind_rows(Ih_df, Id_df, R_df, D_df, Dcum_df)
    
    return(list(Trajectory,Ra_T))
    
  }
}


