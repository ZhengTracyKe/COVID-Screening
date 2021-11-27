ma_smooth = function(v){
  # 7-days moving average smoothing
  diff_v = c(v[1],diff(v))
  diff_v_s = stats::filter(diff_v, rep(1/7,7))
  index = which(is.na(diff_v_s))
  diff_v_s[index] = diff_v[index]
  v_s = cumsum(diff_v_s)
  return(v_s)
}


data_preparation <- function(state,d){
  
  if (state == "Wuhan"){
    # drive_distance matrix
    filename1 = "/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/Hubei_dridis_network.csv"
    drive_dis = read.csv(filename1,stringsAsFactors = F)
    cities = drive_dis$X
    dis_matrix = as.matrix(drive_dis[,-1])/1600 # mile
    n = nrow(dis_matrix)
    
    for (i in 2:n){
      for (j in 1:i){
        dis_matrix[i,j] = dis_matrix[j,i]
      }
    }
    
    weight_matrix = 1/(d*dis_matrix^2+diag(rep(1,n))) # d越小联系越紧密，d=0时等权
    # for (i in 1:n){
    #   s = sum(weight_matrix[i,i:n])
    #   std = 1-(sum(weight_matrix[i,])-s)
    #   std_f = std/s
    #   weight_matrix[i,i:n] = weight_matrix[i,i:n]*std_f
    #   weight_matrix[i:n,i] = as.vector(weight_matrix[i,i:n])
    # }
    
    for (w in 1:n){
      weight_matrix[w,w] = 2- sum(weight_matrix[w,])
    }
    
    # diagnosis data
    filename2 = paste0('/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/Hubei_infection.csv')
    comfirmed_matrix <- read.csv(filename2, stringsAsFactors = F) %>%
      select(-population) %>%
      pivot_longer(-name_en, names_to = "Date", values_to = "Comfirmed") %>%
      mutate(Date = mdy(substring(Date,2))) %>%
      pivot_wider(names_from = name_en,values_from = Comfirmed)
    
    
    # population&area data
    filename3 = paste0("/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/Hubei_population.csv")
    geo_information = read.csv(filename3,stringsAsFactors = F)
    
    nodes = cities
    W = weight_matrix
  }else{
    # drive_distance matrix
    filename1 = paste0('/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/US_distance_matrix/',
                       state,'_distance.csv')
    drive_dis = read.csv(filename1,stringsAsFactors = F)
    cities = drive_dis$X
    dis_matrix = as.matrix(drive_dis[,-1])/1.6 # mile
    n = nrow(dis_matrix)
    weight_matrix = 1/(d*dis_matrix^2+diag(rep(1,n))) # d越小联系越紧密，d=0时等权
    # for (i in 1:n){
    #   s = sum(weight_matrix[i,i:n])
    #   std = 1-(sum(weight_matrix[i,])-s)
    #   std_f = std/s
    #   weight_matrix[i,i:n] = weight_matrix[i,i:n]*std_f
    #   weight_matrix[i:n,i] = as.vector(weight_matrix[i,i:n])
    # }
    
    for (w in 1:n){
      weight_matrix[w,w] = 2- sum(weight_matrix[w,])
    }
    
    # diagnosis data
    filename2 = paste0('/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/US_comfirmed/',
                       state,'_comfirmed.csv')
    comfirmed_matrix = read.csv(filename2,stringsAsFactors = F) %>% 
      mutate(Date = mdy(Date))
    
    # for (i in 2:13){
    #   comfirmed_matrix[,i] = ma_smooth(comfirmed_matrix[,i]) #smoothing
    # }
    
    
    # population&area data
    filename3 = paste0("/Users/Yaxuan/PKU/Research/Testing_Allocation/Data/US_information/",
                       state,"_population.csv")
    geo_information = read.csv(filename3,stringsAsFactors = F)
    
    
    
    nodes = cities
    W = weight_matrix
  }
  
  return(list(nodes,W,comfirmed_matrix,geo_information))
}