#------------------------------------------------------------------------------#
# Sensitivity Analysis (SA)                    
# Autor: Molinero Parejo, Ramón                                               
# Created on Wed Jan 18 18:01:07 2023                                                         
#------------------------------------------------------------------------------#

#eliminacion de todas las variables guardadas
rm(list=ls(all=TRUE))

# libraries
library(writexl) # export excel files

# working directory
wd <- 'D:'
setwd(wd)

#------------------------------------------------------------------------------#
## Sensitivity Analysis ####
# new variance: the local mean is the global mean and remove (-1) from (n-1)
new.var <- function(x){
  x = as.numeric(x)
  x = na.omit(x)
  m = mean.y
  return(sum((x - m)**2, na.rm = TRUE) / length(x))
}

# generates table with all factors combinations
#df <- expand.grid(rep(list(0:1), 4))
#colnames(df) <- c('X1', 'X2', 'X3', 'X4')
df <- read.csv('factor_matrix.csv')

# join factor combinations (df) to ccuracy values (df.acc)
df <- cbind(df, df.acc)

for(i in 6:16){
  
  # column with final results (indices)
  n <- paste0('n.', colnames(df[i]))
  df[n] <- 0
  
  #----------------------------------------------------------------------------#
  # statistics of all models
  mean.y <- mean(df[[i]])     #mean
  var.y <- new.var(df[[i]])   #variance
  
  #----------------------------------------------------------------------------#
  ## First order ####
  
  v.x <- function(x1){
    mean.x <- c()
    mean.x <- append(mean.x, mean(df[df[x1] == 1,][[i]]))
    mean.x <- append(mean.x, mean(df[df[x1] == 0,][[i]]))
    var.x <- new.var(mean.x)
    return(var.x)
  }
  
  v1 <- v.x('X1'); df[1,n] <- v1 / var.y
  v2 <- v.x('X2'); df[2,n] <- v2 / var.y
  v3 <- v.x('X3'); df[3,n] <- v3 / var.y
  v4 <- v.x('X4'); df[4,n] <- v4 / var.y
  
  #----------------------------------------------------------------------------#
  ## Second order ####
  
  v.xx <- function(x1, x2){
    mean.xx <- c()
    mean.xx <- append(mean.xx, mean(df[df[x1] == 1 & df[x2] == 1,][[i]]))
    mean.xx <- append(mean.xx, mean(df[df[x1] == 1 & df[x2] == 0,][[i]]))
    mean.xx <- append(mean.xx, mean(df[df[x1] == 0 & df[x2] == 1,][[i]]))
    mean.xx <- append(mean.xx, mean(df[df[x1] == 0 & df[x2] == 0,][[i]]))
    var.xx <- new.var(mean.xx)
    return(var.xx)
  }
  
  v12 <- v.xx('X1', 'X2'); vc12 <- (v12 - v1 - v2); df[5 ,n] <- vc12 / var.y   
  v13 <- v.xx('X1', 'X3'); vc13 <- (v13 - v1 - v3); df[6 ,n] <- vc13 / var.y   
  v14 <- v.xx('X1', 'X4'); vc14 <- (v14 - v1 - v4); df[7 ,n] <- vc14 / var.y    
  v23 <- v.xx('X2', 'X3'); vc23 <- (v23 - v2 - v3); df[8 ,n] <- vc23 / var.y  
  v24 <- v.xx('X2', 'X4'); vc24 <- (v24 - v2 - v4); df[9 ,n] <- vc24 / var.y   
  v34 <- v.xx('X3', 'X4'); vc34 <- (v34 - v3 - v4); df[10,n] <- vc34 / var.y   
  
  #----------------------------------------------------------------------------#
  ## Third order ####
  
  v.xxx <- function(x1, x2, x3){
    mean.xxx <- c()
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 1 & df[x2] == 1 & df[x3] == 1,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 1 & df[x2] == 1 & df[x3] == 0,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 1 & df[x2] == 0 & df[x3] == 1,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 0 & df[x2] == 1 & df[x3] == 1,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 1 & df[x2] == 0 & df[x3] == 0,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 0 & df[x2] == 1 & df[x3] == 0,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 0 & df[x2] == 0 & df[x3] == 1,][[i]]))
    mean.xxx <- append(mean.xxx, mean(df[df[x1] == 0 & df[x2] == 0 & df[x3] == 0,][[i]]))
    var.xxx <- new.var(mean.xxx)
    return(var.xxx)
  }
  
  v123 <- v.xxx('X1', 'X2', 'X3'); vc123 <- (v123 - vc12 - vc13 - vc23 - v1 - v2 - v3)
  v124 <- v.xxx('X1', 'X2', 'X4'); vc124 <- (v124 - vc12 - vc14 - vc24 - v1 - v2 - v4)
  v134 <- v.xxx('X1', 'X3', 'X4'); vc134 <- (v134 - vc13 - vc14 - vc34 - v1 - v3 - v4)   
  v234 <- v.xxx('X2', 'X3', 'X4'); vc234 <- (v234 - vc23 - vc24 - vc34 - v2 - v3 - v4)
  
  df[11,n] <- vc123 / var.y
  df[12,n] <- vc124 / var.y
  df[13,n] <- vc134 / var.y
  df[14,n] <- vc234 / var.y
  
  #----------------------------------------------------------------------------#
  ## Fourth order ####
  v1234 <- var.y; vc1234 <- (v1234 - vc123 - vc124 - vc134 - vc234 - vc12 - 
                               vc13 - vc14 - vc23 - vc24 - vc34 - v1 - v2- v3 - v4); 
  df[15,n] <- vc1234 / var.y  
  
  #----------------------------------------------------------------------------#
  ## Total indices or Total effects ####
  
  df.t.acc[1, n] <- sum(df[df['X1'] == 1,][[i+11]]) # Factor 1. Stochasticity (v)
  df.t.acc[2, n] <- sum(df[df['X2'] == 1,][[i+11]]) # Factor 2. Accessibility (A)
  df.t.acc[3, n] <- sum(df[df['X3'] == 1,][[i+11]]) # Factor 3. Suitability (S)
  df.t.acc[4, n] <- sum(df[df['X4'] == 1,][[i+11]]) # Factor 4. Neighbourhood (N)
  
  # variance testing
  print(paste0('Variance ', n, ': ', sum(df[n])))
  
}

# round df to 4 decimals
df <- round(df, 4)
df.t.acc <- round(df.t.acc, 4)

# summary results
df.summary <- rbind(df[1:4, 17:27], df.t.acc)

# set row names
colnames(df.summary) <- colnames(df[6:16])
df.summary['Names'] <- c('v', 'A', 'S', 'N', 'Total.v', 'Total.A', 'Total.S', 'Total.N')

# export df to excel file
write_xlsx(df, 'sa-results-4.xlsx')
write_xlsx(df.summary, 'sa-summary-4.xlsx')