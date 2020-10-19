#-------------------------
# Define parameters - Linear Change
#-------------------------
sim_tx <-
  function(n,
           tp,
           mean_b0,
           sd_b0,
           low_b0,
           up_b0,
           mean_yf,
           sd_yf,
           mu,
           s,
           mu_j,
           s_j,
           tm,
           mean_ym,
           sd_ym,
           jiggle = TRUE,
           na_pct = 0,
           return = c("long", "wide"),
           seed = 1234) {
    
    return <- match.arg(return)
    
    #-------------------------
    # Linear Change
    #-------------------------
    
    #Set seed to replicate results - different for each trend so they do not start in the same point
    set.seed(seed)
    
    df1 <- NULL #Set empty dataset to start
    for (j in 1:n) {
      t <- NULL #t time x-axis
      y <- NULL # y score y-axis
      m <- NULL #slope
      
      #For first value and first slope
      b0 <-
        rnorm(n = 1, mean = mean_b0, sd = sd_b0) #select random number for normal distribution with parameters b0
      while (b0 < low_b0 |
             b0 > up_b0) {
        #Not allowing it to be lower than 15 or higher than 63
        b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
      }
      yf <-
        rnorm(n = 1, mean = mean_yf, sd = sd_yf) #random number for distribution of yf to create slope
      while (yf > b0) {
        #Not allowing it to be higher than b0
        yf <- rnorm(n = 1, mean = mean_yf, sd = sd_yf)
      }
      yf <- ifelse(yf < 0, 0, yf) #not allowing it to be lower than 0
      m1 <- (yf - b0) / tp  #Set slope from b0 and Yf created before
      
      #Define first data points
      t[1] = 1
      y[1] = b0
      m[1] = m1
      t[2] = 2
      y[2] = m1 + b0
      m[2] = m1
      
      #Loop for timepoint 3 to 20
      for (i in 3:tp) {
        t[i] = i
        m[i] <-
          m[i - 1] + rnorm(1, mean = mu, sd = s) #Add variation to slope so it is not the same for all 20 datapoints
        while (m[i] > 0) {
          #Not allowing slope to be positive
          m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
        }
        
        y[i] <- m[i] + y[i - 1]  #calculate new y
        yt <-
          m[i] * (tp - (i - 1)) + y[i - 1] #calculate last value for that slope to see if it falls in distribution for yf
        
        while (yt < mean_yf - 1.5 * sd_yf |
               yt > mean_yf + 1.5 * sd_yf |
               yt < 0) {
          #make sure yf fall in yf distribution
          m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
          
          while (m[i] > 0) {
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
          }
          
          y[i] <- m[i] + y[i - 1]
          yt <- m[i] * (tp - (i - 1)) + y[i - 1]
        }
        
      }
      
      ind <-
        as.data.frame(cbind(id = j, t, y)) #dataset with values creates for each id
      
      #Add jiggle
      for (k in 1:nrow(ind)) {
        ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
        while (ind$new_y[k] < 0) {
          ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
        }
      }
      while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
        ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
      }
      
      df1 <- rbind(df1, ind) #Put all ids together
      
      rm(b0, yf, m1, t, m, y, yt, ind) #clean for next loop
    }
    
    
    
    # SELECT JIGGLE OR NOT
    
    ## LONG
    df_long <- tibble::as_tibble(df1)
    #
    
    if (jiggle == TRUE) {
      
      df_long <- dplyr::select(df_long, id, time = t, value = new_y)
      
    } else if (jiggle == FALSE) {
      
      df_long <- dplyr::select(df_long, id, time = t, value = y)
      
    }
    
    # add missing data
    df_long <- dplyr::mutate(df_long,
                             random_num = runif(nrow(df_long)),
                             value = base::ifelse(random_num <= na_pct, NA, value)) 
    
    df_long <- select(df_long, -random_num)

    # RETURN LONG OR
    if (return == "long") {
      return(df_long)
      
    } else if (return == "wide") {
      df_wide <-
        tidyr::pivot_wider(
          data = df_long,
          names_from = time,
          names_prefix = "t_",
          values_from = value
        )
      
      return(df_wide)
      
    }
    
    
    
  }

# n <- 20
#
# # Number of timepoints
# tp <- 20
#
# # Set b0
# mean_b0 = 29.33
# sd_b0 = 8.04
# low_b0 = 15
# up_b0 =63
#
# # Set Yf
# mean_yf = 15.9
# sd_yf = 11.9
#
# # Variation of slopes  #Autocorrelation within individual
# mu = 0
# s = 0.2
#
# # Jiggle for points around the line
# mu_j = 0
# s_j = 5
#
# # Medium point (Setting mean and sd at timepoint 8)
# tm = 8
# mean_ym = 20
# sd_ym = 7

library(tidyverse)


sim_tx(
    n = 100,
    tp = 20,
    mean_b0 = 29.33,
    sd_b0 = 8.04,
    low_b0 = 15,
    up_b0 = 63,
    mean_yf = 15.9,
    sd_yf = 11.9,
    mu = 0,
    s = 0.2,
    mu_j = 0,
    s_j = 5,
    tm = 8,
    mean_ym = 20,
    sd_ym = 7,
    return = "wide",
    jiggle = F,
    seed = 123, na_pct = .1
  )

df_long_01_jf <-
  sim_tx(
    n = 100,
    tp = 20,
    mean_b0 = 29.33,
    sd_b0 = 8.04,
    low_b0 = 15,
    up_b0 = 63,
    mean_yf = 15.9,
    sd_yf = 11.9,
    mu = 0,
    s = 0.2,
    mu_j = 0,
    s_j = 5,
    tm = 8,
    mean_ym = 20,
    sd_ym = 7,
    return = "long",
    jiggle = F,
    seed = 123
  )



na_pct <- .9

df_long_01_jf %>%
  dplyr::mutate(
    random_num = runif(nrow(.)),
    value = base::ifelse(random_num <= na_pct, NA, value)) %>% View()


  # ) %>%
  # dplyr::select(-random_num) %>%
  # tidyr::spread(vars, value)

df_long_01_jf %>%
  ggplot2::ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  theme(legend.position = "none")


df_long_01_jt <-
  sim_tx(
    n = 100,
    tp = 20,
    mean_b0 = 29.33,
    sd_b0 = 8.04,
    low_b0 = 15,
    up_b0 = 63,
    mean_yf = 15.9,
    sd_yf = 11.9,
    mu = 0,
    s = 0.2,
    mu_j = 0,
    s_j = 3,
    tm = 8,
    mean_ym = 20,
    sd_ym = 7,
    return = "long",
    jiggle = T,
    seed = 123
  )

df_long_01_jt %>%
  ggplot2::ggplot(aes(x = factor(time), y = value, group = id)) +
  geom_line(alpha = .3, colour = "blue") +
  geom_point(alpha = .3, size = .5) +
  theme(legend.position = "none")



#-----
# Plot results

plot(df1$t, df1$new_y, col = df1$id, pch = 16) #with jiggle
plot(df1$t, df1$y, col = df1$id, pch = 16) #no jiggle

#-----
# Verify stats

#Baseline
base1 <- subset(df1, t == 1)

mean(base1$new_y) #with jiggle
mean(base1$y) #no jiggle
sd(base1$new_y) #with jiggle
sd(base1$y) #no jiggle

#End
end1 <- subset(df1, t == 20)

mean(end1$new_y) #with jiggle
mean(end1$y) #no jiggle
sd(end1$new_y) #with jiggle
sd(end1$y) #no jiggle


#-----
# Verify trend
lme4::lmer(new_y ~ t + (1 | id), data = df1)@beta[2] # with jiggle
lme4::lmer(y ~ t + (1 | id), data = df1)@beta[2] # with jiggle














#---------------------------------------------------------------------------------------------------
#-------------------------
# Define parameters - Stable Change
#-------------------------

# Number of subjects
n <- 20

# Number of timepoints
tp <- 20

# Set b0
mean_b0 = 29.33
sd_b0 = 8.04
low_b0 = 15
up_b0 = 63

# Set Yf
mean_yf = 15.9
sd_yf = 11.9

# Variation of slopes  #Autocorrelation within individual
mu = 0
s = 0.2

# Jiggle for points around the line
mu_j = 0
s_j = 5

# Medium point (Setting mean and sd at timepoint 8)
tm = 8
mean_ym = 20
sd_ym = 7




#-------------------------
# Stable Change - from start distribution to start distribution
#-------------------------

#Set seed to replicate results - different for each trend so they do not start in the same point
set.seed(49872)

df2 <- NULL  #Set empty dataset to start
for (j in 1:n) {
  t <- NULL
  y <- NULL
  m <- NULL
  
  #For first value
  b0 <-
    rnorm(n = 1, mean = mean_b0, sd = sd_b0) #select random number for normal distribution with parameters b0
  while (b0 < low_b0 |
         b0 > up_b0) {
    #Not allowing it to be lower than 15 or higher than 63
    b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
  }
  yf <-
    rnorm(n = 1, mean = mean_b0, sd = sd_b0) #random number for distribution of b0 to create slope
  yf <- ifelse(yf < 0, 0, yf) #not allowing it to be lower than 0
  m1 <- (yf - b0) / tp  #Set slope from b0 and Yf created before
  
  #Define first data points
  t[1] = 1
  y[1] = b0
  m[1] = m1
  t[2] = 2
  y[2] = m1 + b0
  m[2] = m1
  
  #Loop for timepoint 3 to 20
  for (i in 3:tp) {
    t[i] = i
    m[i] <-
      m[i - 1] + rnorm(1, mean = mu, sd = s) #Add variation to slope so it is not the same for all 20 datapoints
    y[i] <- m[i] + y[i - 1] #calculate new y
    yt <-
      m[i] * (tp - (i - 1)) + y[i - 1] #calculate last value for that slope to see if it falls in distribution for b0
    
    while (yt < b0 - 1.5 * sd_b0 |
           yt > b0 + 1.5 * sd_b0 |
           yt < 0) {
      #make sure yf fall in b0 distribution
      m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
      y[i] <- m[i] + y[i - 1]
      yt <- m[i] * (tp - (i - 1)) + y[i - 1]
    }
    
  }
  ind <-
    as.data.frame(cbind(id = j, t, y))  #dataset with values creates for each id
  
  #Add jiggle
  for (k in 1:nrow(ind)) {
    ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    while (ind$new_y[k] < 0) {
      ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    }
  }
  while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
    ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
  }
  
  df2 <- rbind(df2, ind) #Put all ids together
  
  rm(b0, yf, m1, t, m, y, yt, ind) #clean for next loop
}



#-----
# Plot results

plot(df2$t, df2$new_y, col = df2$id, pch = 16) #with jiggle
plot(df2$t, df2$y, col = df2$id, pch = 16) #no jiggle

#-----
# Verify stats

#Baseline
base2 <- subset(df2, t == 1)

mean(base2$new_y) #with jiggle
mean(base2$y) #no jiggle
sd(base2$new_y) #with jiggle
sd(base2$y) #no jiggle

#End
end2 <- subset(df2, t == 20)

mean(end2$new_y) #with jiggle
mean(end2$y) #no jiggle
sd(end2$new_y) #with jiggle
sd(end2$y) #no jiggle


#-----
# Verify trend
lme4::lmer(new_y ~ t + (1 | id), data = df2)@beta[2] # with jiggle
lme4::lmer(y ~ t + (1 | id), data = df2)@beta[2] # with jiggle







#------------------------------------------------
#-------------------------
# Stable Change - ANOTHER OPTION WOULD BE TO START SLOPE WITH 0
#-------------------------

#Set seed to replicate results - different for each trend so they do not start in the same point
set.seed(84972)

df2a <- NULL  #Set empty dataset to start
for (j in 1:n) {
  t <- NULL
  y <- NULL
  m <- NULL
  
  #For first value
  b0 <-
    rnorm(n = 1, mean = mean_b0, sd = sd_b0) #select random number for normal distribution with parameters b0
  while (b0 < low_b0 |
         b0 > up_b0) {
    #Not allowing it to be lower than 15 or higher than 63
    b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
  }
  #yf <- rnorm(n=1, mean=mean_b0, sd=sd_b0) #random number for distribution of b0 to create slope
  #yf <- ifelse(yf<0,0,yf) #not allowing it to be lower than 0
  m1 <-
    0  #sTART SLOPE IN ZERO                    #(yf-b0)/tp  #Set slope from b0 and Yf created before
  
  #Define first data points
  t[1] = 1
  y[1] = b0
  m[1] = m1
  #t[2]=2
  #y[2]=m1+b0
  #m[2]=m1
  
  #Loop for timepoint 2 to 20
  for (i in 2:tp) {
    t[i] = i
    m[i] <-
      m[i - 1] + rnorm(1, mean = mu, sd = s) #Add variation to slope so it is not the same for all 20 datapoints
    y[i] <- m[i] + y[i - 1] #calculate new y
    yt <-
      m[i] * (tp - (i - 1)) + y[i - 1] #calculate last value for that slope to see if it falls in distribution for b0
    
    while (yt < b0 - 1.5 * sd_b0 |
           yt > b0 + 1.5 * sd_b0 |
           yt < 0) {
      #make sure yf fall in b0 distribution
      m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
      y[i] <- m[i] + y[i - 1]
      yt <- m[i] * (tp - (i - 1)) + y[i - 1]
    }
    
  }
  ind <-
    as.data.frame(cbind(id = j, t, y))  #dataset with values creates for each id
  
  #Add jiggle
  for (k in 1:nrow(ind)) {
    ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    while (ind$new_y[k] < 0) {
      ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    }
  }
  while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
    ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
  }
  
  df2a <- rbind(df2a, ind) #Put all ids together
  
  rm(b0, m1, t, m, y, yt, ind) #clean for next loop
}



#-----
# Plot results

plot(df2a$t, df2a$new_y, col = df2a$id, pch = 16) #with jiggle
plot(df2a$t, df2a$y, col = df2a$id, pch = 16) #no jiggle

#-----
# Verify stats

#Baseline
base2a <- subset(df2a, t == 1)

mean(base2a$new_y) #with jiggle
mean(base2a$y) #no jiggle
sd(base2a$new_y) #with jiggle
sd(base2a$y) #no jiggle

#End
end2a <- subset(df2a, t == 20)

mean(end2a$new_y) #with jiggle
mean(end2a$y) #no jiggle
sd(end2a$new_y) #with jiggle
sd(end2a$y) #no jiggle


#-----
# Verify trend
lme4::lmer(new_y ~ t + (1 | id), data = df2a)@beta[2] # with jiggle
lme4::lmer(y ~ t + (1 | id), data = df2a)@beta[2] # with jiggle










#---------------------------------------------------------------------------------------------------
#-------------------------
# Define parameters - Inverse Change
#-------------------------

# Number of subjects
n <- 20

# Number of timepoints
tp <- 20

# Set b0
mean_b0 = 29.33
sd_b0 = 8.04
low_b0 = 15
up_b0 = 63

# Set Yf
mean_yf = 15.9
sd_yf = 11.9

# Variation of slopes  #Autocorrelation within individual
mu = 0
s = 0.2

# Jiggle for points around the line
mu_j = 0
s_j = 5

# Medium point (Setting mean and sd at timepoint 8)
tm = 8
mean_ym = 20
sd_ym = 7


#-------------------------
# Inverse Change
#-------------------------

#Set seed to replicate results - different for each trend so they do not start in the same point
set.seed(369845)

df3 <- NULL #Set empty dataset to start
for (j in 1:n) {
  t <- NULL
  y <- NULL
  m <- NULL
  
  #For first value
  b0 <-
    rnorm(n = 1, mean = mean_b0, sd = sd_b0) #select random number for normal distribution with parameters b0
  while (b0 < low_b0 |
         b0 > up_b0) {
    #Not allowing it to be lower than 15 or higher than 63
    b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
  }
  ym <-
    rnorm(n = 1, mean = mean_ym, sd = sd_ym) #random number for distribution of ym to create slope
  while (ym > b0) {
    #Not allowing it to be higher than b0
    ym <- rnorm(n = 1, mean = mean_ym, sd = sd_ym)
  }
  ym <- ifelse(ym < 0, 0, ym)   #not allowing it to be lower than 0
  m1 <- (ym - b0) / tm  #Set slope from b0 and Ym created before
  
  #Define first data points
  t[1] = 1
  y[1] = b0
  m[1] = m1
  t[2] = 2
  y[2] = m1 + b0
  m[2] = m1
  
  #Loop for timepoint 3 to 8 (middle point)
  for (i in 3:tm) {
    t[i] = i
    m[i] <-
      m[i - 1] + rnorm(1, mean = mu, sd = s)  #Add variation to slope so it is not the same for all 20 datapoints
    while (m[i] > 0) {
      #Not allowing slope to be positive
      m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
    }
    y[i] <- m[i] + y[i - 1]  #calculate new y
    ym <-
      m[i] * (tm - (i - 1)) + y[i - 1] #calculate last value for that slope to see if it falls in distribution for ym
    
    while (ym < mean_ym - 1.5 * sd_ym |
           ym > mean_ym + 1.5 * sd_ym |
           ym < 0) {
      #make sure ym fall in ym distribution
      m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
      while (m[i] > 0) {
        #Not allowing slope to be positive
        m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
      }
      y[i] <- m[i] + y[i - 1]
      ym <- m[i] * (tm - (i - 1)) + y[i - 1]
    }
  }
  
  #Redefine slope
  yf <-
    rnorm(n = 1, mean = mean_yf, sd = sd_ym) ## NOTE: SEE IF MODIFY BACK SD TO sd_yf - New last point to redefine slope
  yf <- ifelse(yf < 0, 0, yf)  #not allowing it to be lower than 0
  mm <- (yf - y[tm]) / (tp - tm)  #Set slope from ym and yf created before
  
  #Loop for timepoint 9 to 20
  for (i in (tm + 1):tp) {
    t[i] = i
    m[i] <-
      mm + rnorm(1, mean = mu, sd = s)  #Add variation to slope so it is not the same for all 20 datapoints
    y[i] <- m[i] + y[i - 1]  #calculate new y
    yt <-
      m[i] * (tp - (i - 1)) + y[i - 1]  #calculate last value for that slope to see if it falls in distribution for yf
    
    while (yt < mean_yf - 1.5 * sd_ym |
           yt > mean_yf + 1.5 * sd_ym |
           yt < 0) {
      ## NOTE: SEE IF MODIFY BACK SD TO sd_yf - make sure it falls in distribution for yf
      m[i] <- mm + rnorm(1, mean = mu, sd = s)
      y[i] <- m[i] + y[i - 1]
      yt <- m[i] * (tp - (i - 1)) + y[i - 1]
    }
  }
  
  ind <-
    as.data.frame(cbind(id = j, t, y)) #dataset with values creates for each id
  
  #Add jiggle
  for (k in 1:nrow(ind)) {
    ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    while (ind$new_y[k] < 0) {
      ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
    }
  }
  while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
    ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
  }
  
  df3 <- rbind(df3, ind) #Put all ids together
  
  rm(b0, ym, m1, t, m, y, yt, ind, yf, mm) #clean for next loop
}



#-----
# Plot results

plot(df3$t, df3$new_y, col = df3$id, pch = 16) #with jiggle
plot(df3$t, df3$y, col = df3$id, pch = 16) #no jiggle

#-----
# Verify stats

#Baseline
base3 <- subset(df3, t == 1)

mean(base3$new_y) #with jiggle
mean(base3$y) #no jiggle
sd(base3$new_y) #with jiggle
sd(base3$y) #no jiggle


#End
end3 <- subset(df3, t == 20)

mean(end3$new_y) #with jiggle
mean(end3$y) #no jiggle
sd(end3$new_y) #with jiggle
sd(end3$y) #no jiggle

#Mid
mid3 <- subset(df3, t == 8)

mean(mid3$new_y) #with jiggle
mean(mid3$y) #no jiggle
sd(mid3$new_y) #with jiggle
sd(mid3$y) #no jiggle


#-----
# Verify trend
lme4::lmer(new_y ~ t + (1 | id), data = df3)@beta[2] # with jiggle
lme4::lmer(y ~ t + (1 | id), data = df3)@beta[2] # with jiggle
