#' Simulate longitudinal data
#'
#' @param n Numeric, specifying the sample size
#' @param tp Numeric, specifying the number of repeated measurements
#' @param mean_b0 Numeric, specifying the mean at the first time point
#' @param sd_b0 Numeric, specifying the standard deviation at the first time point
#' @param low_b0 Numeric, specifying the lower limit at the first time point
#' @param up_b0 Numeric, specifying the upper limit at the first time point
#' @param mean_yf Numeric, specifying the mean at the last time point
#' @param sd_yf Numeric, specifying the standard deviation at the last time point
#' @param mu Numeric, specifying the mean autocorrelation
#' @param s Numeric, specifying the standard deviation of autocorrelations
#' @param mu_j Numeric, specifying the mean of thejiggle
#' @param s_j Numeric, specifying the standard deviation of the jiggle
#' @param tm Numeric, specifying the time point separating the first and second piece of the change 
#' @param mean_ym Numeric, specifying the mean at time point "tm"
#' @param sd_ym Numeric, specifying the standard deviation at time point "tm"
#' @param jiggle Logical, specifying whether to add jiggle (TRUE) or not (FALSE)
#' @param na_pct Numeric, specifying percentage of missing data from 0 (no missing data) to 1 (all values are missing).
#' @param sim_method String, specifying the underlying trajectories for simulating data
#' @param return String, specifying whether to return a "long" (one row per time point per ID)
#' or "wide" data set
#' @param seed Numeric, specifying seed to allow replication of results
#'
#' @return
#' @export
#'
#' @examples
sim_tx <- function(n, tp,
                   mean_b0, sd_b0,
                   low_b0, up_b0,
                   mean_yf, sd_yf,
                   mu, s,
                   mu_j, s_j,
                   tm, mean_ym, sd_ym,
                   jiggle = TRUE,
                   na_pct = NULL,
                   sim_method = c("nochange", "linear", "pseudolog"),
                   return = c("long", "wide"),
                   seed = 1234) {
  
    
    # Check arguments of function
    sim_method <- match.arg(sim_method)
    return <- match.arg(return)
    
    # Set seed to allow replication of results
    set.seed(seed)
    
    # Linear Change ----
    if (sim_method == "linear") {
      
      # Set empty dataset to start
      df1 <- NULL 
      
      for (j in 1:n) {
        
        t <- NULL # t time x-axis
        y <- NULL # y score y-axis
        m <- NULL # slope
        
        # For first value and first slope
        # select random number for normal distribution with parameters b0
        b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0) 
        
        while (b0 < low_b0 | b0 > up_b0) {
          
          # Not allowing it to be lower than 15 or higher than 63
          b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
          
        }
        
        # random number for distribution of yf to create slope
        yf <- rnorm(n = 1, mean = mean_yf, sd = sd_yf) 
        
        while (yf > b0) {
          # Not allowing it to be higher than b0
          yf <- rnorm(n = 1, mean = mean_yf, sd = sd_yf)
        }
        
        # not allowing it to be lower than 0
        yf <- ifelse(yf < 0, 0, yf) 
        
        # Set slope from b0 and Yf created before
        m1 <- (yf - b0) / tp  
        
        # Define first data points
        t[1] <- 1
        y[1] <- b0
        m[1] <- m1
        t[2] <- 2
        y[2] <- m1 + b0
        m[2] <- m1
        
        # Loop for timepoint 3 to 20
        for (i in 3:tp) {
          t[i] = i
          
          # Add variation to slope so it is not the same for all 20 datapoints
          m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s) 
          
          while (m[i] > 0) {
            
            # Not allowing slope to be positive
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
            
          }
          
          # calculate new y
          y[i] <- m[i] + y[i - 1]  
          
          # calculate last value for that slope to see if it falls in distribution for yf
          yt <- m[i] * (tp - (i - 1)) + y[i - 1] 
          
          while (yt < mean_yf - 1.5 * sd_yf |
                 yt > mean_yf + 1.5 * sd_yf |
                 yt < 0) {
            
            # make sure yf fall in yf distribution
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
            
            while (m[i] > 0) {
              
              m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
              
            }
            
            y[i] <- m[i] + y[i - 1]
            
            yt <- m[i] * (tp - (i - 1)) + y[i - 1]
            
          }
          
        }
        
        # dataset with values creates for each id
        ind <- as.data.frame(cbind(id = j, t, y)) 
        
        # Add jiggle
        for (k in 1:nrow(ind)) {
          
          ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
          
          while (ind$new_y[k] < 0) {
            
            ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
            
          }
        }
        
        while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
          
          ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
          
        }
        
        # Put all ids together
        df1 <- rbind(df1, ind) 
        
        # clean for next loop
        # TODO 2021-04-10 MW: Check if this is needed
        rm(b0, yf, m1, t, m, y, yt, ind) 
        
      }
      
      df_long <- tibble::as_tibble(df1)

    }
    
    if (sim_method == "nochange") {
      
      df2 <- NULL  #Set empty dataset to start
      
      for (j in 1:n) {
        t <- NULL
        y <- NULL
        m <- NULL
        
        # For first value
        # select random number for normal distribution with parameters b0
        b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0) 
        while (b0 < low_b0 | b0 > up_b0) {
          # Not allowing it to be lower than 15 or higher than 63
          b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
        }
        
        # random number for distribution of b0 to create slope
        yf <- rnorm(n = 1, mean = mean_b0, sd = sd_b0) 
        # not allowing it to be lower than 0
        yf <- ifelse(yf < 0, 0, yf) 
        # Set slope from b0 and Yf created before
        m1 <- (yf - b0) / tp  
        
        # Define first data points
        t[1] = 1
        y[1] = b0
        m[1] = m1
        t[2] = 2
        y[2] = m1 + b0
        m[2] = m1
        
        # Loop for timepoint 3 to 20
        for (i in 3:tp) {
          t[i] = i
          # Add variation to slope so it is not the same for all 20 datapoints
          m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
          # calculate new y
          y[i] <- m[i] + y[i - 1] 
          # calculate last value for that slope to see if it falls in distribution for b0
          yt <- m[i] * (tp - (i - 1)) + y[i - 1] 
          
          while (yt < b0 - 1.5 * sd_b0 | yt > b0 + 1.5 * sd_b0 | yt < 0) {
            # make sure yf fall in b0 distribution
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
            y[i] <- m[i] + y[i - 1]
            yt <- m[i] * (tp - (i - 1)) + y[i - 1]
          }
          
        }
        # dataset with values creates for each id
        ind <- as.data.frame(cbind(id = j, t, y))  
        
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
        
        # Put all ids together
        df2 <- rbind(df2, ind) 
        
        # clean for next loop
        rm(b0, yf, m1, t, m, y, yt, ind) 
      }
      
      df_long <- tibble::as_tibble(df2)
      
    }
    
    if (sim_method == "pseudolog") {
      
      # Set empty dataset to start
      df3 <- NULL 
      
      for (j in 1:n) {
        t <- NULL
        y <- NULL
        m <- NULL
        
        # For first value
        # select random number for normal distribution with parameters b0
        b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0) 
        while (b0 < low_b0 | b0 > up_b0) {
          # Not allowing it to be lower than 15 or higher than 63
          b0 <- rnorm(n = 1, mean = mean_b0, sd = sd_b0)
        }
        # random number for distribution of ym to create slope
        ym <- rnorm(n = 1, mean = mean_ym, sd = sd_ym) 
        while (ym > b0) {
          # Not allowing it to be higher than b0
          ym <- rnorm(n = 1, mean = mean_ym, sd = sd_ym)
        }
        
        # not allowing it to be lower than 0
        ym <- ifelse(ym < 0, 0, ym)   
        
        # Set slope from b0 and Ym created before
        m1 <- (ym - b0) / tm  
        
        # Define first data points
        t[1] = 1
        y[1] = b0
        m[1] = m1
        t[2] = 2
        y[2] = m1 + b0
        m[2] = m1
        
        # Loop for timepoint 3 to 8 (middle point)
        for (i in 3:tm) {
          t[i] = i
          # Add variation to slope so it is not the same for all 20 datapoints
          m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s) 
          while (m[i] > 0) {
            # Not allowing slope to be positive
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
          }
          # calculate new y
          y[i] <- m[i] + y[i - 1]
          ym <-
            m[i] * (tm - (i - 1)) + y[i - 1] #calculate last value for that slope to see if it falls in distribution for ym
          
          while (ym < mean_ym - 1.5 * sd_ym | ym > mean_ym + 1.5 * sd_ym | ym < 0) {
            # make sure ym fall in ym distribution
            m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
            
            while (m[i] > 0) {
              #Not allowing slope to be positive
              m[i] <- m[i - 1] + rnorm(1, mean = mu, sd = s)
              }
            y[i] <- m[i] + y[i - 1]
            ym <- m[i] * (tm - (i - 1)) + y[i - 1]
          }
          }
        
        # Redefine slope
        # NOTE: SEE IF MODIFY BACK SD TO sd_yf - New last point to redefine slope
        yf <- rnorm(n = 1, mean = mean_yf, sd = sd_ym) 
        
        # not allowing it to be lower than 0
        yf <- ifelse(yf < 0, 0, yf) 
        
        # Set slope from ym and yf created before
        mm <- (yf - y[tm]) / (tp - tm)  
        
        #Loop for timepoint 9 to 20
        for (i in (tm + 1):tp) {
          t[i] = i
          # Add variation to slope so it is not the same for all 20 datapoints
          m[i] <- mm + rnorm(1, mean = mu, sd = s) 
          # calculate new y
          y[i] <- m[i] + y[i - 1]  
          # calculate last value for that slope to see if it falls in distribution for yf
          yt <- m[i] * (tp - (i - 1)) + y[i - 1]  
          
          while (yt < mean_yf - 1.5 * sd_ym | yt > mean_yf + 1.5 * sd_ym | yt < 0) {
            # NOTE: SEE IF MODIFY BACK SD TO sd_yf - make sure it falls in distribution for yf
            m[i] <- mm + rnorm(1, mean = mu, sd = s)
            y[i] <- m[i] + y[i - 1]
            yt <- m[i] * (tp - (i - 1)) + y[i - 1]
          }
        }
        
        # dataset with values creates for each id
        ind <- as.data.frame(cbind(id = j, t, y)) 
        
        # Add jiggle
        for (k in 1:nrow(ind)) {
          ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
          while (ind$new_y[k] < 0) {
            ind$new_y[k] <- ind$y[k] + rnorm(1, mean = mu_j, sd = s_j)
          }
        }
        while (ind$new_y[1] < low_b0 | ind$new_y[1] > up_b0) {
          ind$new_y[1] <- ind$y[1] + rnorm(1, mean = mu_j, sd = s_j)
        }
        
        # Put all ids together
        df3 <- rbind(df3, ind) 
        
        # clean for next loop
        rm(b0, ym, m1, t, m, y, yt, ind, yf, mm) 
      }
      
      df_long <- tibble::as_tibble(df3)
      
    }
    
    # Select jiggle ----
    if (jiggle == TRUE) {
      df_long <- dplyr::select(df_long, id, time = t, value = new_y)
    } else if (jiggle == FALSE) {
      df_long <- dplyr::select(df_long, id, time = t, value = y)
    }
    
    # Add missing data ----
    if (!is.null(na_pct)) {
      
      df_long <- dplyr::mutate(df_long,
                               random_num = runif(nrow(df_long)),
                               value = base::ifelse(random_num <= na_pct, NA, value)) 
      
      df_long <- select(df_long, -random_num)
    }

    
    # Return data ----
    if (return == "long") {
      
      return(df_long)
      
    } else if (return == "wide") {
      
      df_wide <- tidyr::pivot_wider(data = df_long,
                                    names_from = time,
                                    names_prefix = "t_",
                                    values_from = value)
      
      return(df_wide)
    }
  }
