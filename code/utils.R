
#' Load EPW climate file.
load_epw <- function(file){
    # load data from CSV file
  dataraw <- read.csv(file, header = TRUE, skip = 18, 
                      stringsAsFactors = FALSE)
  # set all the years to 2000
  substring(dataraw$Date, 1) <- '2000'
  
  # paste together the components of the time
  dataraw <- mutate(dataraw, time.local = paste(Date, HH.MM)) %>%
    mutate(time.local = 
             as.POSIXct(strptime(time.local, format = '%Y/%m/%d %H:%M'))) %>%
    # select and rename useful columns
    select(-Date, -HH.MM)
  
  dataraw
}

#' Adds time information in columns.
create_time_cols <- function(tse) {
  tse$time.hour <- hour(tse$time.local)
  tse$time.day <- as.POSIXct(trunc(tse$time.local, 'day'))
  tse$time.wday <- wday(tse$time.local)
  tse$time.mday <- mday(tse$time.local)
  tse$time.month <- month(tse$time.local)
  tse$time.year <- year(tse$time.local)
  
  return(tse)
}

#' Smoothing the data in a criminal way.
#' @return Vector with the smoothed data.
#' @export
smooth_criminal <- function(data) {
  ## First step: smooth time-wise
  na_flag <- is.na(data$temp)
  data$temp_s <- NA
  data$temp_s[!na_flag] <- c(smooth(data$temp[!na_flag]))
  
  ## Second step: smooth the profiles for each hour
  # daily profiles table
  r <- melt(anadata_e, id.vars = c('time.hour', 'time.day'),
            measure.vars = 'temp') %>% 
    acast(time.hour ~ time.day)
  
  # for each row
  for(i in 1:dim(r)[1]){
    na_flag <- is.na(r[i, ])
    r[i, !na_flag] <- smooth(r[i, !na_flag]) # smooth
  }
  
  # put back into long format
  smooth_d <- melt(r) %>%
    mutate(time.day = (as.character(Var2) %>%
                         strptime(format = '%Y-%m-%d') %>%
                         as.POSIXct),
           time.hour = Var1,
           temp = value) %>%
    arrange(time.day) # order by time
  
  smooth_d$temp
}

#' Quite smooth.
#' @export
smooth_like_butter <- function(data) {
  data <- filter(data, !is.na(temp)) %>%
    mutate(time.day = as.double(time.day))
  
  # train a model
  mod <- gam(temp ~ te(time.day, time.hour, k = 20), 
             data = data)
  res <- rep(NA, dim(data)[1])
  res[!is.na(data$temp)] <- fitted(mod)
}