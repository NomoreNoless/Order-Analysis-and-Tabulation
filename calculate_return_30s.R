
#***************************return************************************
#return new columns return_bps, return_spread in dataframe msgs10_21_32
#______________________________________________________________________

calculate_return_30s <- function(msgs10_21_32){
  
  msgs10_21_32 <- msgs10_21_32 %>% mutate(mid_30 = NA,
                                          spread_30 = NA,
                                          time_30 = NA)
  
  #In the previous step, we fill the msgs21's t_bid and t_ask with the values of msgs10, in this step, we should put the mid of value to NA
  msgs10_21_32 <- msgs10_21_32 %>% mutate(mid=ifelse(msg_id==10,mid,NA))
  
  #? number of look forward is 50
  for(count in 1:100){
    print(count)
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(temp_mid = lead(mid,count),
             temp_spread = lead(spread,count),
             temp_time_in_sec = lead(time_in_sec,count),
             time_30 = ifelse(is.na(mid_30),temp_time_in_sec,time_30),
             spread_30 = ifelse(is.na(mid_30),temp_spread,spread_30),
             mid_30 = ifelse(is.na(mid_30),temp_mid,mid_30))
  }
  
  #delete the support columns
  msgs10_21_32 <- msgs10_21_32 %>% mutate(temp_mid = NULL,
                                          temp_spread = NULL,
                                          temp_time_in_sec = NULL)
  
  msgs10_21_32 <- msgs10_21_32 %>% mutate(return_bps = ifelse((side==1 | side=="A"),(mid_30/lastpx-1)*10000,-(mid_30/lastpx-1)*10000),
                                          return_spread = ifelse((side==1 | side=="A"),(mid_30-lastpx)/spread_30,-(mid_30-lastpx)/spread_30),
                                          time_interval = time_30 - time_in_sec)
  
  msgs10_21_32 <- msgs10_21_32 %>% mutate(return_bps = ifelse(time_interval<=30, return_bps,NA),
                                          return_spread = ifelse(time_interval<=30, return_spread,NA))
  
  msgs10_21_32 <- msgs10_21_32 %>% mutate(time_interval = NULL)
  
  #fill back the mid price value
  msgs10_21_32 <- msgs10_21_32 %>% mutate(mid = 0.5*(t_ask+t_bid))
  
  gc(verbose = FALSE)
  
  return(msgs10_21_32)
}