#return msgs10_sequence with columns cancel_cost_bps and cancel_cost_spread

sequence_cancel_cost <- function(msgs10_21_32,ordertype){
  
  if(ordertype==1){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,side,transacttime,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,t_bid,t_ask)
    
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(rownumber = row_number(transacttime),
             pre_fill = lag(fill_or_not,1),
             post_fill = lead(fill_or_not,1),
             pre_fill = ifelse(is.na(pre_fill),1,pre_fill),
             post_fill = ifelse(is.na(post_fill),1,post_fill),
             temp = 100*pre_fill + 10*fill_or_not + post_fill,
             pre_far_price = ifelse(lag(side,1)==1|lag(side,1)=="A", lag(t_ask,1), lag(t_bid,1)))
    
    
    msgs10 <- msgs10 %>% mutate(start_mid = ifelse(temp==101 | temp==100, mid,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval <= 30,price,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval > 30, pre_far_price, end_fill))
    
    
    msgs10_sequence <- msgs10 %>% filter((!is.na(start_mid)) | (!is.na(end_fill)))
    
    #
    msgs10_sequence <- msgs10_sequence %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(move_end = lead(end_fill,1),
             move_spread = lead(spread,1),
             move_row_number = lead(rownumber,1),
             cancel_cost_bps = ifelse((side==1 | side=="A"),(move_end-start_mid)/start_mid*10000, -(move_end-start_mid)/start_mid*10000),
             cancel_cost_spread = ifelse((side==1 | side=="A"), (move_end-start_mid)/move_spread, -(move_end-start_mid)/move_spread),
             cancel_cost_bps = ifelse(!is.na(cancel_cost_bps) & cancel_cost_bps>-Inf & cancel_cost_bps<Inf,cancel_cost_bps,NA),
             cancel_cost_spread = ifelse(!is.na(cancel_cost_spread) & cancel_cost_spread > -Inf & cancel_cost_spread < Inf, cancel_cost_spread,NA))
    
    
    
    msgs10_sequence <- msgs10_sequence %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    msgs10_sequence <- msgs10_sequence %>% mutate(number_cancel_sequence = move_row_number - rownumber)
    
  }
  
  if(ordertype==3){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,side,transacttime,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,AVG_PRC,t_bid,t_ask)
    
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(rownumber = row_number(transacttime),
             pre_fill = lag(fill_or_not,1),
             post_fill = lead(fill_or_not,1),
             pre_fill = ifelse(is.na(pre_fill),1,pre_fill),
             post_fill = ifelse(is.na(post_fill),1,post_fill),
             temp = 100*pre_fill + 10*fill_or_not + post_fill,
             pre_mid = lag(mid,1))
    
    
    msgs10 <- msgs10 %>% mutate(start_mid = ifelse(temp==101 | temp==100, mid,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval <= 30,AVG_PRC,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval > 30, pre_mid, end_fill))
    
    
    msgs10_sequence <- msgs10 %>% filter((!is.na(start_mid)) | (!is.na(end_fill)))
    
    msgs10_sequence <- msgs10_sequence %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(move_end = lead(end_fill,1),
             move_spread = lead(spread,1),
             move_row_number = lead(rownumber,1),
             cancel_cost_bps = ifelse((side==1 | side=="A"),(move_end-start_mid)/start_mid*10000, -(move_end-start_mid)/start_mid*10000),
             cancel_cost_spread = ifelse((side==1 | side=="A"), (move_end-start_mid)/move_spread, -(move_end-start_mid)/move_spread),
             cancel_cost_bps = ifelse(!is.na(cancel_cost_bps) & cancel_cost_bps>-Inf & cancel_cost_bps<Inf,cancel_cost_bps,NA),
             cancel_cost_spread = ifelse(!is.na(cancel_cost_spread) & cancel_cost_spread > -Inf & cancel_cost_spread < Inf, cancel_cost_spread,NA))
    
    
    
    msgs10_sequence <- msgs10_sequence %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    msgs10_sequence <- msgs10_sequence %>% mutate(number_cancel_sequence = move_row_number - rownumber)
  }
  
  if(ordertype==4){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,side,transacttime,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,AVG_PRC,t_bid,t_ask)
    
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(rownumber = row_number(transacttime),
             pre_fill = lag(fill_or_not,1),
             post_fill = lead(fill_or_not,1),
             pre_fill = ifelse(is.na(pre_fill),1,pre_fill),
             post_fill = ifelse(is.na(post_fill),1,post_fill),
             temp = 100*pre_fill + 10*fill_or_not + post_fill,
             pre_far_price = ifelse(lag(side,1)==1|lag(side,1)=="A", lag(t_ask,1), lag(t_bid,1)))
    
    
    msgs10 <- msgs10 %>% mutate(start_mid = ifelse(temp==101 | temp==100, mid,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval <= 30,AVG_PRC,NA),
                                end_fill = ifelse(temp %in% c(11,10) & time_interval > 30, pre_far_price, end_fill))
    
    
    msgs10_sequence <- msgs10 %>% filter((!is.na(start_mid)) | (!is.na(end_fill)))
    
    #
    msgs10_sequence <- msgs10_sequence %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(move_end = lead(end_fill,1),
             move_spread = lead(spread,1),
             move_row_number = lead(rownumber,1),
             cancel_cost_bps = ifelse((side==1 | side=="A"),(move_end-start_mid)/start_mid*10000, -(move_end-start_mid)/start_mid*10000),
             cancel_cost_spread = ifelse((side==1 | side=="A"), (move_end-start_mid)/move_spread, -(move_end-start_mid)/move_spread),
             cancel_cost_bps = ifelse(!is.na(cancel_cost_bps) & cancel_cost_bps>-Inf & cancel_cost_bps<Inf,cancel_cost_bps,NA),
             cancel_cost_spread = ifelse(!is.na(cancel_cost_spread) & cancel_cost_spread > -Inf & cancel_cost_spread < Inf, cancel_cost_spread,NA))
    
    
    
    msgs10_sequence <- msgs10_sequence %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    msgs10_sequence <- msgs10_sequence %>% mutate(number_cancel_sequence = move_row_number - rownumber)
    
  }
  
  gc(verbose = FALSE)
  
  return(msgs10_sequence)
  
}