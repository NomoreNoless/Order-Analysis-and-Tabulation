separate_cancel_cost <- function(msgs10_21_32,ordertype){
  
  
  if(ordertype==1){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    #time_interval: time difference between sequential cancel32 and new order10
    
    #select all relevant msgs10
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,side,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,t_bid,t_ask)
    
    
    #1. if a cancelled order's next order is cancelled, then assume the current cancelled order is filled at the far price of the current order
    #2. if a cancelled order's next order is filled and the filled order is placed within 30s of the cancelled order, then the current cancelled order is filled at the next filled price
    #3. if a cancelled order's next order is filled and the filled order is placed after 30s of the cancelled order, then assume the next the order is cancelled and use the case 1
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      mutate(post_fill = lead(fill_or_not,1),
      post_time_interval = lead(time_interval,1),
      post_fill = ifelse(post_fill==1 & post_time_interval>30,0,post_fill),
      end_fill = ifelse(post_fill==1,lead(price,1),NA),
      end_fill = ifelse(post_fill==0 & (side==1 | side=="A"),t_ask,end_fill),
      end_fill = ifelse(post_fill==0 & (side==2 | side==5), t_bid,end_fill),
      cancel_cost_bps = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/mid*10000,NA),
      cancel_cost_bps = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/mid*10000, cancel_cost_bps),
      cancel_cost_spread = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/spread,NA),
      cancel_cost_spread = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/spread, cancel_cost_spread))
    
    msgs10_separate <- msgs10 %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    
  }
  
  if(ordertype==3){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    #time_interval: time difference between sequential cancel32 and new order10
    
    #select all relevant msgs10
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,transacttime,side,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,t_bid,t_ask,AVG_PRC)
    
    
    #1. if a cancelled order's next order is cancelled, then assume the current cancelled order is filled at the far price of the current order
    #2. if a cancelled order's next order is filled and the filled order is placed within 30s of the cancelled order, then the current cancelled order is filled at the next filled price
    #3. if a cancelled order's next order is filled and the filled order is placed after 30s of the cancelled order, then assume the next the order is cancelled and use the case 1
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(post_fill = lead(fill_or_not,1),
             post_time_interval = lead(time_interval,1),
             post_fill = ifelse(post_fill==1 & post_time_interval>30,0,post_fill),
             end_fill = ifelse(post_fill==1,lead(AVG_PRC,1),NA),
             end_fill = ifelse(post_fill==0,lead(mid,1), end_fill),
             cancel_cost_bps = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/mid*10000,NA),
             cancel_cost_bps = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/mid*10000, cancel_cost_bps),
             cancel_cost_spread = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/spread,NA),
             cancel_cost_spread = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/spread, cancel_cost_spread))
    
    msgs10_separate <- msgs10 %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    
  }
  
  if(ordertype==4){
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>% 
      arrange(transacttime) %>%
      mutate(before_time = lag(time_in_sec,1),
             time_interval = time_in_sec - before_time)
    #time_interval: time difference between sequential cancel32 and new order10
    
    #select all relevant msgs10
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10) %>% 
      select(clordid,day,symbol,transacttime,side,spread,fill_or_not,mid,exdestination,udf3,time_interval,time_arrive_bin,price,t_bid,t_ask,AVG_PRC)
    
    
    #1. if a cancelled order's next order is cancelled, then assume the current cancelled order is filled at the far price of the current order
    #2. if a cancelled order's next order is filled and the filled order is placed within 30s of the cancelled order, then the current cancelled order is filled at the next filled price
    #3. if a cancelled order's next order is filled and the filled order is placed after 30s of the cancelled order, then assume the next the order is cancelled and use the case 1
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      mutate(post_fill = lead(fill_or_not,1),
             post_time_interval = lead(time_interval,1),
             post_fill = ifelse(post_fill==1 & post_time_interval>30,0,post_fill), # if next fill is placed more than 30s than current cancel, then assume next is not filled
             end_fill = ifelse(post_fill==1,lead(AVG_PRC,1),NA),
             end_fill = ifelse(post_fill==0 & (side==1 | side=="A"),t_ask,end_fill),
             end_fill = ifelse(post_fill==0 & (side==2 | side==5), t_bid,end_fill),
             cancel_cost_bps = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/mid*10000,NA),
             cancel_cost_bps = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/mid*10000, cancel_cost_bps),
             cancel_cost_spread = ifelse(fill_or_not==0 & (side==1 | side=="A"), (end_fill-mid)/spread,NA),
             cancel_cost_spread = ifelse(fill_or_not==0 & (side==2 | side==5), -(end_fill-mid)/spread, cancel_cost_spread))
    
    msgs10_separate <- msgs10 %>% filter(!is.na(cancel_cost_bps) & !is.na(cancel_cost_spread))
    
  }
  
  
  gc(verbose = FALSE)
  
  return(msgs10_separate)
  
}