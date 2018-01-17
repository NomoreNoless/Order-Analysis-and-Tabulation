#ordertype parameter
#_____________________________________
#ordertype=0: all orders
#ordertype=1: passive limit orders
#ordertype=2: limit orders
#_____________________________________


#___________________________________________________________________________________________________________________________________________________
#Preprocessing process
#1.fill existing columns with msgs10 values: price,t_bid,t_ask,side,ordtype,orderqty,exdestination,udf3,MaxFloor
#2.add columns: time_diff(control the time diff between UTC and EST), time_arrive_bin, day, time_in_sec, spread, mid
#3.filter out bad data where spread <= 0
#4.ordertype filtering
#5.find the finish_time of msgs10, if no filles then finish_time should be NA
#6.fill msgs10 cumqty with 0 and fill msgs32 cumqty with max(cumqty) of msgs21 with the same clordid
#7.fill msgs10, msgs32 lastshares with 0
#8.fill msgs10, msgs32 leavesqty = orderqty - cumqty
#___________________________________________________________________________________________________________________________________________________

#********ordertype parameter**********
#_____________________________________
#ordertype=0: all orders
#ordertype=1: passive limit orders
#ordertype=2: limit orders
#ordertype=3: peg orders
#ordertype=4: hidden orders
#_____________________________________

preprocessing <- function(msgs10_21_32, ordertype){
  
  
  #filter out bad data where clordid is NA
  msgs10_21_32 <- msgs10_21_32 %>% filter(!is.na(clordid))
  msgs10_21_32 <- msgs10_21_32 %>% mutate(clordid=ifelse(msg_id==32,origclordid,clordid))
  
  
  #step 1 
  msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% mutate(price = ifelse(length(which(!is.na(price)))>0,unique(price[which(!is.na(price))]),NA),
                                                                t_bid = ifelse(length(which(!is.na(t_bid)))>0,unique(t_bid[which(!is.na(t_bid))]),NA),
                                                                t_ask = ifelse(length(which(!is.na(t_ask)))>0,unique(t_ask[which(!is.na(t_ask))]),NA),
                                                                side = ifelse(length(which(!is.na(side)))>0,unique(side[which(!is.na(side))]),NA),
                                                                ordtype = ifelse(length(which(!is.na(ordtype)))>0, unique(ordtype[which(!is.na(ordtype))]),NA),
                                                                execinst = ifelse(length(which(!is.na(execinst)))>0, unique(execinst[which(!is.na(execinst))]),NA),
                                                                orderqty = ifelse(length(which(!is.na(orderqty)))>0, unique(orderqty[which(!is.na(orderqty))]),NA),
                                                                exdestination = ifelse(length(which(!is.na(exdestination)))>0, unique(exdestination[which(!is.na(exdestination))]),NA),
                                                                udf3 = ifelse(length(which(!is.na(udf3)))>0, unique(udf3[which(!is.na(udf3))]),NA),
                                                                MaxFloor= ifelse(length(which(!is.na(MaxFloor)))>0, unique(MaxFloor[which(!is.na(MaxFloor))]),NA),
                                                                total_number = n(),
                                                                fill_or_not = ifelse(total_number>1 & length(which(msg_id==21))>0,1,ifelse(total_number>1 & length(which(msg_id==21))==0,0,NA)))
  
  #If bad data, then fill_or_not should be NA
  msgs10_21_32 <- msgs10_21_32 %>% filter(!is.na(fill_or_not))
  
  
  #step 2 
  msgs10_21_32 <- msgs10_21_32 %>%
    mutate(time_diff = ifelse(substring(transacttime,5,8) < "1106" & substring(transacttime,5,8) > "0312", 4 ,5),
           time_arrive_bin = as.numeric(as.integer(substring(transacttime,10,11))-time_diff) + 0.01*(as.integer(as.integer(substring(transacttime,13,14))/5)*5),
           day = as.integer(substring(transacttime,5,8)),
           time_in_sec = as.numeric(as.integer(substring(transacttime,10,11))-time_diff)*3600 + as.numeric(substring(transacttime,13,14))*60 + as.numeric(substring(transacttime,16,21)),
           spread=t_ask-t_bid,
           mid = 0.5*(t_ask+t_bid))
  
  
  #step 3 
  msgs10_21_32 <- msgs10_21_32 %>% filter(spread>0)
  
  
  #********ordertype parameter**********
  #_____________________________________
  #ordertype=1: passive limit orders
  #ordertype=2: marketable limit orders
  #ordertype=3: peg orders
  #ordertype=4: hidden orders
  #_____________________________________
  #step 4
  #_________________________________________________________________________________________________________________________________________________________________________________
  if(ordertype==1){
    print(paste0("ordertype is ",as.character(ordertype)))
    print("calculating passive limit orders")
    msgs10_21_32 <- msgs10_21_32 %>% filter(((side==1 | side=="A") & price==t_bid & ordtype==2) | ((side==2 | side==5) & price==t_ask & ordtype==2))
  }
  if(ordertype==2){
    print(paste0("ordertype is ",as.character(ordertype)))
    print("calculating marketable limit orders")
    msgs10_21_32 <- msgs10_21_32 %>% (((side==1 | side=="A") & price > t_bid & ordtype==2) | ((side==2 | side==5) & price < t_ask & ordtype==2))
  }
  if(ordertype==3){
    
    print(paste0("ordertype is ",as.character(ordertype)))
    print("calculating peg orders")
    
    #peg to mid orders and T_MID orders
    msgs10_21_32 <- msgs10_21_32 %>% filter((ordtype=="P" & execinst=="M")|substring(exdestination,1,5)=="T_MID")
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% mutate(cancel_time = ifelse(length(which(msg_id==32))>0,transacttime[which(msg_id==32)],NA))
    
    #if msgs10's previous msgs is 32, then last_cancel_to_now should be the time diff between last cancel time and now
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(last_cancel_to_now = lag(time_in_sec,1),
             last_cancel_to_now = ifelse(is.na(last_cancel_to_now),0,last_cancel_to_now))
    
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10)
    
    #make the clordid unique
    msgs10 <- unique(msgs10)
    
    #if mid price is the same and place within 15s and go to the same destination ,then we assume they are the same order 
    #combined=1, then this order should be combined to next order
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(time_lag = time_in_sec - last_cancel_to_now,
             last_fill_or_not = lag(fill_or_not,1),
             #doesn't matter whether last_fill_or_not is NA, because time_lag <= 15 is always FALSE for the first order
             last_mid = lag(mid,1),
             last_exdestination = lag(exdestination,1),
             combined = ifelse(time_lag<=15 & last_mid==mid & last_exdestination==exdestination & fill_or_not==0 & last_fill_or_not==0,1,0))
    #ensure that combined is always not NA
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      mutate(next_combined = lead(combined,1),
             next_combined = ifelse(is.na(next_combined),0,next_combined),
             combined = ifelse(next_combined==1 & combined==0,1,combined))
    
    #create new column of not_consecutive and fill in NA of not_consecutive
    #_____________________________________________________________________________________________________
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(not_consecutive = ((combined != lag(combined,1)) | (mid != lag(mid,1)) | exdestination != lag(exdestination,1)))
    msgs10 <- msgs10 %>% mutate(not_consecutive=ifelse(is.na(not_consecutive) ,F,not_consecutive))
    #_____________________________________________________________________________________________________
    
    #if in the goup, alive_or_not=1, the transacttime should be minimum value of transacttime in the group
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(temp = cumsum(not_consecutive))
    
    msgs10 <- msgs10 %>% group_by(day,symbol,temp) %>%
      mutate(transacttime = ifelse(combined==1,min(transacttime,na.rm=TRUE),transacttime),
             cancel_time = ifelse(combined==1,max(cancel_time,na.rm=TRUE),cancel_time))
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(delete = ifelse(lag(temp,1)==temp & combined==1,1,0))
    msgs10 <- msgs10 %>% filter(delete==0 | is.na(delete))
    
    #______________________________________________________________________________________________________
    #if previous alive_or_not=1, fill the current transacttime with previous transacttime
    
    msgs10 <- msgs10 %>% ungroup()
    msgs10 <- msgs10 %>% mutate(time_lag = NULL,
                                last_fill_or_not = NULL,
                                last_mid = NULL,
                                last_exdestination = NULL,
                                combined = NULL,
                                next_combined = NULL,
                                not_consecutive = NULL,
                                temp = NULL,
                                delete = NULL)
    
    msgs21_32 <- msgs10_21_32 %>% filter((msg_id %in% c(21,32)) & (clordid %in% msgs10$clordid))
    
    
    msgs10 <- data.frame(msgs10)
    msgs21_32 <- data.frame(msgs21_32)
    msgs10_21_32 <- rbind(msgs10,msgs21_32)
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>%
      mutate(transacttime=ifelse(msg_id==32,cancel_time[which(msg_id==10)],transacttime))
    msgs10_21_32$cancel_time <- NULL
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% 
      mutate(AVG_PRC = ifelse(msg_id==10 & fill_or_not==1,wt.mean(lastpx[which(msg_id==21)],lastshares[which(msg_id==21)]),NA))
    
    #have changed the transacttime of some of msgs32, should recalculate the time_in_sec
    msgs10_21_32 <- msgs10_21_32 %>%
      mutate(time_diff = ifelse(substring(transacttime,5,8) < "1106" & substring(transacttime,5,8) > "0312", 4 ,5),
             time_arrive_bin = as.numeric(as.integer(substring(transacttime,10,11))-time_diff) + 0.01*(as.integer(as.integer(substring(transacttime,13,14))/5)*5),
             time_in_sec = as.numeric(as.integer(substring(transacttime,10,11))-time_diff)*3600 + as.numeric(substring(transacttime,13,14))*60 + as.numeric(substring(transacttime,16,21)))
    
  }
  
  if(ordertype==4){
    
    print(paste0("ordertype is ",as.character(ordertype)))
    print("calculating hidden orders")
    
    #hidden orders
    msgs10_21_32 <- msgs10_21_32 %>% filter(MaxFloor==0)

    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% mutate(cancel_time = ifelse(length(which(msg_id==32))>0,transacttime[which(msg_id==32)],NA))
    
    #if msgs10's previous msgs is 32, then last_cancel_to_now should be the time diff between last cancel time and now
    msgs10_21_32 <- msgs10_21_32 %>% group_by(day,symbol) %>%
      arrange(transacttime) %>%
      mutate(last_cancel_to_now = lag(time_in_sec,1),
             last_cancel_to_now = ifelse(is.na(last_cancel_to_now),0,last_cancel_to_now))
    
    
    msgs10 <- msgs10_21_32 %>% filter(msg_id==10)
    
    #make the clordid unique
    msgs10 <- unique(msgs10)
    
    #if mid price is the same and place within 15s and go to the same destination ,then we assume they are the same order 
    #combined=1, then this order should be combined to next order
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(time_lag = time_in_sec - last_cancel_to_now,
             last_fill_or_not = lag(fill_or_not,1),
             #doesn't matter whether last_fill_or_not is NA, because time_lag <= 15 is always FALSE for the first order
             last_mid = lag(mid,1),
             last_exdestination = lag(exdestination,1),
             combined = ifelse(time_lag<=15 & last_mid==mid & last_exdestination==exdestination & fill_or_not==0 & last_fill_or_not==0,1,0))
             #ensure that combined is always not NA
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>%
      mutate(next_combined = lead(combined,1),
             next_combined = ifelse(is.na(next_combined),0,next_combined),
             combined = ifelse(next_combined==1 & combined==0,1,combined))

    #create new column of not_consecutive and fill in NA of not_consecutive
    #_____________________________________________________________________________________________________
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(not_consecutive = ((combined != lag(combined,1)) | (mid != lag(mid,1)) | exdestination != lag(exdestination,1)))
    msgs10 <- msgs10 %>% mutate(not_consecutive=ifelse(is.na(not_consecutive) ,F,not_consecutive))
    #_____________________________________________________________________________________________________
    
    #if in the goup, alive_or_not=1, the transacttime should be minimum value of transacttime in the group
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(temp = cumsum(not_consecutive))
    
    msgs10 <- msgs10 %>% group_by(day,symbol,temp) %>%
      mutate(transacttime = ifelse(combined==1,min(transacttime,na.rm=TRUE),transacttime),
             cancel_time = ifelse(combined==1,max(cancel_time,na.rm=TRUE),cancel_time))
    
    msgs10 <- msgs10 %>% group_by(day,symbol) %>% arrange(transacttime) %>%
      mutate(delete = ifelse(lag(temp,1)==temp & combined==1,1,0))
    msgs10 <- msgs10 %>% filter(delete==0 | is.na(delete))
    
    #______________________________________________________________________________________________________
    #if previous alive_or_not=1, fill the current transacttime with previous transacttime
    
    msgs10 <- msgs10 %>% ungroup()
    msgs10 <- msgs10 %>% mutate(time_lag = NULL,
                                last_fill_or_not = NULL,
                                last_mid = NULL,
                                last_exdestination = NULL,
                                combined = NULL,
                                next_combined = NULL,
                                not_consecutive = NULL,
                                temp = NULL,
                                delete = NULL)
    
    msgs21_32 <- msgs10_21_32 %>% filter((msg_id %in% c(21,32)) & (clordid %in% msgs10$clordid))
    
    
    msgs10 <- data.frame(msgs10)
    msgs21_32 <- data.frame(msgs21_32)
    msgs10_21_32 <- rbind(msgs10,msgs21_32)
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>%
      mutate(transacttime=ifelse(msg_id==32,cancel_time[which(msg_id==10)],transacttime))
    msgs10_21_32$cancel_time <- NULL
    
    msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% 
      mutate(AVG_PRC = ifelse(msg_id==10 & fill_or_not==1,wt.mean(lastpx[which(msg_id==21)],lastshares[which(msg_id==21)]),NA))
    
    #have changed the transacttime of some of msgs32, should recalculate the time_in_sec
    msgs10_21_32 <- msgs10_21_32 %>%
      mutate(time_diff = ifelse(substring(transacttime,5,8) < "1106" & substring(transacttime,5,8) > "0312", 4 ,5),
             time_arrive_bin = as.numeric(as.integer(substring(transacttime,10,11))-time_diff) + 0.01*(as.integer(as.integer(substring(transacttime,13,14))/5)*5),
             time_in_sec = as.numeric(as.integer(substring(transacttime,10,11))-time_diff)*3600 + as.numeric(substring(transacttime,13,14))*60 + as.numeric(substring(transacttime,16,21)))
    
  }
  #_________________________________________________________________________________________________________________________________________________________________________________

  #step 5
  msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>%
    arrange(transacttime) %>%
    mutate(time_finish = ifelse(msg_id==10 & length(which(msg_id==21))>0,max(transacttime[which(msg_id==21)],na.rm=TRUE),NA))
  
  msgs10_21_32 <- msgs10_21_32 %>% mutate(time_finish = ifelse(time_finish<transacttime,NA,time_finish))
  
  #step 6
  msgs10_21_32 <- msgs10_21_32 %>% mutate(cumqty = ifelse(msg_id==10,0,cumqty))
  msgs10_21_32 <- msgs10_21_32 %>% group_by(clordid) %>% 
    arrange(transacttime) %>%
    mutate(cumqty = ifelse(is.na(cumqty),max(cumqty,na.rm=TRUE),cumqty))
  
  #step 7
  msgs10_21_32 <- msgs10_21_32 %>% mutate(lastshares = ifelse(is.na(lastshares),0,lastshares))
  
  #step 8
  msgs10_21_32 <- msgs10_21_32 %>% mutate(leavesqty = ifelse(is.na(leavesqty),orderqty-cumqty,leavesqty))
  
  gc(verbose = FALSE)
  
  return(msgs10_21_32)
  
  
}