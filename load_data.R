#load msgs10, msgs21, msgs32 from database
#**********************************parameter********************************************
#country = "US" or "Canada"
#start_date: start date of the week
#end_date: end date of the week
#Firm_list_uncon: select certain firms' order msgs without algo condition
#Firm_list_con: select certain firms' order msgs with algo condition
#algo: certain algos to be chosen
#________________________________________________________________________________________

load_data <- function(country,start_date,end_date,Firm_list_uncon,Firm_list_con,algo){
  
  if(country=="US"){
    
    msgs10_21_32 <- order_msgs %>% 
      filter(msg_id %in% c(10,21,32)) %>%
      filter(FirmName %in% Firm_list_uncon) %>%
      filter(!(symbol %like% "/%"),!(symbol %like% "%.%")) %>% 
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid,origclordid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3) %>% collect()
    
    msgs10_21_32 <- data.frame(msgs10_21_32)
    #select the conditional msgs10_21_32
    #________________________________________________________________________________________________________________________________________________
    
    msgs10_21_32_con <- order_msgs %>%
      filter(msg_id %in% c(10,21,32)) %>%
      filter(FirmName %in% Firm_list_con) %>%
      filter(!(symbol %like% "/%"),!(symbol %like% "%.%")) %>%
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid,origclordid,masterorderid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3) %>% collect()
    
    msgs10_21_32_con <- msgs10_21_32_con %>% mutate(clordid = ifelse(msg_id==32,origclordid,clordid))
    msgs10_21_32_con <- msgs10_21_32_con %>% group_by(clordid) %>% mutate(masterorderid = ifelse(is.na(masterorderid),unique(masterorderid[which(!is.na(masterorderid))]),masterorderid))
    
    masterordid <- master_order_msgs %>%
      filter(substring(udfbag,6,5) %in% algo) %>%
      filter(FirmName %in% Firm_list_con) %>%
      filter(!(symbol %like% "/%"),!(symbol %like% "%.%")) %>%
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid) %>% collect()
    masterordid <- unique(masterordid$clordid)

    
    msgs10_21_32_con <- msgs10_21_32_con %>%
      filter(masterorderid %in% masterordid) %>%
      select(clordid,origclordid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3)
    
    msgs10_21_32_con <- data.frame(msgs10_21_32_con)

    remove(masterordid)
    #_________________________________________________________________________________________________________________________________________________
    
    if(dim(msgs10_21_32_con)[1]>0){
      msgs10_21_32 <- rbind(msgs10_21_32, msgs10_21_32_con)
    }
    
    msgs10_21_32 <- unique(msgs10_21_32)
    
    remove(msgs10_21_32_con)
    
  }
  
  if(country=="Canada"){
    
    msgs10_21_32 <- order_msgs %>% 
      filter(msg_id %in% c(10,21,32)) %>%
      filter(FirmName %in% Firm_list_uncon) %>%
      filter((symbol %like% "%.TOR") | (symbol %like% "%.TSX") | (symbol %like% "%.CAC")) %>% 
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid,origclordid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3) %>% collect()
    
    msgs10_21_32 <- data.frame(msgs10_21_32)
    #select conditional msgs10_21_32
    #_______________________________________________________________________________________________________________________________________________
    
    msgs10_21_32_con <- order_msgs %>%
      filter(msg_id %in% c(10,21,32)) %>%
      filter(FirmName %in% Firm_list_con) %>%
      filter((symbol %like% "%.TOR") | (symbol %like% "%.TSX") | (symbol %like% "%.CAC")) %>% 
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid,origclordid,masterorderid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3) %>% collect()
    
    msgs10_21_32_con <- msgs10_21_32_con %>% mutate(clordid = ifelse(msg_id==32,origclordid,clordid))
    msgs10_21_32_con <- msgs10_21_32_con %>% group_by(clordid) %>% mutate(masterorderid = ifelse(is.na(masterorderid),unique(masterorderid[which(!is.na(masterorderid))]),masterorderid))
    
    
    masterordid <- master_order_msgs %>%
      filter(substring(udfbag,6,5) %in% algo) %>%
      filter(FirmName %in% Firm_list_con) %>%
      filter((symbol %like% "%.TOR") | (symbol %like% "%.TSX") | (symbol %like% "%.CAC")) %>%
      filter(substring(transacttime,1,8) >= start_date, substring(transacttime,1,8) <= end_date) %>%
      select(clordid) %>% collect()
    masterordid <- unique(masterordid$clordid)


    msgs10_21_32_con <- msgs10_21_32_con %>%
      filter(masterorderid %in% masterordid) %>%
      select(clordid,origclordid,msg_id,FirmName,symbol,orderqty,side,ordtype,execinst,cumqty,leavesqty,lastpx,lastshares,t_ask,t_asksize,t_bid,t_bidsize,
             price,MaxFloor,exdestination,transacttime,udf3)
    
    msgs10_21_32_con <- data.frame(msgs10_21_32_con)

    remove(masterordid)
    #________________________________________________________________________________________________________________________________________________
    
    if(dim(msgs10_21_32_con)[1]>0){
      msgs10_21_32 <- rbind(msgs10_21_32, msgs10_21_32_con)
    }
    
    msgs10_21_32 <- unique(msgs10_21_32)
    
    remove(msgs10_21_32_con)
    
  }
  
  gc(reset = TRUE)
  
  return(msgs10_21_32)
  
  
}