source("load_data.R")
source("preprocessing.R")
source("calculate_fill_ratio.R")
source("calculate_return_30s.R")
source("calculate_price_improve.R")
source("sequence_cancel_cost.R")
source("separate_cancel_cost.R")

weekly_result <- function(country,start_date,end_date,Firm_list_uncon,Firm_list_con,algo,ordertype){
  
  
  msgs10_21_32 <- load_data(country=country, start_date=start_date,end_date=end_date, Firm_list_uncon=Firm_list_uncon, Firm_list_con=Firm_list_con,algo=algo)
  
  #___________________________________________________________________________________________________________________________________________________
  #Preprocessing process
  #1.fill existing columns with msgs10 values: price,t_bid,t_ask,side,ordtype,orderqty,exdestination,udf3
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
  
  
  msgs10_21_32 <- msgs10_21_32 %>% preprocessing(ordertype=ordertype)
  #___________________________________________________________________________________________________________________________________________________
  
  
  
  
  #result1
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #calculate first result matrix: result1
  #1. total number of orders placed
  #2. average/median order size
  #3. average/median time spent for full-filled and partial filled orders
  #4. average/median spread
  #5. average/median spread bps
  #6. average/median asksize
  #7. average/median bidsize
  #8. average/median Formula1 = orderqty/(0.5*(t_bidsize+t_asksize))
  #9. average/median Formula2 = log(100*0.5*(t_bidsize+t_asksize)/orderqty)
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  msgs10 <- msgs10_21_32 %>% filter(msg_id == 10) %>% 
    mutate(time_spent = (as.integer(substring(time_finish,10,11))-as.integer(substring(transacttime,10,11)))*3600 + (as.integer(substring(time_finish,13,14))-as.integer(substring(transacttime,13,14)))*60 + as.integer(substring(time_finish,16,17))-as.integer(substring(transacttime,16,17)))
  
  
  msgs10 <- msgs10 %>% mutate(spread_bps = round(spread/mid*10000,1),
                              Formula1 = round(orderqty/(0.5*(t_bidsize+t_asksize)),1),
                              Formula2 = round(log(100*0.5*(t_bidsize + t_asksize)/orderqty),1))
  
  
  result1 <- msgs10 %>% group_by(time_arrive_bin) %>%
    summarise(
      numobs_placed=length(which(!is.na(clordid))),
      qty_placed=sum(orderqty,na.rm=TRUE),
      #-----------------------------------------------------------------------    
      sum_time_spent = sum(time_spent,na.rm=TRUE),
      numobs_filled = length(which(!is.na(time_spent))),
      #-----------------------------------------------------------------------    
      sum_spread = sum(spread,na.rm=TRUE),
      sum_spread_bps = sum(spread/mid*10000,na.rm=TRUE),
      #-----------------------------------------------------------------------    
      sum_bidsize = sum(t_bidsize,na.rm=TRUE),
      sum_asksize = sum(t_asksize,na.rm=TRUE),
      #------------------------------------------------------------------------
      sum_Formula1 = sum(Formula1,na.rm=TRUE),
      sum_Formula2 = sum(Formula2,na.rm=TRUE)
    )
  
  result1 <- merge(df,result1,by="time_arrive_bin",all.x=TRUE)
  result1[is.na(result1)] <- 0
  
  #median should be saved as histogram
  #********************************************************************************
  #--------------------------------------------------------------------------------
  time_spent_hist <- msgs10 %>% filter(!is.na(time_spent)) %>%
    group_by(time_arrive_bin,time_spent) %>% summarise(numobs = n())
  
  spread_hist <- msgs10 %>% filter(!is.na(spread)) %>%
    group_by(time_arrive_bin,spread) %>% summarise(numobs = n())
  
  spread_bps_hist <- msgs10 %>% filter(!is.na(spread_bps)) %>% 
    group_by(time_arrive_bin,spread_bps) %>% summarise(numobs = n())
  
  bidsize_hist <- msgs10 %>% filter(!is.na(t_bidsize)) %>% 
    group_by(time_arrive_bin,t_bidsize) %>% summarise(numobs = n())
  
  asksize_hist <- msgs10 %>% filter(!is.na(t_asksize)) %>%
    group_by(time_arrive_bin,t_asksize) %>% summarise(numobs = n())
  
  orderqty_hist <- msgs10 %>% filter(!is.na(orderqty)) %>%
    group_by(time_arrive_bin,orderqty) %>% summarise(numobs = n())
  
  Formula1_hist <- msgs10 %>% filter(!is.na(Formula1)) %>% 
    group_by(time_arrive_bin,Formula1) %>% summarise(numobs = n())
  
  Formula2_hist <- msgs10 %>% filter(!is.na(Formula2)) %>%
    group_by(time_arrive_bin,Formula2) %>% summarise(numobs = n())
  
  #------------------------------------------------------------------------------------
  #************************************************************************************
  
  
  
  #result2: fill_ord_perc & fill_qty_perc
  #function calculate_fill_ratio return dataframe has 3 columns: time_arrive_bin, numobs, sum_fill_qty_perc, sum_fill_order_perc
  #________________________________________________________________________________________________________________________________
  result2 <- msgs10_21_32 %>% calculate_fill_ratio()
  #________________________________________________________________________________________________________________________________
  
  
  
  #result3: return 30s
  #result4: price improvement
  #-----------------------------------------------------------------------------------------------------------------------------------------------------
  #*****************************************************************************************************************************************************
  
  #add columns return_bps and return_spread to dataframe msgs10_21_32
  msgs10_21_32 <- msgs10_21_32 %>% calculate_return_30s()

  result3 <- msgs10_21_32 %>% filter(msg_id==21) %>% group_by(time_arrive_bin) %>% 
    summarise(numobs = length(which(!is.na(return_bps))),
              sum_return_bps = sum(return_bps,na.rm=TRUE),
              sum_return_spread = sum(return_spread, na.rm=TRUE))
  
  #add columns price_improve_bps and price_improve_spread to dataframe msgs10_21_32
  msgs10_21_32 <- msgs10_21_32 %>% calculate_price_improve(ordertype=ordertype)
  gc(verbose = FALSE)
  
  result4 <- msgs10_21_32 %>% filter(msg_id==21) %>% group_by(time_arrive_bin) %>% 
    summarise(numobs = n(),
              sum_price_improve_bps = sum(price_improve_bps,na.rm=TRUE),
              sum_price_improve_spread = sum(price_improve_spread,na.rm=TRUE))
  
  result3 <- merge(df,result3,by="time_arrive_bin", all.x=TRUE)
  result3[is.na(result3)] <- 0
  result4 <- merge(df,result4,by="time_arrive_bin", all.x=TRUE)
  result4[is.na(result4)] <- 0
  
  #******************************************************************************************************************************************************
  
  
  #result5: sequence cancel cost
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #***********************************************************************************************************************************************************************************
  
  #
  msgs10_sequence <- msgs10_21_32 %>% sequence_cancel_cost(ordertype=ordertype)
  gc(verbose=FALSE)
  
  result5 <- msgs10_sequence %>% group_by(time_arrive_bin) %>%
    summarise(numobs = n(),
              sum_cancel_cost_bps = sum(cancel_cost_bps,na.rm=TRUE),
              sum_cancel_cost_spread = sum(cancel_cost_spread,na.rm=TRUE),
              sum_num_cancel_sequence = sum(number_cancel_sequence,na.rm=TRUE))
  
  result5 <- merge(df,result5,by="time_arrive_bin",all.x=TRUE)
  result5[is.na(result5)] <- 0
  #***********************************************************************************************************************************************************************************************************************************
  
  
  #result6: separate cancel cost
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #***********************************************************************************************************************************************************************************************************************************
  
  msgs10_separate <- msgs10_21_32 %>% separate_cancel_cost(ordertype=ordertype)
  gc(verbose = FALSE)
  
  result6 <- msgs10_separate %>% group_by(time_arrive_bin) %>%
    summarise(numobs = n(),
              sum_cancel_cost_bps = sum(cancel_cost_bps,na.rm=TRUE),
              sum_cancel_cost_spread = sum(cancel_cost_spread,na.rm=TRUE))
  
  result6 <- merge(df,result6,by="time_arrive_bin",all.x=TRUE)
  result6[is.na(result6)] <- 0
  #***************************************************************************************************************************************************************************************************************************
  
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #return content:
  #result1: numobs_placed, qty_placed, sum_time_spent, numobs_filled, sum_spread, sum_spread_bps, sum_bidsize, sum_asksize, sum_Formula1, sum_Formula2
  #histogram: time_spent_hist, spread_hist, spread_bps_hist, bidsize_hist, asksize_hist, orderqty_hist, Formula1_hist, Formula2_hist
  #result2: numobs, fill_qty_perc, fill_order_perc
  #result3: numobs, return_bps, return_spread
  #result4: numobs, price_improve_bps, price_improve_spread
  #sequence cancel cost result5: numobs, cancel_cost_bps, cancel_cost_spread
  #number of separate cancel cost in cancel sequence result6: numobs, cancel_cost_bps, cancel_cost_spread
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  result1$time_arrive_bin <- NULL
  result2$time_arrive_bin <- NULL
  result3$time_arrive_bin <- NULL
  result4$time_arrive_bin <- NULL
  result5$time_arrive_bin <- NULL
  result6$time_arrive_bin <- NULL
  
  gc(verbose = FALSE)
  
  return(list(result1=result1, result2=result2, result3=result3, result4=result4, result5=result5, result6=result6,
              msgs10_21_32=msgs10_21_32, msgs10_sequence=msgs10_sequence, msgs10_separate=msgs10_separate,
              time_spent_hist=time_spent_hist, spread_hist=spread_hist, spread_bps_hist=spread_bps_hist, bidsize_hist=bidsize_hist, asksize_hist=asksize_hist, orderqty_hist=orderqty_hist, Formula1_hist=Formula1_hist, Formula2_hist=Formula2_hist))
  
}











