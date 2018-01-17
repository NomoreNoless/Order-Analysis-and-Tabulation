library(dplyr)
library(odbc)
library(stringr)
library(xlsx)
source("weekly_result.R")

#*************************************************parameters*************************************************************************
#country: "Canada", "US"
#start_dates: start dates of every week, for example: c("20171127","20171204","20171211","20171218")
#end_dates: end dates of every week, for example: c("20171201","20171208","20171215","20171222")
#Firm_list_uncon: Firm list that don't have algo conditions
#Firm_list_con: Firm list that have algo conditions
#algo: specific algo to be chosen for Firm_list_con
#ordertype: ordertype=0: all orders
#           ordertype=1: passive limit orders
#           ordertype=2: limit orders
#           ordertype=3: peg orders
#           ordertype=4: hidden orders
#************************************************************************************************************************************

#*************************************************output*****************************************************************************
#output file :US_result.csv  or
#             Ca_result.csv depends on the parameter input
#************************************************************************************************************************************

order_analysis <- function(country,start_dates,end_dates,Firm_list_uncon,Firm_list_con,algo,ordertype){
  
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #return content:
  #result1: numobs_placed, qty_placed, sum_time_spent, numobs_filled, sum_spread, sum_spread_bps, sum_bidsize, sum_asksize, sum_Formula1, sum_Formula2
  #histogram: time_spent_hist, spread_hist, spread_bps_hist, bidsize_hist, asksize_hist, orderqty_hist, Formula1_hist, Formula2_hist
  #result2: numobs, fill_qty_perc, fill_order_perc
  #result3: numobs, return_bps, return_spread
  #result4: numobs, price_improve_bps, price_improve_spread
  #sequence cancel cost result5: numobs, cancel_cost_bps, cancel_cost_spread
  #separate cancel cost result6: numobs, cancel_cost_bps, cancel_cost_spread
  
  #time_spent_hist: time_arrive_bin, time_spent, numobs
  #spread_hist: time_arrive_bin, spread, numobs
  #spread_bps_hist: time_arrive_bin, spread_bps, numobs
  #bidsize_hist: time_arrive_bin, t_bidsize, numobs
  #asksize_hist: time_arrive_bin, t_asksize, numobs
  #orderqty_hist: time_arrive_bin, orderqty, numobs
  #Formula1_hist: time_arrive_bin, Formula1, numobs
  #Formula2_hist: time_arrive_bin, Formula2, numobs
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  num_weeks <- length(start_dates)
  
  for (i in 1:num_weeks){
    
    print(i)
    result <- weekly_result(country=country,start_date=start_dates[i],end_date=end_dates[i],Firm_list_uncon = Firm_list_uncon,Firm_list_con=Firm_list_con,algo = algo,ordertype = ordertype)
    gc(verbose = FALSE)
    
    if(i==1){
      
      result1 <- result$result1
      result2 <- result$result2
      result3 <- result$result3
      result4 <- result$result4
      result5 <- result$result5
      result6 <- result$result6
      
      time_spent_hist <- result$time_spent_hist
      spread_hist <- result$spread_hist
      spread_bps_hist <- result$spread_bps_hist
      bidsize_hist <- result$bidsize_hist
      asksize_hist <- result$asksize_hist
      orderqty_hist <- result$orderqty_hist
      Formula1_hist <- result$Formula1_hist
      Formula2_hist <- result$Formula2_hist
      
    }
    else{
      
      result1 <- result1 + result$result1
      result2 <- result2 + result$result2
      result3 <- result3 + result$result3
      result4 <- result4 + result$result4
      result5 <- result5 + result$result5
      result6 <- result6 + result$result6
      
      time_spent_hist <- rbind(time_spent_hist,result$time_spent_hist)
      time_spent_hist <- time_spent_hist %>% group_by(time_arrive_bin, time_spent) %>%
        summarise(numobs = sum(numobs,na.rm=TRUE))
      
      spread_hist <- rbind(spread_hist, result$spread_hist)
      spread_hist <- spread_hist %>% group_by(time_arrive_bin, spread) %>% 
        summarise(numobs = sum(numobs, na.rm=TRUE))
      
      spread_bps_hist <- rbind(spread_bps_hist, result$spread_bps_hist)
      spread_bps_hist <- spread_bps_hist %>% group_by(time_arrive_bin, spread_bps) %>%
        summarise(numobs = sum(numobs,na.rm=TRUE))
      
      bidsize_hist <- rbind(bidsize_hist, result$bidsize_hist)
      bidsize_hist <- bidsize_hist %>% group_by(time_arrive_bin, t_bidsize) %>%
        summarise(numobs = sum(numobs, na.rm=TRUE))
      
      asksize_hist <- rbind(asksize_hist, result$asksize_hist)
      asksize_hist <- asksize_hist %>% group_by(time_arrive_bin, t_asksize) %>%
        summarise(numobs = sum(numobs, na.rm=TRUE))
      
      orderqty_hist <- rbind(orderqty_hist, result$orderqty_hist)
      orderqty_hist <- orderqty_hist %>% group_by(time_arrive_bin, orderqty) %>% 
        summarise(numobs = sum(numobs, na.rm=TRUE))
      
      Formula1_hist <- rbind(Formula1_hist,result$Formula1_hist)
      Formula1_hist <- Formula1_hist %>% group_by(time_arrive_bin, Formula1) %>% 
        summarise(numobs = sum(numobs,na.rm=TRUE))
      
      Formula2_hist <- rbind(Formula2_hist,result$Formula2_hist)
      Formula2_hist <- Formula2_hist %>% group_by(time_arrive_bin, Formula2) %>%
        summarise(numobs = sum(numobs,na.rm=TRUE))
      
    }
    
  }
  
  #-------------------------------------------------------------------------------------------------
  time_spent_hist <- time_spent_hist %>% group_by(time_arrive_bin) %>% arrange(time_spent) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_time_spent <- time_spent_hist %>% group_by(time_arrive_bin) %>% 
    summarise(total = sum(numobs),
              median_time_spent = head(time_spent[which(cumsum_obs>total/2)],1))
  
  median_time_spent$total <- NULL
  median_time_spent <- merge(df,median_time_spent,by="time_arrive_bin",all.x=TRUE)
  #--------------------------------------------------------------------------------------------------
  spread_hist <- spread_hist %>% group_by(time_arrive_bin) %>% arrange(spread) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_spread <- spread_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_spread = head(spread[which(cumsum_obs>total/2)],1))
  
  median_spread$total <- NULL
  median_spread <- merge(df,median_spread,by="time_arrive_bin",all.x=TRUE)
  #---------------------------------------------------------------------------------------------------
  spread_bps_hist <- spread_bps_hist %>% group_by(time_arrive_bin) %>% arrange(spread_bps) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_spread_bps <- spread_bps_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_spread_bps = head(spread_bps[which(cumsum_obs>total/2)],1))
  
  median_spread_bps$total <- NULL
  median_spread_bps <- merge(df,median_spread_bps,by="time_arrive_bin",all.x=TRUE)
  #---------------------------------------------------------------------------------------------------
  bidsize_hist <- bidsize_hist %>% group_by(time_arrive_bin) %>% arrange(t_bidsize) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_bidsize <- bidsize_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_bidsize = head(t_bidsize[which(cumsum_obs>total/2)],1))
  
  median_bidsize$total <- NULL
  median_bidsize <- merge(df,median_bidsize,by="time_arrive_bin",all.x=TRUE)
  #----------------------------------------------------------------------------------------------------
  asksize_hist <- asksize_hist %>% group_by(time_arrive_bin) %>% arrange(t_asksize) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_asksize <- asksize_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_asksize = head(t_asksize[which(cumsum_obs>total/2)],1))
  
  median_asksize$total <- NULL
  median_asksize <- merge(df,median_asksize,by="time_arrive_bin",all.x=TRUE)
  #----------------------------------------------------------------------------------------------------
  orderqty_hist <- orderqty_hist %>% group_by(time_arrive_bin) %>% arrange(orderqty) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_orderqty <- orderqty_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_orderqty = head(orderqty[which(cumsum_obs>total/2)],1))
  
  median_orderqty$total <- NULL
  median_orderqty <- merge(df,median_orderqty,by="time_arrive_bin",all.x=TRUE)
  #----------------------------------------------------------------------------------------------------
  Formula1_hist <- Formula1_hist %>% group_by(time_arrive_bin) %>% arrange(Formula1) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_Formula1 <- Formula1_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_Formula1 = head(Formula1[which(cumsum_obs>total/2)],1))
  
  median_Formula1$total <- NULL
  median_Formula1 <- merge(df,median_Formula1,by="time_arrive_bin",all.x=TRUE)
  #----------------------------------------------------------------------------------------------------
  Formula2_hist <- Formula2_hist %>% group_by(time_arrive_bin) %>% arrange(Formula2) %>%
    mutate(cumsum_obs = cumsum(numobs))
  
  median_Formula2 <- Formula2_hist %>% group_by(time_arrive_bin) %>%
    summarise(total = sum(numobs),
              median_Formula2 = head(Formula2[which(cumsum_obs>total/2)],1))
  
  median_Formula2$total <- NULL
  median_Formula2 <- merge(df,median_Formula2,by="time_arrive_bin",all.x=TRUE)
  #-----------------------------------------------------------------------------------------------------
  
  
  #combine the result together
  #**********************************************************************************************************************
  result_matrix <- data.frame(
    time_arrive_bin = df$time_arrive_bin
  )
  
  result_matrix$num_placed <- result1$numobs_placed
  result_matrix$fill_order_perc <- result2$sum_fill_order_perc/result2$numobs
  result_matrix$quantity_placed <- result1$qty_placed
  result_matrix$fill_qty_perc <- result2$sum_fill_qty_perc/result2$numobs
  result_matrix$average_time <- result1$sum_time_spent/result1$numobs_filled
  result_matrix$median_time <- median_time_spent$median_time_spent
  result_matrix$median_spread <- median_spread$median_spread
  result_matrix$median_spread_bps <- median_spread_bps$median_spread_bps
  result_matrix$average_spread <- result1$sum_spread/result1$numobs_placed
  result_matrix$average_spread_bps <- result1$sum_spread_bps/result1$numobs_placed
  result_matrix$average_bidsize <- result1$sum_bidsize/result1$numobs_placed
  result_matrix$median_bidsize <- median_bidsize$median_bidsize
  result_matrix$average_asksize <- result1$sum_asksize/result1$numobs_placed
  result_matrix$median_asksize <- median_asksize$median_asksize
  result_matrix$average_ordersize <- result1$qty_placed/result1$numobs_placed
  result_matrix$median_ordersize <- median_orderqty$median_orderqty
  result_matrix$average_Formula1 <- result1$sum_Formula1/result1$numobs_placed
  result_matrix$median_Formula1 <- median_Formula1$median_Formula1
  result_matrix$average_Formula2 <- result1$sum_Formula2/result1$numobs_placed
  result_matrix$median_Formula2 <- median_Formula2$median_Formula2
  result_matrix$return_bps <- result3$sum_return_bps/result3$numobs
  result_matrix$return_spread <- result3$sum_return_spread/result3$numobs
  result_matrix$price_improve_bps <- result4$sum_price_improve_bps/result4$numobs
  result_matrix$price_improve_spread <- result4$sum_price_improve_spread/result4$numobs
  result_matrix$cancel_cost_bps <- result5$sum_cancel_cost_bps/result5$numobs
  result_matrix$cancel_cost_spread <- result5$sum_cancel_cost_spread/result5$numobs
  result_matrix$num_cancellation <- result5$sum_num_cancel_sequence/result5$numobs
  result_matrix$cancel_cost_bps_sep <- result6$sum_cancel_cost_bps/result6$numobs
  result_matrix$cancel_cost_spread_sep <- result6$sum_cancel_cost_spread/result6$numobs
  
  if(country=="US"){
    write.csv(result_matrix, file="C:\\Users\\xueb\\Desktop\\passive_template\\US_result.csv")
  }
  if(country=="Canada"){
    write.csv(result_matrix, file="C:\\Users\\xueb\\Desktop\\passive_template\\Ca_result.csv")
  }
  
  gc(verbose = FALSE)
  
}
