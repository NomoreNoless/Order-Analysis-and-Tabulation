#_________________________________________________________________________________________________
#return of the function calculate_fill_ratio():
#dataframe result has 3 columns: time_arrive_bin, numobs, sum_fill_qty_perc, sum_fill_order_perc
#_________________________________________________________________________________________________

calculate_fill_ratio <- function(msgs10_21_32){
  
  result <- msgs10_21_32 %>% 
    group_by(clordid,time_arrive_bin) %>%
    arrange(transacttime) %>% # arrange() put in here is more efficient
    summarise(cum_filled=sum(lastshares,na.rm=TRUE),
              qty_base=unique(orderqty)-min(cumqty)+head(lastshares,1),
              fill_qty_perc = cum_filled/qty_base,
              fill_order_perc = ifelse(cum_filled>0,1,0))
  
  result <- result %>% group_by(time_arrive_bin) %>%
    summarise(numobs = length(unique(clordid)),
              sum_fill_qty_perc = sum(fill_qty_perc,na.rm=TRUE), 
              sum_fill_order_perc = sum(fill_order_perc,na.rm=TRUE))
  
  #merge result with the time_frame to make sure the results are in the same time_frame
  result <- merge(df,result,by="time_arrive_bin",all.x=TRUE)
  result[is.na(result)] <- 0
  
  gc(verbose = FALSE)
  return(result)
}