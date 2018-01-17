source("weekly_result.R")
source("calculate_fill_ratio.R")

Tabulation <- function(start_dates,end_dates,US_Firm_list_uncon,US_Firm_list_con,US_algo,Ca_Firm_list_uncon,Ca_Firm_list_con,Ca_algo,ordertype){
  
  print("start function Tabulation")
  
  #define the result dataframe
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  num_placed <- data.frame(
    time_bin = time_frame
  )
  #****************************************
  fill_order_perc <- data.frame(
    time_bin = time_frame
  )
  fill_qty_perc <- data.frame(
    time_bin = time_frame
  )
  fill_numobs <- data.frame(
    time_bin = time_frame
  )
  #*****************************************
  return_bps <- data.frame(
    time_bin = time_frame
  )
  return_spread <- data.frame(
    time_bin = time_frame
  )
  return_numobs <- data.frame(
    time_bin = time_frame
  )
  #*****************************************
  price_improve_bps <- data.frame(
    time_bin = time_frame
  )
  price_improve_spread <- data.frame(
    time_bin = time_frame
  )
  price_improve_numobs <- data.frame(
    time_bin = time_frame
  )
  #*****************************************
  cancel_cost_bps <- data.frame(
    time_bin = time_frame
  )
  cancel_cost_spread <- data.frame(
    time_bin = time_frame
  )
  cancel_sequence_numobs <- data.frame(
    time_bin = time_frame
  )
  #******************************************
  average_cancel_cost_bps <- data.frame(
    time_bin = time_frame
  )
  average_cancel_cost_spread <- data.frame(
    time_bin = time_frame
  )
  cancel_separate_numobs <- data.frame(
    time_bin = time_frame
  )
  #-----------------------------------------------------------------------------------------------------------------------------------------------------------
  
  num_weeks <- length(start_dates)
  
  for (i in 1:num_weeks){
    
    print(i)
    result_Ca <- weekly_result(country="Canada",start_date=start_dates[i],end_date=end_dates[i],Firm_list_uncon = Ca_Firm_list_uncon,Firm_list_con = Ca_Firm_list_con,algo = Ca_algo,ordertype=ordertype)
    result_US <- weekly_result(country="US",start_date=start_dates[i],end_date=end_dates[i],Firm_list_uncon = US_Firm_list_uncon,Firm_list_con=US_Firm_list_con, algo = US_algo,ordertype=ordertype)
    
    #Tabulation Calculation
    #------------------------------------------------------------------------------------------------------------------
    msgs10_21_32_Ca <- result_Ca$msgs10_21_32
    msgs10_21_32_US <- result_US$msgs10_21_32
    msgs10_21_32 <- rbind(result_Ca$msgs10_21_32,result_US$msgs10_21_32)
    msgs10_sequence_Ca <- result_Ca$msgs10_sequence
    msgs10_sequence_US <- result_US$msgs10_sequence
    msgs10_separate_Ca <- result_Ca$msgs10_separate
    msgs10_separate_US <- result_US$msgs10_separate
    msgs10_sequence <- rbind(result_Ca$msgs10_sequence, result_US$msgs10_sequence)
    msgs10_separate <- rbind(result_Ca$msgs10_separate, result_US$msgs10_separate)
    
    #find all the Tabulation categories
    #------------------------------------------------------------------------------------------------------------------
    des_list <- unique(msgs10_21_32$exdestination)
    udf3_list <- unique(msgs10_21_32$udf3[(which(substring(msgs10_21_32$udf3,1,1)=="I"))])
    udf3_list <- setdiff(udf3_list,c("INV","INV2","INV3"))
    des_list <- c(c("all_USD","all_CAD","INV1","INV23"),des_list,udf3_list,"T_MID_DARK_US","T_MID_DARK_Ca","T_MID_ALL_US","T_MID_ALL_Ca")
    #------------------------------------------------------------------------------------------------------------------
    
    #num_placed, fill_order_perc, fill_qty_perc, fill_numobs
    #__________________________________________________________________________________________________________________
    for(des in des_list){
      
      print("deal with fill_order_perc and fill_qty_perc")
      print(des)
      
      if(des=="all_USD"){
        selected_des <- msgs10_21_32_US
      }
      else if (des=="all_CAD"){
        selected_des <- msgs10_21_32_Ca
      }
      else if (des=="INV1"){
        selected_des <- msgs10_21_32 %>% filter(udf3=="INV",!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des=="INV23"){
        selected_des <- msgs10_21_32 %>% filter(udf3 %in% c("INV2","INV3"),!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des %in% udf3_list){
        selected_des <- msgs10_21_32 %>% filter(udf3==des)
      }
      else if (des=="T_MID_DARK_US"){
        selected_des <- msgs10_21_32_US %>% filter(exdestination=="T_MID_DARK")
      }
      else if (des == "T_MID_DARK_Ca"){
        selected_des <- msgs10_21_32_Ca %>% filter(exdestination =="T_MID_DARK")
      }
      else if (des=="T_MID_ALL_US"){
        selected_des <- msgs10_21_32_US %>% filter(exdestination == "T_MID_ALL")
      }
      else if (des=="T_MID_ALL_Ca"){
        selected_des <- msgs10_21_32_Ca %>% filter(exdestination == "T_MID_ALL")
      }
      else{
        selected_des <- msgs10_21_32 %>% filter(exdestination==des)
      }
      
      
      #result of fill_order_perc and fill_qty_perc
      #***********************************************************************
      result_des <- selected_des %>% calculate_fill_ratio()
      #*************************************************************************
      
      #number of observations in each bin
      #*************************************************************************
      result_num <- selected_des %>% group_by(time_arrive_bin) %>%
        summarise(numobs = length(which(msg_id==10)))
      
      result_num <- merge(df,result_num,by="time_arrive_bin", all.x=TRUE)
      result_num[is.na(result_num)] <- 0
      #*************************************************************************
      
      
      if(des %in% colnames(num_placed)){
        num_placed[,des] <- num_placed[,des] + result_num$numobs
      }
      else{
        num_placed[,des] <- result_num$numobs
      }
      
      if(des %in% colnames(fill_order_perc)){
        fill_order_perc[,des] <- fill_order_perc[,des] + result_des$sum_fill_order_perc
      }
      else{
        fill_order_perc[,des] <- result_des$sum_fill_order_perc
      }
      
      if(des %in% colnames(fill_qty_perc)){
        fill_qty_perc[,des] <- fill_qty_perc[,des] + result_des$sum_fill_qty_perc
      }
      else{
        fill_qty_perc[,des] <- result_des$sum_fill_qty_perc
      }
      
      if(des %in% colnames(fill_numobs)){
        fill_numobs[,des] <- fill_numobs[,des] + result_des$numobs
      }
      else{
        fill_numobs[,des] <- result_des$numobs
      }
      
    }
    #___________________________________________________________________________________________________________________________
    
    
    #return_bps, return_spread, return_numobs, price_improve_bps, price_improve_spread, price_improve_numobs
    #____________________________________________________________________________________________________________________________
    
    msgs21_all <- msgs10_21_32 %>% filter(msg_id==21)
    msgs21_US <- msgs10_21_32_US %>% filter(msg_id==21)
    msgs21_Ca <- msgs10_21_32_Ca %>% filter(msg_id==21)
    
    for(des in des_list){
      
      print("deal with 30s_return and price_improvement")
      print(des)
      
      if(des=="all_USD"){
        selected_des <- msgs21_US
      }
      else if (des=="all_CAD"){
        selected_des <- msgs21_Ca
      }
      else if (des=="INV1"){
        selected_des <- msgs21_all %>% filter(udf3=="INV",!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des=="INV23"){
        selected_des <- msgs21_all %>% filter(udf3 %in% c("INV2","INV3"),!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des %in% udf3_list){
        selected_des <- msgs21_all %>% filter(udf3==des)
      }
      else if(des =="T_MID_DARK_US"){
        selected_des <- msgs21_US %>% filter(exdestination=="T_MID_DARK")
      }
      else if(des == "T_MID_DARK_Ca"){
        selected_des <- msgs21_Ca %>% filter(exdestination =="T_MID_DARK")
      }
      else if(des =="T_MID_ALL_US"){
        selected_des <- msgs21_US %>% filter(exdestination=="T_MID_ALL")
      }
      else if(des == "T_MID_ALL_Ca"){
        selected_des <- msgs21_Ca %>% filter(exdestination =="T_MID_ALL")
      }
      else{
        selected_des <- msgs21_all %>% filter(exdestination==des)
      }
      
      result_des <- selected_des %>% group_by(time_arrive_bin) %>%
        summarise(return_numobs = length(which(!is.na(return_bps))),
                  return_bps = sum(return_bps,na.rm=TRUE),
                  return_spread = sum(return_spread,na.rm=TRUE),
                  price_improve_numobs = n(),
                  price_improve_bps = sum(price_improve_bps,na.rm=TRUE),
                  price_improve_spread = sum(price_improve_spread,na.rm=TRUE))
      
      result_des <- merge(df,result_des,by="time_arrive_bin",all.x = TRUE)
      
      result_des[is.na(result_des)] <- 0
      
      if(des %in% colnames(return_numobs)){
        return_numobs[,des] <- return_numobs[,des] + result_des$return_numobs
        return_bps[,des] <- return_bps[,des] + result_des$return_bps
        return_spread[,des] <- return_spread[,des] + result_des$return_spread
      }
      else{
        return_numobs[,des] <- result_des$return_numobs
        return_bps[,des] <- result_des$return_bps
        return_spread[,des] <- result_des$return_spread
      }
      
      if(des %in% colnames(price_improve_numobs)){
        price_improve_numobs[,des] <- price_improve_numobs[,des] + result_des$price_improve_numobs
        price_improve_bps[,des] <- price_improve_bps[,des] + result_des$price_improve_bps
        price_improve_spread[,des] <- price_improve_spread[,des] + result_des$price_improve_spread
      }
      else{
        price_improve_numobs[,des] <- result_des$price_improve_numobs
        price_improve_bps[,des] <- result_des$price_improve_bps
        price_improve_spread[,des] <- result_des$price_improve_spread
      }
      
    }
    #_______________________________________________________________________________________________________________________________________
    
    
    # cancel_sequence_numobs, cancel_cost_bps, cancel_cost_spread
    #________________________________________________________________________________________________________________________________________
    for(des in des_list){
      
      print("deal with sequence cancel cost")
      print(des)
      
      if(des=="all_USD"){
        selected_des <- msgs10_sequence_US
      }
      else if (des=="all_CAD"){
        selected_des <- msgs10_sequence_Ca
      }
      else if (des=="INV1"){
        selected_des <- msgs10_sequence %>% filter(udf3=="INV",!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des=="INV23"){
        selected_des <- msgs10_sequence %>% filter(udf3 %in% c("INV2","INV3"),!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des %in% udf3_list){
        selected_des <- msgs10_sequence %>% filter(udf3==des)
      }
      else if(des =="T_MID_DARK_US"){
        selected_des <- msgs10_sequence_US %>% filter(exdestination=="T_MID_DARK")
      }
      else if(des == "T_MID_DARK_Ca"){
        selected_des <- msgs10_sequence_Ca %>% filter(exdestination =="T_MID_DARK")
      }
      else if(des =="T_MID_ALL_US"){
        selected_des <- msgs10_sequence_US %>% filter(exdestination=="T_MID_ALL")
      }
      else if(des == "T_MID_ALL_Ca"){
        selected_des <- msgs10_sequence_Ca %>% filter(exdestination =="T_MID_ALL")
      }
      else{
        selected_des <- msgs10_sequence %>% filter(exdestination==des)
      }
      
      result_des <- selected_des %>% group_by(time_arrive_bin) %>%
        summarise(cancel_sequence_numobs = n(),
                  cancel_cost_bps = sum(cancel_cost_bps,na.rm=TRUE),
                  cancel_cost_spread = sum(cancel_cost_spread, na.rm=TRUE),
                  number_cancel_sequence = sum(number_cancel_sequence,na.rm=TRUE))
      
      result_des <- merge(df,result_des,by="time_arrive_bin",all.x = TRUE)
      
      result_des[is.na(result_des)] <- 0
      
      if(des %in% colnames(cancel_sequence_numobs)){
        cancel_sequence_numobs[,des] <- cancel_sequence_numobs[,des] + result_des$cancel_sequence_numobs
        cancel_cost_bps[,des] <- cancel_cost_bps[,des] + result_des$cancel_cost_bps
        cancel_cost_spread[,des] <- cancel_cost_spread[,des] + result_des$cancel_cost_spread
        cancel_separate_numobs[,des] <- cancel_separate_numobs[,des] + result_des$number_cancel_sequence
      }
      else{
        cancel_sequence_numobs[,des] <- result_des$cancel_sequence_numobs
        cancel_cost_bps[,des] <- result_des$cancel_cost_bps
        cancel_cost_spread[,des] <- result_des$cancel_cost_spread
        cancel_separate_numobs[,des] <- result_des$number_cancel_sequence
      }
    }
    #________________________________________________________________________________________________________________________________________________
    
    
    
    # cancel_separate_numobs, average_cancel_cost_bps, average_cancel_cost_spread
    #________________________________________________________________________________________________________________________________________
    for(des in des_list){
      
      print("deal with separate cancel cost")
      print(des)
      
      if(des=="all_USD"){
        selected_des <- msgs10_separate_US
      }
      else if(des=="all_CAD"){
        selected_des <- msgs10_separate_Ca
      }
      else if (des=="INV1"){
        selected_des <- msgs10_separate %>% filter(udf3=="INV",!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des=="INV23"){
        selected_des <- msgs10_separate %>% filter(udf3 %in% c("INV2","INV3"),!(exdestination %in% c("EDGA","BATY","NQBX","IEXG")))
      }
      else if(des %in% udf3_list){
        selected_des <- msgs10_separate %>% filter(udf3==des)
      }
      else if(des =="T_MID_DARK_US"){
        selected_des <- msgs10_separate_US %>% filter(exdestination=="T_MID_DARK")
      }
      else if(des == "T_MID_DARK_Ca"){
        selected_des <- msgs10_separate_Ca %>% filter(exdestination =="T_MID_DARK")
      }
      else if(des =="T_MID_ALL_US"){
        selected_des <- msgs10_separate_US %>% filter(exdestination=="T_MID_ALL")
      }
      else if(des == "T_MID_ALL_Ca"){
        selected_des <- msgs10_separate_Ca %>% filter(exdestination =="T_MID_ALL")
      }
      else{
        selected_des <- msgs10_separate %>% filter(exdestination==des)
      }
      
      result_des <- selected_des %>% group_by(time_arrive_bin) %>%
        summarise(cancel_separate_numobs = n(),
                  average_cancel_cost_bps = sum(cancel_cost_bps,na.rm=TRUE),
                  average_cancel_cost_spread = sum(cancel_cost_spread, na.rm=TRUE))
      
      result_des <- merge(df,result_des,by="time_arrive_bin",all.x = TRUE)
      
      result_des[is.na(result_des)] <- 0
      
      if(des %in% colnames(average_cancel_cost_bps)){
        average_cancel_cost_bps[,des] <- average_cancel_cost_bps[,des] + result_des$average_cancel_cost_bps
        average_cancel_cost_spread[,des] <- average_cancel_cost_spread[,des] + result_des$average_cancel_cost_spread
      }
      else{
        average_cancel_cost_bps[,des] <- result_des$average_cancel_cost_bps
        average_cancel_cost_spread[,des] <- result_des$average_cancel_cost_spread
      }
    }
    #________________________________________________________________________________________________________________________________________________
    
    remove(msgs10_21_32)
    remove(msgs10_21_32_Ca)
    remove(msgs10_21_32_US)
    remove(msgs21_all)
    remove(msgs21_Ca)
    remove(msgs21_US)
    remove(msgs10_separate)
    remove(msgs10_sequence)
    gc(verbose = FALSE)
    
  }
  
  #save the result
  #_____________________________________________________________________________________________________________
  # save_fill_order_perc <- fill_order_perc
  # save_fill_qty_perc <- fill_qty_perc
  # 
  # save_return_bps <- return_bps
  # save_return_spread <- return_spread
  # 
  # save_price_improve_bps <- price_improve_bps
  # save_price_improve_spread <- price_improve_spread
  # 
  # save_cancel_cost_bps <- cancel_cost_bps
  # save_cancel_cost_spread <- cancel_cost_spread
  # 
  # save_average_cancel_cost_bps <- average_cancel_cost_bps
  # save_average_cancel_cost_spread <- average_cancel_cost_spread
  # 
  #______________________________________________________________________________________________________________
  fill_order_perc <- fill_order_perc/fill_numobs
  fill_order_perc$time_bin <- time_frame
  fill_qty_perc <- fill_qty_perc/fill_numobs
  fill_qty_perc$time_bin <- time_frame
  
  return_bps <- return_bps/return_numobs
  return_bps$time_bin <- time_frame
  return_spread <- return_spread/return_numobs
  return_spread$time_bin <- time_frame
  
  price_improve_bps <- price_improve_bps/price_improve_numobs
  price_improve_bps$time_bin <- time_frame
  price_improve_spread <- price_improve_spread/price_improve_numobs
  price_improve_spread$time_bin <- time_frame
  
  cancel_cost_bps <- cancel_cost_bps/cancel_sequence_numobs
  cancel_cost_bps$time_bin <- time_frame
  cancel_cost_spread <- cancel_cost_spread/cancel_sequence_numobs
  cancel_cost_spread$time_bin <- time_frame
  
  average_cancel_cost_bps <- average_cancel_cost_bps/cancel_separate_numobs
  average_cancel_cost_bps$time_bin <- time_frame
  average_cancel_cost_spread <- average_cancel_cost_spread/cancel_separate_numobs
  average_cancel_cost_spread$time_bin <-time_frame
  
  cancel_separate_numobs <- cancel_separate_numobs/cancel_sequence_numobs
  cancel_separate_numobs$time_bin <- time_frame
  
  
  write.xlsx(num_placed, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "num_placed",append = TRUE)
  write.xlsx(cancel_separate_numobs, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "cancel_separate_numobs",append = TRUE)
  write.xlsx(fill_order_perc, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "fill_order_perc",append = TRUE)
  write.xlsx(fill_qty_perc, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "fill_qty_perc",append = TRUE)
  write.xlsx(return_bps, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "return_bps",append = TRUE)
  write.xlsx(return_spread, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "return_spread",append = TRUE)
  write.xlsx(price_improve_bps, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "price_improve_bps",append = TRUE)
  write.xlsx(price_improve_spread, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "price_improve_spread",append = TRUE)
  write.xlsx(cancel_cost_bps, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "cancel_cost_bps",append = TRUE)
  write.xlsx(cancel_cost_spread, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "cancel_cost_spread",append = TRUE)
  write.xlsx(average_cancel_cost_bps, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "average_cancel_cost_bps",append = TRUE)
  write.xlsx(average_cancel_cost_spread, file="C:\\Users\\xueb\\Desktop\\passive_template\\Tabulation.xlsx",sheetName = "average_cancel_cost_spread",append = TRUE)
  
  
}