#*********************parameter***********************************
#ordertype=1 limit order
#ordertype=2 market order
#_________________________________________________________________

#*********************return***************************************
#return new columns price_improve_bps and price_improve_spread to the dataframe msgs10_21_32
#___________________________________________________________________________________________

calculate_price_improve <- function(msgs10_21_32,ordertype){
  
  if(ordertype %in% c(1,3,4)){
    msgs10_21_32 <- msgs10_21_32 %>% mutate(price_improve_bps = ifelse(side==1| side=="A",10000*(price-lastpx)/price,-10000*(price-lastpx)/price),
                                            price_improve_spread = ifelse(side==1| side=="A",(price-lastpx)/spread,-(price-lastpx)/spread))
    
  }
  
  if(ordertype=="MO"){
    
    msgs10_21_32 <- msgs10_21_32 %>% mutate(price_improve_bps = ifelse(side==1 | side=="A", 10000*(min(mid,price,na.rm=TRUE)-lastpx)/min(mid,price,na.rm=TRUE), -10000*(max(mid,price,na.rm=TRUE)-lastpx)/max(mid,price,na.rm=TRUE)),
                                            price_improve_spread =  ifelse(side==1 | side=="A", (min(mid,price,na.rm=TRUE)-lastpx)/spread, -(max(mid,price,na.rm=TRUE)-lastpx)/spread))
   
  }
  
  gc(verbose = FALSE)
  
  return(msgs10_21_32)
 
}