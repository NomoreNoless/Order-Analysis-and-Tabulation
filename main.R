library(dplyr)
library(odbc)
library(stringr)
library(xlsx)
library(SDMTools)
source("order_analysis.R")
source("Tabulation.R")
source("weekly_result.R")




#*****************specific algos to be chosen**
#set up the database connection
#________________________________________________________________________________________________________________________________
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "{SQL Server Native Client 11.0}",
                      #Driver = "{SQL Server}",
                      Server = "192.168.1.110",
                      Database = "tethystrader_R",
                      UID = "tethys",
                      PWD = "tethys100",
                      MARS_connection="yes")

order_msgs <- tbl(con,"order_msgs")
master_order_msgs <- tbl(con,"order_msgs_incoming")
#_________________________________________________________________________________________________________________________________


#define the analysis period week by week
#_________________________________________________________________________________________________________________________________
#start dates of every week
start_dates <- c("20171218","20171225","20180101","20180108")
#end dates of every week
end_dates <- c("20171222","20171229","20180105","20180112")

# start_dates <- c("20180101")
# end_dates <- c("20180105")
#_________________________________________________________________________________________________________________________________

#define the timeframe
time_frame <- c()
for (i in 9:16){
  temp <- seq(i, i+0.55, 0.05)
  time_frame <- c(time_frame,temp)
}
time_frame <- c(time_frame,c(17.00))

df <- data.frame(
  time_arrive_bin = time_frame
)

#_________________________________________________________________________________________________________________________________
#*****************US Firm List that don't have the algo conditions**********************
US_Firm_list_uncon <- c("Magnetar","Paloma","QIM","Voloridge","Guzman")
#US_Firm_list_uncon <- c("Magnetar")
#*****************US Firm List that have the algo conditions****************************
US_Firm_list_con <- c("Fidessa","NBSI","Chardan","Speedroute")
#*****************specific algos to be chosen*******************************************
US_algo <- c("EVWAP","ETWAP","ELINE","ESENS")


#*****************Canada Firm List that don't have algo conditions**********************
Ca_Firm_list_uncon <- c("Magnetar","TDS")
#*****************Canada Firm List that have the algo conditions************************
Ca_Firm_list_con <- c("Fidessa","NBSI")
#*****************specific algos to be chosen*******************************************
Ca_algo <- c("EVWAP","ETWAP","ELINE","ESENS")




#********ordertype parameter**********
#_____________________________________
#ordertype=0: all orders
#ordertype=1: passive limit orders
#ordertype=2: limit orders
#ordertype=3: peg orders
#ordertype=4: hidden orders
#_____________________________________

order_analysis("Canada",start_dates,end_dates,Ca_Firm_list_uncon,Ca_Firm_list_con,Ca_algo,ordertype=4)
gc(verbose = FALSE)
order_analysis("US",start_dates,end_dates,US_Firm_list_uncon,US_Firm_list_con,US_algo,ordertype=4)
gc(verbose = FALSE)
Tabulation(start_dates,end_dates,US_Firm_list_uncon,US_Firm_list_con,US_algo,Ca_Firm_list_uncon,Ca_Firm_list_con,Ca_algo,ordertype=4)
gc(verbose = FALSE)
#_________________________________________________________________________________________________________________________________







