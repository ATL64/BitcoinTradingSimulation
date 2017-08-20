library(ggplot2)
library(anytime)
### define between operator
`%between%`<-function(x,rng) x>=rng[1] & x<=rng[2]
###



kraken_ts<-read.csv('kraken_eur.csv')

kraken_ts<-kraken_ts[complete.cases(kraken_ts),]
kraken_ts$time<-anytime(kraken_ts$Timestamp)


kraken_ts<-kraken_ts[kraken_ts$time>'2016-01-01',]
kraken_ts$Timestamp<-NULL
kraken_ts$Open<-NULL
kraken_ts$High<-NULL
kraken_ts$Low<-NULL
kraken_ts$Close<-NULL
kraken_ts$Volume_.BTC.<-NULL
kraken_ts$Volume_.Currency.<-NULL




short<-kraken_ts[kraken_ts$time>'2017-03-01',]
short$local_extrema<-NULL


short$buy_sell<-NA
short$current_buy_sell<-NA

lp<-nrow(short)
short$current_buy_sell[1]<-'sell'
short$difference_price_transaction<-0
current_buy_price<-0
minutes_for_change<-5760
perc_increase<-0.015
perc_decrease<-0.015
percentage_profit<-0.1

i<-2
while(i <(lp-minutes_for_change)){
  if(short$current_buy_sell[i-1]=='sell'){
    if(short$Weighted_Price[i]<short$Weighted_Price[i-1]&short$Weighted_Price[i]<short$Weighted_Price[i+1]){ #check for local min
      if(any((short$Weighted_Price[(i+1):(i+minutes_for_change)]-short$Weighted_Price[i])/short$Weighted_Price[i]>=perc_increase)){ #check if there is 2% increase
        ind<-which((short$Weighted_Price[(i+1):(i+minutes_for_change)]-short$Weighted_Price[i])/short$Weighted_Price[i]>=perc_increase)[1]
        short$buy_sell[i+ind]<-'buy'
        short$current_buy_sell[i+ind]<-'buy'
        current_buy_price<-short$Weighted_Price[i+ind]
        i<-i+ind+1
      }else{
        short$current_buy_sell[i]<-'sell'  
        i<-i+1}
    }else{
      short$current_buy_sell[i]<-'sell'
      i<-i+1}
  }else{#being that current_buy_sell=='buy'
    if(short$Weighted_Price[i]>short$Weighted_Price[i-1]&short$Weighted_Price[i]>short$Weighted_Price[i+1]){ #check for local max
      if(any((short$Weighted_Price[(i+1):(i+minutes_for_change)]-short$Weighted_Price[i])/short$Weighted_Price[i]<=-perc_decrease)){ #check if there is 2% decrease
        ind<-which((short$Weighted_Price[(i+1):(i+minutes_for_change)]-short$Weighted_Price[i])/short$Weighted_Price[i]<=-perc_decrease)[1]
        ten_perc<-(short$Weighted_Price[i+ind]-current_buy_price)/short$Weighted_Price[i+ind]
        if(ten_perc>percentage_profit){
          short$buy_sell[i+ind]<-'sell'
          short$current_buy_sell[i+ind]<-'sell'
          short$difference_price_transaction[i+ind]<-ten_perc
          i<-i+ind+1
        }else{
          short$current_buy_sell[i]<-'buy'  
          i<-i+1}
        
      }else{
        short$current_buy_sell[i]<-'buy'  
        i<-i+1}
    }else{
      short$current_buy_sell[i]<-'buy'  
      i<-i+1}
  }
  print(i)
}







View(short[short$difference_price_transaction!=0,])

sum(short[short$difference_price_transaction!=0,]$difference_price_transaction*2000)



