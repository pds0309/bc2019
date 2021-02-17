df<-read.csv("AFSNT.csv")
dl<-read.csv("AFSNT_DLY.csv")

library(lubridate)
library(dplyr)

df<-df %>% 
  mutate(SDT=ymd_hm(paste(SDT_YY,SDT_MM,SDT_DD,STT)),ID=row_number())
dl<-dl %>% 
  mutate(SDT=ymd_hm(paste(SDT_YY,SDT_MM,SDT_DD,STT)),ID=row_number())

CNT<-numeric()
for (i in df$ID){
  CNT[i]<-length(which(df$ARP==df$ARP[i] & df$SDT>=df$SDT[i]-3600 & df$SDT<=df$SDT[i]))
}
CNL<-numeric()
for (i in dl$ID){
  CNL[i]<-length(which(dl$ARP==dl$ARP[i] & dl$SDT>=dl$SDT[i]-3600 & dl$SDT<=dl$SDT[i]))
}

df<-df %>% 
  select(-SDT,-ID)
dl<-dl %>% 
  select(-SDT,-ID)

df<-cbind(df,CNT=CNT)
write.csv(df,"AFSNT_P.csv",row.names=F,na="")
dl<-cbind(dl,CNT=CNL)
write.csv(dl,"AFSNT_DLY_P.csv",row.names=F,na="")
