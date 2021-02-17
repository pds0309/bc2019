library(dplyr)
if(!require('sqldf')){install.packages('sqldf');library(sqldf)}

## 탐색적 자료분석

SDT <-read.csv("C:/Users/dong/Desktop/데이터/AFSNT_P.csv") 
str(SDT)
# 날짜 팩터 변환
SDT[,2] <- as.data.frame(as.factor(SDT[,2]))
SDT[,3] <- as.data.frame(as.factor(SDT[,3]))
SDT[,4] <- as.data.frame(as.factor(SDT[,4]))


####FLO####

#FLO에 따른 지연율 확인해보기
plot(SDT$FLO , SDT$DLY , xlab = 'FLO' , ylab = 'DLY')
# FLO 적은 case 제거
SDT_FLOTEST <- SDT %>%
  filter(FLO %in% c('A', 'B', 'F', 'H', 'I', 'J', 'L'))
table(SDT_FLOTEST$FLO)

# FLO 별 지연율과 영향력이 큰 모든 case들에서의 지연율 간의 차이가 있는지 검정하기 
J<- sqldf("select count() from SDT_FLOTEST where SDT_FLOTEST.FLO = 'J' AND SDT_FLOTEST.DLY = 'Y'")
ALL <- sqldf("select count() from SDT_FLOTEST where SDT_FLOTEST.DLY ='Y'")
prop.test(x = c(sum(J) , sum(ALL)) , n = c(sum(SDT_FLOTEST$FLO == 'J') , length(SDT_FLOTEST$FLO)))
A<- sqldf("select count() from SDT_FLOTEST where SDT_FLOTEST.FLO = 'A' AND SDT_FLOTEST.DLY = 'Y'")
prop.test(x = c(sum(A) , sum(ALL)) , n = c(sum(SDT_FLOTEST$FLO == 'A') , length(SDT_FLOTEST$FLO)))
B<- sqldf("select count() from SDT_FLOTEST where SDT_FLOTEST.FLO = 'B' AND SDT_FLOTEST.DLY = 'Y'")
prop.test(x = c(sum(B) , sum(ALL)) , n = c(sum(SDT_FLOTEST$FLO == 'B') , length(SDT_FLOTEST$FLO)))



####날짜####

table(SDT$SDT_DY) # 요일별 운항수는 크게 차이가 없다
SDT_MMTEST <- SDT %>%
  filter(!SDT_YY =='2019')
#월별 지연율 확인하기 # 2019년 데이터 제외
plot(SDT_MMTEST$SDT_MM , SDT_MMTEST$DLY , xlab = 'MONTH' , ylab = 'DLY')  #3월의 지연율이 크게 낮게 나왔다.
MM_ALL <- sqldf("select count() from SDT_MMTEST where SDT_MMTEST.DLY =='Y'")

M3 <- sqldf("select count() from SDT_MMTEST where SDT_MMTEST.SDT_MM == '3' AND SDT_MMTEST.DLY =='Y'")
prop.test(x = c(sum(M3) , sum(MM_ALL)) , n = c(sum(SDT_MMTEST$SDT_MM=='3') , length(SDT_MMTEST$SDT_DY)))

####요일별 지연율 확인하기####

plot(SDT$SDT_DY , SDT$DLY) # 큰 차이는 없어보이나 금요일에 지연율이 가장 높게 나왔다. 
DY_ALL <- sqldf("select count() from SDT where SDT.DLY =='Y'")
#금요일
FRI <- sqldf("select count() from SDT where SDT.SDT_DY =='금' AND SDT.DLY =='Y'")
prop.test(x = c(sum(FRI), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '금') , length(SDT$SDT_DY)))

#일요일
SUN <- sqldf("select count() from SDT where SDT.SDT_DY =='일' AND SDT.DLY =='Y'")
prop.test(x = c(sum(SUN), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '일') , length(SDT$SDT_DY)))

#토요일
SAT <- sqldf("select count() from SDT where SDT.SDT_DY =='토' AND SDT.DLY =='Y'")
prop.test(x = c(sum(SAT), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '토') , length(SDT$SDT_DY)))

#월
MON <-sqldf("select count() from SDT where SDT.SDT_DY == '월' AND SDT.DLY == 'Y'")
prop.test(x = c(sum(MON), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '월') , length(SDT$SDT_DY)))

#화
TUE <- sqldf("select count() from SDT where SDT.SDT_DY =='화' AND SDT.DLY == 'Y'")
prop.test(x = c(sum(TUE), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '화') , length(SDT$SDT_DY)))

#수
WED <- sqldf("select count() from SDT where SDT.SDT_DY == '수' AND SDT.DLY == 'Y'")
prop.test(x = c(sum(WED), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '수') , length(SDT$SDT_DY)))

#목
THR <- sqldf("select count() from SDT where SDT.SDT_DY =='목' AND SDT.DLY == 'Y'")
prop.test(x = c(sum(THR), sum(DY_ALL)) , n = c(sum(SDT$SDT_DY == '목') , length(SDT$SDT_DY)))


####시간####

#6~22시의 운항
SDT_TIMETEST <- SDT%>%
  filter(STT_HR > 5 & STT_HR < 23)
#시간 factor 로 변환
SDT_TIMETEST[,20] <- as.data.frame(as.factor(SDT_TIMETEST[,20]))
table(SDT_TIMETEST$STT_HR)
par(mfrow=c(1,1))
plot(SDT_TIMETEST$STT_HR , SDT_TIMETEST$DLY)  #오후 시간대에 지연율이 높은것을 알 수 있다.

####공항####
sort(table(SDT$ARP)) # ARP1 ARP3 의 이용이 가장 많음
plot(SDT$ARP , SDT$DLY , xlab = 'ARP' , ylab = 'DLY')
#이용률 상위 5개 공항
SDT_AFTEST <- SDT %>%
  filter(ARP %in% c('ARP1', 'ARP2' , 'ARP3' , 'ARP4', 'ARP6') , ODP %in% c('ARP1', 'ARP2' , 'ARP3' , 'ARP4', 'ARP6'))
SDT_AFTEST2<-SDT%>%
  filter(!(ARP %in% c('ARP10' , 'ARP11', 'ARP12','ARP14', 'ARP13', 'ARP7')), !(ODP %in% c('ARP10' , 'ARP11', 'ARP12','ARP14', 'ARP13', 'ARP7')))

###공항별 확인
AP_ALL <- sqldf("select count() from SDT_AFTEST2 where SDT_AFTEST2.DLY =='Y'")
#ARP1
ARP1 <-sqldf("select count() from SDT_AFTEST2 where SDT_AFTEST2.ARP == 'ARP1' AND SDT_AFTEST2.DLY == 'Y'")
prop.test(x = c(sum(ARP1), sum(AP_ALL)) , n = c(sum(SDT_AFTEST2$ARP == 'ARP1') , length(SDT_AFTEST2$SDT_DY)))

#ARP2
ARP2<-sqldf("select count() from SDT_AFTEST2 where SDT_AFTEST2.ARP == 'ARP2' AND SDT_AFTEST2.DLY =='Y'")
prop.test(x = c(sum(ARP2), sum(AP_ALL)) , n = c(sum(SDT_AFTEST2$ARP == 'ARP2') , length(SDT_AFTEST2$SDT_DY)))

#ARP3
ARP3<-sqldf("select count() from SDT_AFTEST2 where SDT_AFTEST2.ARP == 'ARP3' AND SDT_AFTEST2.DLY =='Y'")
prop.test(x = c(sum(ARP3), sum(AP_ALL)) , n = c(sum(SDT_AFTEST2$ARP == 'ARP3') , length(SDT_AFTEST2$SDT_DY)))



install.packages('car')
library(car)
# welch
#만든 변수 확인
ONE <- oneway.test(CNT~DLY , data = SDT_TIMETEST , var.equal = F)
ONE
boxplot(CNT~DLY , data = SDT_TIMETEST)
