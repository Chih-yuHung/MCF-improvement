#To calculate MCF

library(weathermetrics)

setwd("D:/Dropbox/AAFC/Project 1_MCF/3_results/Weather data/DNDC weather data/SLC_Weather_WRH")
T.avg<-read.table("monthly avg T.txt",header=TRUE)

#constants
VS_Yr<-1200  #kg/yr
VS_LQD<-100  #100%
f_T1<-308.16 #K
f_Ea<-19347  #cal/mol
f_R<-1.987   #cal/K.mol
f_Tmin<-1.0  #degree C
f_T2d<-3.0   #T2 damping, degree C 
B0<-0.24     #dimensionless
E_eff<-95    #95%
site<-seq_along(T.avg)
#for manure removed or not, 0 = N, 1 = Y
Manure.rm<-c(0,0,0,0,0,0,0,0,1,0,0,0)

#convert air temp to manure temp.
T.m<-data.frame()
for (i in seq_along(T.avg)){
  if (sum(Manure.rm)>1) {
    for (j in 1:12){
  T.m[j,i]<-max(T.avg[j,i],f_Tmin)  
  } 
    }else {
    for (j in 1:12){
  T.m[j,i]<-max((T.avg[j,i]-f_T2d),f_Tmin)
    }
  } 
}
T.m<-rbind(T.m[12,],T.m[1:11,])

#conver C to K
T.m.K<-celsius.to.kelvin(T.m)

#function to calculate f, rounded
v.hoff<- function(x){
  round(exp((f_Ea*(x-f_T1))/(f_R*x*f_T1)),3)
}

f.m<-v.hoff(T.m.K)

#Vs excreted an loaded
VS_month<-rep(VS_Yr/12,each=12)
VS_loaded<-VS_month*(VS_LQD/100)
VS_ava<-vector()
VS_con<-vector()
CH4<-vector()


time.s<-Sys.time()
for (j in seq_along(f.m)){
  yr<-1
  repeat {
    if (yr == 1) {
      for (i in 1:12) {
       if (Manure.rm[i]==0){
         if (i == 1){
         VS_ava[i]<-VS_loaded[i]
         VS_con[i]<-VS_ava[i]*f.m[i,j]
         } else {
         VS_ava[i]<-VS_loaded[i]+VS_ava[i-1]-VS_con[i-1]
         VS_con[i]<-VS_ava[i]*f.m[i,j]
         } 
      } else {
      VS_emp<-(VS_ava[i-1]-VS_con[i-1])*(E_eff/100)
      VS_ava[i]<-VS_loaded[i]+((VS_ava[i-1]-VS_con[i-1])*(1-(E_eff/100)))
      VS_con[i]<-VS_ava[i]*f.m[i,j]
      }
        }
    } else {
      for (i in 1:12) {
        if (Manure.rm[i]==0){
          if (i == 1){
            VS_ava[i]<-VS_loaded[i]+VS_ava[12]
            VS_con[i]<-VS_ava[i]*f.m[i,j]
          } else {
            VS_ava[i]<-VS_loaded[i]+VS_ava[i-1]-VS_con[i-1]
            VS_con[i]<-VS_ava[i]*f.m[i,j]
          } 
        } else {
          VS_emp<-(VS_ava[i-1]-VS_con[i-1])*(E_eff/100)
          VS_ava[i]<-VS_loaded[i]+((VS_ava[i-1]-VS_con[i-1])*(1-(E_eff/100)))
          VS_con[i]<-VS_ava[i]*f.m[i,j]
        }
      }
    }
    yr<-yr+1  
    if (yr == 4){
      CH4[j]<-sum(VS_con)*B0 
      break
    }
    }
}
time.f<-Sys.time()
