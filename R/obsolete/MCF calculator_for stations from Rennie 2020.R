#To calculate MCF
library(weathermetrics)
setwd("D:/Dropbox/AAFC/Project 1_MCF/3_results/Weather data/DNDC weather data/SLC_Weather_WRH")

#constants
VS_Yr<-356924  #kg/yr
VS_LQD<-100  #100%
f_T1<-308.16 #K
f_Ea<-19347  #cal/mol
f_R<-1.987   #cal/K.mol
f_Tmin<-1.0  #degree C
f_T2d<-3.0   #T2 damping, degree C 
B0<-0.24     #dimensionless
E_eff<-95    #95%

#function to calculate f, rounded
v.hoff<- function(x){
  round(exp((f_Ea*(x-f_T1))/(f_R*x*f_T1)),3)
}

#Read data and
T.avg<-read.table("monthly avg T.txt",header=TRUE)
#select the station want to calculate MCF, P1001005 for example here
#There are 6 locations in Rennie 2020.
MCF.ca<-data.frame(matrix(nrow=6,ncol=4))
colnames(MCF.ca)<-c("site","lat","lon","MCF")
MCF.ca$site<-c("NS.c","QC.c","ON.e","ON.sw","AB.c","FV")
MCF.ca$lat<-c(46.3667,46.8000,45.3844,42.0333,53.5667,49.2500)
MCF.ca$lon<-c(-63.2667,-71.3833,-75.7167,-82.9000,-113.5167,-121.7667)  
MCF.ca$station<-c("536001","540094","547003","572006","727009","959019")
#selcte station from MCF.ca$station
T.sel<-T.avg$P547003

#for manure removed or not, 0 = N, 1 = Y
M.rm<-c(0,0,0,0,0,1,0,0,0,0,1,0) # for manure temp
Manure.rm<-rep(M.rm,3) # for stablization 3 yr

#convert air temp to manure temp.
T.m<-vector()
  if (sum(M.rm)>1) {
    for (i in 1:12){
      T.m[i]<-max(T.sel[i],f_Tmin)  
    } 
  }else {
    for (i in 1:12){
      T.m[i]<-max((T.sel[i]-f_T2d),f_Tmin)
    }
  } 
T.m<-c(T.m[12],T.m[1:11])
#conver C to K
T.m.K<-celsius.to.kelvin(T.m)
f.m<-v.hoff(T.m.K)
f.m[13:36]<-rep(f.m[1:12])

#Vs excreted an loaded
VS_month<-rep(VS_Yr/12,each=36)
VS_loaded<-VS_month*(VS_LQD/100)
VS_ava<-vector()
VS_con<-vector()
CH4<-vector()
temp<-vector() 

#Calculate CH4 produced and MCF
  for (i in 1:36) {
    if (Manure.rm[i]==0){
      if (i == 1){
        VS_ava[i]<-VS_loaded[i]
        VS_con[i]<-VS_ava[i]*f.m[i]
      } else {
        VS_ava[i]<-VS_loaded[i]+VS_ava[i-1]-VS_con[i-1]
        VS_con[i]<-VS_ava[i]*f.m[i]
      } 
    } else {
      VS_emp<-(VS_ava[i-1]-VS_con[i-1])*(E_eff/100)
      VS_ava[i]<-VS_loaded[i]+((VS_ava[i-1]-VS_con[i-1])*(1-(E_eff/100)))
      VS_con[i]<-VS_ava[i]*f.m[i]
    }
    temp[i]<-VS_con[i]*B0
    CH4.potential<-VS_Yr*B0
    CH4_sel<-round(sum(temp[25:36]),3)
    MCF<-round(CH4_sel/CH4.potential,3)
  }
  
#To know the CH4 production for every month
  print(paste("monthly CH4 in third year",month.abb,round(temp[25:36],1)))
#To print the result
    cat(paste("total CH4 in third year:",CH4_sel,"\n"
            ,"potential CH4 in third year:",CH4.potential,"\n"
            ,"MCF:",MCF))

  
