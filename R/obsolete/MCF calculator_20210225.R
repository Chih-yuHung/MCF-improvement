#To calculate MCF

library(weathermetrics)

setwd("D:/Dropbox/AAFC/Project 1_MCF/3_results/Weather data/DNDC weather data/SLC_Weather_WRH")
T.avg<-read.table("monthly avg T.txt",header=TRUE)
name.list<-colnames(T.avg)
name.list<-as.character(sub('P', '', name.list))
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
Manure.rm<-rep(c(0,0,0,0,0,0,0,0,0,1,0,0),3) # for loop
M.rm<-c(0,0,0,0,0,0,0,0,0,1,0,0) # for manure temp

#convert air temp to manure temp.
T.m<-data.frame()
for (i in seq_along(T.avg)){
  if (sum(M.rm)>1) {
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
f.m[13:36,]<-rep(f.m[1:12,])
                
#Vs excreted an loaded
VS_month<-rep(VS_Yr/12,each=36)
VS_loaded<-VS_month*(VS_LQD/100)
VS_ava<-vector()
VS_con<-vector()
CH4<-vector()
temp<-vector() 

#Calculate CH4 produced and MCF
# time.s<-Sys.time()
for (j in seq_along(f.m)){
    for (i in 1:36) {
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
      temp[i]<-VS_con[i]*B0
     }
    CH4[j]<-sum(temp[25:36])
    print(paste("station ID",name.list[j]))
    print("monthly CH4 in third year")
    print(round(temp[25:36],1))#use to check the calcuation
    }
# time.f<-Sys.time() about 12 sec
CH4.potential<-VS_Yr*B0
MCF<-data.frame(name.list,CH4/CH4.potential)
colnames(MCF)<-c("stationID","MCF")
#Organize the result and assign station ID
ID<-read.csv("D:/Dropbox/AAFC/Project 1_MCF/3_results/Weather data/List_3403Polygons_withXY.csv",header=T)
colnames(ID)<-c("stationID","Lon","Lat")
ID1<-merge(ID,MCF,by="stationID")

#Station ID, MCF, Lat, Lot, 
# write.table(ID1,file="D:/Dropbox/AAFC/Project 1_MCF/3_results/MCF with locations.txt", sep="\t")
write.csv(ID1,file="D:/Dropbox/AAFC/Project 1_MCF/3_results/MCF with locations.csv", row.names = FALSE)
