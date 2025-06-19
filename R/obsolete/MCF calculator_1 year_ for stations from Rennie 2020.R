#To calculate MCF
library(weathermetrics)
library(lubridate)

setwd("D:/Dropbox/AAFC/Project 1_MCF/3_results/Weather data/TimManureTemp1Year")
#Read data and add month
NSE<-read.table("S1-ER-T.txt",header=FALSE)
QCE<-read.table("S1-ER-Q.txt",header=FALSE)
FVE<-read.table("S1-ER-A.txt",header=FALSE)
ONSE<-read.table("S1-ER-H.txt",header=FALSE)
ONEE<-read.table("S1-ER-O.txt",header=FALSE)
ABE<-read.table("S1-ER-E.txt",header=FALSE)
NSL<-read.table("S1-LR-T.txt",header=FALSE)
QCL<-read.table("S1-LR-Q.txt",header=FALSE)
FVL<-read.table("S1-LR-A.txt",header=FALSE)
ONSL<-read.table("S1-LR-H.txt",header=FALSE)
ONEL<-read.table("S1-LR-O.txt",header=FALSE)
ABL<-read.table("S1-LR-E.txt",header=FALSE)
T.avg<-cbind(NSE,NSL,QCE,QCL,ONEE,ONEL,ONSE,ONSL
             ,ABE,ABL,FVE,FVL)
T.avg<-T.avg[,c(1:2,4,6,8,10,12,14,16,18,20,22,24)]
colnames(T.avg)<-c("day","NSE","NSL","QCE","QCL","ONEE","ONEL"
                   ,"ONSE","ONSL","ABE","ABL","FVE","FVL")

dates<-seq(as.Date("1979-1-1"),as.Date("1979-12-31"),by = "days")
T.avg[,1]<-dates
T.avg[,1]<-month(as.POSIXlt(T.avg[,1],format="%Y-%m-%d"))

#calculate monthly average temperature
NSCE.avg<-as.vector(round(tapply(T.avg[,2],T.avg$day,mean),2))
NSCL.avg<-as.vector(round(tapply(T.avg[,3],T.avg$day,mean),2))
QCCE.avg<-as.vector(round(tapply(T.avg[,4],T.avg$day,mean),2))
QCCL.avg<-as.vector(round(tapply(T.avg[,5],T.avg$day,mean),2))
ONEE.avg<-as.vector(round(tapply(T.avg[,6],T.avg$day,mean),2))
ONEL.avg<-as.vector(round(tapply(T.avg[,7],T.avg$day,mean),2))
ONSE.avg<-as.vector(round(tapply(T.avg[,8],T.avg$day,mean),2))
ONSL.avg<-as.vector(round(tapply(T.avg[,9],T.avg$day,mean),2))
ABCE.avg<-as.vector(round(tapply(T.avg[,10],T.avg$day,mean),2))
ABCL.avg<-as.vector(round(tapply(T.avg[,11],T.avg$day,mean),2))
FVE.avg<-as.vector(round(tapply(T.avg[,12],T.avg$day,mean),2))
FVL.avg<-as.vector(round(tapply(T.avg[,13],T.avg$day,mean),2))

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
#Vs excreted an loaded
VS_month<-rep(VS_Yr/12,each=36)
VS_loaded<-VS_month*(VS_LQD/100)
VS_ava<-vector()
VS_con<-vector()
CH4<-vector()
temp<-vector() 


#function to calculate f, rounded
v.hoff<- function(x){
  round(exp((f_Ea*(x-f_T1))/(f_R*x*f_T1)),3)
}

#for manure removed or not, 0 = N, 1 = Y
NSCE<-c(0,0,0,0,1,0,0,0,0,1,0,0)
NSCL<-c(0,0,0,0,0,0,1,0,0,0,1,0)
QCCE<-c(0,0,0,0,1,0,0,0,1,0,0,0)
QCCL<-c(0,0,0,0,0,1,0,0,0,0,1,0)
ONEE<-c(0,0,0,1,0,0,0,0,0,1,0,0)
ONEL<-c(0,0,0,0,0,1,0,0,0,0,1,0)
ONSE<-c(0,0,0,1,0,0,0,0,0,1,0,0)
ONSL<-c(0,0,0,0,0,1,0,0,0,0,1,0)
ABCE<-c(0,0,0,1,0,0,0,0,1,0,0,0)
ABCL<-c(0,0,0,0,0,1,0,0,0,1,0,0)
FVE<-c(0,0,0,1,0,0,0,0,0,1,0,0)
FVL<-c(0,0,0,0,1,0,0,0,0,0,1,0)
M.rm<-FVL # for manure temp
Manure.rm<-rep(M.rm,3) # for stablization 3 yr

#conver C to K
T.m.K<-celsius.to.kelvin(FVL.avg)
f.m<-v.hoff(T.m.K)
f.m[13:36]<-rep(f.m[1:12])

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
#Print temperautre
  print(paste("monthly VS.available in third year",month.abb,round(VS_ava[25:36],0)))
#Print temperautre
  # print(paste("monthly temp",month.abb,round(ABCE.avg,1)))
#To print the result
    cat(paste("total CH4 in third year:",CH4_sel,"\n"
            ,"potential CH4 in third year:",CH4.potential,"\n"
            ,"MCF:",MCF))

  
