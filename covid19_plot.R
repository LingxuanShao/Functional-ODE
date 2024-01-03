library("readxl");
library("foreach")
library("doParallel")
library("ggplot2")
library("expm")
set.seed(97);

#data <- read_excel("Project 4 Differential operator regression/covid19/data.xlsx")
data <- read_excel("D:/Document/PKU/Project 4 Differential operator regression/Rcode7/covid19/data.xlsx")
# 4 for cured; 6 for current confirmed; 9 for dead; 14 for name
n=33;
m=40; 
#wash data
index=which(as.matrix(data[,8])==20200210);
name=as.matrix(data[index,14]);
nohubeiindex=index[-18];
nohubeiname=as.matrix(data[nohubeiindex,14]);
Xcurrent=array(0,c(n,m));
Xcured=array(0,c(n,m));
Xdead=array(0,c(n,m));
Xcheck=array(0,c(n,m));
for(i in 1:n){
  Xcurrent[i,]=as.matrix(data[c(nohubeiindex[i]:(nohubeiindex[i]+m-1)),6]);
  Xcured[i,]=as.matrix(data[c(nohubeiindex[i]:(nohubeiindex[i]+m-1)),4]);
  Xdead[i,]=as.matrix(data[c(nohubeiindex[i]:(nohubeiindex[i]+m-1)),9]);
  Xcheck[i,]=as.matrix(data[c(nohubeiindex[i]:(nohubeiindex[i]+m-1)),14]);
}
t=array(0,c(n,m));
for(i in 1:n){
  for(j in 1:m){
    t[i,j]=j/m;
  }
}
#plot 
Y=array(0,c(n*m,3));
k=0;
for(i in 1:n){
  for(j in 1:m){
    k=k+1;
    Y[k,1]=as.numeric(t[i,j]);
    Y[k,2]=Xcurrent[i,j];
    #Y[k,2]=Xcured[i,j];
    #Y[k,2]=Xdead[i,j];
    Y[k,3]=i;
  }
}
Y=as.data.frame(Y);
ggplot(Y)+geom_line(aes(x=V1,y=V2,group=V3),color="grey",size=0.5)+
    geom_point(aes(x=V1,y=V2),color="grey",size=0.8)+xlab("")+ylab("")+
    scale_x_continuous(breaks = c())

