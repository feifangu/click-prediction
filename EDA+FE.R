# machine learning final pj EDA and feature engineering part

library(stringr)
library(data.table)
library(ggplot2)

Traindata <- fread("trytry-001.csv")
Traindata0 <- fread("Train-000.csv")
colnames(Traindata)<-colnames(Traindata0)

# -----------------------------------------------------------------------------------------
# EDA

Traindata0$hour <- str_sub(Traindata0$hour,-2,-1)
hourclick1 <- Traindata0[,hourclick:=sum(click),by="hour"]
hourclick1 <- hourclick1[order(hourclick1$click),]
hourclick1$click <- as.factor(hourclick1$click)

# Stacked click-hour
g<- ggplot(hourclick1, aes(fill=forcats::fct_rev(click), y=hourclick, x=hour)) + 
     geom_bar(position="stack", stat="identity")+
     ylab("count of click")+
     xlab("hour")
g +labs(fill = "click or not")

sampleTrain <- Traindata0[1:100000,]

par(mfrow=c(2,2))    # set the plotting area into a 1*2 array
hist(sampleTrain$C1, main="C1")
hist(sampleTrain$banner_pos, main="banner_pos")
hist(sampleTrain$device_type, main="device_type")
hist(sampleTrain$device_conn_type,main="device_conn_type")


par(mfrow=c(2,4))    # set the plotting area into a 1*2 array
hist(sampleTrain$C14, main="C14")
hist(sampleTrain$C15, main="C15")
hist(sampleTrain$C16, main="C16")
hist(sampleTrain$C17, main="C17")
hist(sampleTrain$C18, main="C18")
hist(sampleTrain$C19, main="C19")
hist(sampleTrain$C20, main="C20")
hist(sampleTrain$C21, main="C21")


#rm(Traindata0)
# -----------------------------------------------------------------------------------------
# feature engineering

# transform hour: 24
Traindata$hour <- str_sub(Traindata$hour,-2,-1)
#Traindata$hour <- as.factor(Traindata$hour)

# tranform device_ip
Traindata$device_ip <- str_sub(Traindata$device_ip,1,4)
device_ip <- as.data.table(sort(table(Traindata$device_ip),decreasing=TRUE))
device_ip[,cumpercentage:=cumsum(N)/3000000]
main_ip <- device_ip$V1[1:21] # find a small gap between 21 and 22
Traindata[!device_ip%in%main_ip,device_ip:="-1"]


# transform site_id: 75%
site_id <- as.data.table(sort(table(Traindata$site_id),decreasing=TRUE))
site_id[,cumpercentage:=cumsum(N)/3000000]
mainsite_id <- site_id[cumpercentage<=0.75,V1]
Traindata[!site_id%in%mainsite_id,site_id:="-1"]
#Traindata$site_id <- as.factor(Traindata$site_id)

# transform app_id: 75%
app_id <- as.data.table(sort(table(Traindata$app_id),decreasing=TRUE))
app_id[,cumpercentage:=cumsum(N)/3000000]
mainapp_id <- app_id[cumpercentage<=0.75,V1]
Traindata[!app_id%in%mainapp_id,app_id:="-1"]
#Traindata$app_id <- as.factor(Traindata$app_id)

# tranform site_domain: 75%
site_domain <- as.data.table(sort(table(Traindata$site_domain),decreasing=TRUE))
site_domain[,cumpercentage:=cumsum(N)/3000000]
main_site_domain <- site_domain[cumpercentage<=0.75,V1]
Traindata[!site_domain%in%main_site_domain,site_domain:="-1"]
#Traindata$site_domain <- as.factor(Traindata$site_domain)

# transform device_id: 2 categories, apple or not
device_id <- as.data.table(sort(table(Traindata$device_id),decreasing=TRUE))
device_id[,cumpercentage:=cumsum(N)/3000000]
maindevice_id <- device_id$V1[1]
Traindata[device_id!=maindevice_id,device_id:="-1"]
#Traindata$device_id <- as.factor(Traindata$device_id)

# transform device_model: 30%, 16
device_model <- as.data.table(sort(table(Traindata$device_model),decreasing=TRUE))
device_model[,cumpercentage:=cumsum(N)/3000000]
maindevice_model <- device_model[cumpercentage<=0.3,V1]
Traindata[!device_model%in%maindevice_model,device_model:="-1"]
#Traindata$device_model <- as.factor(Traindata$device_model)

# transform C14: 30%, 19
C14 <- as.data.table(sort(table(Traindata$C14),decreasing=TRUE))
C14[,cumpercentage:=cumsum(N)/3000000]
main_C14 <- C14[cumpercentage<=0.3,V1]
Traindata[!C14%in%main_C14,C14:=-1] # original range is 375 to 24349
#Traindata$C14 <- as.factor(Traindata$C14)

# transform C17: 50%, 19
C17 <- as.data.table(sort(table(Traindata$C17),decreasing=TRUE))
C17[,cumpercentage:=cumsum(N)/3000000]
main_17 <- C17[cumpercentage<=0.5,V1]
Traindata[!C17%in%main_C17,C17:=-1] # original range is 112 to 2793
#Traindata$C17 <- as.factor(Traindata$C17)

# tranform app_domain: 99%, 15
app_domain <- as.data.table(sort(table(Traindata$app_domain),decreasing=TRUE))
app_domain[,cumpercentage:=cumsum(N)/3000000]
mainapp_domain <- app_domain[cumpercentage<=0.99,V1]
Traindata[!app_domain%in%mainapp_domain,app_domain:="-1"] 
#Traindata$app_domain <- as.factor(Traindata$app_domain)

# transform the rest into factors
fac <- Traindata[,lapply(.SD, as.factor)]
fwrite(fac,"Traindata_cat300.csv")


#------------------


Traindata1 <- fread("Train-001.csv")
Traindata2 <- fread("Train-002.csv")
Traindata0 <- fread("Train-000.csv")
Train <- fread("trytry-001.csv")

test <- fread("ProjectTestData.csv")
test$hour <- str_sub(test$hour,-2,-1)
fwrite(test,"finaltest.csv")

colnames(Traindata1)<-colnames(Traindata0)
colnames(Traindata2)<-colnames(Traindata0)
colnames(Traindata3)<-colnames(Traindata0)

colnames(Train)<- colnames(Traindata0)

# transform hour: 24
Train$hour <- str_sub(Train$hour,-2,-1)
# transform hour: 24
Traindata2$hour <- str_sub(Traindata2$hour,-2,-1)

fwrite(Train,"Train300.csv")
fwrite(Traindata2,"Test.csv")
