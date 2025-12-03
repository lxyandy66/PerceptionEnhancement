# 用于传感器校准用

options(digits.secs=3)
# 读取原始数据
data.pe.cali.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/calibration_lightSensor.csv")%>%as.data.table()

# 原始数据处理
data.pe.cali.raw$rec_time<-as.POSIXct(data.pe.cali.raw$rec_time)

#除去一些有问题的JSON数据
data.pe.cali.raw<-data.pe.cali.raw[nchar(msg_content)>10&startsWith(msg_content,"{")&data_label!=""]
data.pe.cali.raw[,msg_content:=gsub('""','"',msg_content)]
# JSON提取
data.pe.cali.raw$msgJson<-lapply(data.pe.cali.raw$msg_content,FUN = jsonToListProcessor)
data.pe.cali.raw[,':='(lux1=extractFromList(msgJson,"LUX1"),#t_in
                       lux2=extractFromList(msgJson,"LUX2")
)]

data.pe.cali.raw[,":="(log_id=NULL,msg_content=NULL,msgJson=NULL)]
write.xlsx(x=data.pe.cali.raw,file="PE_cali_lightSensor.xlsx") #输出

# 手工修改后数据输入
data.pe.cali.raw<-read.xlsx(file="/Volumes/Stroage/PercepetionEnhancement_Share/PE_cali_lightSensor.xlsx",sheetIndex = 1) 
ggplot(data.pe.cali.raw,aes(x=lux1,y=lux2))+geom_point()+facet_wrap(.~test_id)


# 统计
data.pe.cali.raw[,delta:=abs(lux1-lux2)]
data.pe.cali.long<-data.pe.cali.raw%>%.[,delta:=abs(lux1-lux2)]%>%melt(id.var=c( "rec_time","msg_id","test_id","data_label","NA."))%>%as.data.table
ggplot(data.pe.cali.long,aes(x=data_label,y=value,group=data_label,color=variable))+geom_boxplot()+facet_wrap(.~test_id)

stat.pe.cali.raw<-data.pe.cali.long[][,.(sensorId=paste(substring(test_id,15),variable,sep="_")[1],
                                       setPower=data_label[1],
                                       mean=mean(value,na.rm = TRUE),
                                       sd=sd(value,na.rm = TRUE)),by=(labelTestSensor=paste(substring(test_id,15),variable,data_label,sep="_"))]

data.pe.cali.raw<-read.xlsx("传感器校正数据.xlsx",sheetName = "IF5 EF1用传感器")%>%as.data.table
nn<-lm(data_label~lux,data.pe.cali.raw[test_id=="L_V7_BH_2"])
summary(nn)




