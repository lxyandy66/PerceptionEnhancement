#### 用于处理其余数据源的脚本 ####

################################################################################

#### 用于处理DAM采集的数据####
# 会同时采集DAM的模拟量数据和IoT的温度数据，均收集在同一个csv文件中 需要单独处理
data.pe.raw.tc<-data.pe.raw[str_detect(data.pe.raw$msg_content,"T_IN")
][,':='(labelDatetime=format(round.POSIXt(rec_time,units="secs"),format="%Y-%m-%d %H:%M:%S"),msgJson=lapply(msg_content,FUN = jsonToListProcessor))
][,':='(t_in=extractFromList(msgJson,"T_IN"),t_out=extractFromList(msgJson,"T_OUT"),msgJson=NULL)]
data.pe.raw.adc<-data.pe.raw[str_detect(data.pe.raw$msg_content,"AIn")
][,":="(labelDatetime=format(round.POSIXt(rec_time,units="secs"),format="%Y-%m-%d %H:%M:%S"),msgJson=lapply(msg_content,FUN = jsonToListProcessor))
][,':='(AIn0=extractFromList(msgJson,"AIn[0]"),AIn1=extractFromList(msgJson,"AIn[1]"),AIn2=extractFromList(msgJson,"AIn[2]"),AIn3=extractFromList(msgJson,"AIn[3]"),msgJson=NULL)]

data.pe.raw.test<-merge(x=data.pe.raw.adc[,c("rec_time","test_id","data_label","labelDatetime","AIn0","AIn1","AIn2","AIn3")],
                        y=data.pe.raw.tc[,c("labelDatetime","t_in","t_out")],by="labelDatetime",all.x=TRUE)

#### 处理数据采样器的采样 #
nameFromJson<-c("AIn[0]","AIn[1]","AIn[2]")
data.pe.raw.test[,':='(A0=extractFromList(msgJson,nameFromJson[1]),
                       A1=extractFromList(msgJson,nameFromJson[2]),
                       A2=extractFromList(msgJson,nameFromJson[3]))]

################################################################################

#### 用于处理电化学工作站数据的脚本 ####
# 数据读取
data.pe.ecs.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/ECStest_ExportData/251112_CY1_ECM_1.csv",
                       data.table = TRUE,skip=3,sep=",",col.names = c("time","voltage","current","reverseI","charge","vRange","iRange"))
# 这个charge以后可以用
data.pe.ecs.raw[,resistance:=voltage/current]

################################################################################

#### 用于处理气象站数据的脚本 ####
# 原数据为分钟级别数据
data.pe.weather.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/251024-251026_CityU_WeatherStation.csv",
                       data.table = TRUE,skip=5,col.names = c("time","t_env","hum","wind","rad"))
data.pe.weather.raw[,time:=mdy_hm(time,tz="PRC")]

data.pe.weather.sec<-data.table(datetime=seq.POSIXt(from = as.POSIXct("2025-10-24 00:00:00"), to = as.POSIXct("2025-10-25 23:59:59"), by = "sec"))

# 按照时间合并数据，分钟内的数据插值补充
data.pe.weather.sec<-merge(x=data.pe.weather.sec,y=data.pe.weather.raw[,isApprox:=FALSE],all.x = TRUE,by.x = "datetime",by.y="time")
data.pe.weather.sec[is.na(isApprox)]$isApprox<-TRUE

data.pe.weather.sec[1:172741,c("t_env","hum","wind","rad")]<-#时间应对应，全时间段除去最后1min数据，此处为172741
    data.pe.weather.sec[,lapply(.SD,na.approx),.SDcols=c("t_env","hum","wind","rad")]


################################################################################

#### 用于从树莓派数据导出的脚本 ####

conn<-dbConnect(MySQL(),dbname="PerceptionEnhancement",user="root",password="K",host="192.168.43.139")
dbListTables(conn)
dbSendQuery(conn,'SET NAMES utf8')
data.pe.raspi.raw<-as.data.table(dbReadTable(conn,"serialacquisition")) #251105查询一共34w条数据
dbDisconnect(conn)

# 读取需要导出的数据
tmp.pe.outputlist<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/ECS_IoT数据/RaspPi_LogList.csv",data.table = TRUE)
tmp.pe.outputlist<-tmp.pe.outputlist[5:34] # 去掉一些之前测试的行
fwrite(tmp.pe.outputlist,"fwriteTest.csv")

for(i in unique(tmp.pe.outputlist$test_id)){
    fwrite(data.pe.raspi.raw[test_id==i],paste("/Volumes/Stroage/PercepetionEnhancement_Share/ECS_IoT数据/批量导出/",i,".csv",sep=""))
}

rm(data.pe.raspi.raw)
