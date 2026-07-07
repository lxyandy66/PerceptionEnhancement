####用于ECS数据处理的脚本####
# 包括IoT数据处理，ECS数据处理，两者数据整合
#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("/Volumes/Stroage/PercepetionEnhancement_Share/251016_PreTest.xlsx",1)%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_D_1K.csv")%>%rbind(fread("基本测量/250731_D_1k.csv"))%>%as.data.table()
data.pe.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/户外测试数据/251030_EF1.csv",data.table = TRUE)
referenceR<-5050

#### 数据格式 ####
# "log_id","rec_time","msg_id","test_id","data_label","msg_content"(JSON)
# 原始数据处理
data.pe.raw$rec_time<-as.POSIXct(data.pe.raw$rec_time)

#除去一些有问题的JSON数据
data.pe.raw[nchar(msg_content)<=10|!startsWith(msg_content,"{")] #看一下问题数据
data.pe.raw<-data.pe.raw[nchar(msg_content)>10&startsWith(msg_content,"{")]
data.pe.raw[,msg_content:=gsub('""','"',msg_content)]

data.pe.raw[,msg_content:=gsub('null','-999',msg_content)] #处理热电偶可能出现的NULL导致无法识别

ggplot(data.pe.raw.test,aes(x=rec_time,y=reqId,color=test_id))+geom_point()

#建立id
setorder(data.pe.raw,rec_time)
# data.pe.raw<-data.pe.raw[6:nrow(data.pe.raw)][test_id=="251024_IF1_5"]


####JSON数据取出####
#注意，有些时候JSON里面有两个双引号
table(data.pe.raw$test_id)
data.pe.raw$id<-as.numeric(NA)
for(i in unique(data.pe.raw$test_id)){
    data.pe.raw[test_id==i]$id<-c(1:(nrow(data.pe.raw[test_id==i])))
}

range(data.pe.raw[test_id=="251030_EF1_2"]$rec_time)

# 按testid取出
data.pe.raw.test<-data.pe.raw#[test_id=="250730_P4A_100k"]
data.pe.raw.test$msgJson<-lapply(data.pe.raw.test$msg_content,FUN = jsonToListProcessor)

nameFromJson<-c( "rq","T_IN","T_OUT","R_ITO" )#c("id","rq","dt","temp_in") "rq","T_IN","T_OUT","R_ITO","R_AgNW","L_IN" "t_in","t_out"
data.pe.raw.test[,':='(reqId=extractFromList(msgJson,"rq"),
                   t_in=extractFromList(msgJson,"T_IN"),#t_in
                   t_out=extractFromList(msgJson,"T_OUT"),#t_out
                   # t_env=extractFromList(msgJson,"T_ENV"),
                   r_ITO=extractFromList(msgJson,"R_ITO"),
                   l_in=extractFromList(msgJson,"L_IN"),
                   l_out=extractFromList(msgJson,"L_OUT")#,
                   # isHeating=extractFromList(msgJson,"HEAT")
                   )]



# 电阻值预估
data.pe.raw.test[,resistance:=r_ITO/(65535-r_ITO)*referenceR]#100000
ggplot(data.pe.raw.test,aes(x=r_ITO,y=resistance))+geom_point() #电阻转换的线性关系预览

#### 温度修正 ####
boxplot(data.pe.raw.test[,c("t_in","t_out")])
# data.pe.raw.test[,t_in:=t_in+1.5] #热电偶偏移修正

# t_in修正
data.pe.raw.test[t_in<20,t_in:=NA]
data.pe.raw.test[t_in>50,t_in:=NA]

data.pe.raw.test[t_in>55|t_in<15]%>%View #超上下限
data.pe.raw.test[t_in>55|t_in<15,t_in:=NA] #几个可能为异常的t_in：29.75
data.pe.raw.test[t_in==29.75]%>%View
data.pe.raw.test[t_out>50|t_out<18]%>%View #超上下限
data.pe.raw.test[t_out>50|t_out<18,t_out:=NA] 
data.pe.raw.test[abs(t_in-t_out)>10]%>%View

data.pe.raw.test[t_in>50&t_out<30]
data.pe.raw.test[t_in>50&t_out<30,t_in:=NA] #删除热电偶异常数据，t_out温度未上升时t_in温度应不高，即两侧温差不会太高
# data.pe.raw.test[t_in<20,t_in:=NA] #删除热电偶异常数据 #真有可能小于20

ggplot(data.pe.raw.test[l_in<1000|l_out<1000,c("rec_time","id","t_out","t_in","r_ITO","l_in","l_out")]%>%.[,":="(r_ITO=r_ITO/1000,l_in=l_in/1000,l_out=l_out/1000)]%>%
           melt(.,id.var=c("rec_time","id")),aes(x=rec_time,y=value,color=variable))+geom_point(size=0.5)


# 电阻异常值去除
boxplot(data.pe.raw.test[resistance<10000]$resistance)
data.pe.raw.test[resistance<10,resistance:=NA] #均需要
data.pe.raw.test[resistance>20000]%>%View
data.pe.raw.test[resistance>20000]$resistance<-NA

boxplot(data.pe.raw.test[r_ITO<12000]$r_ITO)
data.pe.raw.test[r_ITO<10,r_ITO:=NA]
data.pe.raw.test[r_ITO>12000]%>%View
data.pe.raw.test[r_ITO>12000]$r_ITO<-NA

################################################################################
#### ECS数据结合 ####
data.pe.raw.test<-merge(x=data.pe.raw.test,y=data.pe.ecs.raw[,c("time","resistance")],all.x=TRUE,by.x="msg_id",by.y="time")


################################################################################
#### 气象站数据结合 ####
data.pe.raw.test.backup<-data.pe.raw.test
data.pe.raw.test<-merge(x=data.pe.weather.sec[datetime>min(data.pe.raw.test$rec_time)&datetime<max(data.pe.raw.test$rec_time)],
                        y=data.pe.raw.test,all.x=TRUE,by.x = "datetime",by.y="rec_time")

names(data.pe.raw.test)[1]<-"rec_time"

################################################################################
###### 临时修改 ######
# 清理总表，每个field文件分开，并合并原始文件中时间戳
data.temp1<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/TL_Package_250423/Data/combined_cleaned_data_Field_with_split_merged_cycle_normalized_combined_processed.csv",data.table = TRUE)
data.temp2<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/TL_Package_250423/Data/combined_cleaned_data_Field_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE)

data.temp2.ef.raw<-data.temp2[source_folder=="EF1_1030_Field"]

data.temp2.ef.norm<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/TL_Package_250423/Data/combined_cleaned_data_Field_with_split_merged_normalized_EF_smoothed.csv",data.table = TRUE)
#万幸还好另外一个文件有时间戳 虽然不完整但是够了
# 检查一下这两个文件是不是每一行都一样
sum(round(data.temp2.ef.raw$t_in_norm,3)==round(data.temp2.ef.norm$t_in,3)) #这两个ef文件的每一行是对应的
nrow(data.temp2.ef.raw)
# 比较奇怪，t_in/l_out/l_in/resistance这几个norm的是一样，但是t_out不同


data.temp2.ef.norm[,rec_time:=as.POSIXct(rec_time)]
data.temp2.ef.norm[,labelTimeMsgid:=paste(format(rec_time,format="%m-%d_%H:%M"),msg_id,sep = "_")]

data.temp2.ef.norm$rec_time_old<-data.temp2.ef.norm$rec_time
data.temp2.ef.norm$rec_time<-NULL

data.temp2.ef.norm<-merge(x=data.temp2.ef.norm,y=data.pe.raw.test[,c("labelTimeMsgid","rec_time")],
                          all.x = TRUE,by.x="labelTimeMsgid",by.y="labelTimeMsgid",sort = FALSE)

data.temp2.ef.raw<-cbind(data.temp2.ef.raw,data.temp2.ef.norm[,c("rec_time","labelTimeMsgid")]) #保留label核对是不是拼接对了

data.temp2.ef.raw[,c("msg_id","rec_time","labelTimeMsgid")]%>%View #核一下

data.temp2.ef.raw[,msg_id_old:=msg_id]
data.temp2.ef.raw$msg_id<-c(1:nrow(data.temp2.ef.raw))

write.csv(data.temp2.ef.raw,file="EF1_Raw_Processed_Sort.csv")
setorder(data.temp2.ef.raw,rec_time,msg_id)


data.temp2.ef.norm$rec_time_old<-NULL
write.csv(data.temp2.ef.norm,file="EF1_Norm_Processed_Sort.csv")
setorder(data.temp2.ef.norm,rec_time,msg_id)
data.temp2.ef.norm$msg_id<-c(1:nrow(data.temp2.ef.norm))

data.temp2.ef.raw[,rec_time:=as.POSIXct(rec_time)]
data.pe.raw.test[,labelTimeMsgid:=paste(format(rec_time,format="%m-%d_%H:%M"),msg_id,sep = "_")]
nrow(data.pe.raw.test[labelTimeMsgid%in%data.temp2.ef.norm$labelTimeMsgid]) #检查一下处理后的norm数据label是不是大表中都有



data.temp2.ef.raw<-fread("EF1_Raw_Processed_noSort.csv",data.table = TRUE)
nn1<-cbind(data.temp2.ef.raw[,c("rec_time","Rate_L_norm")],data.temp2.ef.norm[,"Rate_L"])
ggplot(nn1%>%melt(.,id.var="rec_time"),aes(x=rec_time,y=value,color=variable))+geom_point()

# 大天才 Raw里面的norm 和Norm里面的Norm 完全不一样 服了

###### 临时修改 ######
# 处理清洗掉数据中缺失25-10-31 10:30-13:00部分


ggplot(data = data.temp2.ef.raw[,c("rec_time","t_out","l_in","l_out","msg_id","resistance")]%>%#"t_in","t_out","t_env",rec_time<as.POSIXct("2025-10-30 12:00")
           #.[,r_nor:=scale(resistance)]%>%
           # .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*40000,10,onlyPast = FALSE))]%>% .[,c("rec_time","r_ITO_est","t_mid","id")]
           .[,":="(#t_in=(t_in)*5E1,
               t_out=(t_out)*5E3,
               #t_env=(t_env)*5E1,
               l_in=l_in*1E1,l_out=l_out*1E1)]%>%.[,c("rec_time","resistance","t_out","l_in","l_out","msg_id")]%>%#"t_in",,"t_env"
           melt(.,id.var=c("msg_id","rec_time")),
       aes(x=rec_time,y=value,color=variable,lty=variable,group=variable))+geom_line()+
    labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./5E1),name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


################################################################################
# 此部分数据处理供EnergyPlus用 #

# 思路：EF原始数据处理--加入机器学习内容--合并天气数据--Energyplus对比有数据的部分
# 秒级数据先处理及平滑
# 对于原始数据：先平滑，再计算（如rate），再合并分钟，再平滑

#### 实测数据处理 ####
data.pe.energysim.field.raw<-data.pe.raw.test[,c("rec_time","log_id","msg_id","reqId","t_in","t_out","l_in","l_out","r_ITO")]
# 秒级数据有重复，共 118853条数据，但仅有118666个秒数据
colName<-c("log_id","msg_id","reqId","t_in","t_out","l_in","l_out","r_ITO")
data.pe.energysim.field.raw<-data.pe.energysim.field.raw[,(colName=lapply(.SD,mean,na.rm=TRUE)),by=rec_time,.SDcols=colName]#这样可以实现但是似乎logid保留不了第一个
# 将所有NaN处理为NA
for(i in names(data.pe.energysim.field.raw)){
    data.pe.energysim.field.raw[which(is.nan(as.matrix(data.pe.energysim.field.raw[,..i]))) ,i] <- NA
    #很奇怪，理论来说应该是..i, 但是这里..i会提示找不到对象，直接i才行，提示是赋值时候出错
}
# 秒级序列构建 共120327条！！！非常重要，需要对EnergyPlus提供完整的时间表
data.pe.energysim.field.raw<-merge(x=data.table(rec_time=seq.POSIXt(from=min(data.pe.energysim.field.raw$rec_time),
                                                                    to = max(data.pe.energysim.field.raw$rec_time),by = "sec")),
                                   y=data.pe.energysim.field.raw,all.x = TRUE,by="rec_time")

# 秒级数据平滑
colName<-c("t_in","t_out","l_in","l_out","r_ITO")#,"Rate_L","Rate_L_norm"  ,"Rate_L_cal" #批量处理的内容
data.pe.energysim.field.raw[,paste0(colName,"_smt"):=lapply(.SD,getMovingAverageValue,onlyPast=FALSE,n=20),
                            .SDcols=colName]
data.pe.energysim.field.raw[,paste0(colName,"_smt"):=lapply(.SD,as.numeric),.SDcols=paste0(colName,"_smt")]
data.pe.energysim.field.raw[,Rate_L_cal:=(l_in_smt/l_out_smt)][Rate_L_cal>1,Rate_L_cal:=1][l_in<1000|l_out<1000,Rate_L_cal:=1]


# 秒级平滑缺失的数据，用已有的补上
for(i in colName){
    #这个管用 data.table原生的处理方式
    data.pe.energysim.field.raw[!complete.cases(data.pe.energysim.field.raw[,paste0(..i,"_smt")]),
                                paste0(i,"_smt"):=apply(.SD,MARGIN = 1,function(x){ return(ifelse(is.na(x[2]),x[1],x[2]))  }),.SDcols=paste0(i,c("","_smt"))]
}
data.pe.energysim.field.raw[,Rate_L:=l_in_smt/l_out_smt][Rate_L>1,Rate_L:=1][l_in_smt<2000|l_out_smt<2000,Rate_L:=1][,Rate_L:=as.numeric(getMovingAverageValue(Rate_L,onlyPast=FALSE,n=20))]
data.pe.energysim.field.raw[is.na(Rate_L),Rate_L:=l_in_smt/l_out_smt][Rate_L>1,Rate_L:=1]


data.pe.energysim.field.raw[rec_time>=as.POSIXct("2025-10-30 11:00:00")][
                                !complete.cases(data.pe.energysim.field.raw[rec_time>=as.POSIXct("2025-10-30 11:00:00")])]%>%View

data.pe.energysim.field.raw[,c("msg_id",paste0(colName,"_smt")):=lapply(.SD,na.approx,na.rm=FALSE),.SDcols=c("msg_id",paste0(colName,"_smt"))]

data.pe.energysim.field.raw[is.na(t_out_smt),t_out_smt:=t_in_smt]


data.pe.energysim.field.raw<-fread("EF_Full.csv",data.table = TRUE)%>%.[,rec_time:=as.POSIXct(rec_time)] #读入收工修改的数据

data.pe.energysim.field.raw<-merge(x=data.pe.energysim.field.raw,y=data.pe.weather.sec[,c("datetime","t_env")],by.x = "rec_time",by.y="datetime",all.x=TRUE,sort = FALSE)

# 数据输出
# 新处理的数据输出
write.csv(data.pe.energysim.field.raw[rec_time>=as.POSIXct("2025-10-30 11:00:00"),
                                      c("rec_time","msg_id","t_in_smt","t_out_smt","t_env","l_in_smt","l_out_smt","r_ITO_smt","Rate_L")],file="EF_Full.csv",na = "",row.names = FALSE)

ggplot(data.pe.energysim.field.raw[rec_time>as.POSIXct("2025-10-30 11:00:00"),c("rec_time",paste0(..colName,"_smt"),"Rate_L")]%>% #,"Rate_L","t_glass"
           # .[,":="(l_in=l_in/1000,l_out=l_out/1000,r_ITO=r_ITO/200)]%>%#,Rate_L_cal=Rate_L_cal*50
           .[,":="(Rate_L=Rate_L*50,l_in_smt=l_in_smt/1000,l_out_smt=l_out_smt/1000,r_ITO_smt=r_ITO_smt/200)]%>%
           melt(.,id.var=c("rec_time")),#Rate_L=Rate_L*50,
       aes(x=rec_time,y=value,color=variable))+geom_point()#size=0.1

#### 检查新处理的数据 ####
# 旧处理的数据和新处理的数据差的挺大的

# 思路
# 原始处理数据 - python脚本处理 新数据重新倒回，只取旧处理数据的缺失值 拼接处平滑
data.pe.post.new<-fread("EF_Full_processed.csv",data.table = TRUE)%>%.[,":="(rec_time=as.POSIXct(rec_time),type="new")]
# nn<-data.temp2.ef.raw[,c("rec_time","t_out_norm","t_out", "t_env_norm","t_env","Rate_L_norm","Rate_L","resistance_norm","resistance")] # Raw的归一化数据不对，似乎不是最新的
nn<-data.temp2.ef.norm[,c("rec_time","t_out","t_env","Rate_L","resistance")]
data.pe.post.combined<-rbind(data.pe.post.new[,c("rec_time","t_out","t_out_raw","t_env","t_env_raw","Rate_L","Rate_L_raw","resistance","resistance_raw")],nn)
# 用来合并的
names(nn)<-c("rec_time","t_out_old","t_env_old","Rate_L_old","resistance_old")
data.pe.post.combined.wide<-merge(x=data.pe.post.new[,c("rec_time","t_out","t_out_raw","t_env","t_env_raw","Rate_L","Rate_L_raw","resistance","resistance_raw")],
                                  y=nn,by.x="rec_time",by.y="rec_time",sort = FALSE,all = TRUE)

#####????????

# 仅把部分时间段的旧数据用新数据替换
colName<-c("t_out","t_env","Rate_L","resistance")
# 手动处理部分数据
data.pe.post.combined.wide[(rec_time>as.POSIXct("2025-10-30 17:05:00")&rec_time<as.POSIXct("2025-10-30 18:43:00"))|
                               (rec_time>as.POSIXct("2025-10-31 07:57:00")&rec_time<as.POSIXct("2025-10-31 13:07:51")),
                                                             paste0(colName,"_old"):=lapply(.SD,as.numeric),.SDcol=colName]
data.pe.post.combined.wide[(rec_time>as.POSIXct("2025-10-30 13:00:00")&rec_time<as.POSIXct("2025-10-30 15:00:00")&resistance_old<0.25)|
                               (rec_time<as.POSIXct("2025-10-30 12:00:00")&t_env_old>0.25),
                                                         paste0(colName,"_old"):=lapply(.SD,function(x){return(NA)}),.SDcol=colName]
 #(rec_time>as.POSIXct("2025-10-30 18:00:00")&rec_time<as.POSIXct("2025-10-30 20:00:00")&t_env_old<0.28)
data.pe.post.combined.wide[,paste0(colName,"_old"):=lapply(.SD,na.approx,na.rm=FALSE,rule = 2),.SDcol=paste0(colName,"_old")]

# 旧处理的数据和新处理的数据差的挺大的
ggplot(data.pe.post.combined.wide[rec_time>as.POSIXct("2025-10-30 11:00:00"),c("rec_time","t_out_old","t_env_old","Rate_L_old","resistance_old")]%>% #,"Rate_L","t_glass" [,c("rec_time","t_env","resistance","t_out","Rate_L","isHeating")]
                             # .[,":="(l_in=l_in/1000,l_out=l_out/1000,r_ITO=r_ITO/200)]%>%#,Rate_L_cal=Rate_L_cal*50
                             # .[,":="(Rate_L=Rate_L*50,l_in_smt=l_in/1000,l_out=l_out/1000,r_ITO_smt=r_ITO_smt/200)]%>%
                             melt(.,id.var=c("rec_time")),#Rate_L=Rate_L*50,
                     aes(x=rec_time,y=value,color=variable))+geom_point()#+facet_wrap(~variable,ncol=1)#size=0.1

ggplot(data.pe.post.combined[rec_time>as.POSIXct("2025-10-30 11:00:00")]%>% #,"Rate_L","t_glass" [,c("rec_time","t_env","resistance","t_out","Rate_L","isHeating")]
           # .[,":="(l_in=l_in/1000,l_out=l_out/1000,r_ITO=r_ITO/200)]%>%#,Rate_L_cal=Rate_L_cal*50
           # .[,":="(Rate_L=Rate_L*50,l_in_smt=l_in/1000,l_out=l_out/1000,r_ITO_smt=r_ITO_smt/200)]%>%
           melt(.,id.var=c("rec_time","type")),#Rate_L=Rate_L*50,
       aes(x=rec_time,y=value,color=type,shape=type))+geom_point()+facet_wrap(~variable,ncol=1)#size=0.1

write.csv(data.pe.post.combined.wide[rec_time>as.POSIXct("2025-10-30 11:00:00"),
                                     c("rec_time","t_out_old","t_env_old","Rate_L_old","resistance_old","t_out_raw","t_env_raw","Rate_L_raw","resistance_raw")],
          file = "EF_Processed_final.csv",row.names = FALSE,na = "")

# 重新处理的不太好 只能拼合


#### 分钟级数据合并处理 ####
data.pe.energysim.field.min<-data.pe.energysim.field.raw[,.(rec_time=as.POSIXct(format(as.POSIXct(rec_time[1]),format="%Y-%m-%d %H:%M")),
                                                            count=length(t_in_smt),
                                                            t_in=mean(t_in_smt,na.rm=TRUE),
                                                            t_out=mean(t_out_smt,na.rm=TRUE),
                                                            l_in=mean(l_in_smt,na.rm=TRUE),
                                                            l_out=mean(l_out_smt,na.rm=TRUE),
                                                            r_ITO=mean(r_ITO_smt,na.rm=TRUE)),
                                                         by=(labelDatetime=format(as.POSIXct(rec_time),format="%Y-%m-%d %H:%M"))]
data.pe.energysim.field.min[,id:=c(1:nrow(data.pe.energysim.field.min))]
data.pe.energysim.field.min[,paste0(colName,"_smt"):=lapply(.SD,getMovingAverageValue,onlyPast=FALSE,n=10),
                            .SDcols=colName]
data.pe.energysim.field.min[,paste0(colName,"_smt"):=lapply(.SD,as.numeric),.SDcols=paste0(colName,"_smt")]

# 温度处理
data.pe.energysim.field.min[,t_glass:=ifelse(is.na(t_out_smt),t_in_smt,t_out_smt)][,t_glass:=as.numeric(getMovingAverageValue(t_glass,onlyPast=FALSE,n=10))]
colName<-c("t_in_smt","t_out_smt","l_in_smt","l_out_smt","r_ITO_smt","t_glass")
data.pe.energysim.field.min[,paste0(colName):=lapply(.SD,na.approx,na.rm = FALSE),.SDcols=paste0(colName)]

# Rate L处理
data.pe.energysim.field.min[,Rate_L:=l_in_smt/l_out_smt][Rate_L>1,Rate_L:=1][l_in_smt<2000|l_out_smt<2000,Rate_L:=1] #1000合适，2500有点多


#### 直接分钟级数据 ####
data.pe.energysim.output<-merge(x=data.pe.energysim.field.min[,c("labelDatetime","rec_time","t_in_smt","t_out_smt","l_in_smt","l_out_smt","r_ITO_smt","Rate_L","t_glass")],
                                y=data.pe.weather.raw,by.x = "labelDatetime",by.y = "labelDatetime",all.x = TRUE)
data.pe.energysim.output[,Rate_L_norm_cmb:=normalize(Rate_L)]
names(data.pe.energysim.output)<-c("labelDatetime","rec_time","t_in","t_out","l_in","l_out","r_ITO","Rate_L","t_glass","time",
                                   "t_env","hum","wind","rad","Rate_L_norm_cmb")

write.csv(data.pe.energysim.output.scaled[,c("labelDatetime","t_glass","r_ITO","l_in","l_out","Rate_L","Rate_L_norm_cmb","t_env","hum",
                                      "wind","rad" )],file = "PE_HIPSimulation_update.csv")

data.pe.energysim.output.scaled<-data.pe.energysim.output

data.pe.energysim.output.scaled[Rate_L<0.4]$Rate_L<-0.4
data.pe.energysim.output.scaled$Rate_L_norm_cmb<-normalize(data.pe.energysim.output.scaled$Rate_L)


# 看看TransferLearning那块怎么处理的
ggplot(data.temp2.ef.raw)+geom_point(aes(x=l_in,y=l_in_norm,color="l_in"))+geom_point(aes(x=l_out,y=l_out_norm,color="l_out"))
# EF里面，l_in和l_out是统一归一化的
range(data.temp2.ef.raw$Rate_L_norm) #0.4087741 1.0000000


#### 处理Transfer Learning所用数据 #### #!作废，不按此方法处理，直接用field数据重新预测试试!#
data.pe.energysim.transfer.raw<-data.temp2.ef.raw[,c("rec_time","msg_id","t_in","t_out","l_in","l_out","Rate_L","Rate_L_norm")] #取Rate_L_norm数据，用于乘以透射率的系数
data.pe.energysim.transfer.raw[,labelDatetime:=substr(rec_time,1,16)]#

data.pe.energysim.transfer.raw[,":="(id=c(1:nrow(data.pe.energysim.transfer.raw)),rec_time=as.POSIXct(rec_time))]
# 异常数据处理
data.pe.energysim.transfer.raw[rec_time<as.POSIXct("2025-10-30 16:00")&l_out<1000]<-NA #很奇怪 就这几个数有问题
# 平滑
data.pe.energysim.transfer.raw[,paste0(colName,"_smt"):=lapply(.SD,getMovingAverageValue,onlyPast=FALSE,n=20),
                        .SDcols=colName]
data.pe.energysim.transfer.raw[,paste0(colName,"_smt"):=lapply(.SD,as.numeric),.SDcols=paste0(colName,"_smt")]

# 合并至分钟级
data.pe.energysim.transfer.min<-data.pe.energysim.transfer.raw[,.(count=length(t_in),
                                                t_in=mean(t_in_smt,na.rm=TRUE),
                                                t_out=mean(t_out_smt,na.rm=TRUE),
                                                l_in=mean(l_in_smt,na.rm=TRUE),
                                                l_out=mean(l_out_smt,na.rm=TRUE)),
                                                # Rate_L=mean(Rate_L,na.rm=TRUE),
                                                # Rate_L_norm=mean(Rate_L_norm,na.rm=TRUE)),
                                             by=(labelDatetime=format(as.POSIXct(rec_time),format="%Y-%m-%d %H:%M"))]
data.pe.energysim.transfer.min<-data.pe.energysim.transfer.min[complete.cases(data.pe.energysim.transfer.min)]
setorder(data.pe.energysim.transfer.min,labelDatetime)

data.pe.energysim.transfer.min[,":="(id=c(1:nrow(data.pe.energysim.transfer.min)),Rate_L=(l_in/l_out),isField=TRUE)]

# 除去异常值
ggplot(data.pe.energysim.transfer.min,aes(x=id,y=Rate_L))+geom_point()
data.pe.energysim.transfer.min[1173:1181]$Rate_L<-NA ##日出数据
data.pe.energysim.transfer.min$Rate_L<-na.approx(data.pe.energysim.transfer.min$Rate_L) 
data.pe.energysim.transfer.min[Rate_L>1,Rate_L:=1]


#### 合并transfer数据和field数据 ####
# 即 实测+用于迁移学习的数据集
data.pe.energysim.inf<-merge(x=data.pe.energysim.field.min[,c("labelDatetime","id","t_in_smt","t_out_smt","l_in_smt","l_out_smt","Rate_L_cal_smt")],
                             y=data.pe.energysim.transfer.min[,c("labelDatetime","t_in","t_out","l_in","l_out","Rate_L","isField")],
                             all.x = TRUE,by.x="labelDatetime",by.y = "labelDatetime")
ggplot(data.pe.energysim.inf,aes(x=id,y=Rate_L_cal_smt,color="t_in_smt"))+geom_point()+geom_point(aes(y=Rate_L,color="Rate_L"))
data.pe.energysim.inf[,dataType:=ifelse(!is.na(isField),1,2)] # 1表示用了迁移学习 2表示测试数据 3表示插值估计数据

# 列一个合并的数据列 按照1>2的优先级合并数据
colName<-c("t_in","t_out","l_in","l_out","Rate_L")#,"Rate_L","Rate_L_norm"  ,"Rate_L_cal" #批量处理的内容
names(data.pe.energysim.inf)[7]<-"Rate_L_smt"
data.pe.energysim.inf[,paste0(colName,"_cmb"):=lapply(.SD,as.numeric),.SDcols=paste0(colName)] #cmb == combine
#无语
data.pe.energysim.inf[is.na(t_in_cmb),t_in_cmb:=t_in_smt]
data.pe.energysim.inf[is.na(t_out_cmb),t_out_cmb:=t_out_smt]
data.pe.energysim.inf[is.na(l_in_cmb),l_in_cmb:=l_in_smt]
data.pe.energysim.inf[is.na(l_out_cmb),l_out_cmb:=l_out_smt]
data.pe.energysim.inf[is.na(Rate_L_cmb),Rate_L_cmb:=Rate_L_smt]

#合并
data.pe.energysim.output<-data.table(rec_time=seq.POSIXt(from = as.POSIXct(range(data.pe.energysim.inf$labelDatetime)[1]), 
                                                         to = as.POSIXct(range(data.pe.energysim.inf$labelDatetime)[2]), by = "mins")) #一共2005min
data.pe.energysim.output[,labelDatetime:=format(rec_time,format="%Y-%m-%d %H:%M")]
data.pe.energysim.output<-merge(x=data.pe.energysim.output,
                                y=data.pe.energysim.inf[,c("labelDatetime","dataType","t_in_cmb","t_out_cmb","l_in_cmb","l_out_cmb","Rate_L_cmb")],
                                all.x=TRUE,by.x="labelDatetime",by.y = "labelDatetime")
# 这一部分数据 field和transfer的对不上，不能拼接，删去field数据
data.pe.energysim.output[rec_time>as.POSIXct("2025-10-31 10:30")&rec_time<as.POSIXct("2025-10-31 13:00")&dataType==2,
                         c("t_in_cmb","t_out_cmb","l_in_cmb","l_out_cmb","Rate_L_cmb","dataType")]<-NA
# 平滑插值
data.pe.energysim.output[,paste0(colName,"_cmb"):=lapply(.SD,na.approx),.SDcols=paste0(colName,"_cmb")] #cmb == combine
data.pe.energysim.output[is.na(dataType),dataType:=3] # 给插值内容的dataType设为3

data.pe.energysim.output[,Rate_L_norm_cmb:=normalize(Rate_L_cmb)]
# 合并气象站数据
data.pe.energysim.output<-merge(x=data.pe.energysim.output,y=data.pe.weather.raw,by.x = "labelDatetime",by.y = "labelDatetime",all.x = TRUE)

ggplot(data.pe.energysim.output.scaled[,c("rec_time","t_env","rad","Rate_L_norm_cmb")][,":="(rad=rad/30,Rate_L_norm_cmb=Rate_L_norm_cmb*30)]%>%melt(.,id.var="rec_time"),aes(x=rec_time,y=value,color=variable))+geom_line()
ggplot(data.pe.energysim.output)+
    geom_point(aes(x=rec_time,y=t_out_cmb,color="t_out_cmb",shape=as.factor(dataType)),size=1)+
    geom_point(aes(x=rec_time,y=t_in_cmb,color="t_in_cmb",shape=as.factor(dataType)),size=1)



write.csv(data.pe.energysim.output[,c("labelDatetime","t_in_cmb","t_out_cmb","l_in_cmb","l_out_cmb","Rate_L_cmb","Rate_L_norm_cmb","t_env","hum",
                                      "wind","rad" )],file = "PE_HIPSimulation.csv",row.names = FALSE,na = "")
# 列名对应
# labelDatetime-时间戳
# t_in_cmb-玻璃内表面温度
# t_out_cmb-玻璃外表面温度
# l_in_cmb-玻璃内表面照度
# l_out_cmb-玻璃外表面照度
# Rate_L_cmb-玻璃内外照度差
# Rate_L_norm_cmb-标准化的玻璃内外照度差
# t_env-室外温度
# hum-室外湿度
# wind-室外风速
# rad-室外辐射强度

#### Energyplus数据准备结束 ####
################################################################################


#### 异常值识别结果导出 ####
# 异常概率合并至原始数据
data.pe.raw.test<-merge(x=data.pe.raw.test,y=data.pe.raw.outlierCheck.out,by.x="rec_time",by.y = "chkId",all.x=TRUE)


ggplot(data = data.pe.raw.test[rec_time>as.POSIXct("2025-08-16 17:15:00")&rec_time<as.POSIXct("2025-08-16 17:20:00"),
                               c("rec_time","r_ITO","r_ITOOutFlag")],
       aes(x=rec_time,y=r_ITO,shape=r_ITOOutFlag,color=r_ITOOutFlag,group=1))+geom_line()+geom_point()

# 去除异常点并差值
data.pe.raw$modiTemp<-data.pe.raw$temperature
data.pe.raw[tempOutFlag==TRUE]$modiTemp<-NA
data.pe.raw$modiTemp<-na.approx(data.pe.raw$modiTemp)

data.pe.raw$modiResist<-data.pe.raw$resistance
data.pe.raw[resistOutFlag==TRUE]$modiResist<-NA
data.pe.raw$modiResist<-na.approx(data.pe.raw$modiResist)

data.pe.raw.test[,":="(smtResistance=getMovingAverageValue(abs(resistance),100,onlyPast = FALSE))]
data.pe.raw.test[resistance<0]$resistance<-NA

# 数据状态处理
tmp<-data.pe.raw.test[rec_time>as.POSIXct("2025-08-18 17:00:00")]
nn<-ts(tmp$r_AgNW)%>%ets



#### 数据相关性验证 ####
# 数据可视化

# 导入需要可视化的数据
data.pe.raw.test<-fread("Data_TransformLearning/AY1_ECS.csv")
data.pe.raw.test$rec_time<-as.POSIXct(data.pe.raw.test$rec_time)

# 时序数据
ggplot(data = data.pe.raw.test[,c("rec_time","t_out","l_in","l_out","msg_id","resistance")]%>%#"t_in","t_out","t_env",
           #.[,r_nor:=scale(resistance)]%>%
           # .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*40000,10,onlyPast = FALSE))]%>% .[,c("rec_time","r_ITO_est","t_mid","id")]
           .[,":="(#t_in=(t_in)*5E1,
                   t_out=(t_out)*5E1,
                   #t_env=(t_env)*5E1,
                   l_in=l_in*1E1,l_out=l_out*1E1)]%>%.[,c("rec_time","resistance","t_out","l_in","l_out","msg_id")]%>%#"t_in",,"t_env"
               melt(.,id.var=c("msg_id","rec_time")),
       aes(x=rec_time,y=value,color=variable,lty=variable,group=variable))+geom_line()+
    labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./5E1),name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


# 相关性可视化id>1500&!data_label%in%c(NA,"0,1")
ggplot(data = data.pe.raw.test[],#%>%.[,r_nor:=scale(resistance)],
       aes(x=t_out,y=resistance))+geom_point(alpha=0.2,color="blue",position = "jitter")+#ylim(c(0,6000))+
    labs(y="Resistance",x="Temperature")+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12

ggplot(data = data.pe.raw.test[!is.na(test_id),c("t_out","t_in","smtResistance","test_id")]%>%
           .[,tempDiff:=(t_out-28)],aes(x=abs(tempDiff),y=smtResistance))+geom_point(color="blue",alpha=0.2,position = "jitter")+facet_wrap(~test_id)+
    labs(y="Resistance",x="abs(Temperature-28)")+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

#### 数据导出 ####
#删去多余行
data.pe.raw.test[,":="(msg_content=NULL,log_id=NULL,msgJson=NULL,reqId=NULL,data_label=NULL,id=NULL,r_nor=NULL)]
data.pe.raw.test[,":="(r_ITO=NULL,r_nor=NULL)] #仅适用于ECS数据记录中IoT设备
write.csv(data.pe.raw.test[,-"smtResistance"],file="CY1_ECS.csv")
# write.xlsx(data.pe.raw.test,file="251016_PreTest.xlsx")
data.pe.weather.raw

# 平滑数据
outlierDetector<-function(x,multipleSd=1.5){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-multipleSd*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+multipleSd*sd(x,na.rm=TRUE),na.rm=TRUE))
}
