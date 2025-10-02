#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("基本测量/forProcess_电阻+温度_Page样品_ITO玻璃.xlsx",1)%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_D_1K.csv")%>%rbind(fread("基本测量/250731_D_1k.csv"))%>%as.data.table()
data.pe.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/单独数据/250925_AgNW_Rref1M_2.csv")%>%as.data.table()
referenceR<-1000000

#### 数据格式 ####
# "log_id","rec_time","msg_id","test_id","data_label","msg_content"(JSON)
# 原始数据处理
data.pe.raw$rec_time<-as.POSIXct(data.pe.raw$rec_time)

#除去一些有问题的JSON数据
data.pe.raw<-data.pe.raw[nchar(msg_content)>10&startsWith(msg_content,"{")]
data.pe.raw[,msg_content:=gsub('""','"',msg_content)]

data.pe.raw[,msg_content:=gsub('null','-999',msg_content)] #处理热电偶可能出现的NULL导致无法识别
#建立id
data.pe.raw$id<-as.numeric(NA)

####对于DAM采集的数据需要处理####
# 会同时采集DAM的模拟量数据和IoT的温度数据，均收集在同一个csv文件中 需要单独处理
data.pe.raw.tc<-data.pe.raw[str_detect(data.pe.raw$msg_content,"T_IN")
                            ][,':='(labelDatetime=format(round.POSIXt(rec_time,units="secs"),format="%Y-%m-%d %H:%M:%S"),msgJson=lapply(msg_content,FUN = jsonToListProcessor))
                              ][,':='(t_in=extractFromList(msgJson,"T_IN"),t_out=extractFromList(msgJson,"T_OUT"),msgJson=NULL)]
data.pe.raw.adc<-data.pe.raw[str_detect(data.pe.raw$msg_content,"AIn")
                             ][,":="(labelDatetime=format(round.POSIXt(rec_time,units="secs"),format="%Y-%m-%d %H:%M:%S"),msgJson=lapply(msg_content,FUN = jsonToListProcessor))
                               ][,':='(AIn0=extractFromList(msgJson,"AIn[0]"),AIn1=extractFromList(msgJson,"AIn[1]"),AIn2=extractFromList(msgJson,"AIn[2]"),AIn3=extractFromList(msgJson,"AIn[3]"),msgJson=NULL)]

data.pe.raw.test<-merge(x=data.pe.raw.adc[,c("rec_time","test_id","data_label","labelDatetime","AIn0","AIn1","AIn2","AIn3")],
                        y=data.pe.raw.tc[,c("labelDatetime","t_in","t_out")],by="labelDatetime",all.x=TRUE)

####JSON数据取出####
#注意，有些时候JSON里面有两个双引号
for(i in unique(data.pe.raw$test_id)){
    data.pe.raw[test_id==i]$id<-c(1:(nrow(data.pe.raw[test_id==i])))
}


# 按testid取出
data.pe.raw.test<-data.pe.raw#[test_id=="250730_P4A_100k"]
data.pe.raw.test$msgJson<-lapply(data.pe.raw.test$msg_content,FUN = jsonToListProcessor)

nameFromJson<-c( "rq","T_IN","T_OUT","R_ITO" )#c("id","rq","dt","temp_in") "rq","T_IN","T_OUT","R_ITO","R_AgNW","L_IN" "t_in","t_out"
data.pe.raw.test[,':='(reqId=extractFromList(msgJson,"rq"),
                   # odt=extractFromList(msgJson,"dt"),
                   # temp_in=extractFromList(msgJson,"temp_in"),
                   t_in=extractFromList(msgJson,"T_IN"),#t_in
                   t_out=extractFromList(msgJson,"T_OUT"),#t_out
                   r_ITO=extractFromList(msgJson,"R_ITO")
                   # r_AgNW=extractFromList(msgJson,"R_AgNW"),
                   # l_in=extractFromList(msgJson,"L_IN")
                   )]

#### 处理数据采样器的采样 #
nameFromJson<-c("AIn[0]","AIn[1]","AIn[2]")
data.pe.raw.test[,':='(A0=extractFromList(msgJson,nameFromJson[1]),
                        A1=extractFromList(msgJson,nameFromJson[2]),
                        A2=extractFromList(msgJson,nameFromJson[3]))]


# 电阻值预估
# data.pe.raw.test[,resist:=odt/(65535-odt)*100000]#100000
data.pe.raw.test[,r_ITO_est:=r_ITO/(65535-r_ITO)*referenceR]#100000
# data.pe.raw.test[,r_AgNW_est:=r_AgNW/(65535-r_ITO)*1000000]#100000

# 温度修正
# data.pe.raw.test[,t_in:=t_in+1.5] #热电偶偏移修正
data.pe.raw.test[t_in>50&t_out<30,t_in:=NA] #删除热电偶异常数据，t_out温度未上升时t_in温度应不高，即两侧温差不会太高
data.pe.raw.test[t_in<20,t_in:=NA] #删除热电偶异常数据

#删去多余行
data.pe.raw.test[,":="(msg_content=NULL,msg_id=NULL,msgJson=NULL,reqId=NULL)]
# 时间截取
data.pe.raw.test<-data.pe.raw.test[rec_time<as.POSIXct("2025-09-24 19:05:00")]

data.pe.raw.test<-data.pe.raw.test[r_AgNW<50000]

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

data.pe.raw.test[,":="(smoothAgNW=getMovingAverageValue(r_ITO_est,60,onlyPast = FALSE))]


# 数据状态处理
tmp<-data.pe.raw.test[rec_time>as.POSIXct("2025-08-18 17:00:00")]
nn<-ts(tmp$r_AgNW)%>%ets



#### 数据相关性验证 ####
# 数据可视化
# 时序数据
ggplot(data = data.pe.raw.test[,c("rec_time","t_in","t_out","id","r_ITO_est")]%>%
           .[,r_nor:=scale(r_ITO_est)]%>%
           # .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*40000,10,onlyPast = FALSE))]%>% .[,c("rec_time","r_ITO_est","t_mid","id")]
           .[,":="(t_in=(t_in-35)/10,t_out=(t_out-35)/10)]%>%.[,c("rec_time","r_nor","t_in","t_out","id")]%>%
               melt(.,id.var=c("id","rec_time")),
       aes(x=rec_time,y=value,color=variable,lty=variable,group=variable))+geom_line()+
    labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(.*10)+35,name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


# 相关性可视化id>1500&!data_label%in%c(NA,"0,1")
ggplot(data = data.pe.raw.test[]%>%.[,r_nor:=scale(r_ITO_est)],
       aes(x=t_out,y=r_nor))+geom_point(alpha=0.2,color="blue",position = "jitter")+#ylim(c(0,6000))+
    labs(y="Resistance",x="Temperature")+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12


#### 数据导出 ####
write.csv(data.pe.raw.test,file="AA1_1M1u.csv")




# 相关性分析
cor(data.pe.raw[state=="cooling",c("modiTemp","modiResist")],method = "spearman")


# 平滑数据
outlierDetector<-function(x,multipleSd=1.5){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-multipleSd*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+multipleSd*sd(x,na.rm=TRUE),na.rm=TRUE))
}
