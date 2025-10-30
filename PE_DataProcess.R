#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("/Volumes/Stroage/PercepetionEnhancement_Share/251016_PreTest.xlsx",1)%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_D_1K.csv")%>%rbind(fread("基本测量/250731_D_1k.csv"))%>%as.data.table()
data.pe.raw<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/251024_IF1_5.csv",data.table = TRUE)
referenceR<-5050

#### 数据格式 ####
# "log_id","rec_time","msg_id","test_id","data_label","msg_content"(JSON)
# 原始数据处理
data.pe.raw$rec_time<-as.POSIXct(data.pe.raw$rec_time)

#除去一些有问题的JSON数据
data.pe.raw<-data.pe.raw[nchar(msg_content)>10&startsWith(msg_content,"{")]
data.pe.raw[,msg_content:=gsub('""','"',msg_content)]

data.pe.raw[,msg_content:=gsub('null','-999',msg_content)] #处理热电偶可能出现的NULL导致无法识别

#建立id
setorder(data.pe.raw,rec_time,msg_id)
data.pe.raw<-data.pe.raw[6:nrow(data.pe.raw)][test_id=="251024_IF1_5"]


####JSON数据取出####
#注意，有些时候JSON里面有两个双引号
for(i in unique(data.pe.raw$test_id)){
    data.pe.raw[test_id==i]$id<-c(1:(nrow(data.pe.raw[test_id==i])))
}
data.pe.raw$id<-c(1:(nrow(data.pe.raw)))


# 按testid取出
data.pe.raw.test<-data.pe.raw#[test_id=="250730_P4A_100k"]
data.pe.raw.test$msgJson<-lapply(data.pe.raw.test$msg_content,FUN = jsonToListProcessor)

nameFromJson<-c( "rq","T_IN","T_OUT","R_ITO" )#c("id","rq","dt","temp_in") "rq","T_IN","T_OUT","R_ITO","R_AgNW","L_IN" "t_in","t_out"
data.pe.raw.test[,':='(reqId=extractFromList(msgJson,"rq"),
                   # odt=extractFromList(msgJson,"dt"),
                   # temp_in=extractFromList(msgJson,"temp_in"),
                   t_in=extractFromList(msgJson,"T_IN"),#t_in
                   t_out=extractFromList(msgJson,"T_OUT"),#t_out
                   # t_env=extractFromList(msgJson,"T_ENV"),
                   r_ITO=extractFromList(msgJson,"R_ITO"),
                   l_in=extractFromList(msgJson,"L_IN"),
                   l_out=extractFromList(msgJson,"L_OUT")
                   # isHeating=extractFromList(msgJson,"HEAT")
                   )]



# 电阻值预估
# data.pe.raw.test[,resist:=odt/(65535-odt)*100000]#100000
data.pe.raw.test[,resistance:=r_ITO/(65535-r_ITO)*referenceR]#100000
# data.pe.raw.test[,r_AgNW_est:=r_AgNW/(65535-r_ITO)*1000000]#100000

# 温度修正
# data.pe.raw.test[,t_in:=t_in+1.5] #热电偶偏移修正
data.pe.raw.test[t_in>50&t_out<30,t_in:=NA] #删除热电偶异常数据，t_out温度未上升时t_in温度应不高，即两侧温差不会太高
# data.pe.raw.test[t_in<20,t_in:=NA] #删除热电偶异常数据 #真有可能小于20


#### ECS数据结合 ####
data.pe.raw.test<-merge(x=data.pe.raw.test,y=data.pe.ecs.raw[,c("time","resistance")],all.x=TRUE,by.x="msg_id",by.y="time")
#### 气象站数据结合 ####
data.pe.raw.test.backup<-data.pe.raw.test
data.pe.raw.test<-merge(x=data.pe.weather.sec[datetime>min(data.pe.raw.test$rec_time)&datetime<max(data.pe.raw.test$rec_time)],
                        y=data.pe.raw.test,all.x=TRUE,by.x = "datetime",by.y="rec_time")

names(data.pe.raw.test)[1]<-"rec_time"

#删去多余行
data.pe.raw.test[,":="(msg_content=NULL,log_id=NULL,msgJson=NULL,reqId=NULL)]
data.pe.raw.test[,":="(r_ITO=NULL,data_label=NULL,id=NULL)]


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
data.pe.raw.test[resistance<0]$resistance<-NA

# 数据状态处理
tmp<-data.pe.raw.test[rec_time>as.POSIXct("2025-08-18 17:00:00")]
nn<-ts(tmp$r_AgNW)%>%ets



#### 数据相关性验证 ####
# 数据可视化
# 时序数据
ggplot(data = data.pe.raw.test[,c("rec_time","t_in","t_out","l_in","l_out","msg_id","resistance")]%>%
           .[,r_nor:=scale(resistance)]%>%
           # .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*40000,10,onlyPast = FALSE))]%>% .[,c("rec_time","r_ITO_est","t_mid","id")]
           .[,":="(t_in=(t_in)*10,t_out=(t_out)*10,l_in=l_in/100,l_out=l_out/100)]%>%.[,c("rec_time","resistance","t_in","t_out","l_in","l_out","msg_id")]%>%
               melt(.,id.var=c("msg_id","rec_time")),
       aes(x=rec_time,y=value,color=variable,lty=variable,group=variable))+geom_line()+ylim(c(0,1000))+
    labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./10),name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


# 相关性可视化id>1500&!data_label%in%c(NA,"0,1")
ggplot(data = data.pe.raw.test[]%>%.[,r_nor:=scale(r_ITO_est)],
       aes(x=t_out,y=r_nor))+geom_point(alpha=0.2,color="blue",position = "jitter")+#ylim(c(0,6000))+
    labs(y="Resistance",x="Temperature")+theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12


#### 数据导出 ####
write.csv(data.pe.raw.test,file="EF1_temp.csv")
# write.xlsx(data.pe.raw.test,file="251016_PreTest.xlsx")



# 相关性分析
cor(data.pe.raw[state=="cooling",c("modiTemp","modiResist")],method = "spearman")


# 平滑数据
outlierDetector<-function(x,multipleSd=1.5){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-multipleSd*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+multipleSd*sd(x,na.rm=TRUE),na.rm=TRUE))
}
