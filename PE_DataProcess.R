#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("基本测量/forProcess_电阻+温度_Page样品_ITO玻璃.xlsx",1)%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_D_1K.csv")%>%rbind(fread("基本测量/250731_D_1k.csv"))%>%as.data.table()
data.pe.raw<-fread("基本测量/0816-field-inv-2.csv")%>%as.data.table()


#### 数据格式 ####
# "log_id","rec_time","msg_id","test_id","data_label","msg_content"(JSON)
# 原始数据处理
data.pe.raw$rec_time<-as.POSIXct(data.pe.raw$rec_time)

#除去一些有问题的JSON数据
data.pe.raw<-data.pe.raw[nchar(msg_content)>10&startsWith(msg_content,"{")]
data.pe.raw[,msg_content:=gsub('""','"',msg_content)]
#建立id
data.pe.raw$id<-as.numeric(NA)
####JSON数据取出####
#注意，有些时候JSON里面有两个双引号
for(i in unique(data.pe.raw$test_id)){
    data.pe.raw[test_id==i]$id<-c(1:(nrow(data.pe.raw[test_id==i])))
}


# 按testid取出
data.pe.raw.test<-data.pe.raw#[test_id=="250730_P4A_100k"]
data.pe.raw.test$msgJson<-lapply(data.pe.raw.test$msg_content,FUN = jsonToListProcessor)

nameFromJson<-c( "rq","T_IN","T_OUT","R_ITO","R_AgNW","L_IN" )#c("id","rq","dt","temp_in")
data.pe.raw.test[,':='(reqId=extractFromList(msgJson,"rq"),
                   # odt=extractFromList(msgJson,"dt"),
                   # temp_in=extractFromList(msgJson,"temp_in"),
                   t_in=extractFromList(msgJson,"T_IN"),
                   t_out=extractFromList(msgJson,"T_OUT"),
                   r_ITO=extractFromList(msgJson,"R_ITO"),
                   r_AgNW=extractFromList(msgJson,"R_AgNW"),
                   l_in=extractFromList(msgJson,"L_IN")
                   )]

# data.pe.raw.test[,resist:=odt/(65535-odt)*100000]#100000
data.pe.raw.test[,r_ITO:=r_ITO/(65535-r_ITO)*2000]#100000
data.pe.raw.test[,r_AgNW:=r_AgNW/(65535-r_ITO)*10000]#100000

data.pe.raw.test<-data.pe.raw.test[resist<3e05]

#### 异常值识别结果导出 ####

data.pe.raw.test<-merge(x=data.pe.raw.test,y=data.pe.raw.outlierCheck.out,by.x="rec_time",by.y = "chkId",all.x=TRUE)

# 异常概率合并至原始数据
data.pe.raw<-merge(x = data.pe.raw,y = data.pe.raw.outlierCheck[,c("id","tempOutSum","tempOutFlag","resistOutSum","resistOutFlag")],by="id",all.x = TRUE) #

ggplot(data = data.pe.raw[,c("timeElapse","temperature","tempOutFlag")],
       aes(x=timeElapse,y=temperature,shape=tempOutFlag,color=tempOutFlag,group=1))+geom_line()+geom_point()

# 去除异常点并差值
data.pe.raw$modiTemp<-data.pe.raw$temperature
data.pe.raw[tempOutFlag==TRUE]$modiTemp<-NA
data.pe.raw$modiTemp<-na.approx(data.pe.raw$modiTemp)

data.pe.raw$modiResist<-data.pe.raw$resistance
data.pe.raw[resistOutFlag==TRUE]$modiResist<-NA
data.pe.raw$modiResist<-na.approx(data.pe.raw$modiResist)



# 数据状态处理
tmp<-data.pe.raw.test[id>1800,c("temp_in","resist","data_label","test_id")]%>%.[,(data_label[data_label%in%c("-2,0")])]


#### 数据相关性验证 ####
# 数据可视化
# 时序数据
ggplot(data = data.pe.raw.test[,c("rec_time","r_ITO","r_AgNW","t_in","t_out","id")]%>%
           .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*30,10,onlyPast = FALSE),r_ITO=r_ITO*5)]%>% .[,c("rec_time","r_ITO","r_AgNW","t_mid","id")]%>%melt(.,id.var=c("id","rec_time")),
       aes(x=rec_time,y=value,color=variable,lty=variable,group=variable))+geom_line()+scale_y_continuous(sec.axis = sec_axis(~./30))+#facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(data = data.pe.raw.test[test_id=="250730_P4A_100k",c("rec_time","temp_in","resist","data_label","id")]%>%.[,temp_in:=temp_in*5000]%>%melt(.,id.var=c("id","rec_time","data_label")),
       aes(x=id,y=value,color=variable,shape=as.factor(data_label),group=variable))+geom_point()+geom_line()+scale_y_continuous(sec.axis = sec_axis(~./5000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


# 相关性可视化id>1500&!data_label%in%c(NA,"0,1")
ggplot(data = data.pe.raw.test[rec_time>as.POSIXct("2025-08-16 17:00:00")],aes(x=t_in,y=r_AgNW))+geom_point(alpha=0.2,color="blue",position = "jitter")+#ylim(c(0,6000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12

ggplot(data = data.pe.raw.test[!data_label%in%c("0,1","1")&resist>1e05,#rec_time>as.POSIXct("2025-07-30 14:30:00")&test_id=="250730_D_1k",#rec_time>as.POSIXct("2025-07-31 18:30:00")&test_id=="250731_D_1k",#
                               c("temp_in","resist","data_label","test_id")]%>%.[,tempDiff:=abs(temp_in-32.5)],aes(x=tempDiff,y=resist,color=as.factor(data_label)))+geom_point()+#ylim(c(0,6000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12

ggplot(data = data.pe.raw.test[rec_time>as.POSIXct("2025-07-31 15:00:00")&test_id=="250731_P4A_100k"&resist<3e5,
                               c("temp_in","resist","data_label","test_id")]%>%.[,tempDiff:=abs(temp_in-32)],aes(x=tempDiff,y=resist,color=as.factor(data_label)))+geom_point()+facet_wrap(~test_id)

# 相关性分析
cor(data.pe.raw[state=="cooling",c("modiTemp","modiResist")],method = "spearman")


# 平滑数据
outlierDetector<-function(x,multipleSd=1.5){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-multipleSd*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+multipleSd*sd(x,na.rm=TRUE),na.rm=TRUE))
}
