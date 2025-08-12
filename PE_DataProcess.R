#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("基本测量/forProcess_电阻+温度_Page样品_ITO玻璃.xlsx",1)%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_D_1K.csv")%>%rbind(fread("基本测量/250731_D_1k.csv"))%>%as.data.table()
data.pe.raw<-fread("基本测量/250730_P4A_100K.csv")%>%rbind(fread("基本测量/250731_P4A_100K.csv"))%>%as.data.table()


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

nameFromJson<-c("id","rq","dt","temp_in")
data.pe.raw.test[,':='(reqId=extractFromList(msgJson,"rq"),
                   odt=extractFromList(msgJson,"dt"),temp_in=extractFromList(msgJson,"temp_in"))]
data.pe.raw.test[,resist:=odt/(65535-odt)*100000]#100000

data.pe.raw.test<-data.pe.raw.test[resist<3e05]

#### 异常值识别 ####
# 简单测试
data.pe.raw$temperature[210:219]%>%{
    range(mean(.)-2*sd(.),mean(.)+2*sd(.))
}
data.pe.raw$temperature[210:219]%>%sd()
# 批量处理
# 温度异常值
data.pe.raw.outlierCheck<-data.table(id=data.pe.raw$id)
data.pe.raw.outlierCheck[,c(paste("tempOut",c(0:9),sep="_"),paste("resistOut",c(0:9),sep="_")) := .( as.logical(NA))]
for(i in c(0:nrow(data.pe.raw)%/%10)){ #一次批量处理10个（确定滑动次数
    for(j in c(0:9)){ #滑窗起始位置
        cat("i=",i," j=",j,"\n")
        rangeTemp<-outlierDetector(data.pe.raw[c((i*10+j):(i*10+j+10))]$temperature) #滑窗大小
        data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+10)),paste("tempOut",j,sep="_")]<-
            data.pe.raw[c((i*10+j):(i*10+j+10))]$temperature > rangeTemp[1]&data.pe.raw[c((i*10+j):(i*10+j+10))]$temperature < rangeTemp[2]
    }
}
data.pe.raw.outlierCheck$tempOutSum<-apply(X = data.pe.raw.outlierCheck[,c(paste("tempOut",c(0:9),sep="_"))],MARGIN = 1,
                                           FUN = function(x){    sum(x==FALSE)/sum(!is.na(x)) })
data.pe.raw.outlierCheck$tempOutFlag<-data.pe.raw.outlierCheck$tempOutSum>0.7
# 电阻异常值
for(i in c(0:nrow(data.pe.raw)%/%10)){ #一次批量处理10个（确定滑动次数
    for(j in c(0:9)){
        cat("i=",i," j=",j,"\n")
        rangeTemp<-outlierDetector(data.pe.raw[c((i*10+j):(i*10+j+10))]$resistance)
        data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+10)),paste("resistOut",j,sep="_")]<-
            data.pe.raw[c((i*10+j):(i*10+j+10))]$resistance > rangeTemp[1]&data.pe.raw[c((i*10+j):(i*10+j+10))]$resistance < rangeTemp[2]
    }
}
data.pe.raw.outlierCheck$resistOutSum<-apply(X = data.pe.raw.outlierCheck[,c(paste("resistOut",c(0:9),sep="_"))],MARGIN = 1,
                                             FUN = function(x){    sum(x==FALSE)/sum(!is.na(x)) })
data.pe.raw.outlierCheck$resistOutFlag<-data.pe.raw.outlierCheck$resistOutSum>0.8
#!!电阻处理的结果比较理想

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
ggplot(data = data.pe.raw.test[,c("rec_time","resist","temp_in","data_label","id","test_id")]%>%.[,temp_in:=temp_in*5000]%>%melt(.,id.var=c("id","rec_time","data_label","test_id")),
       aes(x=id,y=value,color=variable,lty=variable,group=variable))+geom_line()+scale_y_continuous(sec.axis = sec_axis(~./5000))+facet_wrap(~test_id,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(data = data.pe.raw.test[test_id=="250730_P4A_100k",c("rec_time","temp_in","resist","data_label","id")]%>%.[,temp_in:=temp_in*5000]%>%melt(.,id.var=c("id","rec_time","data_label")),
       aes(x=id,y=value,color=variable,shape=as.factor(data_label),group=variable))+geom_point()+geom_line()+scale_y_continuous(sec.axis = sec_axis(~./5000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


# 相关性可视化id>1500&!data_label%in%c(NA,"0,1")
ggplot(data = data.pe.raw.test[!data_label%in%c("0,1","1")&resist>1e05&temp_in<45,#rec_time>as.POSIXct("2025-07-31 18:30:00")&test_id=="250731_D_1k",#
                               c("temp_in","resist","data_label","test_id")],aes(x=temp_in,y=resist))+geom_point(alpha=0.2,color="blue")+#ylim(c(0,6000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12

ggplot(data = data.pe.raw.test[!data_label%in%c("0,1","1")&resist>1e05,#rec_time>as.POSIXct("2025-07-30 14:30:00")&test_id=="250730_D_1k",#rec_time>as.POSIXct("2025-07-31 18:30:00")&test_id=="250731_D_1k",#
                               c("temp_in","resist","data_label","test_id")]%>%.[,tempDiff:=abs(temp_in-32.5)],aes(x=tempDiff,y=resist,color=as.factor(data_label)))+geom_point()+#ylim(c(0,6000))+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))#,legend.position = c(0.12,0.88))#88，12

ggplot(data = data.pe.raw.test[rec_time>as.POSIXct("2025-07-31 15:00:00")&test_id=="250731_P4A_100k"&resist<3e5,
                               c("temp_in","resist","data_label","test_id")]%>%.[,tempDiff:=abs(temp_in-32)],aes(x=tempDiff,y=resist,color=as.factor(data_label)))+geom_point()+facet_wrap(~test_id)

# 相关性分析
cor(data.pe.raw[state=="cooling",c("modiTemp","modiResist")],method = "spearman")


# 平滑数据
outlierDetector<-function(x){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-1.5*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+1.5*sd(x,na.rm=TRUE),na.rm=TRUE))
}
