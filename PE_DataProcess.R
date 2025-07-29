#用于处理数据的脚本
options(digits.secs=3)
# 读取原始数据
data.pe.raw<-read.xlsx("基本测量/forProcess_电阻+温度_Page样品_ITO玻璃.xlsx",1)%>%as.data.table()

# 原始数据处理
data.pe.raw$time<-as.POSIXct(paste("2025-07-11",as.character(data.pe.raw$time)))
data.pe.raw$timeElapse<-data.pe.raw$time-data.pe.raw$time[1]
data.pe.raw$id<-c(1:(nrow(data.pe.raw)))
data.pe.raw$state<-


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


#### 数据相关性验证 ####
# 数据可视化
ggplot(data = data.pe.raw[,c("modiTemp","timeElapse")],aes(x=timeElapse,y=modiTemp))+geom_line()
write.xlsx(data.pe.raw[,c("timeElapse","modiTemp","modiResist")],file = "processedPrelimaryTest.xlsx")
# 相关性可视化
ggplot(data = data.pe.raw[,c("modiTemp","modiResist","state")],aes(x=modiTemp,y=modiResist,color=state))+geom_point()
# 相关性分析
cor(data.pe.raw[state=="cooling",c("modiTemp","modiResist")],method = "spearman")


# 平滑数据
outlierDetector<-function(x){
    if(anyNA(x))
        warning("NA detected, function continue...",immediate. = TRUE)
    return(range(mean(x,na.rm=TRUE)-1.5*sd(x,na.rm=TRUE),mean(x,na.rm=TRUE)+1.5*sd(x,na.rm=TRUE),na.rm=TRUE))
}
