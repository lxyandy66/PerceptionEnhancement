
# !!! 注意原始数据的时间间隔 !!!
selectedTestId<-"0816-field-inv-2"
data.pe.raw.outlierCheck<-data.table(chkId=seq(from=range(data.pe.raw.test[test_id==selectedTestId]$rec_time)[1],to=range(data.pe.raw.test[test_id==selectedTestId]$rec_time)[2],by=1))
tmp.pe.raw.outlierCheck<-data.pe.raw.test[test_id==selectedTestId,c("rec_time","t_in","t_out","r_ITO","r_AgNW")]
length(unique(tmp.pe.raw.outlierCheck$rec_time))
tmp.pe.raw.outlierCheck<-tmp.pe.raw.outlierCheck[,.(t_in=mean(t_in,na.rm=TRUE),
                                                    t_out=mean(t_out,na.rm=TRUE),
                                                    r_ITO=mean(r_ITO,na.rm=TRUE),
                                                    r_AgNW=mean(r_AgNW,na.rm=TRUE)),by=rec_time]
data.pe.raw.outlierCheck<-merge(data.pe.raw.outlierCheck,tmp.pe.raw.outlierCheck,all.x=TRUE,by.x="chkId",by.y="rec_time")

if(nrow(data.pe.raw.outlierCheck)%%2!=0){
    data.pe.raw.outlierCheck<-data.pe.raw.outlierCheck%>%rbind(.[nrow(.)])
}
#奇数行似乎有问题

varName<-c("t_in","t_out","r_ITO","r_AgNW")
data.pe.raw.outlierCheck[, paste(rep(varName,each=10),c(0:9),sep="_") := .( as.logical(NA)) ]

#### 滑窗统计每个测量点是否超过阈值 ####
for(name in varName){
    cat("in out loop name=",name,"\n")
    for(i in c(0:(nrow(data.pe.raw.outlierCheck)%/%10 -1))){ #一次批量处理10个（确定滑动次数
        for(j in c(0:9)){ #滑窗起始位置
            selectTerm<-paste(name,j,sep="_") #当前验证的变量及轮次 例如"t_in_9"
            cat("name=",name," i=",i," j=",j,"selectTerm=",selectTerm,"\n")
            rangeVar<-outlierDetector(data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+9)),..name]%>%unlist) #滑窗大小
            nn<-(data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+9)),..name] > rangeVar[1]&
                     data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+9)),..name] < rangeVar[2])#有时候写在一句话里面会报错，尤其是写在data.table里面
            data.pe.raw.outlierCheck[c((i*10+j):(i*10+j+9)), (selectTerm):=nn]
        }
    }
}

for(name in varName){
    cat("\n name=",name)
    nn<-apply(X = data.pe.raw.outlierCheck[,c(paste(..name,c(0:9),sep="_"))],MARGIN = 1,
          FUN = function(x){    sum(x==FALSE,na.rm = TRUE)/sum(!is.na(x)) })
    data.pe.raw.outlierCheck[, paste(name,"OutSum",sep = ""):=nn ][ ,paste(name,"OutFlag",sep = ""):=(nn>0.7) ]
}
outputCol<-c("chkId","t_inOutSum","t_inOutFlag","t_outOutSum","t_outOutFlag","r_ITOOutSum","r_ITOOutFlag","r_AgNWOutSum","r_AgNWOutFlag")
#### 输出结果 ####
data.pe.raw.outlierCheck.out<-data.pe.raw.outlierCheck[,..outputCol]

