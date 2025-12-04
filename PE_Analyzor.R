#### 用于分析数据的脚本 ####
# 包括 相关性分析，拟合等
# 读取已清洗数据
################################################################################

excTestId<-c("AA1_ECS","FA4_ECS") #"AY1_ECS"
selTestId<-c("EA1_ECS","FY1_ECS","FY2_ECS","IY4_ECS","IY5_ECS","CY1_ECS")#,"AY1_ECS"


# 读取清洗后数据
data.pe.post<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_ECS_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE)
data.pe.post.field<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_Field_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE)
stat.pe.scale<-read.xlsx("缩放比例.xlsx",sheetIndex = 1)%>%as.data.table()

data.pe.post<-rbind(data.pe.post,
                    fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_ECS_CY1_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE))

# 部分数据仍需手动修改
# 手动修改后的数据同一个id可以直接合并
data.pe.post[dataset_source%in%c("CY1_ECS_575","CY1_ECS_7286")]$dataset_source<-"CY1_ECS"
data.pe.post[dataset_source%in%c("IY5_ECS_750","IY5_ECS_7213")]$dataset_source<-"IY5_ECS"
data.pe.post[dataset_source=="AY1_ECS"&t_out==37.60711]

# field测试需要重新赋值id
for(i in unique(data.pe.post.field$dataset_source)){
    data.pe.post.field[dataset_source==i]$msg_id<-c(1:(nrow(data.pe.post.field[dataset_source==i])))
}
# 切记EF1测试中，多个test_id已合并，因此会出现msg_id不连续且不单调递增的情况，需要重新赋值

nn<-table(data.pe.post.field[,c("dataset_source","msg_id")])%>%as.data.table

setorder(data.pe.post,dataset_source,msg_id)


#### 清洗数据进一步处理 ####

# 将每个循环数据以转变温度（最低温度）为界分开，确定状态
# 统计每个循环的数据
stat.pe.post.lcst<-data.pe.post[,.(test_id=dataset_source[1],
                                   count=length(resistance),
                                   msg_id=msg_id[resistance==min(resistance,na.rm = TRUE)],
                                   r_min=min(resistance,na.rm = TRUE),
                                   t_out_lcst=t_out[resistance==min(resistance,na.rm = TRUE)][1],
                                   msg_id_norm=msg_id[resistance_norm==min(resistance_norm,na.rm = TRUE)],
                                   r_min_norm=min(resistance_norm,na.rm = TRUE),
                                   t_out_lcst_norm=t_out_norm[resistance_norm==min(resistance_norm,na.rm = TRUE)][1]
                                   ),by=(labelIdCyc=paste(dataset_source,CycleNo,sep="_"))] #也可以直接c("dataset_source","CycleNo")，但是多个辅助变量会方便
stat.pe.post.lcst[,labelIdCyc:=as.character(labelIdCyc)]
stat.pe.post.lcst<-stat.pe.post.lcst[count>300,] #去掉循环中数据过少的部分，通常为最后循环后多出来未删除部分

data.pe.post[,":="(labelIdCyc=paste(dataset_source,CycleNo,sep="_"),status=as.character(NA))]
data.pe.post<-merge(x=data.pe.post,y=stat.pe.post.lcst[,c("labelIdCyc","t_out_lcst_norm")],all.x = TRUE,by="labelIdCyc")
data.pe.post[,status:=ifelse(test = t_out_norm>t_out_lcst_norm, yes = "higher", no = "lower")]

data.pe.post<-data.pe.post[labelIdCyc%in%unique(stat.pe.post.lcst$labelIdCyc)]
table(data.pe.post[,c("labelIdCyc","status")])%>%View

# 清洗后数据可视化批量导出
for(i in unique(data.pe.post$dataset_source)){
    {
        ggsave(filename = paste(i,"_cleaned_norm.png",sep=""),width=13,height = 5,dpi=100,
               plot=#data.pe.post[dataset_source==i
                   ggplot(data = data.pe.post[dataset_source==i,c("msg_id","t_in_norm","t_out_norm","t_env_norm","l_in_norm","l_out_norm","resistance_norm","dataset_source")]%>%
                              melt(.,id.var=c("msg_id","dataset_source")),
                          aes(x=msg_id,y=value,color=variable,lty=variable,group=variable))+geom_line()+
                   labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./1),name = "Temperature"))+facet_wrap(~dataset_source,nrow = 2)+
                   theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))
        )
        ggsave(filename = paste(i,"_cleaned.png",sep=""),width=13,height = 5,dpi=100,
               plot=
                   ggplot(data = data.pe.post[dataset_source==i,c("msg_id","t_in","t_out","t_env","l_in","l_out","resistance")]%>%
                              #.[,r_nor:=scale(resistance)]%>%
                              # .[,":="(t_mid=getMovingAverageValue(((t_in+t_out)/2)*40000,10,onlyPast = FALSE))]%>% .[,c("rec_time","r_ITO_est","t_mid","id")]
                              .[,":="(t_in=(t_in*stat.pe.scale[test_id==i]$scale_temp),t_out=(t_out*stat.pe.scale[test_id==i]$scale_temp),t_env=(t_env*stat.pe.scale[test_id==i]$scale_temp),
                                      l_in=l_in*stat.pe.scale[test_id==i]$scale_illu,l_out=l_out*stat.pe.scale[test_id==i]$scale_illu)]%>%.[,c("msg_id","resistance","t_in","t_out","t_env","l_in","l_out")]%>%
                              melt(.,id.var=c("msg_id")),
                          aes(x=msg_id,y=value,color=variable,lty=variable,group=variable))+geom_line()+
                   labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./stat.pe.scale[test_id==i]$scale_temp),name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
                   theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))
        )
    }
}



# 可视化
boxplot(formula=resistance~dataset_source,data = data.pe.post[!dataset_source %in% c("AA1_ECS","AY1_ECS")])

ggplot(data = data.pe.post.field,#data.pe.post[dataset_source%in%selTestId]
       aes(x=resistance_norm ,y=t_out_norm,color=dataset_source))+geom_point(alpha=0.1,position = "jitter")+#facet_wrap(~dataset_source,nrow=3)+
    labs(y="Resistance",x="Temperature")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(data=data.pe.post[dataset_source==i&CycleNo==20],aes(x=t_out,y=dL_nor))+geom_point(alpha=0.2,position = "jitter")

ggplot(data=data.pe.post.nor,aes(x=Delta_L))+geom_density()
ggplot(data=data.pe.post,aes(x=dL_nor))+geom_density()


ggplot(data=data.pe.post.nor,aes(x=msg_id,y=Delta_L))+geom_point()+facet_wrap(~dataset_source,nrow = 2)
ggplot(data=data.pe.post[dataset_source==i],aes(x=msg_id,y=dL_nor))+geom_point()+facet_wrap(~CycleNo,nrow = 2)

#### 函数拟合 ####
#单独测试
nn<-data.pe.post[dataset_source=="IY5_ECS"&status=="higher"&msg_id>800]
fit.pe.r2l<-glm(Delta_L_norm~resistance_norm+t_out_divPred,data = nn,family = quasibinomial)
nn$predL<-predict(fit.pe.r2l,nn,type = "response")

summary(fit.pe.r2l)
getRSquare(pred=nn$predL,ref = nn$Delta_L_norm)
getMAPE(yPred=nn$predL,yLook = nn$Delta_L_norm)
RMSE(pred=nn$predL,obs = nn$Delta_L_norm)
# 整体 电阻~照度差关系
ggplot(nn,aes(x=resistance_norm,y=Delta_L_norm,color=dataset_source))+geom_point(alpha=0.2,position = "jitter")
# 时序
ggplot(nn)+geom_point(aes(x=msg_id,y=predL,color="Regression"))+geom_point(aes(x=msg_id,y=Delta_L_norm,color="Measurement"))+geom_point(aes(x=msg_id,y=resistance_norm,color="Resistance"))



####################################

#### 批量电阻~温度拟合 ####
data.pe.post[,":="(t_out_divPred=as.numeric(NA),t_out_allPred=as.numeric(NA),labelIdCyc=NULL,labelIdStatus=paste(dataset_source,status,sep="_"))]

# 拟合结果汇总
stat.pe.pred.temp<-data.table(test_id=as.character(NA),count=as.numeric(NA),targetStatus=as.character(NA),
                              rSquare=as.numeric(NA),MAPE=as.numeric(NA),RMSE=as.numeric(NA),
                              rSquare_norm=as.numeric(NA),MAPE_norm=as.numeric(NA),RMSE_norm=as.numeric(NA))[-1]

data.pe.post[,":="(t_out_allPred_denorm=as.numeric(NA),t_out_divPred_denorm=as.numeric(NA))] # denorm部分暂时不可用，反归一化有偏差 251127
for(i in unique(stat.pe.post.lcst$test_id)){
    for(j in c(NA,"higher","lower")){
        if(is.na(j)){k<-"t_out_allPred"}else{k<-"t_out_divPred"}
        data.pe.post[dataset_source==i&(is.na(j)|status==j)]<-data.pe.post[dataset_source==i&(is.na(j)|status==j)]%>%{
            # 当前数据提示
            cat("test_id: ",i,"\tnrow: ",nrow(.),"\tj: ",j,"\tindata: ",unique(.$status),"\toutVar: ",k,"\n")
            fit.pe.r2l<-glm(t_out_norm~resistance_norm,data = .,family = quasibinomial)
            .[[k]]<-predict(fit.pe.r2l,newdata=.,type = "response")
            .[,paste(k,"_denorm",sep = "")]<-denormalize(targetNorm = .[[k]],refReal = .$t_out,refNorm = .$t_out_norm)
            summary(fit.pe.r2l)
            stat.pe.pred.temp<<-rbind(stat.pe.pred.temp,
                                      data.table(test_id=i,count=nrow(.),targetStatus=ifelse(is.na(j),"all",j),
                                                 rSquare=getRSquare(pred=.[[paste(k,"_denorm",sep = "")]],ref = .$t_out),
                                                 MAPE=getMAPE(yPred=.[[paste(k,"_denorm",sep = "")]],yLook = .$t_out),
                                                 RMSE=RMSE(pred=.[[paste(k,"_denorm",sep = "")]],obs = .$t_out,na.rm = TRUE),
                                                 rSquare_norm=getRSquare(pred=.[[k]],ref = .$t_out_norm),
                                                 MAPE_norm=getMAPE(yPred=.[[k]],yLook = .$t_out_norm),
                                                 RMSE_norm=RMSE(pred=.[[k]],obs = .$t_out_norm,na.rm = TRUE)))
            nn1<<-.
            .
        }
    }
}

####################################
#### 反归一化 ####
# group_1对应三个温度，group_2对应照度 前序号对应循环数
# X_original = X_norm * (X_max - X_min) + X_min 0.01-0.99归一化的
# group_1就是三个温度，group_2就是两个l，其他的都是相应的名字
# t_out_denorm = (t_out_norm - 0.01) * (tempRange[2] - tempRange[1]) / 0.98 + tempRange[1]
# 0-22代表的是循环数，相当于每个循环单独归一化，比如22->resistance 就是cycNo=22 电阻值的归一化参数
"0->group_1"

stat.pe.post.denorm<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/scalers_min_max_info.csv",data.table = TRUE)
stat.pe.post.denorm[,data_max:=gsub("[\\[\\]]",'',data_max,perl = TRUE)%>%as.numeric][
    ,data_min:=gsub("[\\[\\]]",'',data_min,perl = TRUE)%>%as.numeric][
        ,std:=gsub("[\\[\\]]",'',std,perl = TRUE)%>%as.numeric]

# stat.pe.post.denorm[,lapply(.SD, function(x){x<-gsub("[\\[\\]]",'',x,perl = TRUE)}),.SDcols=c("data_min","data_max","std")] #不能赋值回去

data.pe.post[,":="(t_out_divPred_denorm=as.numeric(NA),t_out_allPred_denorm=as.numeric(NA),dL_allPred=as.numeric(NA),dL_divPred=as.numeric(NA))]
for(i in unique(data.pe.post$CycleNo)){
    tempRange<-stat.pe.post.denorm[group_path==paste(i,"->","group_1",sep=""),c("data_min","data_max")]%>%unlist
    illumRange<-stat.pe.post.denorm[group_path==paste(i,"->","group_2",sep=""),c("data_min","data_max")]%>%unlist
    data.pe.post[CycleNo==i,":="(t_out_allPred_denorm=(t_out_allPred - 0.01) * (tempRange[2] - tempRange[1]) / 0.98 + tempRange[1],
                                 t_out_divPred_denorm=(t_out_divPred - 0.01) * (tempRange[2] - tempRange[1]) / 0.98 + tempRange[1],
                                 dL_allPred_denorm=(dL_allPred - 0.01) * (illumRange[2] - illumRange[1]) / 0.98 + illumRange[1],
                                 dL_divPred_denorm=(dL_divPred - 0.01) * (illumRange[2] - illumRange[1]) / 0.98 + illumRange[1])]
}

####################################
# 整体评估
stat.pe.pred.temp.sel<-data.table(test_id=as.character(NA),count=as.numeric(NA),targetStatus=as.character(NA),
                                                     rSquare=as.numeric(NA),MAPE=as.numeric(NA),RMSE=as.numeric(NA))[-1]

stat.pe.pred.temp.sel<-data.pe.post[dataset_source%in%selTestId]%>%{
   
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
          data.table(test_id="allSample",count=nrow(.),targetStatus="all",
                                     rSquare=getRSquare(pred=.$t_out_allPred,ref = .$t_out_norm),
                                     MAPE=getMAPE(yPred=.$t_out_allPred,yLook = .$t_out_norm),
                                     RMSE=RMSE(pred=.$t_out_allPred,obs = .$t_out_norm,na.rm = TRUE)
                     )          )
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
          data.table(test_id="allSample",count=nrow(.),targetStatus="div_all",
                     rSquare=getRSquare(pred=.$t_out_divPred,ref = .$t_out_norm),
                     MAPE=getMAPE(yPred=.$t_out_divPred,yLook = .$t_out_norm),
                     RMSE=RMSE(pred=.$t_out_divPred,obs = .$t_out_norm,na.rm = TRUE)
          )          )
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
                                 data.table(test_id="allSample",count=nrow(.),targetStatus="all_denorm",
                                            rSquare=getRSquare(pred=.$t_out_allPred_denorm,ref = .$t_out),
                                            MAPE=getMAPE(yPred=.$t_out_allPred_denorm,yLook = .$t_out),
                                            RMSE=RMSE(pred=.$t_out_allPred_denorm,obs = .$t_out,na.rm = TRUE)
                                 )          )
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
                                 data.table(test_id="allSample",count=nrow(.),targetStatus="div_all_denorm",
                                            rSquare=getRSquare(pred=.$t_out_divPred_denorm,ref = .$t_out),
                                            MAPE=getMAPE(yPred=.$t_out_divPred_denorm,yLook = .$t_out),
                                            RMSE=RMSE(pred=.$t_out_divPred_denorm,obs = .$t_out,na.rm = TRUE)
                                 )          )
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
          data.table(test_id="allSample",count=nrow(.[status=="lower"]),targetStatus="div_lower",
                     rSquare=getRSquare(pred=.[status=="lower"]$t_out_divPred,ref = .[status=="lower"]$t_out_norm),
                     MAPE=getMAPE(yPred=.[status=="lower"]$t_out_divPred,yLook = .[status=="lower"]$t_out_norm),
                     RMSE=RMSE(pred=.[status=="lower"]$t_out_divPred,obs = .[status=="lower"]$t_out_norm,na.rm = TRUE)
          )          )
    stat.pe.pred.temp.sel<-rbind(stat.pe.pred.temp.sel,
          data.table(test_id="allSample",count=nrow(.[status=="higher"]),targetStatus="div_higher",
                     rSquare=getRSquare(pred=.[status=="higher"]$t_out_divPred,ref = .[status=="higher"]$t_out_norm),
                     MAPE=getMAPE(yPred=.[status=="higher"]$t_out_divPred,yLook = .[status=="higher"]$t_out_norm),
                     RMSE=RMSE(pred=.[status=="higher"]$t_out_divPred,obs = .[status=="higher"]$t_out_norm,na.rm = TRUE)
          )          )
    stat.pe.pred.temp.sel
}




# 原始时序数据
ggplot(data.pe.post[dataset_source%in% selTestId],aes(x=msg_id))+geom_point(aes(x=msg_id,y=l_out_norm,color="origin"))+geom_point(aes(x=msg_id,y=l_in_norm,color="allPred",lty="dash"))+geom_point(aes(x=msg_id,y=resistance_norm,color="resistance",lty="dash"))+
    geom_line(aes(x=msg_id,y=t_out_allPred,color="allPred",lty="dash"))+geom_line(aes(x=msg_id,y=predL,color="divPred",lty="dash"))+facet_wrap(.~dataset_source)

ggplot(data.pe.post[dataset_source%in% selTestId])+
    geom_line(aes(x=msg_id,y=t_out_norm,color="Measurement"),lty="solid",size=1)+geom_line(aes(x=msg_id,y=t_out_divPred,color="Regression"),lty="dashed",size=0.75)+
    facet_wrap(.~dataset_source,nrow=3)+labs(y="Temperature",x="Time (s)")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

# 仅对比
ggplot(data.pe.post[dataset_source%in%selTestId],aes(x=t_out_norm))+
    geom_point(aes(x=t_out_norm,y=t_out_divPred),position = "jitter",color="blue",alpha=0.03)+geom_line(aes(x=t_out_norm,y=t_out_norm),color="red",lty="dashed",size=1)+
    facet_wrap(.~dataset_source,nrow=3)+labs(y="Temperature_Regression",x="Temperature_Measurement")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))




####################################

#### 批量电阻~照度拟合 ####
data.pe.post[,":="(dL_divPred=as.numeric(NA),dL_allPred=as.numeric(NA))]

# 拟合结果汇总
stat.pe.pred.illum<-data.table(test_id=as.character(NA),count=as.numeric(NA),targetStatus=as.character(NA),
                              rSquare=as.numeric(NA),MAPE=as.numeric(NA),RMSE=as.numeric(NA))[-1]
for(i in unique(stat.pe.post.lcst$test_id)){
    for(j in c(NA,"higher","lower")){
        if(is.na(j)){k<-"dL_allPred"}else{k<-"dL_divPred"}
        data.pe.post[dataset_source==i&(is.na(j)|status==j)]<-data.pe.post[dataset_source==i&(is.na(j)|status==j)]%>%{
            # 当前数据提示
            cat("test_id: ",i,"\tnrow: ",nrow(.),"\tj: ",j,"\tindata: ",unique(.$status),"\toutVar: ",k,"\n")
            fit.pe.r2l<-glm(Delta_L_norm~resistance_norm,data = .,family = quasibinomial)
            .[[k]]<-predict(fit.pe.r2l,newdata=.,type = "response")
            summary(fit.pe.r2l)
            stat.pe.pred.illum<<-rbind(stat.pe.pred.illum,
                                      data.table(test_id=i,count=nrow(.),targetStatus=ifelse(is.na(j),"all",j),
                                                 rSquare=getRSquare(pred=.[[k]],ref = .$Delta_L_norm),
                                                 MAPE=getMAPE(yPred=.[[k]],yLook = .$Delta_L_norm),
                                                 RMSE=RMSE(pred=.[[k]],obs = .$Delta_L_norm,na.rm = TRUE)))
            nn1<<-.
            .
        }
    }
}

# 整体评估
stat.pe.pred.illum.sel<-data.table(test_id=as.character(NA),count=as.numeric(NA),targetStatus=as.character(NA),
                                  rSquare=as.numeric(NA),MAPE=as.numeric(NA),RMSE=as.numeric(NA))[-1]
# 需要修改加到循环里代码会好看
stat.pe.pred.illum.sel<-data.pe.post[dataset_source%in%selTestId& dataset_source!="CY1_ECS"]%>%{
    
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                 data.table(test_id="allSample",count=nrow(.),targetStatus="all",
                                            rSquare=getRSquare(pred=.$dL_allPred,ref = .$Delta_L_norm),
                                            MAPE=getMAPE(yPred=.$dL_allPred,yLook = .$Delta_L_norm),
                                            RMSE=RMSE(pred=.$dL_allPred,obs = .$Delta_L_norm,na.rm = TRUE)
                                 )          )
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                  data.table(test_id="allSample",count=nrow(.),targetStatus="div_all",
                                             rSquare=getRSquare(pred=.$dL_divPred,ref = .$Delta_L_norm),
                                             MAPE=getMAPE(yPred=.$dL_divPred,yLook = .$Delta_L_norm),
                                             RMSE=RMSE(pred=.$dL_divPred,obs = .$Delta_L_norm,na.rm = TRUE)
                                  )          )
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                  data.table(test_id="allSample",count=nrow(.),targetStatus="all_denorm",
                                             rSquare=getRSquare(pred=.$dL_allPred_denorm,ref = .$Delta_L),
                                             MAPE=getMAPE(yPred=.$dL_allPred_denorm,yLook = .$Delta_L),
                                             RMSE=RMSE(pred=.$dL_allPred_denorm,obs = .$Delta_L,na.rm = TRUE)
                                  )          )
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                 data.table(test_id="allSample",count=nrow(.),targetStatus="div_all_denorm",
                                            rSquare=getRSquare(pred=.$dL_divPred_denorm,ref = .$Delta_L),
                                            MAPE=getMAPE(yPred=.$dL_divPred_denorm,yLook = .$Delta_L),
                                            RMSE=RMSE(pred=.$dL_divPred_denorm,obs = .$Delta_L,na.rm = TRUE)
                                 )          )
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                 data.table(test_id="allSample",count=nrow(.[status=="lower"]),targetStatus="div_lower",
                                            rSquare=getRSquare(pred=.[status=="lower"]$dL_divPred,ref = .[status=="lower"]$Delta_L_norm),
                                            MAPE=getMAPE(yPred=.[status=="lower"]$dL_divPred,yLook = .[status=="lower"]$Delta_L_norm),
                                            RMSE=RMSE(pred=.[status=="lower"]$dL_divPred,obs = .[status=="lower"]$Delta_L_norm,na.rm = TRUE)
                                 )          )
    stat.pe.pred.illum.sel<-rbind(stat.pe.pred.illum.sel,
                                 data.table(test_id="allSample",count=nrow(.[status=="higher"]),targetStatus="div_higher",
                                            rSquare=getRSquare(pred=.[status=="higher"]$dL_divPred,ref = .[status=="higher"]$Delta_L_norm),
                                            MAPE=getMAPE(yPred=.[status=="higher"]$dL_divPred,yLook = .[status=="higher"]$Delta_L_norm),
                                            RMSE=RMSE(pred=.[status=="higher"]$dL_divPred,obs = .[status=="higher"]$Delta_L_norm,na.rm = TRUE)
                                 )          )
    stat.pe.pred.illum.sel
}


ggplot(data.pe.post[dataset_source%in% selTestId & dataset_source!="CY1_ECS"],aes(x=msg_id))+
    geom_point(aes(x=msg_id,y=l_out_norm,color="origin"))+geom_point(aes(x=msg_id,y=l_in_norm,color="allPred",lty="dash"))+
    geom_point(aes(x=msg_id,y=resistance_norm,color="resistance",lty="dash"))+
    geom_line(aes(x=msg_id,y=t_out_allPred,color="allPred",lty="dash"))+geom_line(aes(x=msg_id,y=predL,color="divPred",lty="dash"))+facet_wrap(.~dataset_source)

ggplot(data.pe.post[dataset_source%in% selTestId& dataset_source!="CY1_ECS"])+
    geom_line(aes(x=msg_id,y=Delta_L_norm,color="Measurement"),lty="solid",size=1)+geom_line(aes(x=msg_id,y=dL_divPred,color="Regression"),lty="dashed",size=0.75)+
    facet_wrap(.~dataset_source,nrow=3)+labs(y="Illuminance",x="Time (s)")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

# 仅对比
ggplot(data.pe.post[dataset_source%in%selTestId& dataset_source!="CY1_ECS"],aes(x=Delta_L_norm,y=dL_divPred))+
    stat_density2d(aes(fill=..density..),geom="tile",contour=FALSE) +scale_fill_gradient(low="blue", high="red")+
    # geom_point(position = "jitter",alpha=0.01,color="red")+
    geom_line(aes(x=Delta_L_norm,y=Delta_L_norm),color="red",lty="dashed",size=1)+
    labs(y="Illuminance difference_Regression",x="Illuminance difference_Measurement")+#facet_wrap(.~dataset_source,nrow=3)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))





# 相关性分析
stat.pe.post.cor<-list()
for(j in c("lower","higher")){
    for(i in c(selTestId)){
        stat.pe.post.cor[[j]][[i]]<-cor(x=data.pe.post[dataset_source==i&status==j,
                                                                              c("resistance_norm","t_out_norm","Delta_L_norm")],method = "spearman",use="na.or.complete")
        stat.pe.post.cor[["all"]][[i]]<-cor(x=data.pe.post[dataset_source==i&!is.na(status),c("resistance_norm","t_out_norm","Delta_L_norm")],method = "spearman",use="na.or.complete")
        
        }
    stat.pe.post.cor[[j]][["all"]]<-cor(x=data.pe.post[status==j&dataset_source%in%selTestId,c("resistance_norm","t_out_norm","Delta_L_norm")],method = "spearman",use="na.or.complete")
}
stat.pe.post.cor[["all"]][["all"]]<-cor(x=data.pe.post[dataset_source%in%selTestId&!is.na(status),c("resistance_norm","t_out_norm","Delta_L_norm")],method = "spearman",use="na.or.complete")

corrplot(stat.pe.post.cor[["lower"]][["FY1_ECS"]],tl.col = "black", tl.cex = 0.8, tl.srt = 45,method="ellipse",type = "upper",tl.pos = "lt")
corrplot(stat.pe.post.cor[["lower"]][["FY1_ECS"]],tl.col = NULL, tl.cex = 0.8,tl.pos = "n", tl.srt = 45,method="number",type = "lower",add=TRUE)


data.pe.post<-mutate(data.pe.post,na.approx)

nn<-data.pe.post[,lapply(.SD,na.approx),.SDcols=c( "t_in","t_out","t_env","l_in","l_out","resistance")]
