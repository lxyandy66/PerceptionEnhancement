#### 用于分析预测数据的脚本 ####
# 读取预测结果数据

################################################################################
data.pe.predict<-fread("/Users/Mr_Li/Documents/博后一/专利_Transfer Learning/Code_ECS所有样品参与测试训练集/EF_Test/all_predictions_20260313_155854.csv",data.table = TRUE)

# test_metrics_20260708_202315.csv

data.pe.predict<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/Transfer_260708/train_predictions_20260708_202314.csv",data.table = TRUE)%>%cbind(data.table("type"="train"))
data.pe.predict<-rbind(data.pe.predict,
                       fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/Transfer_260708/test_predictions.csv",data.table = TRUE)%>%cbind(data.table("type"="test")))

data.pe.ecs<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/TL_Package_250423/Result/ECS/pretrain_predictions_train.csv",data.table = TRUE)%>%cbind(data.table("type"="train"))
data.pe.ecs<-rbind(data.pe.ecs,
                   fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/TL_Package_250423/Result/ECS/pretrain_predictions_test.csv",data.table = TRUE)%>%cbind(data.table("type"="test")))


names(data.pe.predict)[15]
names(data.pe.predict)[15]<-"dataset_source"

data.pe.predict[,rec_time:=as.POSIXct(rec_time)]
setorder(data.pe.predict,rec_time)

# 分钟级数据处理
data.pe.predict.min<-data.pe.predict[,.(
    Rate_L_pred_normalized=mean(Rate_L_pred_normalized,na.rm=TRUE)),by=(TimeMin=format(as.POSIXct(rec_time),format="%Y-%m-%d %H:%M"))]

for(i in unique(data.pe.predict$CycleNo)){#有时是source_folder dataset_source
    data.pe.predict$msg_id<-c(1:(nrow(data.pe.predict)))
}
# 切记EF1测试中，多个test_id已合并，因此会出现msg_id不连续且不单调递增的情况，需要重新赋值

#数据可视化
data.pe.predict[msg_id>12500&dataset_source=="EF1_1030_Field",c("msg_id","Delta_L","Rate_L","Predicted_Rate_L")]%>%View
ggplot(data=data.pe.predict)+#geom_point(aes(x=msg_id,y=Delta_L,shape="Delta_L"),color="red")+,color="blue",color="green"
    geom_line(aes(x=msg_id,y=Predicted_t_out,color="Inference"),size=1)+
    geom_line(aes(x=msg_id,y=t_out,color="Measurement"),size=0.5)+labs(y="Temperature",x="Time")+
    # geom_point(aes(x=msg_id,y=resistance_norm*50,color="Resistance"))+
    # facet_wrap(.~dataset_source+CycleNo,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))


ggplot(data=data.pe.predict)+#geom_point(aes(x=msg_id,y=Delta_L,shape="Delta_L"),color="red")+,color="blue",color="green"
    geom_line(aes(x=msg_id,y=Predicted_Rate_L,color="Inference"),size=1)+
    geom_line(aes(x=msg_id,y=Rate_L,color="Measurement"),size=0.5)+labs(y="Transparency",x="Time")+
    # geom_point(aes(x=msg_id,y=resistance_norm,color="Resistance"))+
    # facet_wrap(.~dataset_source,nrow = 2)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

# ggplot(data=(data.pe.predict[,c("msg_id","Predicted_Rate_L","Rate_L")]%>%melt(id.var="msg_id")),aes(x=msg_id,y=value,color=variable))+geom_point()

nn<-rbind(data.pe.predict[,c("t_out","Predicted_t_out","Rate_L","Predicted_Rate_L")],data.pe.predict.ef[,c("t_out","Predicted_t_out","Rate_L","Predicted_Rate_L")])

getRSquare(pred=nn$Predicted_Rate_L,ref = nn$Rate_L)
getMAPE(yPred=nn$Predicted_Rate_L,yLook = nn$Rate_L)
RMSE(pred=nn$Predicted_Rate_L,obs = nn$Rate_L)

ggplot(data.pe.ecs,
       aes(x=rate_l_true,y=rate_l_pred))+
    stat_density2d(aes(fill=..density..),geom="tile",contour=FALSE) +scale_fill_gradient(low="#FFFFFF", high="#F8766D")+
    # geom_point(position = "jitter",alpha=0.01,color="#F8766D")+00BFC4
    geom_line(aes(x=rate_l_true,y=rate_l_true),color="red",lty="dashed",size=1)+
    labs(y="Transparency Inference",x="Transparency Measurement")+#facet_wrap(.~dataset_source,nrow=3)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))
ggplot(data.pe.ecs,aes(x=t_out_true))+
    geom_point(aes(x=t_out_true,y=t_out_pred),position = "jitter",color="#00BFC4",alpha=0.1,size=0.5)+geom_line(aes(x=t_out_true,y=t_out_true),color="#F8766D",lty="dashed",size=1)+
    labs(y="Temperature Inference",x="Temperature Measurement")+
    # facet_wrap(.~dataset_source,nrow=3)+#labs(y="Temperature_Regression",x="Temperature_Measurement")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(rbind(data.pe.predict[,c("Rate_L","Predicted_Rate_L")],data.pe.predict.ef[,c("Rate_L","Predicted_Rate_L")]),
       aes(x=Rate_L,y=Predicted_Rate_L))+
    stat_density2d(aes(fill=..density..),geom="tile",contour=FALSE) +scale_fill_gradient(low="#FFFFFF", high="#F8766D")+
    # geom_point(position = "jitter",alpha=0.01,color="#F8766D")+
    geom_line(aes(x=Rate_L,y=Rate_L),color="red",lty="dashed",size=1)+
    labs(y="Transparency Inference",x="Transparency Measurement")+#facet_wrap(.~dataset_source,nrow=3)+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(rbind(data.pe.predict[,c("t_out","Predicted_t_out")],data.pe.predict.ef[,c("t_out","Predicted_t_out")]),aes(x=t_out))+
    geom_point(aes(x=t_out,y=Predicted_t_out),position = "jitter",color="#00BFC4",alpha=0.01,size=0.5)+geom_line(aes(x=t_out,y=t_out),color="#F8766D",lty="dashed",size=1)+
    labs(y="Temperature Inference",x="Temperature Measurement")+
    # facet_wrap(.~dataset_source,nrow=3)+#labs(y="Temperature_Regression",x="Temperature_Measurement")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

