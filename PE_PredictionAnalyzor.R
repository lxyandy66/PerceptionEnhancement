#### 用于分析预测数据的脚本 ####
# 读取预测结果数据

################################################################################
data.pe.predict<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_Result/DataForTL_Demo/test_results_1201_Tout_delta_r_NormByTypeByCycleProLin/prediction_results.csv",data.table = TRUE)

for(i in unique(data.pe.predict$dataset_source)){
    data.pe.predict[dataset_source==i]$msg_id<-c(1:(nrow(data.pe.predict[dataset_source==i])))
}
# 切记EF1测试中，多个test_id已合并，因此会出现msg_id不连续且不单调递增的情况，需要重新赋值

#数据可视化
data.pe.predict[msg_id>12500&dataset_source=="EF1_1030_Field",c("msg_id","Delta_L","Rate_L","Predicted_Rate_L")]%>%View
ggplot(data=data.pe.predict)+geom_point(aes(x=msg_id,y=Delta_L,shape="Delta_L"),color="red")+
    geom_point(aes(x=msg_id,y=Predicted_Rate_L*10000,shape="Predicted"),color="blue")+
    geom_point(aes(x=msg_id,y=Rate_L*10000,shape="Measured"),color="green")+
    facet_wrap(.~dataset_source,nrow = 2)

ggplot(data.pe.predict[msg_id<15000&msg_id>12500&dataset_source=="EF1_1030_Field"],aes(x=Rate_L))+
    geom_point(aes(x=Rate_L,y=Predicted_Rate_L),position = "jitter",color="blue",alpha=1)+geom_line(aes(x=Rate_L,y=Rate_L),color="red",lty="dashed",size=1)+
    facet_wrap(.~dataset_source,nrow=3)+#labs(y="Temperature_Regression",x="Temperature_Measurement")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

