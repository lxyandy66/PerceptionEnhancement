#### 用于文章中特征分析的脚本 ####
#### 处理电化学工作站后处理数据 ####
data.pe.ecs.post<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_ECS_with_split_merged_cycle_normalized_combined.csv",
                        data.table = TRUE)
data.pe.ecs.post<-rbind(data.pe.ecs.post,fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/CY1单独_combined_cleaned_data_ECS_CY1_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE))
#原始数据点太多了，每个样品抽样 
# 作废，让AI弄
list.pe.ecs.pickup<-list() #储存每个sample的随机数
data.pe.ecs.pickup<-data.pe.ecs.post[0] #储存每个sample用于画图的数据
for(i in unique(data.pe.ecs.post$source_folder)){
    list.pe.ecs.pickup[[i]]<-sample(nrow(data.pe.ecs.post[source_folder==i]),size = as.integer(nrow(data.pe.ecs.post[source_folder==i])*0.02))
    data.pe.ecs.pickup<-rbind(data.pe.ecs.pickup,data.pe.ecs.post[source_folder==i][list.pe.ecs.pickup[[i]]]) #_revXY
    ggsave(filename = paste(i,"_temp_pickup.png",sep=""),width=5,height = 5,dpi=100,
           plot =ggplot(data=data.pe.ecs.post[source_folder==i][list.pe.ecs.pickup[[i]]],aes(x=t_in,y=resistance))+
               geom_point(position="jitter")+facet_wrap(~source_folder,nrow=3))
    ggsave(filename = paste(i,"_ratel_pickup.png",sep=""),width=5,height = 5,dpi=100,
           plot =ggplot(data=data.pe.ecs.post[source_folder==i][list.pe.ecs.pickup[[i]]],aes(x=Rate_L_norm,y=resistance))+
               geom_point(position="jitter")+facet_wrap(~source_folder,nrow=3))
    
}


data.pe.ecs.post.tin
ggplot(data=data.pe.ecs.post.tin[source_folder=="FY2_ECS"],aes(x=(t_in+t_out)/2,y=resistance,color=msg_id,size=as.factor(isHeating)))+
    geom_point(position="jitter")+facet_wrap(~source_folder,nrow=3)

data.pe.ecs.pickup<-data.pe.ecs.pickup[,c(1:14,42,43,46)]
write.csv(data.pe.ecs.post[,c(1:14,42,43,46)],file="PE_ECS_Full.csv",row.names = FALSE,na = "")
write.csv(data.pe.ecs.pickup,file="PE_ECS_PickUp.csv",row.names = FALSE,na = "")


#### Agent选取数据可视化 ####
data.pe.ecs.post.tin<-fread("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/AgentDataProcessor/PE_ECS_Full_Tin_REV.csv",data.table=TRUE)
data.pe.ecs.post.rateL<-fread("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/AgentDataProcessor/过程可视化文件/selected_plot_data_rateL.csv",data.table=TRUE)
table(data.pe.ecs.post.rateL$source_folder)
ggplot(data=data.pe.ecs.post.rateL,aes(x=resistance_norm,y=Rate_L_norm))+
    geom_point(position="jitter")+facet_wrap(~source_folder,nrow=3)
