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

ggplot(data=data.pe.ecs.post.tin[source_folder=="FY2_ECS"],aes(x=(t_in+t_out)/2,y=resistance,color=msg_id,size=as.factor(isHeating)))+
    geom_point(position="jitter")+facet_wrap(~source_folder,nrow=3)

data.pe.ecs.pickup<-data.pe.ecs.pickup[,c(1:14,42,43,46)]
write.csv(data.pe.ecs.post[,c(1:14,42,43,46)],file="PE_ECS_Full.csv",row.names = FALSE,na = "")
write.csv(data.pe.ecs.pickup,file="PE_ECS_PickUp.csv",row.names = FALSE,na = "")


#### Agent选取数据可视化 ####

data.pe.ecs.post.tin.draft<-fread("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/AgentDataProcessor/过程可视化文件/annotated_temp.csv",data.table=TRUE)
data.pe.ecs.post.rateL.draft<-fread("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/AgentDataProcessor/过程可视化文件/annotated_rateL.csv",data.table=TRUE)


for(i in unique(data.pe.ecs.post.tin.draft$source_folder)){
    write.csv(data.pe.ecs.post.tin.draft[source_folder==i&kept15,-c("kept15","temp_bin")],file=paste0(i,"_Tave_forFig.csv"),row.names = FALSE,na = "")
}
# 手动修改
data.pe.ecs.tave.draft<-fread(paste0("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/",i,"_ECS_Tave_forFig.csv"),data.table=TRUE)[0]
for (i in c("AA1","EA1","FY1","FY2","IY5","IY4","CY1")){
    data.pe.ecs.tave.draft<-rbind(data.pe.ecs.tave.draft,fread(paste0("/Users/Mr_Li/Documents/博后课题项目/PerceptionEnhancement/",i,"_ECS_Tave_forFig.csv"),data.table=TRUE))
}

# data.pe.ecs.tave.draft<-data.pe.ecs.tave.draft[source_folder!="EA1_ECS"]
data.pe.ecs.tave.mid<-data.pe.ecs.tave.draft[,.(source_folder=source_folder[1],
                                                temp_bin_ctr=temp_bin_ctr[1],
                                                resistance=mean(resistance,na.rm=TRUE)
                                                ),by=(labelSampleTbin=paste0(source_folder,temp_bin_ctr))]
setorder(data.pe.ecs.tave.mid,source_folder,temp_bin_ctr)
for(i in unique(data.pe.ecs.tave.mid$source_folder)){
    write.csv(data.pe.ecs.tave.mid[source_folder==i],file=paste0(i,"_Tmid_Line_forFig.csv"),row.names = FALSE,na = "")
}

fit<-lm(resistance~temp_bin_ctr,data = data.pe.ecs.tave.mid[source_folder=="CY1_ECS_7286"&temp_bin_ctr>=35.25 ])
summary(fit)

#### RateL处理 ####
data.pe.ecs.rateL.mid<-data.pe.ecs.post.rateL.draft[,.(source_folder=source_folder[1],
                                                rateL_res_bin=rateL_res_bin[1], # X坐标，电阻的分箱
                                                resistance=mean(resistance,na.rm=TRUE),
                                                Rate_L_norm=mean(Rate_L_norm,na.rm=TRUE) #透光率
                                                ),by=(labelSampleRateLbin=paste0(source_folder,rateL_res_bin))]

setorder(data.pe.ecs.tave.mid,source_folder,temp_bin_ctr)

ggplot(data.pe.ecs.post.rateL.draft[source_folder=="AA1_ECS"],aes(x=resistance,y=Rate_L_norm,color=isHeating))+geom_point()

ggplot(data.pe.ecs.rateL.mid[source_folder=="AA1_ECS"],aes(x=rateL_res_bin,y=Rate_L_norm))+geom_point()

fit.pe.ecs.r2l<-glm(Rate_L_norm~rateL_res_bin,data = data.pe.ecs.rateL.mid[source_folder=="AA1_ECS"],family = quasibinomial)
summary(fit.pe.ecs.r2l)
PseudoR2(fit.pe.ecs.r2l)
