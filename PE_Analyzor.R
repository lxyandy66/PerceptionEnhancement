#### 用于分析数据的脚本 ####
# 读取已清洗数据
################################################################################

excTestId<-c("AA1_ECS","FA4_ECS","IY5_ECS_750")


# 读取清洗后数据
data.pe.post<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_ECS_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE)
data.pe.post.field<-fread("/Volumes/Stroage/PercepetionEnhancement_Share/PE_PostProcessData/combined_cleaned_data_Field_with_split_merged_cycle_normalized_combined.csv",data.table = TRUE)
stat.pe.scale<-read.xlsx("缩放比例.xlsx",sheetIndex = 1)%>%as.data.table()


# field测试需要重新赋值id
for(i in unique(data.pe.post.field$dataset_source)){
    data.pe.post.field[dataset_source==i]$msg_id<-c(1:(nrow(data.pe.post.field[dataset_source==i])))
}

setorder(data.pe.post,dataset_source,msg_id)







# 清洗后数据可视化批量导出
for(i in unique(data.pe.post$dataset_source)){
    {
        ggsave(filename = paste(i,"_cleaned_norm.png",sep=""),width=13,height = 5,dpi=100,
               plot=
                   ggplot(data = data.pe.post[dataset_source==i,c("msg_id","t_in_norm","t_out_norm","t_env_norm","l_in_norm","l_out_norm","resistance_norm")]%>%
                              melt(.,id.var=c("msg_id")),
                          aes(x=msg_id,y=value,color=variable,lty=variable,group=variable))+geom_line()+
                   labs(y="Resistance")+scale_y_continuous(sec.axis = sec_axis(~(./1),name = "Temperature"))+#facet_wrap(~test_id,nrow = 2)+
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

# stat.pe.scale[test_id==i]$scale_illu
# stat.pe.scale[test_id==i]$scale_temp

# 可视化
boxplot(formula=resistance~dataset_source,data = data.pe.post[!dataset_source %in% c("AA1_ECS","AY1_ECS")])

ggplot(data = data.pe.post[!dataset_source%in%excTestId],
       aes(x=resistance_norm ,y=Delta_L_norm,color=dataset_source))+geom_point(alpha=0.2,position = "jitter")+facet_wrap(~dataset_source,nrow=3)+
    labs(y="Resistance",x="Temperature")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(data=data.pe.post[dataset_source==i&CycleNo==20],aes(x=t_out,y=dL_nor))+geom_point(alpha=0.2,position = "jitter")

ggplot(data=data.pe.post.nor,aes(x=Delta_L))+geom_density()
ggplot(data=data.pe.post,aes(x=dL_nor))+geom_density()


ggplot(data=data.pe.post.nor,aes(x=msg_id,y=Delta_L))+geom_point()+facet_wrap(~dataset_source,nrow = 2)
ggplot(data=data.pe.post[dataset_source==i],aes(x=msg_id,y=dL_nor))+geom_point()+facet_wrap(~CycleNo,nrow = 2)

# 函数拟合
nn<-data.pe.post[dataset_source==i]
fit.pe.r2l<-glm(t_out_norm~resistance_norm,data = nn,family = quasibinomial)
nn$predL<-predict(fit.pe.r2l,nn,type = "response")

summary(fit.pe.r2l)
getRSquare(pred=nn$predL,ref = nn$t_out_norm)
getMAPE(yPred=nn$predL,yLook = nn$t_out_norm)
RMSE(pred=nn$predL,obs = nn$t_out_norm)


# 整体 电阻~照度差关系
ggplot(nn,aes(x=resistance_norm,y=t_out_norm,color=dataset_source))+geom_point(alpha=0.2,position = "jitter")

# 原始时序数据
ggplot(nn,aes(x=msg_id))+geom_line(aes(x=msg_id,y=t_out_norm,color="blue"))+geom_line(aes(x=msg_id,y=predL,color="red",lty="dash"))
# 仅对比
ggplot(nn,aes(x=t_out_norm))+geom_point(aes(x=t_out_norm,y=predL,color="blue"),alpha=0.2,position = "jitter")+geom_line(aes(x=t_out_norm,y=t_out_norm,color="red"))



# 电阻的批量标准化
for(i in unique(data.pe.post$dataset_source)){
    data.pe.post[dataset_source==i,
                 ":="(r_nor=normalize(resistance,upper = 0.9,lower = 0.1,intercept = 0.1),
                 dL_nor=normalize(Delta_L,upper = 0.9,lower = 0.1,intercept = 0.1)),
                 by=dataset_source]
}



# 相关性分析
cor(data.pe.post[dataset_source=="EA1_ECS",c("t_in","t_out","resistance")],method = "spearman")


data.pe.post<-mutate(data.pe.post,na.approx)

nn<-data.pe.post[,lapply(.SD,na.approx),.SDcols=c( "t_in","t_out","t_env","l_in","l_out","resistance")]
