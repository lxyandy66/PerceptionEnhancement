#### 用于处理EnergyPlus进行能耗模拟的脚本 ####

# │    列     │ 单位 │                  说明                   │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ TimeIndex │ min  │ 模拟时间步（1=10/30 00:01, 585=09:45）  │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Tzone_C   │ °C   │ 房间空气温度                            │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Q_sol_W   │ W    │ 南窗透射太阳辐射（短波直射）            │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Q_gain_W  │ W    │ 南窗总得热（透射 + 玻璃吸收后向内传导） │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Tw_in_C   │ °C   │ 南窗内表面温度                          │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Tw_out_C  │ °C   │ 南窗外表面温度                          │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ I_inc_Wm2 │ W/m² │ 南窗外表面入射太阳辐射强度              │
# ├───────────┼──────┼─────────────────────────────────────────┤
# │ Tsol      │ W/m² │ 透明度                                  │


################################################################################
data.pe.energysim.native.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/Demo_EnergySimulationData/MidOffice_Eplus_Native.csv",
                                    data.table = TRUE)%>%.[,":="(type="native",datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"))]
data.pe.energysim.switch.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/Demo_EnergySimulationData/MidOffice_HIL_13state.csv",
                                    data.table = TRUE)%>%.[,":="(type="switch",datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"))]

# 不同策略批量导入
data.pe.energysim.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/EnergySimResults_0708/Eplus_Native.csv",
                             data.table = TRUE)%>%.[0,":="(datetime=as.POSIXct("2026-07-08 00:00:00"),source="null")]%>%.[0]

for( i in list.files("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/EnergySimResults_0708")){
    data.pe.energysim.raw<-rbind(data.pe.energysim.raw,
                                 fread(file = paste0("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/EnergySimResults_0708/",i),
                                                             data.table = TRUE)%>%
                                     .[,":="(datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"),source=paste0(i))])
}
data.pe.energysim.raw[,source:=gsub('.csv','',source)]
# 合并所有窗负荷
# 思路：
# Q直接相加
data.pe.energysim.raw[,":="(T_zone_ave_C=(Tzone_Bot_C+Tzone_Mid_C+Tzone_Top_C)/3,
                            Q_sol_sum_W=(Q_sol_W1_W+Q_sol_W2_W+Q_sol_W3_W+Q_sol_W4_W+Q_sol_W5_W),
                            Q_gain_sum_W=(Q_gain_W1_W+Q_gain_W2_W+Q_gain_W3_W+Q_gain_W4_W+Q_gain_W5_W))]
# > names(data.pe.energysim.raw)
# [1] "TimeIndex"    "Tzone_Bot_C"  "Tzone_Mid_C"  "Tzone_Top_C"  "Q_sol_W1_W"   "Q_gain_W1_W"  "Tw_in_W1_C"   "I_inc_W1_Wm2" "Q_sol_W2_W"  
# [10] "Q_gain_W2_W"  "Tw_in_W2_C"   "I_inc_W2_Wm2" "Q_sol_W3_W"   "Q_gain_W3_W"  "Tw_in_W3_C"   "I_inc_W3_Wm2" "Q_sol_W4_W"   "Q_gain_W4_W" 
# [19] "Tw_in_W4_C"   "I_inc_W4_Wm2" "Q_sol_W5_W"   "Q_gain_W5_W"  "Tw_in_W5_C"   "I_inc_W5_Wm2" "Cooling_J"    "Heating_J"    "datetime"    
# [28] "source"       "Q_sol_sum_W"  "Q_gain_sum_W" "T_zone_ave_C"
data.pe.energysim.hour<-data.pe.energysim.raw[,labelHourSource:=paste0(format(datetime,format="%Y-%m-%d_%H"),"_",source)][
    ,.(datetime=datetime[1],
       source=source[1],
       T_zone_ave_C=mean(T_zone_ave_C,na.rm=TRUE),
       Q_sol_sum_W=mean(Q_sol_sum_W,na.rm=TRUE),
       Q_gain_sum_W=mean(Q_gain_sum_W,na.rm=TRUE),
       I_inc_W5_Wm2=mean(I_inc_W5_Wm2,na.rm=TRUE),
       Cooling_J=sum(Cooling_J,na.rm = TRUE)/3600000 #转换为kwh
       ),by=labelHourSource][,labelHourSource:=NULL]

ggplot(data.pe.energysim.hour[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&source%in%c("HIL","Predict","Hot_fixed","Cold_fixed"),
                              c("datetime","source","Cooling_J")],aes(x=datetime,y=Cooling_J,color=source,shape=source))+geom_line()+geom_point()+
    labs(y="HVAC energy consumption (kW)",x="Time")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

data.pe.energysim.hour[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&source%in%c("HIL","Predict","Hot_fixed","Cold_fixed"),
                       c("datetime","source","T_zone_ave_C")]%>%dcast(.,datetime~source)%>%as.data.table%>%.[,.(HIL2Pred=abs(HIL-Predict),
                                                                                                HIL2Cold=abs(HIL-Cold_fixed),
                                                                                                HIL2Hot=abs(HIL-Hot_fixed)),by=datetime]%>%View

# &source%in%c("HIL","Predict")

################

# > names(data.pe.energysim.native.raw)
# [1] "TimeIndex"   "Tzone_C"     "Q_sol_W"     "Q_gain_W"    "Tw_in_C"     "Tw_out_C"    "I_inc_Wm2"  
# [8] "TC_Spec_T_C" "Tsol_TC"     "type"        "datetime"
names(data.pe.energysim.switch.raw)[8]<-"Tsol_TC"

data.pe.energysim.combined<-rbind(data.pe.energysim.native.raw[,c("datetime","Q_gain_W","type")],
                                  data.pe.energysim.switch.raw[,c("datetime","Q_gain_W","type")])
ggplot(data = data.pe.energysim.combined,aes(x=datetime,y= Q_gain_W ,color=type))+geom_line()


#### 批量合并并处理 ####
data.pe.energysim.native.long<-melt(data.pe.energysim.native.raw[,-c("type","TC_Spec_T_C")],id.vars = c("TimeIndex","datetime"))%>%
    .[,labelMinutesVar:=paste(TimeIndex,variable,sep="_")]
names(data.pe.energysim.native.long)[4]<-"native" #计算方法名字
data.pe.energysim.switch.long<-melt(data.pe.energysim.switch.raw[,-c("type")],id.vars = c("TimeIndex","datetime"))%>%
    .[,labelMinutesVar:=paste(TimeIndex,variable,sep="_")]
names(data.pe.energysim.switch.long)[4]<-"switch"

data.pe.energysim.compared<-merge(x=data.pe.energysim.native.long,y=data.pe.energysim.switch.long[,c("labelMinutesVar","switch")],
                                  all.x=TRUE,by = "labelMinutesVar",sort = FALSE)
data.pe.energysim.compared[,bias:=abs(native-switch)]

# 日内逐时误差统计
stat.pe.energysim.hour<-data.pe.energysim.compared%>%{
    # .$labelVarHour<-paste(variable,format(.$datetime,format="%Y-%m-%d_%H"))
    .<-.[,.(variable=variable[1],
            
            native=sum(native/60,na.rm = TRUE),
            switch=sum(switch/60,na.rm = TRUE)), #这么直接加对吗？
         by=(labelVarHour=paste(variable,format(.$datetime,format="%Y-%m-%d_%H")))]
    # .[,hour:=format(datetime,format="%Y-%m-%d_%H")] #不能用:=，会直接改原始表格
    .$bias<-abs(.$native-.$switch) 
    .
}

ggplot(stat.pe.energysim.hour[variable=="Q_gain_W"],aes(x=labelVarHour,y=bias))+geom_point()


