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
# 不再使用
# data.pe.energysim.native.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/Demo_EnergySimulationData/MidOffice_Eplus_Native.csv",
#                                     data.table = TRUE)%>%.[,":="(type="native",datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"))]
# data.pe.energysim.switch.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/Demo_EnergySimulationData/MidOffice_HIL_13state.csv",
#                                     data.table = TRUE)%>%.[,":="(type="switch",datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"))]

# 不同策略批量导入
data.pe.energysim.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TMY_0724_Full/Denver_building_S1_Eplus_Native_minutely.csv",
                             data.table = TRUE)%>%.[0,":="(datetime=as.POSIXct("2026-07-08 00:00:00"),source="null")]%>%.[0]

for( i in list.files("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TypicalDay_0723_newGlass")){
    data.pe.energysim.raw<-rbind(data.pe.energysim.raw,
                                 fread(file = paste0("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TypicalDay_0723_newGlass/",i),
                                                             data.table = TRUE)%>%
                                     .[,":="(datetime=seq.POSIXt(from = as.POSIXct("2025-10-30 00:00"),to = as.POSIXct("2025-10-31 23:59"),by="min"),source=paste0(i))])
}
data.pe.energysim.raw[,source:=gsub('.csv','',source)]
data.pe.energysim.raw[,source:=gsub('Denver_building_','',source)]
data.pe.energysim.raw[,source:=gsub('_minutely','',source)]
# 合并所有窗负荷
# 思路：
# Q,温度取平均 能耗直接相加
data.pe.energysim.raw[,Q_sol_sum_W:=apply(.SD,MARGIN = 1,sum),.SDcol= grep("Qsol", names(data.pe.energysim.raw), value = TRUE)]
data.pe.energysim.raw[,Q_gain_sum_W:=apply(.SD,MARGIN = 1,sum),.SDcol= grep("Qgain", names(data.pe.energysim.raw), value = TRUE)]
data.pe.energysim.raw[,T_zone_ave_C:=apply(.SD,MARGIN = 1,mean),.SDcol= c("T_PERIMETER_TOP_ZN_1","T_PERIMETER_MID_ZN_1","T_PERIMETER_TOP_ZN_1")]

# > names(data.pe.energysim.raw)
# [1] "TimeIndex"    "Tzone_Bot_C"  "Tzone_Mid_C"  "Tzone_Top_C"  "Q_sol_W1_W"   "Q_gain_W1_W"  "Tw_in_W1_C"   "I_inc_W1_Wm2" "Q_sol_W2_W"  
# [10] "Q_gain_W2_W"  "Tw_in_W2_C"   "I_inc_W2_Wm2" "Q_sol_W3_W"   "Q_gain_W3_W"  "Tw_in_W3_C"   "I_inc_W3_Wm2" "Q_sol_W4_W"   "Q_gain_W4_W" 
# [19] "Tw_in_W4_C"   "I_inc_W4_Wm2" "Q_sol_W5_W"   "Q_gain_W5_W"  "Tw_in_W5_C"   "I_inc_W5_Wm2" "Cooling_J"    "Heating_J"    "datetime"    
# [28] "source"       "Q_sol_sum_W"  "Q_gain_sum_W" "T_zone_ave_C"
data.pe.energysim.hour<-data.pe.energysim.raw[,labelHourSource:=paste0(format(datetime,format="%Y-%m-%d_%H"),"_",source)][
    ,.(datetime=datetime[1],
       source=source[1],
       T_zone_ave_C=mean(T_zone_ave_C,na.rm=TRUE), #所有南向热区温度
       T_PERIMETER_MID_ZN_1=mean(T_PERIMETER_MID_ZN_1,na.rm=TRUE), #取一个特别的中层南区
       Qgain_P_MID_ZN_1_SOUTH_WINDOW=mean(Qgain_P_MID_ZN_1_SOUTH_WINDOW,na.rm=TRUE),
       Qsol_P_MID_ZN_1_SOUTH_WINDOW =mean(Qsol_P_MID_ZN_1_SOUTH_WINDOW,na.rm=TRUE),
       Q_sol_sum_W=mean(Q_sol_sum_W,na.rm=TRUE),
       Q_gain_sum_W=mean(Q_gain_sum_W,na.rm=TRUE),
       # I_inc_W5_Wm2=mean(I_inc_W5_Wm2,na.rm=TRUE),
       Cooling_J=sum(Cooling_J,na.rm = TRUE)/3600000 #转换为kwh
       ),by=labelHourSource][,labelHourSource:=NULL]

unique(data.pe.energysim.hour$source)
ggplot(data.pe.energysim.hour[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&
                                 source%in%c("S3_predictRateL","S3m_MixedMeasurement"),#,"HotFixed","ColdFixed""GlassTemp",#"S1_Eplus_Native","S4_ColdFixed","S5_HotFixed","S2_GlassTemp_25",
                              c("datetime","source","Cooling_J")],#"T_PERIMETER_TOP_ZN_1","T_PERIMETER_BOT_ZN_1","T_PERIMETER_MID_ZN_1"
       aes(x=datetime,y=Cooling_J,color=source,shape=source))+geom_line()+geom_point()+
    # labs(y="HVAC energy consumption (kW)",x="Time")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

ggplot(data.pe.energysim.raw[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&
                                  source%in%c("S4_ColdFixed","S5_HotFixed","S3_predictRateL","S3m_MixedMeasurement"),#,"HotFixed","ColdFixed""GlassTemp",#"S1_Eplus_Native","S4_ColdFixed","S5_HotFixed","S2_GlassTemp_25",
                              c("datetime","source","T_PERIMETER_TOP_ZN_1","T_PERIMETER_BOT_ZN_1","T_PERIMETER_MID_ZN_1")]%>%melt(.,id.var=c("datetime","source")),#
       aes(x=datetime,y=value,color=variable,shape=variable,group=variable))+geom_line()+geom_point()+facet_wrap(~source,ncol=1)+
    # labs(y="HVAC energy consumption (kW)",x="Time")+
    theme_bw()+theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"),legend.text = element_text(size=14))

#### 统计数据：小时级各策略区别 ####
stat.pe.energysim.compare<-data.table(datetime=as.POSIXct("2025-10-31 06:00:00"),Variable="",HIL2Pred=-999,HIL2Cold=-999,HIL2Hot=-999)[0]
for(i in c("T_PERIMETER_MID_ZN_1","T_zone_ave_C")){
    stat.pe.energysim.compare<-rbind(stat.pe.energysim.compare,
                                     data.pe.energysim.hour[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&
                                                                source%in%c("S3m_MixedMeasurement","S3_predictRateL","S5_HotFixed","S4_ColdFixed"),
                                                            c("datetime","source","T_zone_ave_C","Q_sol_sum_W","Q_gain_sum_W","Cooling_J","T_PERIMETER_MID_ZN_1","Qgain_P_MID_ZN_1_SOUTH_WINDOW","Qsol_P_MID_ZN_1_SOUTH_WINDOW")]%>%
                                         dcast(.,datetime~source,value.var = i)%>%as.data.table%>%.[,.(Variable=i,
                                                                                                       HIL2Pred=(S3m_MixedMeasurement-S3_predictRateL),
                                                                                                       HIL2Cold=(S3m_MixedMeasurement-S4_ColdFixed),
                                                                                                       HIL2Hot=(S3m_MixedMeasurement-S5_HotFixed)),by=datetime]
                                     )
}


stat.pe.energysim.ave.min<-data.pe.energysim.raw[datetime>=as.POSIXct("2025-10-31 06:00:00")&datetime<as.POSIXct("2025-10-31 18:00:00")&
                           source%in%c("S3m_MixedMeasurement","S3_predictRateL","S5_HotFixed","S4_ColdFixed"),
                       c("datetime","source","T_zone_ave_C","T_PERIMETER_MID_ZN_1")]%>%
    dcast(.,datetime~source,value.var = "T_PERIMETER_MID_ZN_1")%>%as.data.table%>%.[,.(HIL2Pred=abs(S3m_MixedMeasurement-S3_predictRateL),
                                                                               HIL2Cold=abs(S3m_MixedMeasurement-S4_ColdFixed),
                                                                               HIL2Hot=abs(S3m_MixedMeasurement-S5_HotFixed)),by=datetime]

stat.pe.energysim.mid.hour<-stat.pe.energysim.mid.min[,hour:=format(datetime,format="%Y-%m-%d_%H")][,.(),by=hour]


# &source%in%c("HIL","Predict")

################

# > names(data.pe.energysim.native.raw)
# [1] "TimeIndex"   "Tzone_C"     "Q_sol_W"     "Q_gain_W"    "Tw_in_C"     "Tw_out_C"    "I_inc_Wm2"  
# [8] "TC_Spec_T_C" "Tsol_TC"     "type"        "datetime"
names(data.pe.energysim.switch.raw)[8]<-"Tsol_TC"

data.pe.energysim.combined<-rbind(data.pe.energysim.native.raw[,c("datetime","Q_gain_W","type")],
                                  data.pe.energysim.switch.raw[,c("datetime","Q_gain_W","type")])
ggplot(data = data.pe.energysim.combined,aes(x=datetime,y= Q_gain_W ,color=type))+geom_line()







#### 典型年数据分析 ####

data.pe.energysim.tmy.raw<-fread(file = "/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TMY_0724_Full/PortoVelho_S6-TimeSwitch_TMY.csv",
                             data.table = TRUE)%>%.[0,":="(datetime=as.POSIXct("2026-07-08 00:00:00"),city="",strategy="")]%>%.[0]

for( i in list.files("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TMY_0724_Full")){
    dataInfo<-strsplit(i,split = "_")%>%unlist
    data.pe.energysim.tmy.raw<-rbind(data.pe.energysim.tmy.raw,
                                 fread(file = paste0("/Volumes/Stroage/PercepetionEnhancement_Share/PE_EnergyplusSimulation/Building_TMY_0724_Full/",i),
                                       data.table = TRUE)%>%
                                     .[,":="(datetime=seq.POSIXt(from = as.POSIXct("2025-01-01 00:00"),to = as.POSIXct("2025-12-31 23:59"),by="hour"),
                                             city=dataInfo[1],strategy=dataInfo[2])])
}
setcolorder(data.pe.energysim.tmy.raw,c( "datetime","city","strategy"))

# 合并所有窗负荷
# 思路：
# Q,温度取平均 能耗直接相加 (Q单位为W，时间步长为小时，除以1000累加则即为kWh)
data.pe.energysim.tmy.raw[,Q_sol_sum_W:=apply(.SD,MARGIN = 1,mean),.SDcol= grep("Qsol", names(data.pe.energysim.raw), value = TRUE)]
data.pe.energysim.tmy.raw[,Q_gain_sum_W:=apply(.SD,MARGIN = 1,mean),.SDcol= grep("Qgain", names(data.pe.energysim.raw), value = TRUE)]
data.pe.energysim.tmy.raw[,T_zone_ave_C:=apply(.SD,MARGIN = 1,mean),.SDcol= c("T_PERIMETER_TOP_ZN_1","T_PERIMETER_MID_ZN_1","T_PERIMETER_TOP_ZN_1")]
data.pe.energysim.tmy.raw[,strategy:=gsub('-','_',strategy)]

data.pe.energysim.tmy.month<-data.pe.energysim.tmy.raw[,":="(month=month(datetime),labelCityStrategyMonth=paste(city,strategy,month(datetime)))][
    ,.(city=city[1],
       strategy=strategy[1],
       month=month[1],
       Q_sol_sum_W=mean(Q_sol_sum_W,na.rm=TRUE),
       Q_gain_sum_W=mean(Q_gain_sum_W,na.rm=TRUE),
       E_sol_sum_kWh=sum(Q_sol_sum_W,na.rm=TRUE)/1000,
       E_gain_sum_kWh=sum(Q_gain_sum_W,na.rm=TRUE)/1000, # data.pe.energysim.tmy.raw为小时步长，Q/1000即为kW
       Cooling_J=sum(Cooling_J,na.rm = TRUE)
       ),by=labelCityStrategyMonth]

data.pe.energysim.tmy.loc<-data.pe.energysim.tmy.raw[,.(city=city[1],
                                                        strategy=strategy[1],
                                                        Q_sol_sum_W=mean(Q_sol_sum_W,na.rm=TRUE),
                                                        Q_gain_sum_W=mean(Q_gain_sum_W,na.rm=TRUE),
                                                        E_sol_sum_kWh=sum(Q_sol_sum_W,na.rm=TRUE)/1000,
                                                        E_gain_sum_kWh=sum(Q_gain_sum_W,na.rm=TRUE)/1000,
                                                        Cooling_J=sum(Cooling_J,na.rm = TRUE)),
                                                     by=(labelCityStrategy=paste(city,strategy))] %>%
                                    cbind(data.pe.energysim.tmy.raw[hour(datetime)%in%c(6:18),.(
                                                                Q_sol_sum_W_BizTime=mean(Q_sol_sum_W,na.rm=TRUE),
                                                                Q_gain_sum_W_BizTime=mean(Q_gain_sum_W,na.rm=TRUE),
                                                                E_sol_sum_kWh_BizTime=sum(Q_sol_sum_W,na.rm=TRUE)/1000,
                                                                E_gain_sum_kWh_BizTime=sum(Q_gain_sum_W,na.rm=TRUE)/1000,
                                                                Cooling_J_BizTime=sum(Cooling_J,na.rm = TRUE)),
                                    by=(labelCityStrategy_BizTime=paste(city,strategy))])


# 年度数据统计对比
stat.pe.energysim.tmy.compare<-data.table(city="",S1_EplusNative=-999,S2_GlassTemp=-999,	S4_ColdFixed=-999,	S5_HotFixed=-999,	S6_TimeSwitch=-999,	S7_EnvTempSwitch=-999,
                                          Variable="",type="",HIL2Est=-999,	HIL2Cold=-999,	HIL2Hot=-999,	HIL2Est_P=-999,	HIL2Cold_P=-999,	HIL2Hot_P=-999)[0]

for(i in c("bizDay","all")){
    for(j in c("E_sol_sum_kWh","E_gain_sum_kWh","Cooling_J")){
        j<-ifelse(i=="all",j,paste0(j,"_BizTime"))
        stat.pe.energysim.tmy.compare<-rbind(stat.pe.energysim.tmy.compare,
        data.pe.energysim.tmy.loc[,c(..j,"city","strategy") ] %>%
            dcast(.,city~strategy,value.var = j)%>%as.data.table%>%.[,":="(type=i,
                                                                          Variable=j,
                                                                          HIL2Est=(S1_EplusNative-S2_GlassTemp),
                                                                          HIL2Cold=(S1_EplusNative - S4_ColdFixed),
                                                                          HIL2Hot=(S1_EplusNative-S5_HotFixed),
                                                                          HIL2Est_P=(S1_EplusNative-S2_GlassTemp)/S1_EplusNative,
                                                                          HIL2Cold_P=(S1_EplusNative - S4_ColdFixed)/S1_EplusNative,
                                                                          HIL2Hot_P=(S1_EplusNative-S5_HotFixed)/S1_EplusNative
                                                                          )])
    }
}
stat.pe.energysim.tmy.compare[,Variable:=gsub('_BizTime','',Variable)]
write.csv(stat.pe.energysim.tmy.compare[Variable=="E_sol_sum_kWh"&type=="all",c("city","HIL2Cold","HIL2Hot")],
          file="PE_Esol_Global_TMY_HILcompare.csv",
          row.names = FALSE,na = "")


ggplot(data.pe.energysim.tmy.month[city=="HongKong"&strategy%in%c("S1_EplusNative","S2_GlassTemp","S4_ColdFixed","S5_HotFixed") ],
       aes(x=month,y=Cooling_J,color=strategy))+geom_line()+facet_wrap(~city,ncol=5)

ggplot(stat.pe.energysim.tmy.compare[,c("city","Variable","type","HIL2Est_P","HIL2Cold_P","HIL2Hot_P")] %>% melt(.,id.var=c("city","Variable","type")), #,"S2-GlassTemp"
       aes(x=city,y=value,color=type,fill=type))+geom_col(position = "dodge",width = 0.6)+facet_wrap(~Variable+variable,ncol=3)

# > unique(data.pe.energysim.tmy.month$strategy)
# [1] "S1-EplusNative"   "S2-GlassTemp"     "S4-ColdFixed"     "S5-HotFixed"      "S6-TimeSwitch"    "S7-EnvTempSwitch"
# > unique(data.pe.energysim.tmy.month$city)
# [1] "AbuDhabi"    "Albuquerque" "Charleston"  "Dalian"      "Denver"      "EiPaso"      "Fairbanks"   "GreatFalls"  "Harbin"     
# [10] "HongKong"    "Honolulu"    "NewDelhi"    "NewYork"     "PortAngeles" "PortoVelho"  "SanDiego"    "Seattle"     "Shenyang"   
# [19] "Tucson"

#### ####




#### 批量合并并处理 ####
#作废！
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


