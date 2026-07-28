"""
策略1: 环境温度切换冷热两态
用 CSV t_env_raw 判断: T<=30→冷态, T>=34→热态, 中间保持上一状态
"""
import csv, math, os
from datetime import datetime, timedelta
from pyenergyplus.plugin import EnergyPlusPlugin

LAT=22.32; LON=114.17; TZ=8.0
BASE_YEAR, BASE_MONTH, BASE_DAY = 2025, 10, 30
CSV_START = datetime(2025, 10, 30, 10, 0)
SOLAR_CONSTANT = 1367.0
CONSTRUCTION_OFFSET = 30  # PNIPAM Step 0 在 Denver IDF 中的全局索引（1-based）
# Step 0 = Hot (Tsol=0.100), Step 12 = Med (Tsol=0.460), Step 24 = Cold (Tsol=0.820)
# 索引 30=Step0(Hot), 31=Step1, ..., 54=Step24(Cold)

FEN_NAMES = [
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW1",
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW2",
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW3",
    "PERIMETER_MID_ZN_1_WALL_SOUTH_WINDOW",
    "PERIMETER_TOP_ZN_1_WALL_SOUTH_WINDOW",
]

# 太阳几何 + Erbs (同之前)
def _day_of_year(y,m,d):
    days=[0,31,59,90,120,151,181,212,243,273,304,334]
    doy=days[m-1]+d
    if m>2 and y%4==0 and (y%100!=0 or y%400==0): doy+=1
    return doy
def _solar_declination(doy):
    return math.radians(23.45)*math.sin(2*math.pi*(284+doy)/365.0)
def _equation_of_time(doy):
    B=2*math.pi*(doy-1)/365.0
    return 229.18*(0.000075+0.001868*math.cos(B)-0.032077*math.sin(B)-0.014615*math.cos(2*B)-0.040849*math.sin(2*B))
def _solar_position(lat,lon,tz,y,mo,d,h,minute):
    doy=_day_of_year(y,mo,d); decl=_solar_declination(doy); eot=_equation_of_time(doy)
    lstm=15.0*tz; b_corr=4.0*(lon-lstm); t_sol_min=h*60.0+minute+eot+b_corr
    ha=math.radians((t_sol_min/60.0-12.0)*15.0); lat_r=math.radians(lat)
    cosz=math.sin(decl)*math.sin(lat_r)+math.cos(decl)*math.cos(lat_r)*math.cos(ha)
    cosz=max(-1.0,min(1.0,cosz)); z=math.acos(cosz)
    return z,0
def _split_ghi(ghi,z_rad):
    if ghi<=0 or z_rad>math.pi/2: return 0.0,0.0
    i0=SOLAR_CONSTANT*(1+0.033*math.cos(2*math.pi*_day_of_year(2025,10,30)/365.0))
    cosz=max(math.cos(z_rad),0.01); kt=ghi/max(i0*cosz,1.0)
    if kt<=0.22: kd=1.0-0.09*kt
    elif kt<=0.80: kd=0.9511-0.1604*kt+4.388*kt**2-16.638*kt**3+12.336*kt**4
    else: kd=0.165
    kd=max(0.0,min(1.0,kd)); dhi=kd*ghi; dni=(ghi-dhi)/cosz
    return max(0.0,min(dni,i0*1.05)),max(0.0,min(dhi,ghi))

class EnvTempSwitch(EnergyPlusPlugin):
    def __init__(self):
        super().__init__()
        self._csv_data=[]; self._csv_count=0; self._warmup_done=False; self._h_ready=False
        self._timestep_idx=0
        self._h_env=self._h_wind=self._h_rh=self._h_dni=self._h_dhi=-1
        self._h_cs=[]  # construction state actuators
        self._next_state_idx = 24 + CONSTRUCTION_OFFSET  # default cold
        self._load_csv()

    def _load_csv(self):
        csv_path=os.path.join(os.path.dirname(os.path.abspath(__file__)),"PE_HIPSimulation_final.csv")
        if not os.path.exists(csv_path): csv_path="PE_HIPSimulation_final.csv"
        with open(csv_path,"r",newline="") as f:
            for row in csv.DictReader(f):
                self._csv_data.append({
                    "t_env":float(row.get("t_env_raw",25)), "hum":float(row.get("hum",50)),
                    "wind":float(row.get("wind",0)), "rad":float(row.get("rad",0))
                })
        self._csv_count=len(self._csv_data)
        print(f"[EnvTemp] Loaded {self._csv_count} rows")

    def _csv_row(self,sim_total_min):
        if self._csv_count==0: return {"t_env":25,"hum":50,"wind":0,"rad":0}
        sim_dt=datetime(BASE_YEAR,BASE_MONTH,BASE_DAY)+timedelta(minutes=sim_total_min)
        offset=(sim_dt-CSV_START).total_seconds()/60.0
        idx=max(0,min(int(offset),self._csv_count-1))
        return self._csv_data[idx]

    def _ensure_handles(self,state):
        if self._h_ready: return
        ex=self.api.exchange
        self._h_env=ex.get_actuator_handle(state,"Weather Data","Outdoor Dry Bulb","Environment")
        self._h_wind=ex.get_actuator_handle(state,"Weather Data","Wind Speed","Environment")
        self._h_rh=ex.get_actuator_handle(state,"Weather Data","Outdoor Relative Humidity","Environment")
        self._h_dni=ex.get_actuator_handle(state,"Weather Data","Direct Solar","Environment")
        self._h_dhi=ex.get_actuator_handle(state,"Weather Data","Diffuse Solar","Environment")
        for name in FEN_NAMES:
            h=ex.get_actuator_handle(state,"Surface","Construction State",name)
            self._h_cs.append(h)
        self._h_ready=True
        print(f"[EnvTemp] Handles: weather OK, cs={len(self._h_cs)}")

    def on_after_new_environment_warmup_is_complete(self,state)->int:
        self._warmup_done=True; self._timestep_idx=0; return 0

    def on_begin_zone_timestep_before_set_current_weather(self,state)->int:
        self._ensure_handles(state); ex=self.api.exchange
        if not self._warmup_done:
            row=self._csv_data[0] if self._csv_count else {"t_env":25,"hum":50,"wind":0,"rad":0}
        else:
            row=self._csv_row(self._timestep_idx); self._timestep_idx+=1

        # 天气覆写
        ex.set_actuator_value(state,self._h_env,row["t_env"])
        ex.set_actuator_value(state,self._h_wind,row["wind"])
        ex.set_actuator_value(state,self._h_rh,row["hum"])
        ghi=row["rad"]
        y,mo,d,h,minute=self._sim_datetime(self._timestep_idx)
        zenith,_=_solar_position(LAT,LON,TZ,y,mo,d,h,minute)
        dni,dhi=_split_ghi(ghi,zenith)
        ex.set_actuator_value(state,self._h_dni,dni); ex.set_actuator_value(state,self._h_dhi,dhi)

        # 应用上一时间步计算的构造状态
        for h_cs in self._h_cs:
            if h_cs >= 0:
                ex.set_actuator_value(state, h_cs, float(self._next_state_idx))
        return 0

    def on_end_of_zone_timestep_after_zone_reporting(self,state)->int:
        """根据环境温度计算下一时间步的构造状态"""
        if not self._warmup_done:
            return 0
        row = self._csv_row(self._timestep_idx)
        t_env = row["t_env"]
        if t_env <= 30:
            state = 24  # Cold
        elif t_env >= 34:
            state = 0   # Hot
        else:
            state = 24  # default cold
        self._next_state_idx = state + CONSTRUCTION_OFFSET
        return 0

    def _sim_datetime(self,minutes):
        base=datetime(BASE_YEAR,BASE_MONTH,BASE_DAY); dt=base+timedelta(minutes=minutes)
        return dt.year,dt.month,dt.day,dt.hour,dt.minute
