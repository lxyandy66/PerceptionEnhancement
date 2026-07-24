"""
EnergyPlus Python Plugin：从 CSV 覆写天气数据
==============================================
仅负责将 PE_HIPSimulation_final.csv 中的实测气象数据写入 EnergyPlus。
窗户的热致变色行为由 IDF 中的 WindowMaterial:GlazingGroup:Thermochromic
原生处理，本插件不干预。

如需切换回 HIL 模式，使用 thermochromic_plugin_heat_flux_correction.py。
"""

import csv
import math
import os
from datetime import datetime, timedelta

from pyenergyplus.plugin import EnergyPlusPlugin

# ---- 常量 ----
LAT  = 22.32     # 香港纬度
LON  = 114.17    # 香港经度
TZ   = 8.0       # 香港时区 (UTC+8)
BASE_YEAR, BASE_MONTH, BASE_DAY = 2025, 10, 30
CSV_START = datetime(2025, 10, 30, 10, 0)
SOLAR_CONSTANT = 1367.0


# ---- 太阳几何 + Erbs GHI 拆分 ----

def _day_of_year(y, m, d):
    days = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
    doy = days[m - 1] + d
    if m > 2 and y % 4 == 0 and (y % 100 != 0 or y % 400 == 0):
        doy += 1
    return doy


def _solar_declination(doy):
    return math.radians(23.45) * math.sin(2 * math.pi * (284 + doy) / 365.0)


def _equation_of_time(doy):
    B = 2 * math.pi * (doy - 1) / 365.0
    return 229.18 * (0.000075 + 0.001868 * math.cos(B)
                     - 0.032077 * math.sin(B)
                     - 0.014615 * math.cos(2 * B)
                     - 0.040849 * math.sin(2 * B))


def _solar_position(lat, lon, tz, y, mo, d, h, minute):
    doy = _day_of_year(y, mo, d)
    decl = _solar_declination(doy)
    eot  = _equation_of_time(doy)
    lstm = 15.0 * tz
    b_corr = 4.0 * (lon - lstm)
    t_sol_min = h * 60.0 + minute + eot + b_corr
    ha = math.radians((t_sol_min / 60.0 - 12.0) * 15.0)
    lat_r = math.radians(lat)
    cosz = (math.sin(decl) * math.sin(lat_r)
            + math.cos(decl) * math.cos(lat_r) * math.cos(ha))
    cosz = max(-1.0, min(1.0, cosz))
    z = math.acos(cosz)
    sa = (-math.cos(decl) * math.sin(ha)) / max(1e-10, math.sin(z))
    ca = ((math.sin(decl) - math.sin(lat_r) * math.cos(z))
          / (math.cos(lat_r) * math.sin(z) + 1e-10))
    sa = max(-1.0, min(1.0, sa))
    ca = max(-1.0, min(1.0, ca))
    return z, math.atan2(sa, ca)


def _split_ghi(ghi, z_rad):
    if ghi <= 0 or z_rad > math.pi / 2:
        return 0.0, 0.0
    i0 = SOLAR_CONSTANT * (1 + 0.033 * math.cos(
        2 * math.pi * _day_of_year(2025, 10, 30) / 365.0))
    cosz = max(math.cos(z_rad), 0.01)
    kt = ghi / max(i0 * cosz, 1.0)
    if kt <= 0.22:
        kd = 1.0 - 0.09 * kt
    elif kt <= 0.80:
        kd = (0.9511 - 0.1604 * kt + 4.388 * kt**2
              - 16.638 * kt**3 + 12.336 * kt**4)
    else:
        kd = 0.165
    kd = max(0.0, min(1.0, kd))
    dhi = kd * ghi
    dni = (ghi - dhi) / cosz
    dni_max = i0 * 1.05
    return max(0.0, min(dni, dni_max)), max(0.0, min(dhi, ghi))


# ---- 插件类 ----

class WeatherFromCSV(EnergyPlusPlugin):
    """从 CSV 覆写天气数据。窗户热致变色由 E+ 原生处理。"""

    def __init__(self):
        super().__init__()
        self._csv_data    = []
        self._csv_count   = 0
        self._warmup_done = False
        self._h_ready     = False
        self._timestep_idx = 0

        # 执行器句柄
        self._h_env = self._h_wind = self._h_rh = -1
        self._h_dni = self._h_dhi = -1

        self._load_csv()

    # ---- CSV 加载 ----

    def _load_csv(self):
        csv_path = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            "PE_HIPSimulation_final.csv")
        if not os.path.exists(csv_path):
            csv_path = "PE_HIPSimulation_final.csv"
        with open(csv_path, "r", newline="") as f:
            for row in csv.DictReader(f):
                self._csv_data.append({
                    "t_env": float(row.get("t_env_raw", 25)),
                    "hum":   float(row.get("hum", 50)),
                    "wind":  float(row.get("wind", 0)),
                    "rad":   float(row.get("rad", 0)),
                })
        self._csv_count = len(self._csv_data)
        print(f"[WeatherCSV] 已加载 {self._csv_count} 行数据")

    def _csv_row(self, sim_total_min):
        """按模拟时间对齐 CSV 行。"""
        if self._csv_count == 0:
            return {"t_env": 25, "hum": 50, "wind": 0, "rad": 0}
        sim_dt = datetime(BASE_YEAR, BASE_MONTH, BASE_DAY) \
                 + timedelta(minutes=sim_total_min)
        offset = (sim_dt - CSV_START).total_seconds() / 60.0
        idx = max(0, min(int(offset), self._csv_count - 1))
        return self._csv_data[idx]

    def _ensure_handles(self, state):
        if self._h_ready:
            return
        ex = self.api.exchange
        self._h_env  = ex.get_actuator_handle(
            state, "Weather Data", "Outdoor Dry Bulb", "Environment")
        self._h_wind = ex.get_actuator_handle(
            state, "Weather Data", "Wind Speed", "Environment")
        self._h_rh   = ex.get_actuator_handle(
            state, "Weather Data", "Outdoor Relative Humidity", "Environment")
        self._h_dni  = ex.get_actuator_handle(
            state, "Weather Data", "Direct Solar", "Environment")
        self._h_dhi  = ex.get_actuator_handle(
            state, "Weather Data", "Diffuse Solar", "Environment")
        self._h_ready = True
        ok = all(h >= 0 for h in [self._h_env, self._h_wind, self._h_rh,
                                   self._h_dni, self._h_dhi])
        print(f"[WeatherCSV] 句柄就绪: ALL_OK={ok}")

    # ---- 回调 ----

    def on_after_new_environment_warmup_is_complete(self, state) -> int:
        self._warmup_done = True
        self._timestep_idx = 0
        return 0

    def on_begin_zone_timestep_before_set_current_weather(self, state) -> int:
        """每时间步覆写天气。"""
        self._ensure_handles(state)
        ex = self.api.exchange

        # 暖机期用第一行，正式模拟按时间对齐
        if not self._warmup_done:
            row = self._csv_data[0] if self._csv_count else {
                "t_env": 25, "hum": 50, "wind": 0, "rad": 0}
        else:
            row = self._csv_row(self._timestep_idx)
            self._timestep_idx += 1

        # 覆写温湿度、风速
        ex.set_actuator_value(state, self._h_env,  row["t_env"])
        ex.set_actuator_value(state, self._h_wind, row["wind"])
        ex.set_actuator_value(state, self._h_rh,   row["hum"])

        # 覆写太阳 (GHI → DNI + DHI)
        ghi = row["rad"]
        y, mo, d, h, minute = self._sim_datetime(self._timestep_idx)
        zenith, _ = _solar_position(LAT, LON, TZ, y, mo, d, h, minute)
        dni, dhi = _split_ghi(ghi, zenith)
        ex.set_actuator_value(state, self._h_dni, dni)
        ex.set_actuator_value(state, self._h_dhi, dhi)

        return 0

    def _sim_datetime(self, minutes_from_midnight):
        total = minutes_from_midnight
        day_off = total // 1440
        rem = total % 1440
        h = rem // 60
        m = rem % 60
        base = datetime(BASE_YEAR, BASE_MONTH, BASE_DAY)
        dt = base + timedelta(days=day_off, hours=h, minutes=m)
        return dt.year, dt.month, dt.day, h, m
