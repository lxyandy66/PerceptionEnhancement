"""
HIL 多态构造切换插件 — 纯短波太阳分布
======================================
读取 CSV Rate_L_norm → 计算目标 Tsol → 选择最近的预定义窗户构造
→ 通过 ConstructionState 执行器切换各窗构造

优势（vs 热流修正法）：
  - 窗户透射为真正的短波辐射，太阳几何分布（非均匀长波）
  - 玻璃吸收率与目标 Tsol 一致（不再固定为热态 18%）
  - 无需 OtherEquipment 修正注入

架构：
  CSV Rate → Tsol_target = 0.72×Rate + 0.10
          → idx = round((Tsol_target − 0.10) / 0.72 × N_steps)
          → ConstructionState actuator 切换到第 idx 号构造
"""

import csv
import math
import os
from datetime import datetime, timedelta

from pyenergyplus.plugin import EnergyPlusPlugin

# ---- 常数 ----
LAT  = 22.32     # 香港纬度
LON  = 114.17    # 香港经度
TZ   = 8.0       # 香港时区 (UTC+8)
BASE_YEAR, BASE_MONTH, BASE_DAY = 2025, 10, 30
CSV_START = datetime(2025, 10, 30, 10, 0)
SOLAR_CONSTANT = 1367.0

TSOL_HOT  = 0.10   # 热态 Tsol（构造索引 0）
TSOL_COLD = 0.82   # 冷态 Tsol（构造索引 N_STEPS）
N_STEPS   = 24     # 离散档位数（0..24，共 25 个构造，步长 0.03）

# 窗户表面名称（须与 IDF 中一致）
FEN_NAMES = [
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW1",
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW2",
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW3",
    "PERIMETER_MID_ZN_1_WALL_SOUTH_WINDOW",
    "PERIMETER_TOP_ZN_1_WALL_SOUTH_WINDOW",
]


# ---- 太阳几何 + Erbs 拆分（同之前插件，略）----

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
    return z, math.atan2(max(-1, min(1, sa)), max(-1, min(1, ca)))


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


# ---- 选择最近构造 ----

def _nearest_index(tsol_target):
    """返回离散构造索引 0..N_STEPS，对应最接近 Tsol_target 的档位。"""
    frac = (tsol_target - TSOL_HOT) / (TSOL_COLD - TSOL_HOT)
    frac = max(0.0, min(1.0, frac))
    return int(round(frac * N_STEPS))


# ---- 构造名称列表（须与 IDF 中 PNIPAM Step 0..12 的顺序一致）----
CONSTRUCTION_NAMES = [f"PNIPAM STEP {i}" for i in range(N_STEPS + 1)]

# EnergyPlus ConstructionState 执行器使用全局构造索引（1-based）
# PNIPAM Step 0..12 在 IDF 中排在所有构造的第 15..27 位
# 该偏移量需根据 IDF 实际构造数量调整：
#   前 14 个构造 = 3 roof + 8 wall + 2 window(其他气候区) + 1 door = 14
#                + 1 ext slab + 1 interior ceiling + 1 interior door
#                + 1 interior floor + 1 interior partition + 1 interior wall
#                + 1 interior window + 1 air boundary + 1 ext window CZ 4-5 = 24
#   ... 实际为 14 个非 PNIPAM 构造排在前面
#   通过脚本验证：PNIPAM Step 0 的全局索引 = 15
CONSTRUCTION_GLOBAL_INDEX_OFFSET = 30  # PNIPAM Step 0 = index 30 (Denver IDF, Step0=Hot, Step24=Cold)


# =====================================================================
#  插件类
# =====================================================================

class HILConstructionSwitch(EnergyPlusPlugin):
    """多态构造切换：CSV Rate → Tsol → 最近构造 → ConstructionState 执行器"""

    def __init__(self):
        super().__init__()
        self._csv_data    = []
        self._csv_count   = 0
        self._warmup_done = False
        self._h_ready     = False
        self._timestep_idx = 0

        # 下一时间步要应用的构造索引（0..N_STEPS），默认冷态
        self._next_idx    = [N_STEPS] * 4  # 每窗一个

        # ---- 天气执行器 ----
        self._h_env = self._h_wind = self._h_rh = -1
        self._h_dni = self._h_dhi = -1

        # ---- 构造切换执行器（每窗一个）----
        self._h_cs = []  # ConstructionState actuator handles

        self._load_csv()

    # ---- CSV 加载 ----

    def _load_csv(self):
        csv_path = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            "PE_HIPSimulation_predictRateL.csv")
        if not os.path.exists(csv_path):
            csv_path = "PE_HIPSimulation_predictRateL.csv"
        with open(csv_path, "r", newline="") as f:
            for row in csv.DictReader(f):
                self._csv_data.append({
                    "t_env": float(row.get("t_env_raw", 25)),
                    "hum":   float(row.get("hum", 50)),
                    "wind":  float(row.get("wind", 0)),
                    "rad":   float(row.get("rad", 0)),
                    "rate":  float(row.get("Rate_L_norm", 0.5)),
                })
        self._csv_count = len(self._csv_data)
        print(f"[HIL-MultiState] 已加载 {self._csv_count} 行 CSV 数据")

    def _csv_row(self, sim_total_min):
        """按模拟时间对齐 CSV 行。"""
        if self._csv_count == 0:
            return {"t_env": 25, "hum": 50, "wind": 0, "rad": 0, "rate": 0.5}
        sim_dt = datetime(BASE_YEAR, BASE_MONTH, BASE_DAY) \
                 + timedelta(minutes=sim_total_min)
        offset = (sim_dt - CSV_START).total_seconds() / 60.0
        idx = max(0, min(int(offset), self._csv_count - 1))
        return self._csv_data[idx]

    def _sim_datetime(self, minutes_from_midnight):
        total = minutes_from_midnight
        day_off = total // 1440
        rem = total % 1440
        h = rem // 60
        m = rem % 60
        base = datetime(BASE_YEAR, BASE_MONTH, BASE_DAY)
        dt = base + timedelta(days=day_off, hours=h, minutes=m)
        return dt.year, dt.month, dt.day, h, m

    # ---- 句柄初始化 ----

    def _ensure_handles(self, state):
        if self._h_ready:
            return
        ex = self.api.exchange

        # 天气执行器
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

        # 构造切换执行器（每窗一个）
        for name in FEN_NAMES:
            h = ex.get_actuator_handle(
                state, "Surface", "Construction State", name)
            self._h_cs.append(h)

        self._h_ready = True
        weather_ok = all(h >= 0 for h in
                         [self._h_env, self._h_wind, self._h_rh,
                          self._h_dni, self._h_dhi])
        cs_ok = all(h >= 0 for h in self._h_cs)
        print(f"[HIL-MultiState] 句柄就绪: weather={weather_ok} "
              f"construction_states={cs_ok} (n={len(self._h_cs)})")

    # ══════════════════════════════════════════════════════════════════
    #  回调
    # ══════════════════════════════════════════════════════════════════

    def on_after_new_environment_warmup_is_complete(self, state) -> int:
        self._warmup_done = True
        self._timestep_idx = 0
        self._next_idx = [N_STEPS] * 4  # 默认冷态
        return 0

    def on_begin_zone_timestep_before_set_current_weather(self, state) -> int:
        """应用天气覆写 + 切换窗户构造。"""
        self._ensure_handles(state)
        ex = self.api.exchange

        # ---- 获取 CSV 数据 ----
        if not self._warmup_done:
            row = self._csv_data[0] if self._csv_count else {
                "t_env": 25, "hum": 50, "wind": 0, "rad": 0, "rate": 0.5}
        else:
            row = self._csv_row(self._timestep_idx)

        # ---- 天气覆写 ----
        ex.set_actuator_value(state, self._h_env,  row["t_env"])
        ex.set_actuator_value(state, self._h_wind, row["wind"])
        ex.set_actuator_value(state, self._h_rh,   row["hum"])

        ghi = row["rad"]
        y, mo, d, h, minute = self._sim_datetime(self._timestep_idx)
        zenith, _ = _solar_position(LAT, LON, TZ, y, mo, d, h, minute)
        dni, dhi = _split_ghi(ghi, zenith)
        ex.set_actuator_value(state, self._h_dni, dni)
        ex.set_actuator_value(state, self._h_dhi, dhi)

        # ---- 切换窗户构造（使用上一时间步计算的目标索引）----
        # EnergyPlus ConstructionState 执行器接受全局构造索引（1-based）
        # PNIPAM Step 0 → 全局索引 OFFSET+1, Step 12 → OFFSET+13
        for i, h_cs in enumerate(self._h_cs):
            if h_cs >= 0 and i < len(self._next_idx):
                global_idx = (self._next_idx[i]
                              + CONSTRUCTION_GLOBAL_INDEX_OFFSET)
                ex.set_actuator_value(state, h_cs, float(global_idx))

        return 0

    def on_end_of_zone_timestep_after_zone_reporting(self, state) -> int:
        """从 CSV Rate 计算目标 Tsol，选定下一时间步的构造。"""
        if not self._warmup_done:
            return 0

        row = self._csv_row(self._timestep_idx)
        rate = row["rate"]
        tsol_target = (TSOL_COLD - TSOL_HOT) * rate + TSOL_HOT
        idx = _nearest_index(tsol_target)
        self._next_idx = [idx] * 4  # 四面窗统一切换

        self._timestep_idx += 1
        return 0
