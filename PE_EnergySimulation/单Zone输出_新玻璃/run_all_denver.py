"""
Denver 办公楼 — 初始化 + 全部 6 种策略一键运行
=============================================
步骤:
  1. 从原始 IDF 备份恢复
  2. 注入 PNIPAM 材料、构造、Plugin、输出变量
  3. 依次运行 6 种策略，每种直接修改 IDF 后运行
"""
import subprocess, os, re, shutil

E_PLUS = "C:/EnergyPlusV24-2-0/energyplus.exe"
EPW    = "C:/EnergyPlusV24-2-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"
BASE   = os.path.dirname(os.path.abspath(__file__))
ORIGIN = os.path.join(BASE, "Origin-ASHRAE901_OfficeMedium_STD2019_Denver.idf")
IDF    = os.path.join(BASE, "REV_ASHRAE901_OfficeMedium_STD2019_Denver.idf")

SOUTH = [
    "Perimeter_bot_ZN_1_Wall_South_Window1",
    "Perimeter_bot_ZN_1_Wall_South_Window2",
    "Perimeter_bot_ZN_1_Wall_South_Window3",
    "Perimeter_mid_ZN_1_Wall_South_Window",
    "Perimeter_top_ZN_1_Wall_South_Window",
]

STRATEGIES = [
    ("S0a_Eplus_Native",   "Denver_S0a_Native",   "WeatherFromCSV",       "plugin_weather_only",      "Window_South_PNIPAM_TC"),
    ("S0b_HIL_25state",    "Denver_S0b_HIL25",    "HILConstructionSwitch","plugin_hil_multi_state",   "Window_South_PNIPAM_HIL"),
    ("S1_env_temp_switch", "Denver_S1_env_temp",  "EnvTempSwitch",        "plugin_env_temp_switch",   "Window_South_PNIPAM_HIL"),
    ("S2_cold_fixed",      "Denver_S2_cold_fixed","WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 24"),
    ("S3_hot_fixed",       "Denver_S3_hot_fixed", "WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 0"),
    ("S4_time_switch",     "Denver_S4_time_switch","TimeSwitch",           "plugin_time_switch",       "Window_South_PNIPAM_HIL"),
]


# ═══════════════════════════════════════════════════════════════
#  IDF 初始化（仅首次运行）
# ═══════════════════════════════════════════════════════════════

def generate_25state_block():
    """生成 25 态玻璃材料 + 构造（展开格式）"""
    N = 24
    step = 0.72 / N
    lines = ["!- ====== PNIPAM 25 态玻璃材料 (step=0.03) ======", ""]
    for i in range(N + 1):
        tsol = 0.10 + i * step
        rsol = round(0.72 - (i / N) * 0.63, 4)
        tvis = round(0.06 + (i / N) * 0.81, 4)
        rvis = round(0.78 - (i / N) * 0.71, 4)
        lines += [
            f"WindowMaterial:Glazing,",
            f"    PNIPAM Step {i},           !- Name (Tsol={tsol:.3f})",
            f"    SpectralAverage,         !- Optical Data Type",
            f"    ,                        !- Window Glass Spectral Data Set Name",
            f"    0.003,                   !- Thickness {{m}}",
            f"    {tsol:.4f},                  !- Solar Transmittance",
            f"    {rsol:.4f},                  !- Front Side Solar Reflectance",
            f"    {rsol:.4f},                  !- Back Side Solar Reflectance",
            f"    {tvis:.4f},                  !- Visible Transmittance",
            f"    {rvis:.4f},                  !- Front Side Visible Reflectance",
            f"    {rvis:.4f},                  !- Back Side Visible Reflectance",
            f"    0,                       !- Infrared Transmittance",
            f"    0.84,                    !- Front Side IR Emissivity",
            f"    0.84,                    !- Back Side IR Emissivity",
            f"    0.28,                    !- Conductivity {{W/m-K}}",
            f"    1,                       !- Dirt Correction Factor",
            f"    No;                      !- Solar Diffusing", ""
        ]
    lines += ["!- ====== PNIPAM 25 态构造 =====", ""]
    for i in range(N + 1):
        lines += [f"Construction,", f"    PNIPAM Step {i},  PNIPAM Step {i};", ""]
    return "\n".join(lines)


def init_idf():
    """一次性注入 PNIPAM 材料、构造、Plugin、输出变量"""
    with open(ORIGIN, "r", encoding="utf-8") as f:
        idf = f.read()

    # 1. Timestep
    idf = idf.replace("Timestep,4;", "Timestep,60;")

    # 2. Site:Location → Hong Kong
    idf = re.sub(r"Site:Location,.*?;",
                 "Site:Location,\n    Site 1,  !- Name\n    22.32,   !- Latitude\n    114.17,  !- Longitude\n    8.0,     !- Time Zone\n    5;       !- Elevation",
                 idf, flags=re.DOTALL)

    # 3. RunPeriod: keep first only, change to Oct 30-31
    rps = list(re.finditer(r"RunPeriod,.*?;", idf, re.DOTALL))
    new_rp = ("RunPeriod,\n    Run Period 1,  !- Name\n    10,  !- Begin Month\n    30,  !- Begin Day\n    2025,!- Begin Year\n"
              "    10,  !- End Month\n    31,  !- End Day\n    2025,!- End Year\n    Thursday,  !- Start Day\n"
              "    No,No,No,Yes,Yes;")
    if rps:
        idf = idf[:rps[0].start()] + new_rp + idf[rps[0].end():]
        for rp in rps[1:]:
            idf = idf.replace(rp.group(), "")

    # 4. 替换 SimpleGlazingSystem → PNIPAM 基础材料
    old_glazing = ("WindowMaterial:SimpleGlazingSystem,\n"
                   "    Glazing Layer,           !- Name\n"
                   "    2.0441736,               !- U-Factor {W/m2-K}\n"
                   "    0.38,                    !- Solar Heat Gain Coefficient\n"
                   "    0.418;                   !- Visible Transmittance")
    new_mats = (
        "WindowMaterial:SimpleGlazingSystem,\n    Glazing Layer,  !- Name (非南窗)\n    2.0441736,  !- U-Factor\n    0.38,  !- SHGC\n    0.418;  !- VT\n\n"
        "!- ====== PNIPAM 基础材料 ======\n\n"
        + _glazing_expanded("PNIPAM Hydrogel Hot", 0.003, 0.1000, 0.7200, 0.7200, 0.0600, 0.7800, 0.7800, 0.28) + "\n"
        + _glazing_expanded("PNIPAM Hydrogel Cold", 0.003, 0.8200, 0.0900, 0.0900, 0.8700, 0.0700, 0.0700, 0.28) + "\n"
        + _glazing_expanded("Dummy Transparent Glass", 0.0001, 0.999, 0.0005, 0.0005, 0.999, 0.0005, 0.0005, 221) + "\n"
        "WindowMaterial:Gas,\n    Zero Gap,  !- Name\n    Air, 0.0001;\n\n"
        "WindowMaterial:GlazingGroup:Thermochromic,\n    PNIPAM Thermochromic Window,  !- Name\n"
        "    20, PNIPAM Hydrogel Cold,\n    30, PNIPAM Hydrogel Cold,\n    34, PNIPAM Hydrogel Hot;\n"
    )
    idf = idf.replace(old_glazing, new_mats)

    # 5. 在 Window_U_0.36 构造定义之后插入南窗构造
    marker = "Window_U_0.36_SHGC_0.38, !- Name\n    Glazing Layer"
    idx = idf.find(marker)
    if idx > 0:
        end_of_line = idf.find("\n", idf.find("Outside Layer", idx))
        extra = (
            "\nConstruction,\n"
            "    Window_South_PNIPAM_TC,  !- Name (南窗 E+ 原生 TC)\n"
            "    Dummy Transparent Glass, !- Outside Layer\n"
            "    Zero Gap,  !- Layer 2\n"
            "    PNIPAM Thermochromic Window;  !- Layer 3\n\n"
            "Construction,\n"
            "    Window_South_PNIPAM_HIL, !- Name (南窗 HIL)\n"
            "    PNIPAM Step 12;  !- Tsol=0.460\n"
        )
        idf = idf[:end_of_line+1] + extra + idf[end_of_line+1:]

    # 6. 南窗改用 HIL 构造
    for fen in SOUTH:
        idf = idf.replace(
            f"    {fen},  !- Name\n    Window,                  !- Surface Type\n    Window_U_0.36_SHGC_0.38, !- Construction Name",
            f"    {fen},  !- Name\n    Window,                  !- Surface Type\n    Window_South_PNIPAM_HIL, !- Construction Name"
        )

    # 7. 追加 25 态块 + Plugin + 输出
    tail = (
        "\n\n" + generate_25state_block() + "\n\n"
        "Output:SQLite, SimpleAndTabular;\n\n"
        "!- ====== Python Plugin + 输出变量 ======\n\n"
        "PythonPlugin:SearchPaths, ., ;\n\n"
        "PythonPlugin:Instance,\n"
        "    WeatherFromCSV,          !- Name\n"
        "    Yes,  !- Run During Warmup\n"
        "    plugin_weather_only,     !- Plugin File\n"
        "    WeatherFromCSV;          !- Plugin Class\n\n"
        "Output:Variable, *, Zone Mean Air Temperature, Timestep;\n"
        "Output:Variable, *, Surface Window Transmitted Solar Radiation Rate, Timestep;\n"
        "Output:Variable, *, Surface Window Heat Gain Rate, Timestep;\n"
        "Output:Variable, *, Surface Inside Face Temperature, Timestep;\n"
        "Output:Variable, *, Surface Outside Face Temperature, Timestep;\n"
        "Output:Variable, *, Surface Outside Face Incident Solar Radiation Rate per Area, Timestep;\n"
        "Output:Variable, *, Air System Total Cooling Energy, Timestep;\n"
        "Output:Variable, *, Air System Total Heating Energy, Timestep;\n"
        "Output:Variable, *, Zone Air System Sensible Cooling Rate, Timestep;\n"
        "Output:Variable, *, Zone Air System Sensible Heating Rate, Timestep;\n"
    )
    # 去掉末尾可能的多余空白
    idf = idf.rstrip() + tail

    with open(IDF, "w", encoding="utf-8") as f:
        f.write(idf)
    print("IDF initialized.")


def _glazing_expanded(name, t, tsol, rsol_f, rsol_b, tvis, rvis_f, rvis_b, lam):
    return (
        f"WindowMaterial:Glazing,\n    {name},  !- Name\n"
        f"    SpectralAverage,  !- Optical Data Type\n    ,  !- Spectral Data Set\n    {t},  !- Thickness\n"
        f"    {tsol:.4f},  !- Solar Transmittance\n    {rsol_f:.4f},  !- Front Solar Reflectance\n"
        f"    {rsol_b:.4f},  !- Back Solar Reflectance\n    {tvis:.4f},  !- Visible Transmittance\n"
        f"    {rvis_f:.4f},  !- Front Visible Reflectance\n    {rvis_b:.4f},  !- Back Visible Reflectance\n"
        f"    0,  !- IR Transmittance\n    0.84,  !- Front IR Emissivity\n    0.84,  !- Back IR Emissivity\n"
        f"    {lam},  !- Conductivity\n    1,  !- Dirt Factor\n    No;  !- Solar Diffusing"
    )


# ═══════════════════════════════════════════════════════════════
#  策略运行
# ═══════════════════════════════════════════════════════════════

def set_plugin(idf, pclass, pfile):
    return re.sub(
        r"(?<!!)PythonPlugin:Instance,\n.*?;",
        f"PythonPlugin:Instance,\n    {pclass},  !- Name\n    Yes,  !- Warmup\n    {pfile},  !- File\n    {pclass};  !- Class",
        idf, count=1, flags=re.DOTALL,
    )


def set_construction(idf, const):
    for fen in SOUTH:
        idf = re.sub(
            rf"({fen},\s+!- Name\n    Window,\s+!- Surface Type\n    ).+?,( !- Construction Name)",
            rf"\1{const},\2", idf
        )
    return idf


def toggle_tc(idf, activate):
    """激活/注释 TC 构造"""
    pairs = [
        ("!!    Window_South_PNIPAM_TC,  !- Name (南窗 E+ 原生 TC)",
         "    Window_South_PNIPAM_TC,  !- Name (南窗 E+ 原生 TC)"),
        ("!!    Dummy Transparent Glass, !- Outside Layer",
         "    Dummy Transparent Glass, !- Outside Layer"),
        ("!!    Zero Gap,  !- Layer 2",
         "    Zero Gap,  !- Layer 2"),
        ("!!    PNIPAM Thermochromic Window;  !- Layer 3",
         "    PNIPAM Thermochromic Window;  !- Layer 3"),
    ]
    for commented, active in pairs:
        if activate:
            idf = idf.replace(commented, active)
        else:
            idf = idf.replace(active, commented)
    return idf


def run_one(label, out_dir, pclass, pfile, const):
    print(f"\n{'='*50}\n  {label}\n{'='*50}")
    out = os.path.join(BASE, out_dir)
    if os.path.exists(out):
        shutil.rmtree(out)
    os.makedirs(out, exist_ok=True)

    with open(IDF, "r", encoding="utf-8") as f:
        idf = f.read()

    idf = set_plugin(idf, pclass, pfile)
    idf = set_construction(idf, const)

    with open(IDF, "w", encoding="utf-8") as f:
        f.write(idf)

    r = subprocess.run(
        [E_PLUS, "-w", EPW, "-d", out, IDF],
        capture_output=True, text=True, timeout=1800,
        encoding="utf-8", errors="replace",
    )
    ok = "Completed Successfully" in (r.stdout + r.stderr)
    if ok:
        print("  OK")
    else:
        print("  FAILED")
        err_file = os.path.join(out, "eplusout.err")
        if os.path.exists(err_file):
            with open(err_file, "r", encoding="utf-8", errors="replace") as f:
                for line in f:
                    if "Severe" in line or "Fatal" in line:
                        print(f"    {line.strip()}")
    return ok


def main():
    # 首次初始化
    if not os.path.exists(IDF) or os.path.getsize(IDF) < 500000:
        init_idf()

    results = {}
    for label, out_dir, pclass, pfile, const in STRATEGIES:
        ok = run_one(label, out_dir, pclass, pfile, const)
        results[label] = ok
        if not ok:
            print("\n  失败，停止。")
            break

    print(f"\n{'='*50}\n  结果汇总\n{'='*50}")
    for label, ok in results.items():
        print(f"  {label:<25s} {'OK' if ok else 'FAILED'}")


if __name__ == "__main__":
    main()
