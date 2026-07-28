"""
全楼 PNIPAM 25态 + 7 策略运行
"""
import subprocess, os, shutil, re

E_PLUS = "C:/EnergyPlusV24-2-0/energyplus.exe"
EPW    = "C:/EnergyPlusV24-2-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"
BASE   = os.path.dirname(os.path.abspath(__file__))
ORIGIN = os.path.join(BASE, "Origin-ASHRAE901_OfficeMedium_STD2019_Denver.idf")
IDF    = os.path.join(BASE, "REV_ASHRAE901_OfficeMedium_STD2019_Denver_building.idf")

ALL_WINDOWS = [
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW1","PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW2",
    "PERIMETER_BOT_ZN_1_WALL_SOUTH_WINDOW3","PERIMETER_BOT_ZN_3_WALL_NORTH_WINDOW1",
    "PERIMETER_BOT_ZN_3_WALL_NORTH_WINDOW2","PERIMETER_BOT_ZN_3_WALL_NORTH_WINDOW3",
    "PERIMETER_BOT_ZN_2_WALL_EAST_WINDOW1","PERIMETER_BOT_ZN_2_WALL_EAST_WINDOW2",
    "PERIMETER_BOT_ZN_4_WALL_WEST_WINDOW1","PERIMETER_BOT_ZN_4_WALL_WEST_WINDOW2",
    "PERIMETER_MID_ZN_1_WALL_SOUTH_WINDOW","PERIMETER_MID_ZN_2_WALL_EAST_WINDOW",
    "PERIMETER_MID_ZN_3_WALL_NORTH_WINDOW","PERIMETER_MID_ZN_4_WALL_WEST_WINDOW",
    "PERIMETER_TOP_ZN_1_WALL_SOUTH_WINDOW","PERIMETER_TOP_ZN_2_WALL_EAST_WINDOW",
    "PERIMETER_TOP_ZN_3_WALL_NORTH_WINDOW","PERIMETER_TOP_ZN_4_WALL_WEST_WINDOW",
]

STRATEGIES = [
    # (label, output_dir, plugin_class, plugin_file, construction)
    ("S1_Eplus_Native",   "Denver_bld_S1_Native",   "WeatherFromCSV",       "plugin_weather_only",      "Window_All_TC"),
    ("S2_GlassTemp_25",   "Denver_bld_S2_GlassTemp", "AllGlassTempSwitch",   "plugin_all_glass_temp",    "Window_All_HIL"),
    ("S3_Mixed",          "Denver_bld_S3_Mixed",     "MixedControl",         "plugin_mixed_control",     "Window_All_HIL"),
    ("S4_ColdFixed",      "Denver_bld_S4_Cold",      "WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 24"),
    ("S5_HotFixed",       "Denver_bld_S5_Hot",       "WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 0"),
    ("S6_TimeSwitch",     "Denver_bld_S6_Time",      "WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 24"),
    ("S7_EnvTempSwitch",  "Denver_bld_S7_EnvTemp",   "WeatherFromCSV",       "plugin_weather_only",      "PNIPAM Step 24"),
]


def init_building_idf():
    """从 Origin 初始化全楼 PNIPAM IDF"""
    with open(ORIGIN, "r", encoding="utf-8") as f: idf = f.read()

    # 基础修改
    idf = idf.replace("Timestep,4;", "Timestep,60;")
    idf = re.sub(r"Site:Location,.*?;",
        "Site:Location,\n    Site 1,  !- Name\n    22.32,   !- Latitude\n    114.17,  !- Longitude\n    8.0,     !- Time Zone\n    5;       !- Elevation",
        idf, flags=re.DOTALL)
    rps = list(re.finditer(r"RunPeriod,.*?;", idf, re.DOTALL))
    new_rp = ("RunPeriod,\n    Run Period 1,  !- Name\n    10,  !- Begin Month\n    30,  !- Begin Day\n    2025,!- Begin Year\n"
              "    10,  !- End Month\n    31,  !- End Day\n    2025,!- End Year\n    Thursday,  !- Start Day\n    No,No,No,Yes,Yes;")
    if rps:
        idf = idf[:rps[0].start()] + new_rp + idf[rps[0].end():]
        for rp in rps[1:]: idf = idf.replace(rp.group(), "")

    # 替换 SimpleGlazingSystem + 添加 PNIPAM 材料
    old_glazing = "WindowMaterial:SimpleGlazingSystem,\n    Glazing Layer,           !- Name\n    2.0441736,               !- U-Factor {W/m2-K}\n    0.38,                    !- Solar Heat Gain Coefficient\n    0.418;                   !- Visible Transmittance"
    new_mats = (
        "WindowMaterial:SimpleGlazingSystem,\n    Glazing Layer,  !- Name (backup, unused)\n    2.0441736,  !- U-Factor\n    0.38,  !- SHGC\n    0.418;  !- VT\n\n"
        "!- ====== PNIPAM 基础材料 ======\n\n"
        + _glazing_expanded("PNIPAM Hydrogel Hot", 0.003, 0.1000, 0.7200, 0.7200, 0.0600, 0.7800, 0.7800, 0.28) + "\n"
        + _glazing_expanded("PNIPAM Hydrogel Cold", 0.003, 0.8200, 0.0900, 0.0900, 0.8700, 0.0700, 0.0700, 0.28) + "\n"
        + _glazing_expanded("Dummy Transparent Glass", 0.0001, 0.999, 0.0005, 0.0005, 0.999, 0.0005, 0.0005, 221) + "\n"
        "WindowMaterial:Gas,\n    Zero Gap,  !- Name\n    Air, 0.0001;\n\n"
        "WindowMaterial:GlazingGroup:Thermochromic,\n    PNIPAM Thermochromic Window,  !- Name\n"
        "    20, PNIPAM Hydrogel Cold,\n    30, PNIPAM Hydrogel Cold,\n    34, PNIPAM Hydrogel Hot;\n"
    )
    idf = idf.replace(old_glazing, new_mats)

    # 插入全楼 TC 和 HIL 构造
    marker = "Construction,\n    Window_U_0.36_SHGC_0.38, !- Name\n    Glazing Layer;           !- Outside Layer"
    extra = (
        "\nConstruction,\n    Window_All_TC,  !- Name (全楼 E+ 原生 TC)\n"
        "    Dummy Transparent Glass, !- Outside Layer\n    Zero Gap,  !- Layer 2\n"
        "    PNIPAM Thermochromic Window;  !- Layer 3\n\n"
        "Construction,\n    Window_All_HIL, !- Name (全楼 HIL 25态)\n    PNIPAM Step 12;  !- Tsol=0.460\n"
    )
    idx = idf.find(marker)
    if idx > 0:
        end_of_line = idf.find("\n", idf.find("Outside Layer", idx))
        idf = idf[:end_of_line+1] + extra + idf[end_of_line+1:]

    # 所有18窗改用 Window_All_HIL
    for fen in ALL_WINDOWS:
        idf = idf.replace(
            f"    {fen},  !- Name\n    Window,                  !- Surface Type\n    Window_U_0.36_SHGC_0.38, !- Construction Name",
            f"    {fen},  !- Name\n    Window,                  !- Surface Type\n    Window_All_HIL, !- Construction Name"
        )

    # 25态块
    block_25 = _generate_25state()
    tail = (
        "\n\n" + block_25 + "\n\n"
        "Output:SQLite, SimpleAndTabular;\n\n"
        "!- ====== Python Plugin + 输出变量 ======\n\n"
        "PythonPlugin:SearchPaths, ., ;\n\n"
        "PythonPlugin:Instance,\n    WeatherFromCSV,  !- Name\n    Yes,\n    plugin_weather_only,\n    WeatherFromCSV;\n\n"
        "Output:Variable, *, Zone Mean Air Temperature, Timestep;\n"
        "Output:Variable, *, Surface Window Transmitted Solar Radiation Rate, Timestep;\n"
        "Output:Variable, *, Surface Window Heat Gain Rate, Timestep;\n"
        "Output:Variable, *, Surface Inside Face Temperature, Timestep;\n"
        "Output:Variable, *, Surface Outside Face Temperature, Timestep;\n"
        "Output:Variable, *, Surface Outside Face Incident Solar Radiation Rate per Area, Timestep;\n"
        "Output:Variable, *, Air System Total Cooling Energy, Timestep;\n"
        "Output:Variable, *, Air System Total Heating Energy, Timestep;\n"
    )
    idf = idf.rstrip() + tail

    # S6/S7 的 WindowShadingControl 在运行时动态添加

    with open(IDF, "w", encoding="utf-8") as f: f.write(idf)
    print("Building IDF initialized.")


def _glazing_expanded(name, t, tsol, rsol_f, rsol_b, tvis, rvis_f, rvis_b, lam):
    return (f"WindowMaterial:Glazing,\n    {name},  !- Name\n    SpectralAverage,  !- Optical Data Type\n    ,  !- Spectral Data Set\n    {t},  !- Thickness\n"
            f"    {tsol:.4f},  !- Solar Transmittance\n    {rsol_f:.4f},  !- Front Solar Reflectance\n    {rsol_b:.4f},  !- Back Solar Reflectance\n"
            f"    {tvis:.4f},  !- Visible Transmittance\n    {rvis_f:.4f},  !- Front Visible Reflectance\n    {rvis_b:.4f},  !- Back Visible Reflectance\n"
            f"    0,  !- IR Transmittance\n    0.84,  !- Front IR Emissivity\n    0.84,  !- Back IR Emissivity\n    {lam},  !- Conductivity\n    1,  !- Dirt Factor\n    No;  !- Solar Diffusing")

def _generate_25state():
    N=24; step=0.72/N; lines=["!- ====== PNIPAM 25态 ======",""]
    for i in range(N+1):
        tsol=0.10+i*step; rsol=round(0.72-(i/N)*0.63,4); tvis=round(0.06+(i/N)*0.81,4); rvis=round(0.78-(i/N)*0.71,4)
        lines+=[f"WindowMaterial:Glazing,",f"    PNIPAM Step {i},  !- Name (Tsol={tsol:.3f})",
                f"    SpectralAverage,  !- Optical Data Type",f"    ,  !- Spectral Data Set",
                f"    0.003,  !- Thickness",f"    {tsol:.4f},  !- Solar Transmittance",
                f"    {rsol:.4f},  !- Front Solar Reflectance",f"    {rsol:.4f},  !- Back Solar Reflectance",
                f"    {tvis:.4f},  !- Visible Transmittance",f"    {rvis:.4f},  !- Front Visible Reflectance",
                f"    {rvis:.4f},  !- Back Visible Reflectance",f"    0,  !- IR Transmittance",
                f"    0.84,  !- Front IR Emissivity",f"    0.84,  !- Back IR Emissivity",
                f"    0.28,  !- Conductivity",f"    1,  !- Dirt Factor",f"    No;  !- Solar Diffusing",""]
    lines+=["!- ====== 25态构造 =====",""]
    for i in range(N+1): lines+=[f"Construction,",f"    PNIPAM Step {i},  PNIPAM Step {i};",""]
    return "\n".join(lines)

def _switchable_block(name, seq, control_type, schedule_setpoint):
    fen = ",\n    ".join(ALL_WINDOWS)
    return (f"WindowShadingControl,\n    {name},  !- Name\n    Perimeter_bot_ZN_1,  !- Zone Name\n"
            f"    {seq},  !- Sequence Number\n    SwitchableGlazing,  !- Shading Type\n"
            f"    PNIPAM Step 0,  !- Construction with Shading (热态)\n"
            f"    {control_type},  !- Shading Control Type\n    {schedule_setpoint}\n"
            f"    No,  !- Glare Control Is Active\n    ,  !- Shading Device Material Name\n"
            f"    ,  !- Type of Slat Angle Control\n    ,  !- Slat Angle Schedule Name\n"
            f"    ,  !- Setpoint 2\n    ,  !- Daylighting Control Object Name\n"
            f"    ,  !- Multiple Surface Control Type\n    {fen};")


def run_one(label, out_dir, pclass, pfile, const):
    print(f"\n{'='*50}\n  {label}\n{'='*50}")
    out = os.path.join(BASE, out_dir)
    if os.path.exists(out): shutil.rmtree(out)
    os.makedirs(out, exist_ok=True)

    with open(IDF, "r", encoding="utf-8") as f: idf = f.read()

    idf = re.sub(r"(?<!!)PythonPlugin:Instance,\n.*?;",
        f"PythonPlugin:Instance,\n    {pclass},  !- Name\n    Yes,\n    {pfile},\n    {pclass};",
        idf, count=1, flags=re.DOTALL)

    for fen in ALL_WINDOWS:
        idf = re.sub(rf"({fen},  !- Name\n    Window,                  !- Surface Type\n    ).+?,( !- Construction Name)",
                     rf"\1{const},\2", idf)

    # Remove any old SwitchableGlazing blocks
    idf = re.sub(r'Schedule:Compact,\n    EC_Time_Schedule.*?;\n', '', idf, flags=re.DOTALL)
    idf = re.sub(r'WindowShadingControl,\n    EC_Time_Control.*?;\n', '', idf, flags=re.DOTALL)
    idf = re.sub(r'WindowShadingControl,\n    EC_EnvTemp_Control.*?;\n', '', idf, flags=re.DOTALL)

    # S6/S7: add SwitchableGlazing
    if "S6" in label or "S7" in label:
        if "S6" in label:
            block = _switchable_block("EC_Time_Control", 1, "OnIfScheduleAllows",
                "EC_Time_Schedule,  !- Schedule Name\n    ,  !- Setpoint\n    Yes,  !- IsScheduled")
            schedule = "Schedule:Compact,\n    EC_Time_Schedule,  !- Name\n    Any Number,\n    Through: 12/31,\n    For: AllDays,\n    Until: 10:00, 0,\n    Until: 17:00, 1,\n    Until: 24:00, 0;\n\n"
            idf = idf.replace("Output:SQLite, SimpleAndTabular;", schedule + block + "\nOutput:SQLite, SimpleAndTabular;")
        else:
            block = _switchable_block("EC_EnvTemp_Control", 1, "OnIfHighOutdoorAirTemperature",
                ",  !- Schedule Name\n    34.0,  !- Setpoint\n    ,  !- IsScheduled")
            idf = idf.replace("Output:SQLite, SimpleAndTabular;", block + "\nOutput:SQLite, SimpleAndTabular;")

    with open(IDF, "w", encoding="utf-8") as f: f.write(idf)

    r = subprocess.run([E_PLUS, "-w", EPW, "-d", out, IDF],
        capture_output=True, text=True, timeout=1800, encoding="utf-8", errors="replace")
    ok = "Completed Successfully" in (r.stdout + r.stderr)
    print(f"  {'OK' if ok else 'FAILED'}")
    if not ok:
        err_file = os.path.join(out, "eplusout.err")
        if os.path.exists(err_file):
            with open(err_file, "r", encoding="utf-8", errors="replace") as f:
                for line in f:
                    if "Severe" in line or "Fatal" in line: print(f"    {line.strip()[:150]}")
    return ok


def main():
    if not os.path.exists(IDF): init_building_idf()
    results = {}
    for label, out_dir, pclass, pfile, const in STRATEGIES:
        ok = run_one(label, out_dir, pclass, pfile, const)
        results[label] = ok
        if not ok: break
    print(f"\n{'='*50}\n  结果汇总\n{'='*50}")
    for label, ok in results.items(): print(f"  {label:<25s} {'OK' if ok else 'FAILED'}")


if __name__ == "__main__":
    main()
