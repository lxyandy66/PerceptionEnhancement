"""全楼 7 策略运行 + 导出 CSV + 汇总"""
import subprocess, os, shutil, re, csv, sqlite3
from collections import defaultdict

E_PLUS = "C:/EnergyPlusV24-2-0/energyplus.exe"
EPW    = "C:/EnergyPlusV24-2-0/WeatherData/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.epw"
BASE   = os.path.dirname(os.path.abspath(__file__))
IDF    = os.path.join(BASE, "REV_ASHRAE901_OfficeMedium_STD2019_Denver_building.idf")

ALL_W = [
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

strategies = [
    ("S1_Eplus_Native","Denver_bld_S1_Native","WeatherFromCSV","plugin_weather_only","Window_All_TC",False,False),
    ("S2_GlassTemp_25","Denver_bld_S2_GlassTemp","AllGlassTempSwitch","plugin_all_glass_temp","Window_All_HIL",False,False),
    ("S3_Mixed","Denver_bld_S3_Mixed","MixedControl","plugin_mixed_control","Window_All_HIL",False,False),
    ("S4_ColdFixed","Denver_bld_S4_Cold","WeatherFromCSV","plugin_weather_only","PNIPAM Step 24",False,False),
    ("S5_HotFixed","Denver_bld_S5_Hot","WeatherFromCSV","plugin_weather_only","PNIPAM Step 0",False,False),
    ("S6_TimeSwitch","Denver_bld_S6_Time","WeatherFromCSV","plugin_weather_only","PNIPAM Step 24",True,False),
    ("S7_EnvTempSwitch","Denver_bld_S7_EnvTemp","WeatherFromCSV","plugin_weather_only","PNIPAM Step 24",False,True),
]

fen_list = ",\n    ".join(ALL_W)

for label, out_dir, pclass, pfile, const, is_s6, is_s7 in strategies:
    print(f"{label}...", end=" ", flush=True)

    with open(IDF, "r", encoding="utf-8") as f:
        idf = f.read()

    # Plugin
    idf = re.sub(
        r"(?<!!)PythonPlugin:Instance,\n.*?;",
        f"PythonPlugin:Instance,\n    {pclass},\n    Yes,\n    {pfile},\n    {pclass};",
        idf, count=1, flags=re.DOTALL,
    )

    # Construction: regex replace ANY current name with target
    for fen in ALL_W:
        idf = re.sub(
            rf"({fen},  !- Name\n    Window,                  !- Surface Type\n    ).+?(, !- Construction Name)",
            rf"\1{const}\2", idf, flags=re.IGNORECASE
        )

    # Clean EC blocks
    idf = re.sub(r"Schedule:Compact,\n    EC_Time_Schedule.*?;\n", "", idf, flags=re.DOTALL)
    idf = re.sub(r"WindowShadingControl,\n    EC_Time_Control.*?;\n", "", idf, flags=re.DOTALL)
    idf = re.sub(r"WindowShadingControl,\n    EC_EnvTemp_Control.*?;\n", "", idf, flags=re.DOTALL)

    if is_s6:
        sched = "Schedule:Compact,\n    EC_Time_Schedule,  !- Name\n    Any Number,\n    Through: 12/31,\n    For: AllDays,\n    Until: 10:00, 0,\n    Until: 17:00, 1,\n    Until: 24:00, 0;\n\n"
        block = f"WindowShadingControl,\n    EC_Time_Control,  !- Name\n    Perimeter_bot_ZN_1,  !- Zone\n    1,\n    SwitchableGlazing,\n    PNIPAM Step 0,\n    OnIfScheduleAllows,\n    EC_Time_Schedule,\n    ,\n    Yes,\n    No,\n    ,\n    ,\n    ,\n    ,\n    ,\n    ,\n    {fen_list};"
        idf = idf.replace("Output:SQLite, SimpleAndTabular;", sched + block + "\nOutput:SQLite, SimpleAndTabular;")
    if is_s7:
        block = f"WindowShadingControl,\n    EC_EnvTemp_Control,  !- Name\n    Perimeter_bot_ZN_1,  !- Zone\n    1,\n    SwitchableGlazing,\n    PNIPAM Step 0,\n    OnIfHighOutdoorAirTemperature,\n    ,\n    34.0,\n    ,\n    No,\n    ,\n    ,\n    ,\n    ,\n    ,\n    ,\n    {fen_list};"
        idf = idf.replace("Output:SQLite, SimpleAndTabular;", block + "\nOutput:SQLite, SimpleAndTabular;")

    with open(IDF, "w", encoding="utf-8") as f:
        f.write(idf)

    out = os.path.join(BASE, out_dir)
    if os.path.exists(out):
        shutil.rmtree(out)
    os.makedirs(out, exist_ok=True)

    r = subprocess.run([E_PLUS, "-w", EPW, "-d", out, IDF],
        capture_output=True, text=True, timeout=1800, encoding="utf-8", errors="replace")
    ok = "Completed Successfully" in (r.stdout + r.stderr)
    print("OK" if ok else "FAILED")
    if not ok:
        break

# Export CSVs
print("\nExporting CSVs...")
for label, out_dir, *_ in strategies:
    db_path = os.path.join(BASE, out_dir, "eplusout.sql")
    if not os.path.exists(db_path):
        continue
    db = sqlite3.connect(db_path)
    data = defaultdict(dict)

    for row in db.execute("SELECT r.TimeIndex, r.Value, d.KeyValue FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = 'Zone Mean Air Temperature'"):
        data[row[0]][f"T_{row[2][:25]}"] = round(row[1], 3)

    for name in ALL_W:
        short = name.replace("PERIMETER_", "P_").replace("_WALL_", "_")[:30]
        for var, prefix in [("Surface Window Transmitted Solar Radiation Rate", "Qsol"),
                             ("Surface Window Heat Gain Rate", "Qgain"),
                             ("Surface Inside Face Temperature", "Ti"),
                             ("Surface Outside Face Incident Solar Radiation Rate per Area", "Iinc")]:
            for row in db.execute(f"SELECT r.TimeIndex, r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = '{var}' AND d.KeyValue = '{name}'"):
                data[row[0]][f"{prefix}_{short}"] = round(row[1], 3 if "Temperature" in var or "per Area" in var else 1)

    for var, col in [("Air System Total Cooling Energy", "Cooling_J"), ("Air System Total Heating Energy", "Heating_J")]:
        for row in db.execute(f"SELECT r.TimeIndex, r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = '{var}'"):
            data[row[0]][col] = round(row[1], 1)

    if not data:
        continue
    cols = sorted(data[min(data.keys())].keys())
    out_csv = os.path.join(BASE, f"Denver_building_{label}_minutely.csv")
    with open(out_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["TimeIndex"] + cols)
        w.writeheader()
        for t in sorted(data.keys()):
            row = {"TimeIndex": t, **{c: data[t].get(c, "") for c in cols}}
            w.writerow(row)
    print(f"  {label}: {len(data)} rows x {len(cols)+1} cols")
    db.close()

# Summary
print(f"\n{'Strategy':<22s} {'Tavg':>6s} {'Qsol':>10s} {'Cool':>8s} {'Heat':>8s}")
print("-" * 56)
for label, out_dir, *_ in strategies:
    db_path = os.path.join(BASE, out_dir, "eplusout.sql")
    if not os.path.exists(db_path):
        continue
    db = sqlite3.connect(db_path)
    z = [r[0] for r in db.execute("SELECT r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = 'Zone Mean Air Temperature'")]
    t = sum(z) / len(z) if z else 0
    s = sum(v[0] for n in ALL_W for v in db.execute(f"SELECT r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = 'Surface Window Transmitted Solar Radiation Rate' AND d.KeyValue = '{n}'") if v[0] > 0) * 60
    c_val = max([r[0] for r in db.execute("SELECT r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = 'Air System Total Cooling Energy'")] or [0]) / 1e6
    h_val = max([r[0] for r in db.execute("SELECT r.Value FROM ReportData r JOIN ReportDataDictionary d ON r.ReportDataDictionaryIndex = d.ReportDataDictionaryIndex WHERE d.Name = 'Air System Total Heating Energy'")] or [0]) / 1e6
    print(f"{label:<22s} {t:>5.1f}C {s/1e6:>8.1f}MJ {c_val:>6.1f}MJ {h_val:>6.1f}MJ")
    db.close()

print("Done.")
