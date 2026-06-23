#!/usr/bin/env python3
"""Sammelt die relevanten Jahreswerte aus den Output-Dateien der R-Skripte.

Wichtig: Das Skript liegt in scripts/. Standardmässig wird deshalb eine Ebene
nach oben gegangen und dort der Ordner output/ ausgewertet.

Verwendete Dateien aus den R-Skripten:
- output/autarky_with_without_battery_summary.csv
- output/energy_15min_prepared.csv
- output/annual_sums_production_theoretical_consumption.csv
Optional fuer aktuelle PV-Anlage:
- PV_MGH_Winterthur_15min-Werte_2025.xlsx
- Verbrauch/Verbrauch_Gesamt_Giesserei_2025.csv
"""
from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Optional

import pandas as pd

ORDER = ["Aktuelle PV-Anlage", "Polysun ohne Akku", "Polysun mit Akku"]
COLUMNS = [
    "Szenario",
    "Quelle",
    "Verbrauch [kWh/a]",
    "PV-Erzeugung [kWh/a]",
    "Autarkiegrad [%]",
    "Eingespeiste Energie [kWh/a]",
    "Genutzte eigene Energie [kWh/a]",
    "Genutzte PV-Energie [kWh/a]",
    "Bezogene Energie [kWh/a]",
    "Eigenverbrauchsquote [%]",
    "Batteriekapazität [kWh]",
    "Hinweis",
]

IMPORTANT_OUTPUT_FILES = {
    "autarky_summary": "autarky_with_without_battery_summary.csv",
    "energy_prepared": "energy_15min_prepared.csv",
    "annual_sums": "annual_sums_production_theoretical_consumption.csv",
    "simulation_detail": "simulation_with_battery_detail.csv",
    "fixed_battery_detail": "simulation_fixed_battery_output.csv",
}


def script_repo_root() -> Path:
    here = Path(__file__).resolve()
    return here.parents[1] if here.parent.name == "scripts" else Path.cwd().resolve()


def norm(text: object) -> str:
    value = str(text).strip().lower()
    for a, b in {"ä": "ae", "ö": "oe", "ü": "ue", "ß": "ss"}.items():
        value = value.replace(a, b)
    return value


def num(value: object) -> Optional[float]:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip().replace("\u00a0", "").replace("'", "").replace(" ", "")
    if "," in text and "." in text:
        text = text.replace(".", "")
    text = text.replace(",", ".")
    match = re.search(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", text)
    return float(match.group(0)) if match else None


def series_num(s: pd.Series) -> pd.Series:
    return s.map(num).astype(float)


def read_csv_flexible(path: Path) -> pd.DataFrame:
    last_error = None
    for enc in ("utf-8-sig", "utf-8", "latin1"):
        for sep in (",", ";", "\t", None):
            try:
                df = pd.read_csv(path, sep=sep, engine="python", encoding=enc)
                if len(df.columns) > 1:
                    return df
            except Exception as exc:  # bewusst tolerant fuer verschiedene CSV-Exporte
                last_error = exc
    raise RuntimeError(f"CSV konnte nicht gelesen werden: {path} ({last_error})")


def parse_time(s: pd.Series) -> pd.Series:
    return pd.to_datetime(s, errors="coerce", dayfirst=True)


def filter_year(df: pd.DataFrame, time_col: str, year: Optional[int]) -> pd.DataFrame:
    if time_col not in df.columns or year is None:
        return df
    t = parse_time(df[time_col])
    return df.loc[t.dt.year == year].copy()


def find_output_file(output_dir: Path, filename: str) -> Optional[Path]:
    direct = output_dir / filename
    if direct.exists():
        return direct
    matches = list(output_dir.rglob(filename))
    if matches:
        return matches[0]
    # Kleine Fallback-Suche, falls Gross-/Kleinschreibung oder Zusatzsuffix abweicht.
    stem = norm(Path(filename).stem)
    candidates = [p for p in output_dir.rglob("*.csv") if stem in norm(p.stem)]
    return candidates[0] if candidates else None


def report_row(
    scenario: str,
    source: str,
    consumption: Optional[float] = None,
    production: Optional[float] = None,
    used_own: Optional[float] = None,
    grid_import: Optional[float] = None,
    feed_in: Optional[float] = None,
    used_pv: Optional[float] = None,
    autarky: Optional[float] = None,
    self_consumption: Optional[float] = None,
    battery_capacity: Optional[float] = None,
    note: str = "",
) -> dict[str, object]:
    if used_own is None and consumption is not None and grid_import is not None:
        used_own = max(consumption - grid_import, 0)
    if used_pv is None and production is not None and feed_in is not None:
        used_pv = max(production - feed_in, 0)
    if used_pv is None:
        used_pv = used_own
    if autarky is None and consumption and used_own is not None:
        autarky = used_own / consumption * 100
    if self_consumption is None and production and used_pv is not None:
        self_consumption = used_pv / production * 100
    return {
        "Szenario": scenario,
        "Quelle": source,
        "Verbrauch [kWh/a]": consumption,
        "PV-Erzeugung [kWh/a]": production,
        "Autarkiegrad [%]": autarky,
        "Eingespeiste Energie [kWh/a]": feed_in,
        "Genutzte eigene Energie [kWh/a]": used_own,
        "Genutzte PV-Energie [kWh/a]": used_pv,
        "Bezogene Energie [kWh/a]": grid_import,
        "Eigenverbrauchsquote [%]": self_consumption,
        "Batteriekapazität [kWh]": battery_capacity,
        "Hinweis": note,
    }


def rows_from_autarky_summary(path: Path) -> list[dict[str, object]]:
    df = read_csv_flexible(path)
    rows = []
    for _, r in df.iterrows():
        scenario_text = norm(r.get("Scenario", ""))
        if "without" in scenario_text or "ohne" in scenario_text:
            label = "Polysun ohne Akku"
        elif "with" in scenario_text or "mit" in scenario_text:
            label = "Polysun mit Akku"
        else:
            continue
        rows.append(report_row(
            scenario=label,
            source=str(path),
            consumption=num(r.get("Consumption_kWh")),
            production=num(r.get("Production_kWh")),
            used_own=num(r.get("SelfUsedElectricity_kWh")),
            grid_import=num(r.get("GridImport_kWh")),
            feed_in=num(r.get("FeedIn_kWh")),
            used_pv=num(r.get("PVUsedOnSiteIncludingBatteryCharging_kWh")),
            autarky=num(r.get("Autarky_percent")),
            self_consumption=num(r.get("SelfConsumptionRate_percent")),
            battery_capacity=num(r.get("BatteryCapacity_kWh")),
        ))
    return rows


def row_without_battery_from_energy(path: Path, year: Optional[int]) -> dict[str, object]:
    df = filter_year(read_csv_flexible(path), "Time", year)
    production = series_num(df["Production_kWh"]).sum() if "Production_kWh" in df else None
    consumption = series_num(df["ConsumptionTotal_kWh"]).sum() if "ConsumptionTotal_kWh" in df else None
    used = series_num(df["SelfConsumptionPotential_kWh"]).sum() if "SelfConsumptionPotential_kWh" in df else None
    feed = series_num(df["FeedInPotential_kWh"]).sum() if "FeedInPotential_kWh" in df else None
    imp = series_num(df["GridImportPotential_kWh"]).sum() if "GridImportPotential_kWh" in df else None
    return report_row(
        scenario="Polysun ohne Akku",
        source=str(path),
        consumption=consumption,
        production=production,
        used_own=used,
        used_pv=used,
        grid_import=imp,
        feed_in=feed,
        battery_capacity=0,
    )


def current_pv_from_raw(repo_root: Path, year: Optional[int]) -> Optional[dict[str, object]]:
    pv_files = list(repo_root.rglob("PV_MGH_Winterthur_15min-Werte_2025.xlsx"))
    consumption_path = repo_root / "Verbrauch" / "Verbrauch_Gesamt_Giesserei_2025.csv"
    if not pv_files or not consumption_path.exists():
        return None

    pv = pd.read_excel(pv_files[0], header=None, skiprows=1, names=["Time", "Production_kWh"])
    pv["Time"] = parse_time(pv["Time"]).dt.floor("15min")
    pv["Production_kWh"] = series_num(pv["Production_kWh"])
    pv = pv.dropna(subset=["Time"]).groupby("Time", as_index=False)["Production_kWh"].sum()

    cons = read_csv_flexible(consumption_path)
    if "Timestamp" not in cons.columns or "Volume" not in cons.columns:
        return None
    cons = cons.assign(Time=parse_time(cons["Timestamp"]).dt.floor("15min"), Consumption_kWh=series_num(cons["Volume"]))
    cons = cons.dropna(subset=["Time"]).groupby("Time", as_index=False)["Consumption_kWh"].sum()

    merged = pd.merge(cons, pv, on="Time", how="outer").fillna({"Consumption_kWh": 0, "Production_kWh": 0})
    if year is not None:
        merged = merged.loc[merged["Time"].dt.year == year].copy()
    consumption = float(merged["Consumption_kWh"].sum())
    production = float(merged["Production_kWh"].sum())
    used = float(pd.concat([merged["Consumption_kWh"], merged["Production_kWh"]], axis=1).min(axis=1).sum())
    feed = float((merged["Production_kWh"] - merged["Consumption_kWh"]).clip(lower=0).sum())
    imp = float((merged["Consumption_kWh"] - merged["Production_kWh"]).clip(lower=0).sum())
    return report_row(
        scenario="Aktuelle PV-Anlage",
        source=f"{pv_files[0]} + {consumption_path}",
        consumption=consumption,
        production=production,
        used_own=used,
        used_pv=used,
        grid_import=imp,
        feed_in=feed,
        battery_capacity=0,
    )


def current_pv_from_annual_sums(path: Path) -> Optional[dict[str, object]]:
    df = read_csv_flexible(path)
    if not {"Metric", "Value_kWh"}.issubset(df.columns):
        return None
    metrics = {norm(r["Metric"]): num(r["Value_kWh"]) for _, r in df.iterrows()}
    production = metrics.get("production")
    consumption = metrics.get("consumption")
    if production is None and consumption is None:
        return None
    return report_row(
        scenario="Aktuelle PV-Anlage",
        source=str(path),
        consumption=consumption,
        production=production,
        note="Nur Jahresproduktion und Verbrauch vorhanden; Netzbezug/Einspeisung/Autarkie können aus dieser Datei nicht sicher berechnet werden.",
    )


def collect(repo_root: Path, output_dir: Path, year: Optional[int]) -> tuple[pd.DataFrame, pd.DataFrame]:
    found = {key: find_output_file(output_dir, name) for key, name in IMPORTANT_OUTPUT_FILES.items()}
    rows: list[dict[str, object]] = []
    notes = []

    current = current_pv_from_raw(repo_root, year)
    if current is not None:
        rows.append(current)
    elif found["annual_sums"] is not None:
        fallback = current_pv_from_annual_sums(found["annual_sums"])
        if fallback is not None:
            rows.append(fallback)

    if found["autarky_summary"] is not None:
        rows.extend(rows_from_autarky_summary(found["autarky_summary"]))
    elif found["energy_prepared"] is not None:
        rows.append(row_without_battery_from_energy(found["energy_prepared"], year))
        notes.append("Mit-Akku-Werte fehlen, weil autarky_with_without_battery_summary.csv nicht gefunden wurde.")

    for key, path in found.items():
        notes.append(f"{key}: {path if path else 'nicht gefunden'}")

    summary = pd.DataFrame(rows, columns=COLUMNS)
    if not summary.empty:
        summary["_order"] = summary["Szenario"].map({name: i for i, name in enumerate(ORDER)}).fillna(99)
        summary = summary.sort_values(["_order", "Szenario"]).drop(columns="_order")
        numeric_cols = [c for c in summary.columns if c.endswith("]") or c.endswith("[%]")]
        summary[numeric_cols] = summary[numeric_cols].round(3)

    details = pd.DataFrame({"Hinweis": notes})
    return summary, details


def write_output(summary: pd.DataFrame, details: pd.DataFrame, output_file: Path) -> None:
    output_file.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(output_file, engine="openpyxl") as writer:
        summary.to_excel(writer, sheet_name="Jahreswerte", index=False)
        details.to_excel(writer, sheet_name="Gefundene Dateien", index=False)
        for sheet in writer.book.worksheets:
            sheet.freeze_panes = "A2"
            for cells in sheet.columns:
                width = max(12, min(max(len(str(c.value or "")) for c in cells) + 2, 80))
                sheet.column_dimensions[cells[0].column_letter].width = width
    summary.to_csv(output_file.with_suffix(".csv"), index=False, sep=";", decimal=",")


def main() -> int:
    default_root = script_repo_root()
    parser = argparse.ArgumentParser(description="Sammelt Jahreswerte aus den wichtigen output/*.csv-Dateien.")
    parser.add_argument("--repo-root", type=Path, default=default_root, help="Repo-Hauptordner. Standard: eine Ebene oberhalb von scripts/.")
    parser.add_argument("--output-dir", type=Path, default=None, help="Ordner mit R-Outputdateien. Standard: <repo-root>/output.")
    parser.add_argument("--year", type=int, default=None, help="Optionaler Jahresfilter fuer Zeitreihen-Dateien.")
    parser.add_argument("--result", type=Path, default=None, help="Excel-Zieldatei. Standard: <output-dir>/jahreszusammenfassung.xlsx.")
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    output_dir = (args.output_dir or repo_root / "output").resolve()
    result = (args.result or output_dir / "jahreszusammenfassung.xlsx").resolve()

    summary, details = collect(repo_root, output_dir, args.year)
    write_output(summary, details, result)

    print(f"Repo-Root: {repo_root}")
    print(f"Output-Ordner: {output_dir}")
    print(f"Geschrieben: {result}")
    print(summary.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
