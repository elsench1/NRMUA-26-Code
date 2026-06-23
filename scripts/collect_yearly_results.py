#!/usr/bin/env python3
"""Jahreszusammenfassung fuer PV-/Polysun-Ergebnisse.

Das Skript durchsucht CSV- und Excel-Dateien und erstellt eine Excel-Datei mit:
Verbrauch, PV-Erzeugung, Autarkiegrad, Einspeisung, genutzter eigener Energie,
genutzter PV-Energie und bezogener Energie fuer:
- aktuelle PV-Anlage
- Polysun ohne Akku
- Polysun mit Akku

Beispiele:
  python scripts/collect_yearly_results.py
  python scripts/collect_yearly_results.py --year 2025
  python scripts/collect_yearly_results.py --current-pv pfad.csv --polysun-without-battery ohne.xlsx --polysun-with-battery mit.xlsx
"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import pandas as pd

SCENARIOS = {
    "current_pv": "Aktuelle PV-Anlage",
    "polysun_without_battery": "Polysun ohne Akku",
    "polysun_with_battery": "Polysun mit Akku",
}
SUFFIXES = {".csv", ".txt", ".xlsx", ".xlsm", ".xls"}
SKIP_DIRS = {".git", ".github", "__pycache__", ".venv", "venv", "env", "node_modules", "results"}
PATTERNS = {
    "load": [r"verbrauch", r"strombedarf", r"energiebedarf", r"last(?!.*profil)", r"load", r"demand", r"consumption", r"bedarf"],
    "pv_generation": [r"pv.*(erzeug|produktion|generation|yield|energy)", r"photovoltaik", r"solar.*(erzeug|produktion|generation|yield|energy)", r"produktion.*pv", r"erzeugung.*pv", r"pv$"],
    "grid_import": [r"netzbezug", r"bezug.*netz", r"grid.*import", r"import.*grid", r"from.*grid", r"purchased", r"bezogene.*energie"],
    "grid_export": [r"einspeis", r"eingespeis", r"feed.?in", r"grid.*export", r"export.*grid", r"to.*grid", r"surplus", r"ueberschuss", r"überschuss"],
}
TIME_PATTERNS = [r"datum", r"date", r"zeit", r"time", r"timestamp", r"datetime", r"stunde", r"hour"]
BAD_COLS = [r"%", r"quote", r"rate", r"grad", r"anteil", r"preis", r"cost", r"kosten", r"eur", r"chf"]


def norm(x: object) -> str:
    s = str(x).strip().lower()
    for a, b in {"ä": "ae", "ö": "oe", "ü": "ue", "ß": "ss"}.items():
        s = s.replace(a, b)
    return re.sub(r"\s+", " ", s)


def to_number(s: pd.Series) -> pd.Series:
    if pd.api.types.is_numeric_dtype(s):
        return pd.to_numeric(s, errors="coerce")
    x = (s.astype(str).str.strip().str.replace("\u00a0", "", regex=False).str.replace("'", "", regex=False).str.replace(" ", "", regex=False))
    both = x.str.contains(",", regex=False) & x.str.contains(".", regex=False)
    x = x.where(~both, x.str.replace(".", "", regex=False)).str.replace(",", ".", regex=False)
    x = x.str.extract(r"([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)", expand=False)
    return pd.to_numeric(x, errors="coerce")


def unit(col: str) -> str:
    n = norm(col)
    if re.search(r"\bmwh\b|\[mwh\]|\(mwh\)", n):
        return "MWh"
    if re.search(r"\bkwh\b|\[kwh\]|\(kwh\)", n):
        return "kWh"
    if re.search(r"\bwh\b|\[wh\]|\(wh\)", n):
        return "Wh"
    if re.search(r"\bkw\b|\[kw\]|\(kw\)", n):
        return "kW"
    if re.search(r"\bw\b|\[w\]|\(w\)", n):
        return "W"
    return "unknown"


def time_column(df: pd.DataFrame) -> Optional[str]:
    candidates = [c for c in df.columns if any(re.search(p, norm(c)) for p in TIME_PATTERNS)] or list(df.columns[:1])
    best, best_count = None, 0
    for c in candidates:
        parsed = pd.to_datetime(df[c], errors="coerce", dayfirst=True)
        count = int(parsed.notna().sum())
        if count > best_count and count >= max(3, len(df) * 0.5):
            best, best_count = c, count
    return best


def timestep_hours(times: pd.Series) -> Optional[float]:
    t = pd.to_datetime(times, errors="coerce", dayfirst=True).dropna().sort_values()
    if len(t) < 3:
        return None
    d = t.diff().dropna().dt.total_seconds() / 3600
    d = d[(d > 0) & (d <= 24 * 31)]
    return None if d.empty else float(d.median())


def as_kwh(series: pd.Series, col: str, dt_h: Optional[float], notes: list[str]) -> float:
    v, u = to_number(series), unit(col)
    if u == "MWh":
        return float((v * 1000).dropna().sum())
    if u == "kWh":
        return float(v.dropna().sum())
    if u == "Wh":
        return float((v / 1000).dropna().sum())
    if u == "kW":
        if dt_h is None:
            notes.append(f"'{col}' sieht nach kW aus, aber Zeitraster fehlt; Werte wie kWh summiert.")
            return float(v.dropna().sum())
        return float((v * dt_h).dropna().sum())
    if u == "W":
        if dt_h is None:
            notes.append(f"'{col}' sieht nach W aus, aber Zeitraster fehlt; Werte in kW umgerechnet und summiert.")
            return float((v / 1000).dropna().sum())
        return float(((v / 1000) * dt_h).dropna().sum())
    notes.append(f"Keine Einheit in '{col}' erkannt; Werte als kWh interpretiert.")
    return float(v.dropna().sum())


def find_columns(df: pd.DataFrame) -> dict[str, str]:
    out = {}
    for metric, patterns in PATTERNS.items():
        best_col, best_score = None, 0
        for c in df.columns:
            n = norm(c)
            if any(re.search(p, n) for p in BAD_COLS):
                continue
            score = sum(10 + len(p) // 4 for p in patterns if re.search(p, n))
            if score == 0:
                continue
            if unit(c) != "unknown":
                score += 3
            if int(to_number(df[c]).notna().sum()) < max(3, len(df) * 0.2):
                score -= 8
            if score > best_score:
                best_col, best_score = c, score
        if best_col is not None and best_score > 0:
            out[metric] = best_col
    return out


def promote_header(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    header_hits = sum(1 for c in df.columns for ps in PATTERNS.values() for p in ps if re.search(p, norm(c)))
    if header_hits:
        return df
    best_idx, best_score = None, 0
    for i in range(min(len(df), 25)):
        vals = [norm(v) for v in df.iloc[i].tolist()]
        score = sum(v not in {"", "nan", "none"} for v in vals)
        score += sum(8 for v in vals for ps in PATTERNS.values() for p in ps if re.search(p, v))
        score += sum(5 for v in vals for p in TIME_PATTERNS if re.search(p, v))
        if score > best_score:
            best_idx, best_score = i, score
    if best_idx is None or best_score < 8:
        return df
    cols, seen = [], {}
    for j, value in enumerate(df.iloc[best_idx].fillna("").astype(str).tolist()):
        base = value.strip() or f"Spalte_{j + 1}"
        seen[base] = seen.get(base, 0) + 1
        cols.append(base if seen[base] == 1 else f"{base}_{seen[base]}")
    out = df.iloc[best_idx + 1 :].copy()
    out.columns = cols
    return out.reset_index(drop=True)


def read_tables(path: Path) -> list[tuple[str, pd.DataFrame]]:
    if path.suffix.lower() in {".xlsx", ".xlsm", ".xls"}:
        return [(str(k), promote_header(v)) for k, v in pd.read_excel(path, sheet_name=None, dtype=str).items() if not v.empty]
    last = None
    for enc in ("utf-8-sig", "utf-8", "latin1"):
        for sep in (None, ";", ",", "\t"):
            try:
                df = promote_header(pd.read_csv(path, sep=sep, engine="python", dtype=str, encoding=enc))
                if len(df.columns) >= 2 and not df.empty:
                    return [("CSV", df)]
            except Exception as exc:
                last = exc
    raise RuntimeError(f"Datei konnte nicht gelesen werden: {last}")


@dataclass
class Result:
    scenario: str
    label: str
    file: Path
    sheet: str
    rows: int
    values: dict[str, Optional[float]] = field(default_factory=dict)
    columns: dict[str, str] = field(default_factory=dict)
    notes: list[str] = field(default_factory=list)

    @property
    def score(self) -> int:
        return len(self.columns) * 10 + sum(v is not None for v in self.values.values()) * 5


def analyse(df: pd.DataFrame, scenario: str, label: str, file: Path, sheet: str, year: Optional[int]) -> Result:
    notes: list[str] = []
    tc = time_column(df)
    dt_h = None
    if tc:
        parsed = pd.to_datetime(df[tc], errors="coerce", dayfirst=True)
        dt_h = timestep_hours(parsed)
        if year is not None:
            df = df.loc[parsed.dt.year == year].copy()
        else:
            df = df.loc[parsed.notna()].copy()
        if dt_h:
            notes.append(f"Zeitspalte: {tc}, Zeitraster: {dt_h:g} h")
    elif year is not None:
        notes.append(f"Kein Datum gefunden; Jahresfilter {year} nicht angewendet.")
    cols = find_columns(df) if not df.empty else {}
    values: dict[str, Optional[float]] = {"load": None, "pv_generation": None, "grid_import": None, "grid_export": None}
    for key, col in cols.items():
        values[key] = as_kwh(df[col], col, dt_h, notes)
    load, pv = values["load"], values["pv_generation"]
    imp, exp = values["grid_import"], values["grid_export"]
    used_own = max(load - imp, 0) if load is not None and imp is not None else None
    used_pv = max(pv - exp, 0) if pv is not None and exp is not None else None
    if used_own is None and used_pv is not None:
        used_own = used_pv
    if used_pv is None and used_own is not None:
        used_pv = used_own
    values.update({
        "used_own": used_own,
        "used_pv": used_pv,
        "autarky": (used_own / load * 100) if load and used_own is not None else None,
        "self_consumption": (used_pv / pv * 100) if pv and used_pv is not None else None,
    })
    return Result(scenario, label, file, sheet, len(df), values, cols, notes)


def classify(path: Path) -> Optional[str]:
    t = norm(str(path))
    if "polysun" in t and any(x in t for x in ["ohne", "without", "no_battery", "no-battery", "kein", "ohneakku"]):
        return "polysun_without_battery"
    if "polysun" in t and any(x in t for x in ["mit", "with", "battery", "akku", "speicher", "storage", "batterie"]):
        return "polysun_with_battery"
    if any(x in t for x in ["aktuell", "current", "bestand", "existing", "ist", "heute"]):
        return "current_pv"
    return None


def discover(input_dir: Path, explicit: dict[str, list[Path]]) -> dict[str, list[Path]]:
    out = {k: [] for k in SCENARIOS}
    for scenario, files in explicit.items():
        out[scenario].extend([p for p in files if p.exists()])
        for p in files:
            if not p.exists():
                print(f"WARNUNG: Datei nicht gefunden: {p}", file=sys.stderr)
    if any(out.values()):
        return out
    for p in input_dir.rglob("*"):
        if not p.is_file() or p.suffix.lower() not in SUFFIXES or any(part in SKIP_DIRS for part in p.parts):
            continue
        scenario = classify(p.relative_to(input_dir))
        if scenario:
            out[scenario].append(p)
    return out


def best_per_file(results: list[Result]) -> list[Result]:
    best: dict[tuple[str, Path], Result] = {}
    for r in results:
        key = (r.scenario, r.file)
        if key not in best or r.score > best[key].score:
            best[key] = r
    return list(best.values())


def rows_summary(results: list[Result]) -> pd.DataFrame:
    rows = []
    for scenario, label in SCENARIOS.items():
        rs = [r for r in results if r.scenario == scenario]
        def total(k: str) -> Optional[float]:
            vals = [r.values.get(k) for r in rs if r.values.get(k) is not None]
            return float(sum(vals)) if vals else None
        load, pv, imp, exp = total("load"), total("pv_generation"), total("grid_import"), total("grid_export")
        used_own, used_pv = total("used_own"), total("used_pv")
        rows.append({
            "Szenario": label,
            "Verbrauch [kWh/a]": load,
            "PV-Erzeugung [kWh/a]": pv,
            "Autarkiegrad [%]": (used_own / load * 100) if load and used_own is not None else None,
            "Eingespeiste Energie [kWh/a]": exp,
            "Genutzte eigene Energie [kWh/a]": used_own,
            "Genutzte PV-Energie [kWh/a]": used_pv,
            "Bezogene Energie [kWh/a]": imp,
            "Eigenverbrauchsquote [%]": (used_pv / pv * 100) if pv and used_pv is not None else None,
            "Ausgewertete Dateien": len(rs),
        })
    return pd.DataFrame(rows)


def rows_details(results: list[Result], input_dir: Path) -> pd.DataFrame:
    rows = []
    for r in sorted(results, key=lambda x: (x.scenario, str(x.file), x.sheet)):
        rel = str(r.file.relative_to(input_dir)) if r.file.is_relative_to(input_dir) else str(r.file)
        rows.append({
            "Szenario": r.label,
            "Datei": rel,
            "Sheet": r.sheet,
            "Zeilen": r.rows,
            "Verbrauch [kWh]": r.values.get("load"),
            "PV-Erzeugung [kWh]": r.values.get("pv_generation"),
            "Netzbezug [kWh]": r.values.get("grid_import"),
            "Einspeisung [kWh]": r.values.get("grid_export"),
            "Autarkiegrad [%]": r.values.get("autarky"),
            "Erkannte Spalten": "; ".join(f"{k}: {v}" for k, v in r.columns.items()),
            "Hinweise": " | ".join(dict.fromkeys(r.notes)),
        })
    return pd.DataFrame(rows)


def rows_notes(results: list[Result], found: dict[str, list[Path]], input_dir: Path, year: Optional[int]) -> pd.DataFrame:
    rows = [{"Typ": "Info", "Hinweis": f"Jahr: {year if year else 'alle vorhandenen Daten'}"}]
    for scenario, label in SCENARIOS.items():
        files = found[scenario]
        if files:
            shown = ", ".join(str(p.relative_to(input_dir)) if p.is_relative_to(input_dir) else str(p) for p in files)
            rows.append({"Typ": "Dateien", "Hinweis": f"{label}: {shown}"})
        else:
            rows.append({"Typ": "Warnung", "Hinweis": f"Keine Datei fuer '{label}' gefunden. Falls Auto-Erkennung nicht passt, Datei per Argument angeben."})
    for r in results:
        if not r.columns:
            rows.append({"Typ": "Warnung", "Hinweis": f"Keine passenden Spalten erkannt in {r.file} / {r.sheet}"})
    return pd.DataFrame(rows)


def write_excel(summary: pd.DataFrame, details: pd.DataFrame, notes: pd.DataFrame, output: Path, csv: bool) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(output, engine="openpyxl") as writer:
        summary.to_excel(writer, sheet_name="Jahreswerte", index=False)
        details.to_excel(writer, sheet_name="Details", index=False)
        notes.to_excel(writer, sheet_name="Hinweise", index=False)
        for sheet in writer.book.worksheets:
            sheet.freeze_panes = "A2"
            for cells in sheet.columns:
                width = max(12, min(max(len(str(c.value or "")) for c in cells) + 2, 60))
                sheet.column_dimensions[cells[0].column_letter].width = width
    if csv:
        summary.to_csv(output.with_suffix(".jahreswerte.csv"), index=False, sep=";", decimal=",")
        details.to_csv(output.with_suffix(".details.csv"), index=False, sep=";", decimal=",")
        notes.to_csv(output.with_suffix(".hinweise.csv"), index=False, sep=";", decimal=",")


def parse_paths(values: Optional[list[str]], base: Path) -> list[Path]:
    return [] if not values else [(base / v).resolve() if not Path(v).is_absolute() else Path(v) for v in values]


def main() -> int:
    p = argparse.ArgumentParser(description="Erstellt eine Jahreszusammenfassung fuer PV-/Polysun-Ergebnisse.")
    p.add_argument("--input-dir", default=".")
    p.add_argument("--output", default="results/jahreszusammenfassung.xlsx")
    p.add_argument("--year", type=int)
    p.add_argument("--no-csv", action="store_true")
    p.add_argument("--current-pv", nargs="*")
    p.add_argument("--polysun-without-battery", nargs="*")
    p.add_argument("--polysun-with-battery", nargs="*")
    args = p.parse_args()
    input_dir = Path(args.input_dir).resolve()
    output = Path(args.output)
    if not output.is_absolute():
        output = (input_dir / output).resolve()
    explicit = {
        "current_pv": parse_paths(args.current_pv, input_dir),
        "polysun_without_battery": parse_paths(args.polysun_without_battery, input_dir),
        "polysun_with_battery": parse_paths(args.polysun_with_battery, input_dir),
    }
    found = discover(input_dir, explicit)
    results: list[Result] = []
    for scenario, files in found.items():
        for path in files:
            try:
                for sheet, df in read_tables(path):
                    results.append(analyse(df, scenario, SCENARIOS[scenario], path, sheet, args.year))
            except Exception as exc:
                results.append(Result(scenario, SCENARIOS[scenario], path, "-", 0, notes=[f"Datei konnte nicht gelesen werden: {exc}"]))
    selected = best_per_file(results)
    summary, details, notes = rows_summary(selected), rows_details(selected, input_dir), rows_notes(selected, found, input_dir, args.year)
    write_excel(summary, details, notes, output, csv=not args.no_csv)
    print(f"Fertig: {output}")
    print(summary.to_string(index=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
