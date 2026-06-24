# NRMUA-26 Code Repository

This repository contains project-specific analysis code for the NRMUA 2026 case study on the Giesserei in Oberwinterthur. The code was used to prepare electricity-balance calculations, self-consumption estimates and selected plots for the accompanying report on local photovoltaic ownership models and PEDvolution.

## Purpose

The repository is not a general-purpose software package. It is a working code collection for one specific student project. The scripts were used to:

- prepare and clean electricity and photovoltaic time-series data,
- compare measured consumption with current and simulated PV production,
- calculate annual electricity balances,
- estimate direct self-consumption, grid import and feed-in,
- evaluate simplified battery scenarios,
- prepare summary tables and plots for the report.

The code belongs to this project only and is not intended to be developed further after submission of the written report.

## Data availability

The data required to run the full analysis are not publicly available. They include project-specific electricity consumption data, PV production data and simulation outputs that were used internally for the case study.

For this reason, the repository should not be expected to run reproducibly from a fresh clone. Some scripts may require local files, file paths or intermediate tables that are not included in the public repository.

## Main calculation logic

The calculations are based on the following simplified methods:

- Annual electricity balance: `PV production - electricity consumption`.
- Direct self-consumption at time-step level: the lower value of PV production and electricity demand in each interval.
- Grid import: demand that remains when electricity consumption is higher than PV production.
- Feed-in: PV surplus when production is higher than demand.
- Autarky/self-sufficiency: share of total electricity demand covered by locally used PV electricity.
- Self-consumption rate: share of PV production used locally.
- Battery scenarios: simplified storage calculation with charging, discharging and losses; used to estimate changes in self-consumption, grid import and feed-in.

Most time-series calculations were prepared at 15-minute resolution where the input data allowed it. The methods are suitable for scenario comparison in the report, but they are not a substitute for a detailed engineering model, legal assessment or investment-grade financial analysis.

## Repository structure

The repository contains several R scripts and one auxiliary Python script. The structure is not cleanly organised, and some files are experimental or intermediate work products.

Important files include:

- `Clean_Yearly_Energy_Evaluation.R` - preparation of corrected yearly summary values.
- `prepareTableForSimulation.R` - preparation of time-series tables for simulation.
- `prepareTableForSimulation_PolysunSPT_data.R` - preparation of Polysun SPT-based PV simulation data.
- `PowerSimulation.R` and `PowerSimulationFunction.R` - power-flow and self-consumption simulation logic.
- `PowerSimulation_Autarky_Evaluation.R` - evaluation of autarky and self-consumption results.
- `FindBatteryCapacity.R` - exploratory battery-capacity calculations.
- `power_plot_eval.R` and `power_plot_eval_PolysunSPT.R` - plot generation and visual evaluation.
- `functions.R` - helper functions used by parts of the analysis.
- `scripts/collect_yearly_results.py` - auxiliary script for collecting yearly results.

This list is descriptive rather than a stable API. The scripts were written and changed during the project process, and there is considerable disorder in the code base.

## Transparency on AI-assisted coding

A substantial part of the code was generated or refactored with the help of ChatGPT. The AI-generated code was created under detailed human instructions specifying which calculation methods should be used, how the electricity balance should be calculated and which outputs were needed for the report.

The project authors remained responsible for the methodological choices, interpretation of results and use of the generated code. The code should therefore be understood as AI-assisted project work, not as independently validated software.

## Limitations

Several limitations should be considered when reading or reusing this repository:

- The required input data are not publicly available.
- Local file paths and intermediate file names may not match another user's environment.
- The code base is messy and was not cleaned into a reproducible package.
- Some scripts may contain redundant, outdated or experimental sections.
- The battery model is simplified and does not include a full economic or life-cycle assessment.
- The results depend on the assumptions and datasets used in the project.
- The repository is not planned to be maintained after the report submission.

## Intended use

This repository is primarily a transparency supplement for the accompanying NRMUA case study report. It documents how parts of the numerical results and plots were produced, but it is not intended as reusable software or as a complete public dataset.

## Status

Project-specific research code. Not maintained after submission.
