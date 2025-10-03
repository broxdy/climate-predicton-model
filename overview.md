# Climate Prediction Research Repository - Overview

## Project Information

**Research Period:** 2009-2014
**Focus:** Statistical downscaling of General Circulation Models (GCMs) for Thailand climate prediction
**Methods:** ARIMA time series, Multiple regression, ML-based multi-GCM regression, Stochastic weather generation

This repository contains a comprehensive suite of R-based statistical downscaling methods developed for predicting local-scale climate variables from large-scale GCM outputs.

---

## Repository Structure

```
/
├── scripts/              # All R analysis scripts (276 scripts)
│   ├── 01-arima-forecasting/
│   ├── 02-multi-regression-downscaling/
│   ├── 03-ml-multiGCM-regression/
│   ├── 04-stochastic-weather-generation/
│   ├── 05-cross-correlation-analysis/
│   ├── 06-frequency-autocorrelation/
│   ├── 07-ncdf-processing/
│   ├── 08-data-filling/
│   └── utils/           # Shared utility functions
├── data/                # Input data files (~1,300 files)
│   ├── observed/        # Station observations
│   ├── gcm/             # GCM predictor data
│   ├── sst-indices/     # Sea Surface Temperature indices
│   └── config/          # Configuration CSVs
├── docs/                # Documentation
│   └── (future documentation)
├── CLAUDE.md            # Guide for Claude Code
├── CLEANUP-PLAN.md      # Detailed cleanup decisions
├── overview.md          # This file
└── progress.txt         # Cleanup progress log
```

---

## Script Inventory

### 1. ARIMA Forecasting (`scripts/01-arima-forecasting/`)

**Purpose:** Time series forecasting using Autoregressive Integrated Moving Average models

**Key Scripts:**
- `arima-prediction-multi obs v9 cal1971-1999sim1971-2004 multi-startMonth afterM*.R` (11 files)
  - Multi-month ahead forecasting (3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33 months)
  - Calibration: 1971-1999
  - Simulation: 1971-2004

- `AR-prediction-multi obs v7f cal+sim.R`
  - Autoregressive prediction for multiple stations

- `Investigate+Optimal ARIMA v3b.R`
  - ARIMA model optimization and parameter selection

- `arimaBKK-2.R`
  - Bangkok-specific ARIMA analysis

**Input Dependencies:**
- `data/observed/monthly-obs*.csv`
- `data/observed/Daily Filled*.csv`

**Main Outputs:** Monthly and multi-month climate variable forecasts

---

### 2. Multi-Regression Downscaling (`scripts/02-multi-regression-downscaling/`)

**Purpose:** Statistical downscaling using multiple linear regression with GCM predictors

**Key Scripts:**
- `reg-allobs v12monthly+future+SST+optimalLag.R`
  - Monthly regression with SST indices and optimized lag
  - Most comprehensive configuration

- `reg-allobs v12monthly+future.R`
  - Monthly regression without SST indices

- `reg-allobs v12monthly+future+SST.R`
  - Monthly regression with default SST lag

- `reg-allobs v12monthly+future+allSST+allLag-TEST.R`
  - Experimental: all SST predictors with all possible lags

- `reg-allobs v12monthly+future+allSST+optimalLag-TEST.R`
  - Experimental: all SST predictors with optimal lag selection

- `reg-allobs v12daily+future.R`
  - Daily-scale regression

**Input Dependencies:**
- `data/observed/monthly-obs*.csv`, `data/observed/ECHO-G monthly*.csv`
- `data/sst-indices/ocean index*.csv`
- `data/observed/day in year.csv`

**Configuration:**
- Calibration periods: 1971-1985 or 1971-1999
- Verification periods: 1986-2000 or 2000-2006
- Simulation: 1971-2100
- Scenarios: A1B, A2, B1

**Main Outputs:** Downscaled daily/monthly precipitation, temperature

---

### 3. ML Multi-GCM Regression (`scripts/03-ml-multiGCM-regression/`)

**Purpose:** Machine learning regression using multiple GCM predictors simultaneously

**Key Scripts - V9f Series (Most Comprehensive):**
- `ML-multiGCMvPrediction-V9f_HighRes+GCMs+optimal-lagSSTs*.R`
  - Combined high-resolution and standard GCMs with optimized SST lags
  - Multiple verification periods (1986-1999, 2000-2006)
  - With/without non-zero lag (nzl) variants
  - Different seasonal configurations (1, 3, 4 seasons)

- `ML-multiGCMvPrediction-V9f_optimal-lagSSTs*.R`
  - SST-optimized variants for different seasons

- `ML-multiGCMvPrediction-V9f_GCMs+optimal-lagSSTs*.R`
  - Standard GCMs with optimized SST lags

- `ML-multiGCMvPrediction-V9f_defaultSSTs*.R`
  - Default SST lag configurations

**Key Scripts - V9g Series (Latest):**
- `ML-multiGCMvPrediction-V9g_HiRes*.R`
  - High-resolution only models

- `ML-multiGCMvPrediction-V9g_optimal-lagSSTs*.R`
  - Latest SST optimization approach

**Configuration System:**
The scripts use CSV configuration files (`data/config/config_*.csv`) to specify:
- Predictor selection (which GCM variables to use)
- Predictor ordering (by correlation strength)
- Maximum number of predictors per predictand
- Seasonal aggregation

**GCM Combinations:**
1. **GCMs only**: `config_GCMv5.csv`
2. **High-resolution only**: `config_high res.csv`
3. **Combined**: `config_GCMv5+high.csv`
4. **With SSTs**: `config_*+SSTs*.csv` variants

**Input Dependencies:**
- `data/observed/monthly-obs_filled1971-1999*.csv` or `1971-2006*.csv`
- `data/gcm/GCMs-real_mo-1971-1999v5*.csv`
- `data/gcm/GCMv4_real-mo future V5+HiRes_*.csv` (multiple scenarios)
- `data/gcm/high-res_2000-2100_*.csv`
- `data/sst-indices/ocean index*.csv`
- `data/config/config_*.csv`

**Analysis Configurations:**
- Calibration: 1971-1999 (or split at 1985)
- Verification: 1986-1999 or 2000-2006
- Future simulation: 2000-2099
- Climate scenarios: A1, A2, B1, B2
- GCM models: CGCM2, CSIRO2, ECHAM4, HadCM3, PCM

**Main Outputs:** Multi-GCM ensemble downscaled climate projections

---

### 4. Stochastic Weather Generation (`scripts/04-stochastic-weather-generation/`)

**Purpose:** Generate realistic daily weather sequences preserving spatial autocorrelation

**Key Scripts:**
- `moran41rds-cal1971-1999 obs 1971-2006-RDSread.R`
  - Final version using Moran's I statistic
  - Calibration: 1971-1999
  - Application: 1971-2006
  - Uses RDS files for efficiency

- `moran41rds-cal1971-1985 obs 1971-2006-RDSread.R`
  - Alternative calibration period (1971-1985)

- `moran41rds-cal1971-1999 SCNopt1971-2006-RDSread.R`
  - Scenario application version

- `moran41rds-cal1971-1999 only probability wet.R`
  - Wet day probability generation

- `moran37c2_sel curve - new ecdf + extrm charts+limit extreme v4b.R`
  - Curve fitting approach with extreme value limits
  - Complementary method to moran41

**Methodology:**
- Implements Moran's I spatial autocorrelation statistic
- Generates spatially consistent daily weather
- Preserves temporal autocorrelation
- Handles extreme events with specialized limiting

**Input Dependencies:**
- `data/observed/Daily Filled all climate*.csv`
- `data/observed/coordinate.csv`
- `data/observed/monthly-obs*.csv`
- Pre-computed spatial weight matrices
- RDS serialized model objects

**Main Outputs:**
- Spatially autocorrelated daily precipitation and temperature realizations
- Multiple realizations (typically 30-1000) for uncertainty quantification

---

### 5. Cross-Correlation Analysis (`scripts/05-cross-correlation-analysis/`)

**Purpose:** Analyze relationships between GCM predictors and observed predictands

**Key Scripts** (~37 scripts):
- Time-lagged cross-correlation analysis
- Spatial correlation patterns
- Seasonal correlation variations
- Predictor-predictand relationship strength

**Input Dependencies:**
- `data/observed/*.csv`
- `data/gcm/*.csv`
- `data/sst-indices/*.csv`

**Main Outputs:**
- Correlation matrices
- Optimal lag identification
- Predictor importance rankings

---

### 6. Frequency-Autocorrelation Analysis (`scripts/06-frequency-autocorrelation/`)

**Purpose:** Spectral analysis and temporal autocorrelation

**Key Scripts** (~20 scripts):
- ACF/PACF analysis
- Spectral decomposition
- Periodicity identification
- Temporal dependency structure

**Main Outputs:**
- Autocorrelation functions
- Power spectra
- Periodic component identification

---

### 7. NetCDF Processing (`scripts/07-ncdf-processing/`)

**Purpose:** Read, process, and extract data from NetCDF climate model files

**Key Scripts** (~28 scripts):
- NetCDF file reading and extraction
- Spatial subsetting for Thailand region
- Temporal aggregation
- Format conversion to CSV

**Input Dependencies:**
- NetCDF files (GCM outputs, reanalysis data)
- Coordinate/grid information

**Main Outputs:**
- Extracted CSV files for analysis
- Region-specific climate data

---

### 8. Data Filling (`scripts/08-data-filling/`)

**Purpose:** Impute missing values in observational datasets

**Key Scripts** (~47 scripts):
- Spatial interpolation methods
- Temporal interpolation
- Regression-based filling
- Quality control

**Input Dependencies:**
- `data/observed/*.csv` (with missing values)

**Main Outputs:**
- Gap-filled observational datasets
- Quality flags and metadata

---

### 9. Utility Scripts (`scripts/utils/`)

**Purpose:** Shared functions and post-processing tools

**Categories:**

#### Visualization (R Boxplot, Plot utilities)
- Boxplot generation for model comparison
- Time series plotting
- Spatial maps
- Probability distributions
- QQ plots

#### Statistical Post-processing (R Dist models)
- Distribution fitting (normal, gamma, GEV, etc.)
- Extreme value analysis
- Probability calculations
- Return period estimation

#### Format Conversion (R RLZ to SwatDBF)
- Convert stochastic realizations to SWAT model format
- DBF file generation for hydrological modeling
- Data reshaping and aggregation

#### Data Extraction (R read and extract, R extract for LARS-WG)
- Extract data for external weather generators (LARS-WG, SDSM)
- Multi-file batch processing
- Station-specific extraction

#### Matrix Operations (R matrix cross rel)
- Cross-correlation matrices
- Distance matrices
- Spatial weight matrices

#### Miscellaneous
- `clear.R`: Workspace clearing utility
- Plotting parameter configuration
- Count unique values
- Anomaly calculations

---

## Data Inventory

### Observed Data (`data/observed/`)

**Daily Observations:**
- `Daily Filled all climate 1971-1985+Wet_V2.csv`
- `Daily Filled all climate 1971-1999+Wet_V2.csv`
- `Daily Filled all climate 1971-2006+Wet_V2.csv`
- Includes: precipitation, temperature (max/min), humidity, solar radiation
- Wet day indicators and ratios included

**Monthly Observations:**
- `monthly-obs_filled1971-2006(4).csv`
- `monthly-obs_filled1971-2006(4)+WetDay+WetRatio_V2.csv`
- `monthly-obs_WetDay1971-2006.csv`

**Metadata:**
- `coordinate.csv`: Station locations
- `day in month.csv`, `day in year.csv`: Calendar utilities

### GCM Data (`data/gcm/`)

**Historical GCM Predictors:**
- `GCMs-real_mo-1971-1999v5+high res.csv`: Combined GCM and high-resolution
- `GCMs-high res-1971-1999.csv`: High-resolution only
- `Monthly_GCMs1971-1999(2000)*.csv`: Monthly GCM data variants

**Future Projections:**
- `GCMv4_real-mo future V5+HiRes_[MODEL]-[SCENARIO]_[YEARS].csv`
  - Models: CGCM2, CSIRO2, ECHAM4, HadCM3, PCM
  - Scenarios: A1, A2, B1, B2
  - Time periods: 2000-2096 to 2000-2099 (varies by model)

- `high-res_2000-2100_[MODEL]-[SCENARIO].csv`
  - High-resolution future projections

**Special Datasets:**
- `ECHO-G monthly 1971-1999*.csv`: ECHO-G model data
- `ECHO-G monthly 2000-2100_[SCENARIO].csv`: ECHO-G projections

### SST Indices (`data/sst-indices/`)

**Files:**
- `ocean index 1971-2009.csv`
- `ocean index 1971-2009-v2.csv`
- `ocean index-selected.csv`: Curated subset

**Indices Included:**
- NINO indices (1, 1+2, 3, 3.4, 4)
- Indian Ocean Dipole (IOD)
- Other Pacific/Indian Ocean SST patterns

### Configuration Files (`data/config/`)

**Predictor Configuration:**
- `config_GCMv5.csv`: Standard GCM predictors
- `config_high res.csv`: High-resolution predictors only
- `config_GCMv5+high.csv`: Combined configuration
- `config_*+SSTs*.csv`: With SST indices

**Correlation Matrices (CORRL files):**
- `CORRL_*.csv`: Pre-computed correlations between predictors and predictands
- Used for automatic predictor selection and ordering
- Variants for different seasonal aggregations (2, 3, 4 seasons)

---

## Getting Started

### Prerequisites

**R Version:** R 2.x or 3.x (scripts developed 2009-2014)

**Required R Packages:**
- `timeSeries`: Time series operations
- `TSA`: Time series analysis
- `car`: Regression diagnostics
- `MASS`: Statistical functions
- `stochmod`: Stochastic modeling (for Stochastic scripts)
- `mcmc`, `msm`, `hmm.discnp`, `depmix`: Markov models (for Stochastic)
- `ncdf` or `ncdf4`: NetCDF file handling

### Execution Order

**Basic Workflow:**

1. **Data Preparation:**
   ```r
   # Fill missing data
   source("scripts/08-data-filling/[appropriate_script].R")

   # Process NetCDF files (if needed)
   source("scripts/07-ncdf-processing/[extraction_script].R")
   ```

2. **Exploratory Analysis:**
   ```r
   # Cross-correlation
   source("scripts/05-cross-correlation-analysis/[correlation_script].R")

   # Frequency analysis
   source("scripts/06-frequency-autocorrelation/[frequency_script].R")
   ```

3. **Downscaling (choose one approach):**

   **Option A: ARIMA Forecasting**
   ```r
   source("scripts/01-arima-forecasting/arima-prediction-multi obs v9 cal1971-1999sim1971-2004 multi-startMonth afterM12.R")
   ```

   **Option B: Multi-Regression**
   ```r
   source("scripts/02-multi-regression-downscaling/reg-allobs v12monthly+future+SST+optimalLag.R")
   ```

   **Option C: ML Multi-GCM (Recommended)**
   ```r
   source("scripts/03-ml-multiGCM-regression/ML-multiGCMvPrediction-V9f_HighRes+GCMs+optimal-lagSSTs-with multi future files - vrf 1986-1999-nzl.R")
   ```

4. **Stochastic Weather Generation:**
   ```r
   source("scripts/04-stochastic-weather-generation/moran41rds-cal1971-1999 obs 1971-2006-RDSread.R")
   ```

5. **Post-processing and Visualization:**
   ```r
   # Use scripts in scripts/utils/ for plots, statistics, format conversion
   ```

### Important Configuration Notes

**Before running scripts, check and modify:**

1. **Working directory paths** - Most scripts have hardcoded paths like:
   ```r
   dir0=dirname(file.path("D:",paste(maindir),"dummy"))
   ```

2. **Input/output file names** - Variables like:
   ```r
   dataf = "monthly-obs_filled1971-1999(2100)+WetDay+WetRatio.csv"
   indexf = "GCMs-real_mo-1971-1999v5+high res.csv"
   config.nprd.f = "config_GCMv5+high.csv"
   ```

3. **Analysis periods** - Calibration/verification splits:
   ```r
   verifyfactor = 0.52  # Split at 1971-1985/1986-1999
   endyearcal.whole.fit = 1999
   ```

4. **Script dependencies** - Many scripts use:
   ```r
   source("clear.R")  # Now in scripts/utils/clear.R
   ```
   Update paths accordingly.

---

## Research Context

### Methodology Overview

This repository implements a comprehensive statistical downscaling framework:

1. **GCM Predictor Selection**: Cross-correlation analysis identifies relationships between large-scale GCM variables and local climate

2. **Statistical Relationships**: Multiple regression or ML techniques establish transfer functions during historical calibration period

3. **Future Projection**: Apply transfer functions to future GCM data to generate local-scale projections

4. **Stochastic Disaggregation**: Generate multiple daily realizations preserving spatial and temporal structure

5. **Ensemble Uncertainty**: Multiple GCMs, scenarios, and realizations quantify projection uncertainty

### Key Features

- **Multi-GCM Ensemble**: Combines CGCM2, CSIRO2, ECHAM4, HadCM3, PCM
- **Multi-scenario**: SRES A1, A2, B1, B2 emission scenarios
- **SST Teleconnections**: Incorporates ENSO and IOD influences
- **Spatial Consistency**: Moran's I statistic preserves spatial autocorrelation
- **Extreme Events**: Specialized handling of extreme precipitation and temperature
- **Temporal Scales**: Daily to monthly analysis
- **Long-term Projections**: Through 2099

### Limitations and Considerations

1. **Historical Period**: Scripts use 2009-2014 era GCM data and methodology
2. **R Version**: May require updates for modern R versions
3. **File Paths**: Hardcoded Windows paths need adjustment
4. **No Version Control**: Original repository lacked git tracking
5. **Documentation**: Limited inline comments in some scripts
6. **Interdependencies**: Complex dependencies between script families

---

## Cleanup Summary

### Original State (Pre-cleanup)
- **Total files:** 136,750
- **R scripts:** 762 (many versioned duplicates)
- **CSV files:** 133,716 (mostly generated outputs)
- **Graphics:** 1,199 PDFs/PNGs

### Final State (Post-cleanup)
- **Total files:** ~1,655 (**98.8% reduction**)
- **R scripts:** 276 (final versions only)
- **Data files:** ~1,300 (inputs only, outputs removed)
- **Documentation:** 4 files (CLAUDE.md, CLEANUP-PLAN.md, overview.md, progress.txt)

### Major Changes

**Deleted:**
- 12 ML-regression output directories with ~129,000 generated CSV files
- Entire `downscaling results/` directory with ~1,000 output files
- ~350 versioned R scripts (v1, v2, test, backup variants)
- Multiple output subdirectories across all analysis folders
- Duplicate utility files (18 copies of clear.R → 1)
- Workspace files (.RData, .Rhistory, .bak)
- Obsolete directories (Stochastic Temp, klasur, nerc)
- External research folder (R SpatialInt)

**Consolidated:**
- Script versions: Kept highest version only (v9 for ARIMA, v12 for reg-allobs, moran41 for Stochastic, V9f/V9g for ML-multiGCM)
- Utility functions: Merged into `scripts/utils/`
- Data files: Organized into logical subdirectories

**Reorganized:**
- Flat directory structure → Hierarchical `scripts/` and `data/` organization
- Numbered script directories (01-08) showing analysis workflow
- Separated concerns: analysis scripts vs utilities vs data

---

## Future Work

### Recommended Next Steps

1. **Update for Modern R**
   - Test with R 4.x
   - Update deprecated package functions
   - Modernize coding practices

2. **Git Version Control**
   - Initialize git repository
   - Create .gitignore for large data files
   - Commit with meaningful history

3. **Documentation Enhancement**
   - Add README.md with quickstart guide
   - Inline code comments
   - Vignettes for key workflows
   - Method validation results

4. **Code Refactoring**
   - Extract common functions to shared library
   - Relative paths instead of hardcoded
   - Configuration file system (YAML/JSON)
   - Command-line argument parsing

5. **Testing and Validation**
   - Unit tests for utility functions
   - Regression tests for analysis outputs
   - Cross-validation framework

6. **Modernization**
   - Consider Python/xarray alternatives for NetCDF
   - Parallel processing for stochastic realizations
   - Interactive visualizations (Shiny apps)
   - Cloud-based execution

---

## Contact and Citation

This research repository represents work conducted 2009-2014 on statistical downscaling for Thailand climate prediction. For questions about methodology or data, refer to the original research publications.

**Repository Cleanup:** October 2025 by Claude (Anthropic)

---

*Last Updated: October 3, 2025*
