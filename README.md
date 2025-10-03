# Short- and Long-Term Climate Prediction : R Scripts from Computational Research

[![Research Period](https://img.shields.io/badge/Research%20Period-2006--2015-blue)]()
[![Language](https://img.shields.io/badge/Language-R-276DC3)]()
[![Domain](https://img.shields.io/badge/Domain-Numerical%20Modeling-green)]()
[![License](https://img.shields.io/badge/License-See%20LICENSE-lightgrey)]()

> **Research Archive:** Computational framework implementing numerical and statistical methods for short-term (monthly to seasonal) and long-term (decadal to century-scale) climate prediction, with applications to water resource management and hydrological impact assessment.

<img width="906" height="1273" alt="image" src="https://github.com/user-attachments/assets/a6f04ad9-6792-49ae-99ea-448daea49dde" />

*Changes of projected precipitation for future periods 2000-2049 (left panels) and 2000-2096 (right panels) under IPCC SRES A1B (a), A2 (b) and B1 (c)*

---

## Overview

This repository contains the complete **numerical modeling framework** developed as part of doctoral dissertation research on climate prediction methodologies. The research addresses two temporal scales of climate prediction:

### Short-Term Climate Prediction (Monthly to Seasonal)
**Time Horizons:** 1-month to 1-year ahead forecasts
**Purpose:** Operational water management and reservoir operation guidelines
**Applications:** Next-season temperature and rainfall forecasting for immediate water resource planning

### Long-Term Climate Prediction (Decadal to Century-Scale)
**Time Horizons:** Inter-annual and decadal variations through 21st century (2000-2099)
**Purpose:** Pre-warning information for future climate change and hydrological impacts
**Applications:** Strategic water supply planning and adaptation strategy development

### Research Motivation and Study Area

**Study Region:** Khlong Yai (KY) watershed, Eastern Seaboard of Thailand
**Regional Significance:** Major industrial zone experiencing severe drought and water shortage
**Research Objective:** Analyze potential climate change impacts on water supply for industrial development
**Hydrological Application:** Multi-realization streamflow simulation and reservoir water budget analysis using SWAT distributed watershed model

### Key Research Contributions

1. **Short-Term Forecasting Methods**
   - **AR, ARIMA, and ARIMAex models** - Autoregressive techniques with external regressors
   - **Ocean-climate teleconnection analysis** - SST indices (ENSO, IOD) providing 1-4 month lead time
   - **GCM-based seasonal prediction** - Extended predictor sets for reliable multi-month forecasts
   - **Multiple Linear Regression (MLR)** - Statistical models incorporating oceanic and atmospheric predictors

2. **Long-Term Downscaling Framework**
   - **Multi-GCM ensemble downscaling** - Novel MLR method incorporating multiple climate predictors from single- and multi-domain GCMs
   - **Comparative validation** - MLR method demonstrated superior accuracy versus SDSM and LARS-WG for 20th century (1971-2000) climate reconstruction
   - **SRES scenario projections** - 21st century climate under A1B, A2, and B1 emission scenarios
   - **Multi-domain GCM integration** - Flexible framework accommodating various GCM spatial resolutions

3. **Stochastic Daily Climate Generator**
   - **Temporal disaggregation** - Monthly → daily rescaling preserving statistical attributes
   - **Geospatial consistency** - Moran's I spatial autocorrelation preservation across station network
   - **Distributional fidelity** - Adherence to observed daily climate statistical properties
   - **Multi-realization ensemble** - 30+ realizations for uncertainty quantification in hydrological modeling

4. **Hydrological Impact Assessment**
   - **SWAT watershed modeling** - Daily climate inputs for distributed hydrological simulation
   - **Streamflow projections** - Future water availability under climate change scenarios
   - **Reservoir water budgets** - Seasonal storage analysis for three main reservoirs in KY basin
   - **Water supply implications** - Quantitative assessment for industrial and agricultural planning

### Key Findings from Dissertation Research

**21st Century Climate Projections (vs. 20th century baseline):**
- **Temperature trends:** Increased mean temperatures with larger fluctuations, particularly under SRES A1B
- **Rainfall characteristics:** Near-constant intensity but changing spatial distribution patterns
- **Temporal patterns:** Decreased sequential rainfall occurrence - shorter high-intensity periods followed by longer dry spells
- **Hydrological impacts:** Seasonal streamflow reductions and decreased reservoir water storage
- **Extreme events:** More extreme climate variability requiring adaptive water management strategies

### Target Users

**Primary:** Water resource planners, hydrological modelers, climate impact researchers
**Secondary:** Numerical modelers, computational scientists, applied mathematicians
**Applications:** Climate prediction, water supply planning, reservoir operation, drought management

---

## Table of Contents

- [Repository Structure](#repository-structure)
- [Installation & Prerequisites](#installation--prerequisites)
- [Methodology](#methodology)
- [Data Requirements](#data-requirements)
- [Usage Examples](#usage-examples)
- [Reproducibility Notes](#reproducibility-notes)
- [Documentation](#documentation)
- [Citation](#citation)
- [License](#license)
- [Contributing](#contributing)

---

## Repository Structure

```
.
├── scripts/                           # Analysis modules (276 R scripts)
│   │
│   ├── 01-arima-forecasting/         # SHORT-TERM PREDICTION: ARIMA models (14 scripts)
│   │   └── AR, ARIMA, ARIMAex models for 3-33 month ahead forecasts
│   │
│   ├── 02-multi-regression-downscaling/  # SHORT-TERM: MLR with teleconnections (6 scripts)
│   │   └── Ocean state indices and GCM predictors for seasonal forecasting
│   │
│   ├── 03-ml-multiGCM-regression/    # LONG-TERM: Multi-GCM downscaling (38 scripts) ⭐
│   │   └── Novel MLR method for 21st century projections (A1B, A2, B1 scenarios)
│   │       Comparison framework with SDSM and LARS-WG methods
│   │
│   ├── 04-stochastic-weather-generation/  # TEMPORAL DISAGGREGATION (7 scripts)
│   │   └── Monthly → Daily rescaling with Moran's I spatial autocorrelation
│   │       30-realization ensemble generation for SWAT hydrological model
│   │
│   ├── 05-cross-correlation-analysis/    # TELECONNECTION ANALYSIS (37 scripts)
│   │   └── Ocean-climate relationships, 1-4 month lead time identification
│   │
│   ├── 06-frequency-autocorrelation/     # SPECTRAL & TEMPORAL ANALYSIS (20 scripts)
│   │   └── Periodicity detection, autocorrelation structure characterization
│   │
│   ├── 07-ncdf-processing/              # GCM DATA PROCESSING (28 scripts)
│   │   └── Single- and multi-domain GCM extraction and formatting
│   │
│   ├── 08-data-filling/                 # DATA PREPROCESSING (47 scripts)
│   │   └── Missing data imputation for Khlong Yai watershed stations
│   │
│   └── utils/                           # SHARED UTILITIES (79 scripts)
│       ├── clear.R                      # Workspace management
│       ├── R Boxplot*/                  # Visualization for climate comparisons
│       ├── R Dist models*/              # Distribution fitting (gamma, GEV, etc.)
│       ├── R RLZ to SwatDBF*/           # SWAT hydrological model format conversion
│       └── R extract for LARS-WG*/      # LARS-WG weather generator interface
│
├── sample data/                       # Sample datasets from KY watershed (~1,300 files)
│   ├── observed/                      # Station observations (1971-2006, daily/monthly)
│   ├── gcm/                          # GCM predictors (historical & A1B/A2/B1 scenarios)
│   ├── sst-indices/                  # Ocean indices for teleconnection analysis
│   ├── config/                       # MLR predictor configurations & correlation matrices
│   └── original/                     # Unprocessed raw data
│
├── docs/                             # Documentation
│   └── (supplementary materials)
├── overview.md                       # Comprehensive methodology (15,000 words)
└── README.md                         # This file
```

**Note:** This is a **doctoral dissertation research archive** from 2006-2015. A major cleanup in October 2025 reduced the repository from 136,750 files (including ~129,000 generated outputs from downscaling experiments and hydrological simulations) to 1,655 essential files (98.8% reduction), retaining only source code and input data for reproducibility.

---

## Installation & Prerequisites

### System Requirements

- **R Version:** 2.x or 3.x (original development environment)
  - *Modern compatibility:* May require minor adjustments for R 4.x
- **Operating System:** Scripts contain Windows-style paths; Unix/Linux users need path modifications
- **Memory:** Recommended 8+ GB RAM for large GCM ensemble processing
- **Storage:** ~500 MB for repository; additional space for generated outputs

### R Package Dependencies

**Core Packages:**
```r
install.packages(c(
  "timeSeries",    # Time series data structures
  "TSA",           # Time series analysis
  "car",           # Regression diagnostics
  "MASS"           # Statistical functions
))
```

**Stochastic Modeling** (for Module 04):
```r
install.packages(c(
  "stochmod",      # Stochastic processes
  "mcmc",          # Markov Chain Monte Carlo
  "msm",           # Multi-state models
  "hmm.discnp",    # Hidden Markov models
  "depmix"         # Dependent mixture models
))
```

**NetCDF Processing** (for Module 07):
```r
install.packages("ncdf4")  # Modern version (or "ncdf" for legacy)
```

### Installation

```bash
# Clone repository
git clone https://github.com/[your-username]/climate-prediction.git
cd climate-prediction

# Launch R and install dependencies
R
> source("install_packages.R")  # If provided, or manually install above
```

---

## Numerical Methodology

This repository implements a **four-stage computational framework** for climate prediction:

### Stage 1: Numerical Pattern Recognition and Signal Processing

**Modules:** `05-cross-correlation-analysis`, `06-frequency-autocorrelation`

**Algorithms Implemented:**
- **Cross-correlation functions** - Pearson and Spearman correlation matrices between predictor-predictand pairs
- **Lag optimization** - Iterative search for optimal time lags in teleconnection patterns (0-11 months)
- **Spectral analysis** - Fast Fourier Transform (FFT) and periodogram analysis for frequency domain decomposition
- **Autocorrelation functions** - ACF/PACF computation for temporal dependency structure
- **Wavelet decomposition** - Multi-resolution time-frequency analysis

**Mathematical Output:** Correlation matrices R(τ), spectral density functions S(f), autocorrelation coefficients ρ(k)

<img width="927" height="1008" alt="image" src="https://github.com/user-attachments/assets/1dd0659c-15c4-4b95-bab5-cbc5cc1653c7" />

*Average cross-correlation coefficients between 58 daily ECHO-G GCM predictors and observed daily maximum and minimum temperatures and precipitation for all recording climate stations in the study region for years 1971-2000*

### Stage 2: Time Series Modeling and Transfer Function Estimation

**Modules:** `01-arima-forecasting`, `02-multi-regression-downscaling`, `03-ml-multiGCM-regression`

**Numerical Method A - ARIMA Box-Jenkins:**
- **Model identification:** AIC/BIC criterion minimization for optimal (p,d,q) selection
- **Parameter estimation:** Maximum likelihood estimation (MLE) via Newton-Raphson iteration
- **Forecasting:** Multi-step ahead prediction using recursive substitution
- **Forecast horizons:** h = 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33 months

<img width="945" height="1184" alt="image" src="https://github.com/user-attachments/assets/80972b85-3a9e-450a-b037-2461efae3067" />

*ARIMAex-model forecasts with ocean indices as external regressors to predict monthly a) max and b) min temperatures and c) rainfall at station 48459 in years 1986 to 1999 following the vrf1- calibration/verification scheme*


**Numerical Method B - Multiple Linear Regression:**
- **Least squares estimation:** Normal equations solution (X'X)⁻¹X'y
- **Variable selection:** Stepwise regression with F-statistic thresholds
- **Multicollinearity detection:** Variance Inflation Factor (VIF) analysis
- **Residual diagnostics:** Durbin-Watson test, Breusch-Pagan test

**Numerical Method C - Multi-GCM Ensemble (⭐ Recommended):**
- **Ensemble integration:** Weighted averaging with correlation-based weights w_i
- **Predictor optimization:** Greedy forward selection algorithm maximizing R²
- **Configuration matrix:** Binary selection matrix P indicating active predictors
- **Cross-validation:** k-fold temporal splitting for model validation

**Mathematical Output:** Transfer functions Y = f(X,β) + ε, coefficient vectors β, forecast sequences Ŷ(t+h)

<img width="855" height="1003" alt="image" src="https://github.com/user-attachments/assets/790da161-9b28-4804-8402-63a35bd28aeb" />

*Scatterplots of the observed and predicted values of a) minimum, b) maximum temperatures at station 48478 and c) precipitation at station 48092 in years 1986-1999, using the MLR- model and employing the single-domain HiRes GCM (left panels) and the multi-model GCMs+HiRes (right panels) large-scale predictor sets*


### Stage 3: Future Scenario Projection and Uncertainty Quantification

**Module:** `03-ml-multiGCM-regression` (future scenario applications)

**Numerical Operations:**
- **Scenario integration:** Apply calibrated transfer functions to 20 GCM-scenario combinations (5 GCMs × 4 SRES scenarios)
- **Ensemble statistics:** Compute mean μ(t), standard deviation σ(t), percentiles across ensemble members
- **Trend analysis:** Linear regression for century-scale trends, Mann-Kendall test for monotonic trends
- **Change detection:** Anomaly computation δT = T_future - T_baseline for 2046-2065 and 2081-2100 periods

**Mathematical Output:** Ensemble projections Y_ens(t), confidence intervals CI(t), trend coefficients β_trend

<img width="895" height="1214" alt="image" src="https://github.com/user-attachments/assets/263570fa-c32f-4ba5-aff0-2d92ce053987" />

*Monthly averages of projected 4-site minimum and maximum temperatures (top) and 24-site precipitation (bottom) over years 1971-1999 (20c3m) and future years 2000-2096 under the SRES-scenarios A1B, A2 and B1, using the multi-domain MLR-downscaling method*

### Stage 4: Stochastic Disaggregation and Spatial Field Generation

**Module:** `04-stochastic-weather-generation`

**Numerical Algorithms:**
- **Moran's I computation:** Spatial autocorrelation statistic I = (N/W) Σ_i Σ_j w_ij(x_i - x̄)(x_j - x̄) / Σ_i(x_i - x̄)²
- **Spatial weight matrix:** Distance-based weighting W with exponential decay kernel
- **Monte Carlo generation:** Latin hypercube sampling for correlated multivariate normals
- **Distribution fitting:** Maximum likelihood estimation for gamma, exponential, GEV distributions
- **Extremes treatment:** Threshold exceedance with bounded realizations
- **Temporal disaggregation:** Monthly → Daily using stochastic cascade model

**Mathematical Output:** N realizations of spatially correlated daily sequences {X_s(t)}_{s=1..S,t=1..T,n=1..N}

<img width="916" height="749" alt="image" src="https://github.com/user-attachments/assets/e940c261-ed25-430b-aabd-e9cdc8c9f23d" />

*Comparison of the simulated daily Moran’s I climate time-series using single-site (left panel) and multi-site (right panel) stochastic climate generation*

### Numerical Validation

**Implemented Tests:**
- **Split-sample validation:** 50-70% calibration, 30-50% verification temporal splits
- **Performance metrics:** RMSE = √(Σ(y_i - ŷ_i)²/n), MAE = Σ|y_i - ŷ_i|/n, R² = 1 - SS_res/SS_tot
- **Distribution tests:** Kolmogorov-Smirnov, Anderson-Darling goodness-of-fit
- **Extremes validation:** Return period comparison, extreme value index matching
- **Spatial coherence:** Moran's I preservation test, variogram analysis

<img width="868" height="609" alt="image" src="https://github.com/user-attachments/assets/bf9c26e9-84e8-4452-a9cd-f0a7c9efbef4" />

*Kernel density estimation of daily maximum temperature at station 48478 for years 1971-2000 using the downscaling models as indicated (cross points) and the daily observation (dashed line)*

---

## Data Requirements

### Input Data Structure

**Observed Climate Data** (`sample data/observed/`):
- **Daily:** Precipitation, T_max, T_min, humidity, solar radiation (1971-2006)
- **Monthly aggregates:** Mean, totals, wet day counts
- **Format:** CSV with columns: Date, Station1, Station2, ..., StationN
- **Missing data:** Pre-filled using scripts in Module 08

**GCM Predictor Data** (`sample data/gcm/`):
- **Variables:** Geopotential height, temperature, humidity, winds, etc.
- **Historical period:** 1971-1999 (or 1971-2006)
- **Future scenarios:** 2000-2099 (SRES A1/A2/B1/B2)
- **Format:** CSV with monthly values per predictor variable

**SST Indices** (`sample data/sst-indices/`):
- NINO indices (1, 1+2, 3, 3.4, 4)
- Indian Ocean Dipole (IOD)
- Other Pacific/Indian Ocean patterns
- **Time-lagged versions:** 0-11 month lags for optimal lag selection

**Configuration Files** (`sample data/config/`):
- `config_GCMv5.csv`: Predictor selection for standard GCM ensemble
- `config_GCMv5+high.csv`: Combined GCM + high-resolution predictors
- `config_*+SSTs*.csv`: Include SST teleconnections
- `CORRL_*.csv`: Pre-computed correlation matrices for predictor ranking

### File Naming Conventions

```
Historical GCM:  GCMs-real_mo-1971-1999v[version].csv
Future GCM:      GCMv4_real-mo future V[version]_[MODEL]-[SCENARIO]_[years].csv
Observations:    monthly-obs_filled[years]+WetDay+WetRatio_V2.csv
SST:             ocean index 1971-2009-v2.csv
Configuration:   config_GCMv5+high+SSTs-4season.csv
```

### Sample Data

This repository includes **sample datasets** for testing and learning:
- 5-station subset of Thailand meteorological network
- Abbreviated GCM predictor sets
- Complete SST index time series
- Example configuration files for all model variants

**Full dataset availability:** Contact repository maintainer for complete 70-station network and all GCM scenario files.

---

## Usage Examples

### Example 1: Short-Term Forecast (ARIMA with External Regressors)

**Objective:** Generate 12-month ahead climate forecast using ARIMAex with ocean state indices.

```r
# Set working directory
setwd("D:/climate-prediction")  # Windows
# setwd("~/climate-prediction")  # Unix/Linux

# Run ARIMA forecast with 12-month horizon
source("scripts/01-arima-forecasting/arima-prediction-multi obs v9 cal1971-1999sim1971-2004 multi-startMonth afterM12.R")

# Expected runtime: 5-15 minutes
# Outputs: 12-month ahead forecasts for operational water management
```

**Application:** Provides next-season climate predictions for reservoir operation planning.

### Example 2: Long-Term Downscaling (Multi-GCM MLR Method)

**Objective:** Generate 21st century climate projections using novel MLR downscaling method with multi-domain GCMs.

```r
# Set working directory
setwd("D:/climate-prediction")

# Run MLR Multi-GCM downscaling for SRES A1B scenario
source("scripts/03-ml-multiGCM-regression/ML-multiGCMvPrediction-V9f_HighRes+GCMs+optimal-lagSSTs-with multi future files - vrf 1986-1999-nzl.R")

# Expected runtime: 15-30 minutes depending on system
# Outputs: Monthly climate projections 1971-2099 for KY watershed
```

**Key configuration parameters** (inspect script header):
```r
dataf = "monthly-obs_filled1971-1999.csv"              # KY watershed observations
indexf = "GCMs-real_mo-1971-1999v5+high res.csv"       # Multi-domain GCM predictors
config.nprd.f = "config_GCMv5+high+SSTs-4season.csv"   # Predictor configuration
verifyfactor = 0.52                                    # Calibration: 1971-1985, Verification: 1986-1999
```

**Dissertation Context:** This MLR method demonstrated superior accuracy compared to SDSM and LARS-WG for 1971-2000 validation period.

### Example 3: Stochastic Daily Weather Generation for SWAT

**Objective:** Generate 30 daily climate realizations for hydrological modeling, preserving spatial autocorrelation.

```r
setwd("D:/climate-prediction")

# Run Moran's I-based stochastic generator
source("scripts/04-stochastic-weather-generation/moran41rds-cal1971-1999 obs 1971-2006-RDSread.R")

# Expected runtime: 1-3 hours for 30 realizations
# Outputs: 30 spatially coherent daily weather files
```

**Dissertation Application:** These daily realizations serve as input to SWAT distributed watershed model for:
- Multi-realization streamflow simulation
- Future water availability assessment in KY basin
- Reservoir water budget analysis under climate change scenarios

### Example 4: Ocean-Climate Teleconnection Analysis

**Objective:** Identify ocean state indices (ENSO, IOD) with optimal lead times for short-term forecasting.

```r
setwd("D:/climate-prediction")

# Compute cross-correlations with various lags (0-11 months)
source("scripts/05-cross-correlation-analysis/[cross_correlation_script].R")

# Outputs: Correlation matrices identifying 1-4 month lead times
```

**Research Finding:** Teleconnective relationships provide up to 4-month lead time for seasonal forecasting in KY watershed.

### Example 5: SWAT Hydrological Model Interface

**Objective:** Convert 30 daily climate realizations to SWAT DBF format for watershed simulation.

```r
setwd("D:/climate-prediction")

# Convert stochastic realizations to SWAT input format
source("scripts/utils/R RLZ to SwatDBF/[appropriate_converter].R")

# Outputs: DBF files for 30-realization ensemble hydrological simulation
```

**Hydrological Impact Assessment:** Multi-realization approach quantifies:
- Seasonal streamflow changes under A1B, A2, B1 scenarios
- Reservoir storage variability due to altered rainfall patterns
- Water supply reliability for industrial development

### Example 6: Comparison with SDSM and LARS-WG

**Objective:** Benchmark novel MLR method against established downscaling techniques.

```r
# Extract data for LARS-WG weather generator
source("scripts/utils/R extract for LARS-WG/[extraction_script].R")

# Run MLR downscaling (Example 2)
# Compare outputs: MLR vs. SDSM vs. LARS-WG

# Performance metrics: RMSE, MAE, correlation for 1971-2000 validation
```

**Dissertation Validation:** MLR method achieved superior accuracy over SDSM and LARS-WG for 20th century climate reconstruction.

---

## Reproducibility Notes

### Path Configuration

**⚠️ Important:** Scripts contain **hardcoded Windows-style paths** that must be updated before execution.

**Common path variables to modify:**
```r
# Typical pattern in scripts:
maindir = "Code/Climate Prediction"
dir0 = dirname(file.path("D:", paste(maindir), "dummy"))

# Update to your actual path:
maindir = "your/actual/path/climate-prediction"
dir0 = dirname(file.path("/home/user", paste(maindir), "dummy"))  # Unix example
```

**Search-and-replace strategy:**
```bash
# Find all scripts with hardcoded paths
grep -r "D:/" scripts/

# Batch replace (use with caution!)
sed -i 's|D:/Code/Climate Prediction|/your/new/path|g' scripts/**/*.R
```

### Workspace Management

Many scripts begin with:
```r
source("clear.R")  # Clear workspace
```

This file is now located at `scripts/utils/clear.R`. Update references:
```r
source("scripts/utils/clear.R")
```

### Output Directory Creation

Scripts automatically create output directories:
```r
dir_name = "V9f_HighRes+GCMs_Cal1971-1985_Vrf1986-1999_Sim1971-2099__ML-Predict"
dir.create(dir_name, showWarnings = FALSE)
```

Ensure write permissions in working directory.

### Execution Order Dependencies

Some analyses require outputs from earlier steps:

1. **Data filling** (Module 08) → Filled observation files
2. **Cross-correlation** (Module 05) → Predictor rankings
3. **Downscaling** (Modules 01-03) → Monthly projections
4. **Stochastic generation** (Module 04) → Daily realizations
5. **Post-processing** (utils/) → Visualizations, format conversion

<img width="911" height="503" alt="image" src="https://github.com/user-attachments/assets/3bc67b8e-5f40-4347-b61b-4ea9a19f6b48" />

*1981-2006 observed and reconstructed time series of maximum and minimum temperature at station 48478 for calibration period 1981-1993 and verification period 1994-2006*


### Legacy R Compatibility

**Deprecated function warnings** (R 4.x):
- `timeSeries::colStdevs()` → `stats::sd()` with `apply()`
- `MASS::mvrnorm()` argument order changes
- `ncdf` package → `ncdf4` package (NetCDF processing)

**Testing recommendation:** Validate against R 3.6.x before deploying on R 4.x.

### Computational Performance

**Approximate runtimes** (Intel i7, 16 GB RAM):
- ARIMA forecasting: 5-15 minutes
- Multi-regression downscaling: 10-20 minutes
- ML Multi-GCM (single configuration): 15-30 minutes
- Stochastic generation (1000 realizations): 1-3 hours

**Parallelization potential:** Stochastic realizations and multi-scenario projections are embarrassingly parallel (not implemented in current code).

---

## Documentation

### Primary Documentation

1. **[overview.md](overview.md)** - Comprehensive methodology (15,000+ words)
   - Complete script inventory with descriptions
   - Input/output file specifications
   - Analysis workflow details
   - Detailed configuration examples
   - Repository organization and structure

### Inline Documentation

**Script header structure** (typical):
```r
# Lines 1-20:    Description and purpose
# Lines 21-80:   Configuration parameters (⭐ KEY SECTION)
# Lines 81-100:  Input file paths
# Lines 101-150: Analysis parameters
# Lines 150+:    Implementation
```

**Key variables** (search these in script headers):
- `dataf` / `obs.f` - Observed data file
- `indexf` / `gcm.f` - GCM predictor file
- `config.nprd.f` - Predictor configuration
- `verifyfactor` - Calibration/verification split ratio
- `endyearcal.whole.fit` - Final year of calibration period

### External Publications

**Original research outputs** (2009-2015):
- Doctoral dissertation and journal articles
- Conference presentations
- Technical reports

*(Contact repository maintainer for publication list and PDFs)*

---

## Citation

If you use these numerical methods or code in your research, please cite:

```bibtex
@software{bureekul_climate_prediction_2015,
  author = {Bureekul, Werapol},
  title = {Numerical Methods for Climate Prediction and Downscaling},
  year = {2015},
  publisher = {GitHub},
  url = {https://github.com/bwerapol/climate-prediction},
  note = {Computational framework for regional climate prediction (2006-2015)}
}
```

**Additionally cite relevant publications:**
- Doctoral dissertation on numerical climate modeling methods
- Journal articles on multi-GCM ensemble techniques
- Conference papers on stochastic weather generation algorithms
- Technical reports on teleconnection pattern analysis

*(Contact repository maintainer for complete publication list)*

### Acknowledging This Archive

When referencing this **computational framework** in publications:

> "Numerical methods adapted from Bureekul (2015), computational framework for regional climate prediction developed 2006-2015, implementing ARIMA forecasting, multi-GCM ensemble downscaling, and Moran's I-based stochastic generation algorithms. Available: https://github.com/bwerapol/climate-prediction"

---

## License

**Research Code License:** [Specify: MIT, GPL-3, Academic Use Only, etc.]

This repository represents **academic research code** developed during doctoral and post-doctoral studies. While shared for reproducibility and educational purposes, users should:

1. **Cite original work** when using methods or code
2. **Acknowledge limitations** of historical codebase (2006-2015)
3. **Validate results** before operational use
4. **Contact maintainer** for collaboration on derivative work

See [LICENSE](LICENSE) file for complete terms.

---

## Contributing

### Contributions Welcome

This is a **research archive**, but contributions are welcome in these areas:

- **Modernization:** Updates for R 4.x compatibility
- **Documentation:** Additional examples, tutorials, vignettes
- **Refactoring:** Improved code organization and generalization
- **Validation:** Independent verification of methods
- **Extensions:** Application to new regions or climate scenarios

### How to Contribute

1. **Fork** the repository
2. **Create feature branch:** `git checkout -b feature/your-improvement`
3. **Document changes:** Update README and relevant docs
4. **Test thoroughly:** Ensure reproducibility
5. **Submit pull request:** Describe changes and motivation

### Contact

**Repository Maintainer:** Werapol Bureekul
**Research Period:** 2006-2015 (Doctoral/Post-doctoral research in Numerical Modeling)
**Research Focus:** Computational climate prediction, numerical algorithms, time series analysis

**Issues:** Use GitHub Issues for technical questions, bugs, or numerical method discussions
**Collaborations:** Contact via GitHub for research collaboration on computational climate modeling

---

## Acknowledgments

**Funding Sources:**
- [Grant/Fellowship 1]
- [Grant/Fellowship 2]

**Data Providers:**
- Thai Meteorological Department (observational data)
- IPCC Data Distribution Centre (GCM scenarios)
- NOAA Climate Prediction Center (SST indices)

**Computational Resources:**
- [Institution] High-Performance Computing facility

**Academic Advisors:**
- Doctoral and post-doctoral supervisors (2006-2015)
- Collaborators in numerical modeling and computational climate science

**Repository Cleanup:**
- October 2025 - Repository reorganization, documentation enhancement, and preparation for public release

---

## Version History

**v1.0** (2015) - Original research codebase
**v2.0** (October 2025) - Repository cleanup and documentation enhancement
- Reduced from 136,750 to 1,655 files (98.8% reduction)
- Reorganized into modular structure
- Enhanced documentation for reproducibility
- Prepared for public GitHub release

---

**Last Updated:** October 3, 2025
**Repository Status:** Active research archive (2006-2015 methods)
**Maintenance:** Community-driven with original author oversight
