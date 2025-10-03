# Utility Scripts - README

This directory contains shared utility functions used across the climate prediction analyses.

## Contents (28 R scripts)

### Core Utilities
- **clear.R** - Workspace clearing utility (used by most scripts)

### Visualization
- **Plot syntax/testPar.R** - Plotting parameter configuration
- **R PlotLine/**
  - `plot-line.R` - Time series line plots
  - `plot-QQline2.R` - Quantile-quantile plots
- **R Boxplot/**
  - `boxplot-general v2.R` - General boxplot generation
  - `boxplot5concludeAll-1c-vertical.R` - Vertical comparative boxplots

### Statistical Analysis
- **R Dist models/** - Distribution fitting and extreme value analysis
  - `dist-fit+ProbCal-2multi-season-multi_rlz-density+percentile v3.R` - Multi-season distribution fitting
  - `dist-fit+ProbCal-3-all-sta in one plot.R` - All stations in one plot
  - `Extreme-fit multi-Season2-multi_rlz v3c-SRES.R` - Extreme value fitting for SRES scenarios

### Correlation and Autocorrelation
- **R matrix cross rel/** - Cross-correlation and autocorrelation analysis
  - `acf pacf-self plot.R` - ACF/PACF plotting
  - `acf-2matrix.R` - ACF matrix generation
  - `cross-cor-2matrix.R` - Cross-correlation matrix
  - `cross-cor-2matrix-with lag.R` - Cross-correlation with lag
  - `cross-cor-self.R` - Self correlation

### Data Processing
- **R Anomaly/anomaly v1.R** - Anomaly calculation
- **R decompose/decompose v1.R** - Time series decomposition
- **R count-unique/count-unique.R** - Count unique values
- **R error cal/error Cal v1.R** - Error calculation and validation metrics
- **R monthly cal/monthly v1.R** - Monthly aggregation calculations

### Spatial Analysis
- **R finding distance/distance matrix.R** - Calculate distance matrices between stations

### Data Extraction and Conversion
- **R extract for LARS-WG/matrix to LARS-WG.R** - Extract data for LARS-WG weather generator
- **R read and extract data/**
  - `matrix to multi-file.R` - Export matrix data to multiple files
  - `matrix to multi-file_future.R` - Export future scenario data
- **R read LARS-WG results/read LARS-WG results.R** - Read LARS-WG output files
- **R read SDSM results/** - Read SDSM (Statistical DownScaling Model) outputs
  - `read SDSM results.R` - General SDSM reader
  - `read SDSM results - sel STA.R` - Selected stations
  - Sample output: `obsal_X48459_MAX_A1Bt1.OUT` (1 example file)

### Format Conversion for Hydrological Models
- **R RLZ to SwatDBF/** - Convert stochastic realizations to SWAT model format
  - `sharerun-all.R` - Process all variables
  - `sharerun-allSCNopt.R` - Process optimal scenarios

## Usage

Most main analysis scripts source utilities as needed:
```r
source("scripts/utils/clear.R")
source("scripts/utils/R Boxplot/boxplot-general v2.R")
```

Update paths in main scripts if utilities are moved.

## Cleanup Summary

**Original:** 81 R scripts + many output files
**Now:** 28 essential R scripts + 1 sample output file

Removed:
- Output directories and generated files
- Duplicate/versioned scripts
- Test files and logs
- Multiple sample outputs (kept 1 representative)
