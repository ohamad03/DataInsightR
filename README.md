# DataInsightR

Advanced R package for intelligent data analysis and automated insights.

## Overview

DataInsightR provides a comprehensive toolkit for exploratory data analysis with smart automation. The package helps you quickly understand your data through statistical summaries, quality checks, outlier detection, and visualization.

## Installation

You can install DataInsightR directly from GitHub:
```r
# install.packages("devtools")
devtools::install_github("ohamad03/DataInsightR")
```

## Key Features

- **Smart Data Profiling** - Automatic detection of data types and statistical summaries
- **Outlier Detection** - Multiple methods (IQR, Z-score) to find unusual values
- **Missing Data Analysis** - Comprehensive reports on missing values with recommendations
- **Correlation Analysis** - Statistical significance testing for variable relationships
- **Automated Plotting** - Smart plot generation based on variable types
- **Feature Engineering** - Automatic creation of polynomial and interaction features
- **Data Quality Assessment** - S4 class implementation for quality scoring
- **Distribution Fitting** - Test which statistical distributions fit your data

## Quick Start
```r
library(DataInsightR)

# Get a comprehensive summary of your data
summary_result = smart_summary(mtcars)
print(summary_result)

# Detect outliers
outliers = detect_outliers(mtcars, method = "both")
print(outliers)

# Check data quality (S4 class)
quality = assess_data_quality(iris)
show(quality)

# Auto-generate appropriate plots
plot = auto_plot(mtcars, "mpg", "hp")
print(plot)
```

## Functions

### Main Functions

1. `smart_summary()` - Intelligent data profiling with S3 class output
2. `detect_outliers()` - Multi-method outlier detection
3. `missing_analyzer()` - Missing value pattern analysis
4. `correlation_matrix()` - Correlation with significance tests
5. `auto_plot()` - Automatic visualization generation
6. `feature_engineer()` - Automated feature creation
7. `assess_data_quality()` - S4 class for quality assessment
8. `distribution_fit()` - Distribution fitting and testing

### Object-Oriented Programming

- **S3 Classes**: `data_profile`, `outlier_report`, `missing_analysis`, `correlation_result`
- **S4 Classes**: `DataQuality` with methods `show()` and `getScore()`

## Dependencies

- R >= 4.0.0
- ggplot2 >= 3.3.0
- dplyr >= 1.0.0
- tidyr >= 1.1.0
- stats, methods, graphics, grDevices

## Examples

### Example 1: Complete Data Analysis Workflow
```r
# Load your data
data(mtcars)

# Step 1: Profile the data
profile = smart_summary(mtcars)
print(profile)

# Step 2: Check quality
quality = assess_data_quality(mtcars)
quality_score = getScore(quality)

# Step 3: Find outliers
outliers = detect_outliers(mtcars)

# Step 4: Analyze correlations
correlations = correlation_matrix(mtcars, min_correlation = 0.5)
```

### Example 2: Working with Missing Data
```r
data(airquality)

# Analyze missing patterns
missing_info = missing_analyzer(airquality)
print(missing_info)
```

## Author

Omar Hamad  
Email: ohamad@usf.edu  
GitHub: [@ohamad03](https://github.com/ohamad03)

## License

MIT License - see LICENSE file for details

## Contributing

Issues and pull requests are welcome! Please feel free to submit feedback or suggestions.

## Citation

If you use this package in your research, please cite:
```
Hamad, O. (2025). DataInsightR: Advanced Data Science Toolkit for Intelligent Analysis.
R package version 1.0.0. https://github.com/ohamad03/DataInsightR
```