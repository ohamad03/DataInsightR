

setwd("/Users/user/Desktop/RPRO FINAL")

file.exists("DESCRIPTION")
file.exists("LICENSE")
file.exists("NAMESPACE")
file.exists("README.md")

list.files("R")
list.files("vignettes")

source("R/smart_summary.R")
source("R/detect_outliers.R")
source("R/missing_analyzer.R")
source("R/correlation_matrix.R")
source("R/auto_plot.R")
source("R/feature_engineer.R")
source("R/data_quality_class.R")
source("R/distribution_fit.R")

result1 = smart_summary(mtcars)
print(result1)

result2 = detect_outliers(mtcars)
print(result2)

result3 = missing_analyzer(airquality)
print(result3)

result4 = assess_data_quality(iris)
show(result4)

