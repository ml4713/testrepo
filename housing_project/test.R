
# setup -------------------------------------------------------------------
rm(list = ls())
gc()

pkg.list <- c("dplyr", 
              "tidyr", 
              "stringr",
              "lubridate",
              "ggplot2",
              "randomForest",
              "caret",
              "data.table",
              "devtools",
              "Quandl",
              "Sweep",
              "tidyquant",
              "forcats")


new.pkgs <- pkg.list[!pkg.list %in% installed.packages()[, "Package"]]

install.packages(new.pkgs, dependencies = TRUE)
sapply(pkg.list, require)
setwd("/Users/trpff26/Desktop/housing_project")


# data read in ------------------------------------------------------------
dat <- fread("housing_data.csv", header = T, na.strings = c("", " ", "NA"))

# clean up ----------------------------------------------------------------

# Price
dat[, Price := as.numeric(gsub('\\$|,','', trimws(Price)))]
dat <- dat[!is.na(Price)]
# Date, Year
dat[, Date := dmy(Date)]
dat[, Month := month(Date)]
# Postcode
dat[, Postcode := as.character(Postcode)]
# factors 
factorCols <- names(dat)[sapply(dat, class) %in% "character"]
dat[, (factorCols) := lapply(.SD, as.factor), .SDcols = factorCols]



# Missing Values ----------------------------------------------------------
MISSING_THRES <- 0.5
dat <- dat[, names(dat)[sapply(dat, function(x) sum(is.na(x))/nrow(dat)) < MISSING_THRES], with = FALSE]
dat[, sapply(.SD, function(x) sum(is.na(x)))/nrow(dat) > MISSING_THRES, .SDcols = names(dat)]
dat <- dat[complete.cases(dat)]

# Exploration -------------------------------------------------------------
te <- dat[, median(Price, na.rm = TRUE), by = c("Month")]
ggplot(te, aes(x = Month, y = V1)) + geom_point()
