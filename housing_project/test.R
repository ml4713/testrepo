# setup -------------------------------------------------------------------
rm(list = ls())
gc()

pkg.list <- c("dplyr", 
              "tidyr", 
              "caret",
              "stringr",
              "lubridate",
              "dummies",
              "ggplot2",
              "randomForest",
              "caret",
              "data.table",
              "devtools",
              "sweep")

new.pkgs <- pkg.list[!pkg.list %in% installed.packages()[, "Package"]]

if (length(new.pkgs) > 0) {install.packages(new.pkgs, dependencies = TRUE)}
lapply(pkg.list, require, character.only = TRUE)
setwd("/Users/trpff26/Desktop/misc/housing_project")


# data read in ------------------------------------------------------------
dat <- fread("housing_data.csv", header = T, na.strings = c("", " ", "NA"))

# clean up ----------------------------------------------------------------

# Price
dat[, Price := as.numeric(gsub('\\$|,','', trimws(Price)))]
dat <- dat[!is.na(Price)]
# Date, Year
dat[, Date := dmy(Date)]
dat[, Month := month(Date)]
dat[, Year := as.character(year(Date))]
# Postcode
dat[, Postcode := as.character(Postcode)]
# factors 
factorCols <- names(dat)[sapply(dat, class) %in% "character"]
dat[, (factorCols) := lapply(.SD, as.factor), .SDcols = factorCols]
factorCols <- names(dat)[sapply(dat, class) %in% "factor"]




# Missing Values ----------------------------------------------------------
MISSING_THRES <- 0.5
dat <- dat[, names(dat)[sapply(dat, function(x) sum(is.na(x))/nrow(dat)) < MISSING_THRES], with = FALSE]
#dat[, sapply(.SD, function(x) sum(is.na(x)))/nrow(dat) > MISSING_THRES, .SDcols = names(dat)]
sapply(dat, function(x) sum(is.na(x))/nrow(dat))

dat[is.na(Bedroom2), "Bathroom", with = F] #bathrooms missing == bedrooms missing
summary(dat[is.na(Bedroom2)]) #missing at random
#dat <- dat[complete.cases(dat)]

# Exploration -------------------------------------------------------------
monthprice <- dat[, .(med.price = median(Price, na.rm = TRUE)), by = c("Month")]
ggplot(monthprice, aes(x = Month, y = med.price)) + geom_line()
ggplot(dat[Rooms < 7], aes(x = as.factor(Rooms), y = Price)) + geom_boxplot()
ggplot(dat[SellerG %in% c("Jellis", "Nelson", "hockingstuart")], aes(x = SellerG, y = Price)) + geom_boxplot()

ggplot(dat, aes(x = Price)) + 
  geom_density(color = "red", fill = "red", alpha = 0.3)





pc <- fread("https://raw.githubusercontent.com/charliesome/australia_postcode/master/lib/australia/postcode/data.csv", header = TRUE)
names(pc)[1] <- "Postcode"
pc[, Postcode := as.character(Postcode)]
#pc <- pc[, c("Postcode", "state"), with = FALSE]
#pc <- pc[!duplicated(pc)]
#dat <- merge(dat, pc, by = "Postcode")
dat[, Postcode := substring(as.character(Postcode), 1, 2)]
dat[, Postcode := as.factor(Postcode)]
# Removing feature round 1 ------------------------------------------------
#dat[, c("Date", "Lattitude", "Longtitude", "SellerG", "Address"):= NULL]
dat <- dat[, !names(dat) %in% c("CouncilArea", "Regionname", "Date", "Lattitude", "Longtitude", "SellerG", "Address", "Suburb"), with = FALSE]

sapply(dat[, names(dat) %in% factorCols, with = F], function(x) length(unique(x)))



# Dummy -------------------------------------------------------------------

dum.dat <- dummy.data.frame(dat, sep = ".")
names(dum.dat)
dum.dat <- dum.dat[complete.cases(dum.dat), ]
# Modeling - train/test split ---------------------------------------------
set.seed(1015)
trainIndex <- createDataPartition(dum.dat$Price, p = .8, list = FALSE, times = 1)
houseTrain <- dum.dat[trainIndex, ]
houseTest <- dum.dat[-trainIndex, ]



# Modeling - Training -----------------------------------------------------
control <- trainControl(method="repeatedcv", number = 5, repeats = 1, search="grid")
tunegrid <- expand.grid(.mtry = c(1:15))
rf_gridsearch <- train(Price ~ ., data = houseTrain, method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = control, verbose = TRUE)
print(rf_gridsearch)
plot(rf_gridsearch)


bestmtry <- tuneRF(houseTrain[, !colnames(houseTrain) %in% "Price"], houseTrain$Price, stepFactor = 1.5, ntree = 1000)
print(bestmtry) 




trellis.par.set(caretTheme())
plot(gbmFit1)
