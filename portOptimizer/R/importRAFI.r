# Import RAFI data from raw Excel file.  Create dataset.

library(readxl)
wd <- getwd()
expected.returns <- read_excel(paste0(wd, "/portOptimizer/inst/extdata/Asset-Allocation-Interactive-Data.xlsx"), 
                                        sheet = "Expected.Returns", range = "B4:L31", col_names = TRUE)

expected.cov <- read_excel(paste0(wd, "/portOptimizer/inst/extdata/Asset-Allocation-Interactive-Data.xlsx"), 
                               sheet = "Expected.Risk.Matrix", range = "B35:AC62", col_names = TRUE)
colnames(expected.cov)[1] <- "Asset Class"

asOfDate <- as.character(read_excel(paste0(wd, "/portOptimizer/inst/extdata/Asset-Allocation-Interactive-Data.xlsx"), 
                       sheet = "Expected.Returns", range = "C50", col_names = FALSE))

rafi_201811 <- list(expected.returns = expected.returns,
                    expected.cov = expected.cov,
                    as.of.date = asOfDate)
save(rafi_201811, file="rafi_201811.rdata")
