## Save the AK (actinic keratosis) data as a plain CSV file.
## ak.sav and ak.sas7bdat contain identical data (see compare_ak_data.R /
## ak_data_description.md), so either source could be used; ak.sav is read
## here.

library(haven)

sav <- read_sav("ak.sav")
write.csv(sav, "ak.csv", row.names = FALSE, na = "")

cat("Wrote ak.csv with", nrow(sav), "rows and", ncol(sav), "columns\n")
