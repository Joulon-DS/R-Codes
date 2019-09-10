# read File #
library(data.table)
library(readxl)
Consumption_Rate <- read_excel("C:/Data/IFS/Consumption_Rate.xlsx")
Consumption_Rate_Per <- read_excel("C:/Data/IFS/Consumption_Rate.xlsx", 
                                   sheet = "Sheet5")

library(readr)
Inventory_main <- read_csv("C:/Data/All_rigs_with_name/All_rigs_with_name.csv")
Inventory_main <- data.frame(Inventory_main)
Inventory_main$Minimum.Level[is.na(Inventory_main$Minimum.Level)] <- 0
Inventory_main$Maximum.Level[is.na(Inventory_main$Maximum.Level)] <- 0

head(Inventory_main)
colnames(Inventory_main) <- make.names(names(Inventory_main))

head(Consumption_Rate)
colnames(Consumption_Rate_Per)[1] <- c("Site_Item")

Consumption_Rate <- data.frame(Consumption_Rate)
Consumption_Rate$Qtr_Yr <- as.character(zoo::as.yearqtr(Consumption_Rate$Applied.Date))
Consumption_Rate$Mon_Yr <- as.character(zoo::as.yearmon(Consumption_Rate$Applied.Date))

colnames(Consumption_Rate)[5] <- c("Site_Item")

Inv <- Inventory_main[,c("Site_Item", "Commodity.Group",  "Issue.Unit", "Current.Balance", "Qty.Avbl", "Qty.Res.vd", "Average.Cost", "Ext.d.Cost", "Rig.Name")]


Inv_Con <- dplyr::left_join(Consumption_Rate, Inv, by = c("Site_Item"))
Inv_Con <- data.frame(Inv_Con)


Inv_Con <- dplyr::filter(Inv_Con, Inv_Con$Commodity.Group == "ESP")

# Treating "A"

a <-as.data.frame.matrix(table(Inv_Con$Site_Item, Inv_Con$Qtr_Yr))
setDT(a, keep.rownames = TRUE)[]
colnames(a)[1] <- c("Site_Item")
a <- data.frame(a)
a <- a[,c(1, 7:33)]

nam_a <- names(a)

colnames(a) <- as.character(seq(1:ncol(a)))

foo <- apply(a[,c(2:28)],1,function(x) which(x > 0))

comp <- c()
for(i in 1:NROW(foo)){
  comp[i] <- toString(names(foo[[i]]))
}

x <- c()
for(i in 1:nrow(a)){
  x[i] <- round(mean(diff(as.integer(unlist(strsplit(comp[i],","))))),2)
}
a$Diff <- x
a$na_a <- rowSums(is.na(a[2:28]))
a$Diff[is.nan(a$Diff)] <- "Used_OLT"
colnames(a)[1:28] <- nam_a

a$Diff_Bin <- ifelse(a$Diff == "Used_OLT", "OLT", 
                     ifelse(as.numeric(as.character(a$Diff)) > 0 & as.numeric(as.character(a$Diff)) < 2, "Quarterly",
                            ifelse(as.numeric(as.character(a$Diff)) > 1 & as.numeric(as.character(a$Diff)) < 3, "Semi_Quarterly",
                                   ifelse(as.numeric(as.character(a$Diff)) > 3 & as.numeric(as.character(a$Diff)) < 5, "Yearly",
                                          ifelse(as.numeric(as.character(a$Diff)) > 4 & as.numeric(as.character(a$Diff)) < 12, "3Yr", "3Yr_Above")))))



a1 <- dplyr::left_join(a, Inv_Con, by = c("Site_Item"))
fwrite(a1, "trend.csv")