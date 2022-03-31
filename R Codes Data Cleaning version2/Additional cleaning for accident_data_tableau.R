setwd("C:/Users/j2504/OneDrive - Nanyang Technological University/Desktop/Y2S2/BC2407 - Analytics 2/Project")

library(data.table)
accident <- fread("accident_data_tableau.csv")
table(accident$local_authority_highway)
length(unique(accident$local_authority_district))
length(unique(accident$local_authority_highway))

data17 <- fread("Local_Authority_District_to_County_(December_2017)_Lookup_in_England_(UPDATED).csv")
table(data17$CTY17NM)
length(unique(data17$CTY17NM))

names(data17)[1] <- "local_authority_ons_district"
df = merge(x = accident, y = data17, by = "local_authority_ons_district", all.x = T)
length(unique(df$local_authority_ons_district))

#Narrow down data to find GSS codes not reflected in reference
df[df=="NA"] <- NA
df <- df[is.na(df$FID)]
table(df$local_authority_ons_district)

#Northumberland code changed
accident$local_authority_ons_district[accident$local_authority_ons_district == "E06000048"] <- "E06000057"
#Gateshead to Northumberland
accident$local_authority_ons_district[accident$local_authority_ons_district == "E08000020"] <- "E06000057"
#Bournemouth and Poole to Bournemouth, Christchurch and Poole
accident$local_authority_ons_district[accident$local_authority_ons_district == "E06000028"] <- "E06000058"
accident$local_authority_ons_district[accident$local_authority_ons_district == "E06000029"] <- "E06000058"

accident$accident_severity[accident$local_authority_ons_district == "EHEATHROW"]
#Remove EHEATHROW
accident <- accident[!local_authority_ons_district == "EHEATHROW"]

setwd("C:/Users/j2504/OneDrive - Nanyang Technological University/Desktop")
write.csv(accident, "accident_data_tableau.csv", row.names = F)