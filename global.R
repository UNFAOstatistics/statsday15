# 
# 
# # # Doanload data from FAOSTAT
# library(FAOSTAT)
# library(dplyr)
# metdat <- data.frame(domainCode  = c("QC","QC","OA"),
#                      elementCode = c(5510,5510,511),
#                      itemCode    = c(486,1717,3010),
#                      start_year = c(1995,1995,1995),
#                      end_year = c(2012,2012,2012),
#                      indicator_name = c("banana.production",
#                                         "cereal.production",
#                                         "total.population"),
#                      stringsAsFactors = FALSE
# )
# # # Population data
# i <- 3
# dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"], 
#                    elementCode = metdat[i,"elementCode"],
#                    itemCode = metdat[i,"itemCode"],
#                    yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
# data <- dat[["entity"]]
# names(data) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
# # 
# for (i in 1:2){
#   dat <- getFAOtoSYB(domainCode = metdat[i,"domainCode"], 
#                      elementCode = metdat[i,"elementCode"],
#                      itemCode = metdat[i,"itemCode"],
#                      yearRange = metdat[i,"start_year"]:metdat[i,"end_year"])
#   dat <- dat[["entity"]]
#   names(dat) <- c("FAOST_CODE","Year",metdat[i,"indicator_name"])
#   data <- merge(data,dat,by=c("FAOST_CODE","Year"))
# }
# 
# library(countrycode)
# data$Country <- countrycode(data$FAOST_CODE, "fao", "country.name")
# data <- data[!is.na(data$banana.h3("Buttons"),
#                     actionButton("action", label = "Action"),
#                     br(),
#                     br(), 
#                     submitButton("Submit"))production),]
# data <- data[!is.na(data$cereal.production),]
# data <- data[!is.na(data$total.population),]
# # Manual adds
# data$Country[data$FAOST_CODE == 41] <- "China"
# 
# # Lets take the region from gisfao shapefiles
# library(gisfao)
# reg <- fao_world@data
# data2 <- reg[c("FAO_CODE","RAF","LAC","RAP","REU","RNE","ADM0_NAME")]
# data2$Region[data2$RAF == TRUE] <- "Africa"
# data2$Region[data2$RNE == TRUE] <- "Near East and North Africa"
# data2$Region[data2$LAC == TRUE] <- "Latin America and the Caribbean"
# data2$Region[data2$RAP == TRUE] <- "Asia and the Pacific"
# data2$Region[data2$REU == TRUE] <- "Europe and Central Asia"
# data2$Region[is.na(data2$Region)] <- "Other regions"
# 
# names(data2)[names(data2)=="FAO_CODE"] <- "FAOST_CODE"
# names(data2)[names(data2)=="ADM0_NAME"] <- "Country"
# 
# reg <- data2
# 
# save(reg, file="reg.RData")
load("reg.RData")

# data$Region <- as.factor(data$Region)
# data$Country <- as.factor(data$Country)
# data$Year <- as.numeric(data$Year)


# data3 <- readRDS("healthexp.Rds")
# data3$Region <- as.factor(data3$Region)
# h(data3)