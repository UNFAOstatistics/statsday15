library(FAOSTAT)
library(dplyr)
library(tidyr)
dat <- getFAOtoSYB(domainCode = "TM",#"QC", 
                   elementCode = 5910,
                   itemCode = 564)
dat <- dat[["entity"]]
dat <- dat[!is.na(dat$QC_486_5510),]

names(dat) <- c("FAOST_CODE","Year","value")


for (FC in unique(dat$FAOST_CODE)) {
  df <- dat[dat$FAOST_CODE == FC,]
  value <- ts(df$value, start = c(1961), frequency = 1)
  value <- as.xts(value)
  assign(paste0("XYZ",FC),value,envir=globalenv())
}

object.list <- apropos("^XYZ", ignore.case = FALSE) # parempi vaihtoehto
z <- do.call(cbind,mget(object.list, envir = globalenv()))

library(dygraphs)
library(xts)
lungDeaths <- cbind(men=as.xts(XYZ100), women=as.xts(XYZ101))
dygraph(lungDeaths)

dygraph(z, main = "New Haven Temperatures") %>% 
  dyRangeSelector(dateWindow = c("1961-01-01", "2013-01-01")) %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyLegend(width = 600)