library(dplyr)
library(readr)
ozone_19 <- read_csv("data/spotlight_ozone.csv")
startdate <- as.Date("2020/3/1")
enddate <- as.Date("2020/5/31")
ndays <- enddate - startdate + 1
tt <- ts(1:ndays, frequency =1, start =as.Date("2020/3/1"))
ss <- as.Date("2020/3/1")
dates <- seq(from=ss, by=1, length.out=ndays)
ozone_19 <- filter(ozone_19, used_in_nat_agg_0_91 == "Yes")
ozone_19_df <- select(ozone_19, -c(1, 2, 6, 7))
names(ozone_19_df) <- c("latitude", "longitude",
                        "local_site_name", as.character(dates))
j <- 1
for (i in dates){
  icon <- as.character(as.Date(365, origin=as.Date(dates[j])))
  
  con <- as.character(dates[j])
  list <- ozone_19_df[[con]]
  ozone_19_df[icon] <- ifelse(
    list<=0.04, "minus1",
    ifelse(
      list<=0.054, "minus2",
      ifelse(
        list<=0.070, "plus1", "plus2"
      )
    )
  )
  j <- j + 1
}

write.csv(ozone_19_df, file = "ozone_19_df.csv")