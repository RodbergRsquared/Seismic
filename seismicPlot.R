library(IRISSeismic)
library(ggplot2)
library(data.table)

iris <- new("IrisClient")

# Test the dataselect
# http://service.iris.edu/fdsnws/dataselect/docs/1/builder/

# Wilber 3: Select Stations  (create graphs online)
# http://ds.iris.edu/wilber3/find_stations/4640178

#CU: Caribbean USGS Network
# GTBY: Guantanamo Bay, Cuba
# SDDR: Presa de Sabenta, Dominican Republic
# MTDJ: Mount Denham, Jamaica
# GRTK: Grand Turk, Turks and Caicos Islands

#Z9 (2010-2014): Southeastern Suture of the Appalachian Margin Experiment
# E01: Silver River State Park, Ocala, Florida
# E02: St. Johns River Water District, Palatka, FL

#IU: Global Seismograph Network (GSN - IRIS/USGS)
# SJG: San Juan, Puerto Rico
# DWPF: Disney Wilderness Preserve, Florida, USA

#ZH (2013-2014): Imaging the Floridan Aquifer System Using Seismic Noise
# http://www.fdsn.org/networks/detail/ZH_2013/
# ILSE1:  thru ILSE9

#starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
#endtime <- as.POSIXct("2010-01-1 23:59:59", tz="GMT")
#st <- getDataselect(iris,"CU","GTBY","*","BHZ",starttime,endtime)

starttime <- as.POSIXct("2014-05-28 21:15:00", tz = "GMT")
endtime <- as.POSIXct("2014-05-28 21:45:00", tz = "GMT")
iris <- new("IrisClient")

st <-getDataselect(iris,network = "CU",station = "TGUH",
                   location = "00",channel = "BHZ",
                   starttime = starttime,endtime = endtime,
                   repository = NULL,inclusiveEnd = TRUE,
                   ignoreEpoch = TRUE)

st <-getDataselect(iris,network = "IU",station = "DWPF",
                   location = "00",channel = "BHZ",
                   starttime = starttime,endtime = endtime,
                   repository = NULL,inclusiveEnd = TRUE,
                   ignoreEpoch = TRUE)
#starttime <- as.POSIXct("2013-11-07 17:00:00", tz="GMT")
#endtime <- as.POSIXct("2014-03-10 23:59:59", tz="GMT")
#Operation timed out after 300000 milliseconds
#with 132,035,647 bytes received
#st <- getDataselect(iris,"ZH","ILSE1","*","EHE",starttime,endtime)

beg <- st@traces[[1]]@stats@starttime
end <- st@traces[[1]]@stats@endtime
npts <- st@traces[[1]]@stats@npts
data <- st@traces[[1]]@data
dateSeq <- seq(beg, end, length.out = npts)

absmax <- function(x) { x[which.max( abs(x) )][1]}
ts <- data.frame(dateSeq, data)
dtTS <- data.table::data.table(ts)
dtTSBy1Sec = dtTS[, mean(data), by = .(dateSeq = cut(dateSeq, breaks = "1 sec"))]
dtTSBy1Sec = dtTS[, absmax(data), by = .(dateSeq = cut(dateSeq, breaks = "1 sec"))]
dtTSBy10Sec = dtTS[, mean(data), by = .(dateSeq = cut(dateSeq, breaks = "10 sec"))]
dtTSBy10Sec = dtTS[, absmax(data), by = .(dateSeq = cut(dateSeq, breaks = "10 sec"))]

ggplot2::ggplot(dtTSBy10Sec) +
  ggplot2::aes(x = as.POSIXct(dateSeq), y = V1) +
  labs(x = "Event Time",
       title = st@traces[[1]]@id,
       subtitle = paste0(beg, ' to ', end)) +
  ggplot2::geom_line(group = 1)

ggplot2::ggplot(dtTSBy1Sec) +
  ggplot2::aes(x = as.POSIXct(dateSeq), y = V1) +
  labs(x = "Event Time",
       title = st@traces[[1]]@id,
       subtitle = paste0(beg, ' to ', end)) +
  ggplot2::geom_line(group = 1)
