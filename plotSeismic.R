
list.of.pkgs <-  c("IRISSeismic", "ggplot2", "data.table")

new.pkgs <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[, "Package"])]

if (length(new.pkgs)){ install.packages(new.pkgs) }
for (pkg in list.of.pkgs){ library(pkg,character.only = TRUE) }


iris <- new("IrisClient")

# Test the dataselect
# http://service.iris.edu/fdsnws/dataselect/docs/1/builder/
# https://www.iris.edu/app/station_monitor/#2019-01-05T19:25:38/IU-DWPF/webicorder/IU-DWPF|10992822

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

starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
endtime <- as.POSIXct("2010-01-12 23:59:59", tz="GMT")
st <- getDataselect(iris,"CU","GTBY","*","BHZ",starttime,endtime)

# starttime <- as.POSIXct("2010-01-11 00:00:01", tz="GMT")
# endtime <- as.POSIXct("2010-01-12 23:59:59", tz="GMT")
# st <- getDataselect(iris,"CU","GTBY","*","BHZ",starttime,endtime)
# 
 starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
 endtime <- as.POSIXct("2010-01-12 23:59:59", tz="GMT")
 st <- getDataselect(iris,"CU","SDDR","*","BHZ",starttime,endtime)
# 
# starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
# endtime <- as.POSIXct("2010-01-12 23:59:59", tz="GMT")
 st <- getDataselect(iris,"IU","DWPF","*","BHZ",starttime,endtime)
# 
# starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
# endtime <- as.POSIXct("2010-01-13 01:59:59", tz="GMT")
# st <- getDataselect(iris,"IU","DWPF","*","BH2",starttime,endtime)
# 
# starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
# endtime <- as.POSIXct("2010-01-13 01:59:59", tz="GMT")
# st <- getDataselect(iris,"IU","DWPF","*","VH1",starttime,endtime)

# starttime <- as.POSIXct("2010-01-12 21:00:00", tz="GMT")
# endtime <- as.POSIXct("2010-01-12 23:59:59", tz="GMT")
# st <- getDataselect(iris,"Z9","E02","*","*",starttime,endtime)


#starttime <- as.POSIXct("2013-11-07 17:00:00", tz="GMT")
#endtime <- as.POSIXct("2014-03-10 23:59:59", tz="GMT")
#Operation timed out after 300000 milliseconds 
#with 132,035,647 bytes received
st <- getDataselect(iris,"ZH","ILSE1","*","EHE",starttime,endtime)

beg<-st@traces[[1]]@stats@starttime
end<-st@traces[[1]]@stats@endtime
npts<-st@traces[[1]]@stats@npts
data<-st@traces[[1]]@data
dateSeq <- seq(beg,end,length.out = npts)

ts <-data.frame(dateSeq,data)
dtTS <- data.table::data.table(ts)
dtTSBy1Sec = dtTS[,mean(data),by = .(dateSeq=cut(dateSeq, breaks= "1 sec"))]
dtTSBy10Sec = dtTS[,mean(data),by = .(dateSeq=cut(dateSeq, breaks= "10 sec"))]
dtTSBy1Sec$dateSeq<- as.POSIXct(dtTSBy1Sec$dateSeq,tz='GMT')
dtTSBy10Sec$dateSeq<- as.POSIXct(dtTSBy10Sec$dateSeq,tz='GMT')


station<-st@traces[[1]]@id
names(dtTSBy1Sec)<-c('DateTime',station)
names(dtTSBy10Sec)<-c('DateTime',station)

write.csv(dtTSBy10Sec, file = paste0(station,'.10sec.csv'))
write.csv(dtTSBy1Sec, file = paste0(station,'.1sec.csv'))
ggplot2::ggplot(dtTSBy10Sec) +
  ggplot2::aes_q(x=as.name('DateTime'), y=as.name(station)) +
  labs(x = "Event Time", 
       title=st@traces[[1]]@id, 
       subtitle =paste0(beg, ' to ', end)) + 
  ggplot2::geom_line(group=1)

ggplot2::ggplot(dtTSBy1Sec) +
  ggplot2::aes_q(x=as.name('DateTime'), y=as.name(station)) +
  labs(x = "Event Time", 
       title=st@traces[[1]]@id, 
       subtitle =paste0(beg, ' to ', end)) + 
  ggplot2::geom_line(group=1)
