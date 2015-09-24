####################################################################################################
#This programme is for web scraping the MPI website to extract log prices for NZ and format them
#For easy analysis.
#This programme requires R to be installed on your system but should install the necessary packages
#Only tested on Windows 7 64 bit OS
#J. Dash Sept 2015
#Bugs or queries please contact jonathan.dash@scionresearch.com
####################################################################################################


# Function to Install and Load R Packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}

# Specify the list of required packages to be installed and load    
Required_Packages=c("rvest", "stringr", "dplyr", "reshape2", "lattice");



# Call the Function
Install_And_Load(Required_Packages);
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Add CPI adjustmentdata
cpi_adjustment<-c(1.51, 1.48, 1.46, 1.44, 1.44, 1.41, 1.37, 1.33, 1.31, 1.28, 1.25, 1.2, 1.18, 1.13, 1.11, 1.09, 1.04, 1.03, 1.02, 1, 1)
Year<-c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
cpi<-data.frame(Year, cpi_adjustment)


#set working directory to output the results
mainDir<- "c:/"
subDir<- "MPI_logprice_outputs"

dir.create(file.path(mainDir, subDir), showWarnings = F)
setwd(file.path(mainDir, subDir))



#Link to MPI website
mpi_log_prices <- html("http://www.mpi.govt.nz/news-and-resources/statistics-and-forecasting/forestry/indicative-new-zealand-radiata-pine-log-prices-by-quarter/")

#Get q1 prices
q1<-mpi_log_prices %>%
  html_nodes("td:nth-child(2)") %>%
  html_text()
#q1<-trim(q1)
Encoding(q1) <- "UTF-8" #Change the encoding to deal with the special characters
#q1<-iconv(q1, 'Latin-9')
q1<-gsub("\\s", "", q1) #Remove white spaces

#Get q2 prices
q2<-mpi_log_prices %>%
  html_nodes("td:nth-child(3)") %>%
  html_text()
#q2<-trim(q2)
Encoding(q2) <- "UTF-8"
#q1<-iconv(q1, 'Latin-9')
q2<-gsub("\\s", "", q2)

#Get q3 prices
q3<-mpi_log_prices %>%
  html_nodes("td:nth-child(4)") %>%
  html_text()
#q3<-trim(q3)
Encoding(q3) <- "UTF-8"
#q1<-iconv(q1, 'Latin-9')
q3<-gsub("\\s", "", q3)

#Get q4 prices
q4<-mpi_log_prices %>%
  html_nodes("td:nth-child(5)") %>%
  html_text()
#q4<-trim(q4)
Encoding(q4) <- "UTF-8"
#q1<-iconv(q1, 'Latin-9')
q4<-gsub("\\s", "", q4)


#Get grade names
grade.names<-mpi_log_prices %>%
  html_nodes("td:nth-child(1)") %>%
  html_text()
Encoding(grade.names)<- "UTF-8"
grade.names<-gsub("\\s", "", grade.names)

#Remove the additional information from the grade name list
drop.names<-c("EXPORT(NZ$perJASm3f.o.b)", "DOMESTIC(NZ$pertonnedeliveredatmill)", 
              "EXPORT(NZ$perJASm3f.o.b.)", "DOMESTIC(NZ$pertonnedeliveredatmill)")

idx = which(grade.names %in% drop.names)
grade.names<- grade.names[! grade.names %in% drop.names]

#Get the year
years<-mpi_log_prices %>%
  html_nodes("h3") %>%
  html_text()

#Make a numeric list of unique years
years<-as.numeric(unique(years))
year.list<-c() #Holder for outputs

#This loop assigns an appropriate number of years based on the number of log-grades reported on.
#After 2012 there were 11 grades, before 1997 - 13, 1998 - 2012 - 12 grades
for (i in 1:length(years))
{
  ifelse(years[i]>2012, holder<-rep.int(years[i], 11), ifelse(years[i]<1997, holder<-rep.int(years[i], 13), holder<-rep.int(years[i], 12)))
  #holder<-rep(years[i], 12)
  year.list<-append(year.list, holder)
}

log.source<-c()
for (i in 1:length(years))
{
  ifelse(years[i]>2012, holder<-rep('export', 4), holder<-rep('export', 5))
  ifelse(years[i]<1997, holder2<-rep('domestic', 8), holder2<-rep('domestic', 7)) 
  log.source<-append(log.source, holder)
  log.source<-append(log.source, holder2)
}


#length(log.source)

#Put the values in a data frame
values<-data.frame(log.source, year.list,grade.names,q1 ,q2, q3, q4, stringsAsFactors=F)
values[values=="*"]<-NA
values[values=="n/a"]<-NA
values[values==""]<-NA



#Find The midpoints
#Ifelse checks whether value contains "-" i.e. is a range and if not just print the stored value
values$q1.mid<-as.numeric(ifelse(grepl("-",values$q1)==TRUE, sapply(strsplit(values$q1, "-"),
                              function(x)mean(as.numeric(x))), values$q1))

values$q2.mid<-as.numeric(ifelse(grepl("-",values$q2)==TRUE, sapply(strsplit(values$q2, "-"),
                                                         function(x)mean(as.numeric(x))), values$q2))

values$q3.mid<-as.numeric(ifelse(grepl("-",values$q3)==TRUE, sapply(strsplit(values$q3, "-"),
                                                         function(x)mean(as.numeric(x))), values$q3))

values$q4.mid<-as.numeric(ifelse(grepl("-",values$q4)==TRUE, sapply(strsplit(values$q4, "-"),
                                                         function(x)mean(as.numeric(x))), values$q4))

#Freshen up them column names boieee!
colnames(values)<-c("Market", "Year", "Log.grade", "Q1.range", "Q2.range", "Q3.range", "Q4.range",
                    "Q1.average", "Q2.average", "Q3.average", "Q4.average")

#Add in a logtype to summarise by
values$Logtype<-ifelse(values$Log.grade %in% c("Pruned", "P1", "P2", "Pruned-Japan,Korea"), "Pruned",ifelse(values$Log.grade=="Pulp", "Pulp", "Unpruned"))

#Add the cpi adjustment factor in to the table
values<-merge(values, cpi, by="Year")

#Add in yearly average value
averages<-data.frame(values$Q1.average, values$Q2.average, values$Q3.average, values$Q4.average)
values$annual.mean<-rowMeans(averages, na.rm=TRUE)


#cpi Adjust annual average value
values$annual.mean.cpi<-values$annual.mean * values$cpi


#Summarise
values.logtype<- values %>% group_by(Year, Logtype) %>% summarise (mean.value.cpi=mean(annual.mean.cpi, na.rm=T))

pruned.logtype<-subset(values.logtype, values.logtype$Logtype=="Pruned")
unpruned.logtype<-subset(values.logtype, values.logtype$Logtype=="Unpruned")

logtype_single_col<-merge(pruned.logtype, unpruned.logtype, by="Year", suffixes=c(".pruned", ".unpruned"))
logtype_single_col<-logtype_single_col[,-c(2,4)] 
logtype_single_col$Differential<-logtype_single_col$mean.value.cpi.pruned-logtype_single_col$mean.value.cpi.unpruned
logtype_single_col$ratio<-logtype_single_col$mean.value.cpi.pruned/logtype_single_col$mean.value.cpi.unpruned


png("Ratios.png", w=15, h=15, units="cm", res=300)
xyplot(ratio ~ Year, data=logtype_single_col
       ,type="l"
       ,ylab="Pruned Sawlog Value / Unpruned Sawlog Value")
dev.off()

png("Difference.png", w=15, h=15, units="cm", res=300)
xyplot(Differential ~ Year, data=logtype_single_col
       ,type="l"
       ,ylab="Pruned Sawlog Value - Unpruned Sawlog Value")
dev.off()


#Write out some datafiles
write.csv(values, "mpi_log_prices.csv", row.names=F)
write.csv(logtype_single_col, "mpi_annual_sawlog_prices.csv", row.names=F)



