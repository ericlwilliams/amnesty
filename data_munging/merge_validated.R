# Following along with chapter 1
library(data.table)
library(ggplot2)
library(scales)
library(plyr)

ufo<-read.delim("data/ufo/ufo_awesome.tsv",sep="\t",stringsAsFactors=FALSE,header=FALSE,na.strings="")
# ufo<-read.delim("data/ufo/ufo_testing.tsv",sep="\t",stringsAsFactors=FALSE,header=FALSE,na.strings="")

# Add column names
names(ufo)<-c("DateOccurred","DateReported","Location","ShortDescription","Duration","LongDescription")

# convert Date Occurred to date object
# specify that current dat format is YYYMMDD

# 
ufo <- data.table(ufo)  #convert to data.table
# Clean data columns with data frame

# book version
# good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8,FALSE,TRUE)
# print(paste('dumping ',length(which(!good.rows)),' rows: malformed date'))
# ufo<-ufo[good.rows,]
# ufo$DateOccurred <-as.Date(ufo$DateOccurred,format="%Y%m%d")

#  simpler
raw_nrow<-nrow(ufo)
ufo<-ufo[(nchar(ufo$DateOccurred)==8 & nchar(ufo$DateReported)==8),];
clean_nrow<-nrow(ufo)
print(paste("Dumping ",raw_nrow-clean_nrow," rows: malformed date"))

# Using data.table
ufo[,DateOccurred:=as.Date(DateOccurred,format="%Y%m%d")]
ufo[,DateReported:=as.Date(DateReported,format="%Y%m%d")]

# Clean city/state information
# get.location returns vector length 2 with (city,state)
# if raw malformed, returns (NA,NA)

get.location <- function(raw_loc) {

	# ensure at least one comma
	split.location <- tryCatch(strsplit(raw_loc,",")[[1]],error = function(e) return(c(NA,NA)))
	# remove leading whitespace
	clean.location <- gsub("^ ","",split.location)
	# ensure two entries
	if (length(clean.location)>2){
		return(c(NA,NA))
	}
	else{
		# return(list(clean.location[1],clean.location[2]))
		return(clean.location)
	}
}

# Unsuccessful at implementing data.table version (how to return mixed NA/string values from function call?)
# sticking with book version

#list of city/states (or na/na)
city.state<-lapply(ufo$Location,get.location)
#turn into matrix to add to data.frame
location.matrix <- do.call(rbind,city.state)
# add to data.frame with transform
# ufo<-transform(ufo,USCity=location.matrix[,1],USState=tolower(location.matrix[,2]),stringsAsFactors=FALSE)
ufo<-transform(ufo,USCity=location.matrix[,1],USState=tolower(location.matrix[,2]))

# REMOVE NON-US ENTRIES
# state abreviations come with R as state.abb
us.states<-tolower(state.abb)
# get matching states, NA if none
ufo$USState<-us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)]<-NA

# ufo.us<-subset(ufo,!is.na(USState))
# Get subset of ufo reports from US
ufo.us <- ufo[!is.na(USState)]


# Look at reports over time
h.report.t <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks="50 years")
ggsave(plot=h.report.t,filename="./images/h_report_t.png",height=6,width=8)

# from histogram, most reports after 1960, focus on 1990-2010

ufo.us<-ufo.us[DateOccurred>=as.Date("1990/01/01")]

h.report.t <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()
ggsave(plot=h.report.t,filename="./images/h_report_1990p_t.png",height=6,width=8)

# to investigate seasonal reporting trends, want to group by years and months
# ufo.us$YearMonth = strftime(ufo.us$DateOccurred,format="%Y-%m")
ufo.us[,YearMonth:=strftime(DateOccurred,"%Y-%m")]

# count how many entries in each YearMonth and state
sightings.counts<-ddply(ufo.us,.(USState,YearMonth),nrow)

# need to fill in missing year-months with counts of 0 sightings!
date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)),to=as.Date(max(ufo.us$DateOccurred)),by="month")
date.strings<-strftime(date.range,"%Y-%m")

#create dataframe
states.dates<-lapply(us.states,function(s) cbind(s,date.strings))
states.dates<-data.frame(do.call(rbind,states.dates),stringsAsFactors=FALSE)

# combine with merge
all.sightings<-merge(states.dates,sightings.counts,by.x=c("s","date.strings"),by.y=c("USState","YearMonth"),all=TRUE)

# prepare for analysis
# - fix column names
# - replace NAs with 0s
# - convert string YearMonth entries to date type
# - change state acroynms to categorical variables (factors)

names(all.sightings)<-c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
# replace string YearMonth with date range (repeated for each state)
all.sightings$YearMonth<-as.Date(rep(date.range,length(us.states)))
all.sightings$State<-as.factor(toupper(all.sightings$State))

# plot sightings over time

state.plot<-ggplot(all.sightings,aes(x=YearMonth,y=Sightings))+
	geom_line(aes(color="darkblue"))+
	facet_wrap(~State,nrow=10,ncol=5)+
	theme_bw()+
	scale_color_manual(values=c("darkblue"="darkblue"),guide="none")+
	scale_x_date(breaks="5 years",labels=date_format("%Y"))+
	xlab("Time")+ylab("Number of Sightings")+
	opts(title="Number of UFO sightings by Month-Year and US State (1990-2010)")

ggsave(plot=state.plot,filename="./images/elw_ufo_sightings.pdf",width=14,height=8.5)

# browser()


