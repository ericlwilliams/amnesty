# Following along with chapter 1
library(data.table)


dp.df<-read.delim("../cleaned_data/lotus_database_w_iso3_and_cats.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,na.strings="")
dp.df<-data.table(dp.df)

# Raw: 	action	all_dates	appeal_date	body	cat_abduction	cat_arbitrary_arrest	cat_arbitrary_detention	cat_conscientious_objector	cat_corporate_abuse	cat_death_penalty	cat_death_threat	cat_enforced_disappearance	cat_excessive_force	cat_extrajudicial_execution	cat_fear_for_safety	cat_fear_of_torture	cat_forced_eviction	cat_forced_return	cat_freedom_of_expression	cat_harassment	cat_harsh_prison_conditions	cat_health_concern	cat_house_arrest	cat_hunger_strike	cat_ill_treatment	cat_imminent_execution	cat_incommunicado_detention	cat_legal_concern	cat_lgbti_rights	cat_mass_arrest	cat_medical_concern	cat_other	cat_prisoner_of_conscience	cat_refugee_rights	cat_risk_of_torture	cat_torture	cat_unfair_trial	cat_unjust_arrest	cat_unjust_imprisonment	cat_unlawful_killing	cat_women_s_rights	cat_youth_children_s_rights	category	class_1	class_10	class_11	class_12	class_13	class_14	class_15	class_16	class_17	class_2	class_3	class_4	class_5	class_6	class_7	class_8	class_9	country	data_id	document	event_1	event_2	event_3	event_4	event_5	event_6	gender	iso3	issue_date	subject	year	year_case_count
# Simplify, Want: X, issue_date, year, action, all_dates, country, for all cat_death_penalty
dp.df<-dp.df[,list(id=X,issue_date,year,action,all_dates,country=tolower(iso3),subject,body=tolower(body),cat_death_penalty)]

dp.df<-dp.df[country=="usa"]

# ignore categorization
# dp.df<-dp.df[cat_death_penalty==1]

ndp=nrow(dp.df)
print(paste(ndp," death penalty UIs"))

# remove entries with weird years
dp.df<-dp.df[which(!is.na(year) | !is.numeric(year))]
print(paste('Dumping ',ndp-nrow(dp.df),' entries: malformed date'))


# Load execution database
# Date	Name	Age	Sex	Race	Number / Race / Sex of Victims	State	County	Region	Method	Juvenile	Federal	Volunteer	Foreign National
ex_df<-read.delim("../cleaned_data/execution_database.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,na.strings="")
ex_df<-data.table(ex_df)
ex_df<-ex_df[,list(Date,Name=tolower(Name),Sex,Race,Age,State)]



ex.names<-ex_df$Name
# confirmed names
conf.names = rep(NA,nrow(dp.df))
for (i in 1:length(ex.names)){
	# browser()
	# append(good.names,ex_df$Name[(grep(ex.names[[i]],dp.df$body))])
	res<-grep(ex.names[[i]],dp.df$body)
	if(length(res)!=0){
		conf.names[res]<-ex.names[[i]]
	}
}
print('done')

dp.df<-cbind(dp.df,conf.names)
dp.df<-dp.df[!is.na(dp.df$conf.names)]
colnames(dp.df)[colnames(dp.df)=="conf.names"]<-"name"
print(paste('Cross referenced cases: ',nrow(dp.df)))

write.csv(dp.df,file="../cleaned_data/valid_exec_db.csv")


# # ufo<-read.delim("data/ufo/ufo_testing.tsv",sep="\t",stringsAsFactors=FALSE,header=FALSE,na.strings="")

# # Add column names
# names(ufo)<-c("DateOccurred","DateReported","Location","ShortDescription","Duration","LongDescription")

# # convert Date Occurred to date object
# # specify that current dat format is YYYMMDD

# # 
# ufo <- data.table(ufo)  #convert to data.table
# # Clean data columns with data frame

# # book version
# # good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8,FALSE,TRUE)
# # print(paste('dumping ',length(which(!good.rows)),' rows: malformed date'))
# # ufo<-ufo[good.rows,]
# # ufo$DateOccurred <-as.Date(ufo$DateOccurred,format="%Y%m%d")

# #  simpler
# raw_nrow<-nrow(ufo)
# ufo<-ufo[(nchar(ufo$DateOccurred)==8 & nchar(ufo$DateReported)==8),];
# clean_nrow<-nrow(ufo)
# print(paste("Dumping ",raw_nrow-clean_nrow," rows: malformed date"))

# # Using data.table
# ufo[,DateOccurred:=as.Date(DateOccurred,format="%Y%m%d")]
# ufo[,DateReported:=as.Date(DateReported,format="%Y%m%d")]

# # Clean city/state information
# # get.location returns vector length 2 with (city,state)
# # if raw malformed, returns (NA,NA)

# get.location <- function(raw_loc) {

# 	# ensure at least one comma
# 	split.location <- tryCatch(strsplit(raw_loc,",")[[1]],error = function(e) return(c(NA,NA)))
# 	# remove leading whitespace
# 	clean.location <- gsub("^ ","",split.location)
# 	# ensure two entries
# 	if (length(clean.location)>2){
# 		return(c(NA,NA))
# 	}
# 	else{
# 		# return(list(clean.location[1],clean.location[2]))
# 		return(clean.location)
# 	}
# }

# # Unsuccessful at implementing data.table version (how to return mixed NA/string values from function call?)
# # sticking with book version

# #list of city/states (or na/na)
# city.state<-lapply(ufo$Location,get.location)
# #turn into matrix to add to data.frame
# location.matrix <- do.call(rbind,city.state)
# # add to data.frame with transform
# # ufo<-transform(ufo,USCity=location.matrix[,1],USState=tolower(location.matrix[,2]),stringsAsFactors=FALSE)
# ufo<-transform(ufo,USCity=location.matrix[,1],USState=tolower(location.matrix[,2]))

# # REMOVE NON-US ENTRIES
# # state abreviations come with R as state.abb
# us.states<-tolower(state.abb)
# # get matching states, NA if none
# ufo$USState<-us.states[match(ufo$USState,us.states)]
# ufo$USCity[is.na(ufo$USState)]<-NA

# # ufo.us<-subset(ufo,!is.na(USState))
# # Get subset of ufo reports from US
# ufo.us <- ufo[!is.na(USState)]


# # Look at reports over time
# h.report.t <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks="50 years")
# ggsave(plot=h.report.t,filename="./images/h_report_t.png",height=6,width=8)

# # from histogram, most reports after 1960, focus on 1990-2010

# ufo.us<-ufo.us[DateOccurred>=as.Date("1990/01/01")]

# h.report.t <- ggplot(ufo.us,aes(x=DateOccurred))+geom_histogram()
# ggsave(plot=h.report.t,filename="./images/h_report_1990p_t.png",height=6,width=8)

# # to investigate seasonal reporting trends, want to group by years and months
# # ufo.us$YearMonth = strftime(ufo.us$DateOccurred,format="%Y-%m")
# ufo.us[,YearMonth:=strftime(DateOccurred,"%Y-%m")]

# # count how many entries in each YearMonth and state
# sightings.counts<-ddply(ufo.us,.(USState,YearMonth),nrow)

# # need to fill in missing year-months with counts of 0 sightings!
# date.range<-seq.Date(from=as.Date(min(ufo.us$DateOccurred)),to=as.Date(max(ufo.us$DateOccurred)),by="month")
# date.strings<-strftime(date.range,"%Y-%m")

# #create dataframe
# states.dates<-lapply(us.states,function(s) cbind(s,date.strings))
# states.dates<-data.frame(do.call(rbind,states.dates),stringsAsFactors=FALSE)

# # combine with merge
# all.sightings<-merge(states.dates,sightings.counts,by.x=c("s","date.strings"),by.y=c("USState","YearMonth"),all=TRUE)

# # prepare for analysis
# # - fix column names
# # - replace NAs with 0s
# # - convert string YearMonth entries to date type
# # - change state acroynms to categorical variables (factors)

# names(all.sightings)<-c("State","YearMonth","Sightings")
# all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
# # replace string YearMonth with date range (repeated for each state)
# all.sightings$YearMonth<-as.Date(rep(date.range,length(us.states)))
# all.sightings$State<-as.factor(toupper(all.sightings$State))

# # plot sightings over time

# state.plot<-ggplot(all.sightings,aes(x=YearMonth,y=Sightings))+
# 	geom_line(aes(color="darkblue"))+
# 	facet_wrap(~State,nrow=10,ncol=5)+
# 	theme_bw()+
# 	scale_color_manual(values=c("darkblue"="darkblue"),guide="none")+
# 	scale_x_date(breaks="5 years",labels=date_format("%Y"))+
# 	xlab("Time")+ylab("Number of Sightings")+
# 	opts(title="Number of UFO sightings by Month-Year and US State (1990-2010)")

# ggsave(plot=state.plot,filename="./images/elw_ufo_sightings.pdf",width=14,height=8.5)

# # browser()


