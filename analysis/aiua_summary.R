library(data.table)
library(ggplot2)
library(scales)
library(plyr)

ua.df<-read.delim("../cleaned_data/lotus_database_w_iso3_and_cats.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,na.strings="")
ua.df<-data.table(ua.df)
browser()
# Raw: 	action	all_dates	appeal_date	body	cat_abduction	cat_arbitrary_arrest	cat_arbitrary_detention	cat_conscientious_objector	cat_corporate_abuse	cat_death_penalty	cat_death_threat	cat_enforced_disappearance	cat_excessive_force	cat_extrajudicial_execution	cat_fear_for_safety	cat_fear_of_torture	cat_forced_eviction	cat_forced_return	cat_freedom_of_expression	cat_harassment	cat_harsh_prison_conditions	cat_health_concern	cat_house_arrest	cat_hunger_strike	cat_ill_treatment	cat_imminent_execution	cat_incommunicado_detention	cat_legal_concern	cat_lgbti_rights	cat_mass_arrest	cat_medical_concern	cat_other	cat_prisoner_of_conscience	cat_refugee_rights	cat_risk_of_torture	cat_torture	cat_unfair_trial	cat_unjust_arrest	cat_unjust_imprisonment	cat_unlawful_killing	cat_women_s_rights	cat_youth_children_s_rights	category	class_1	class_10	class_11	class_12	class_13	class_14	class_15	class_16	class_17	class_2	class_3	class_4	class_5	class_6	class_7	class_8	class_9	country	data_id	document	event_1	event_2	event_3	event_4	event_5	event_6	gender	iso3	issue_date	subject	year	year_case_count
# Simplify, Want: X, issue_date, year, action, all_dates, country, for all cat_death_penalty
ua.df<-ua.df[,list(id=X,issue_date,year=year,action,all_dates,country=tolower(iso3),subject,	cat_abduction,cat_arbitrary_arrest,cat_arbitrary_detention,cat_conscientious_objector,cat_corporate_abuse,cat_death_penalty,cat_death_threat,cat_enforced_disappearance,cat_excessive_force,cat_extrajudicial_execution,cat_fear_for_safety,cat_fear_of_torture,cat_forced_eviction,cat_forced_return,cat_freedom_of_expression,cat_harassment,cat_harsh_prison_conditions,cat_health_concern,cat_house_arrest,cat_hunger_strike,cat_ill_treatment,cat_imminent_execution,cat_incommunicado_detention,cat_legal_concern,cat_lgbti_rights,cat_mass_arrest,cat_medical_concern,cat_other,cat_prisoner_of_conscience,cat_refugee_rights,cat_risk_of_torture,cat_torture,cat_unfair_trial,cat_unjust_arrest,cat_unjust_imprisonment,cat_unlawful_killing,cat_women_s_rights,cat_youth_children_s_rights)]
ua.df<-ua.df[!is.na(country)]

# clean date
ua.df<-ua.df[which(!is.na(year) | !is.numeric(year))]
ua.df<-ua.df[ua.df[,year>1950]]
ua.df[,year:=as.Date(paste(year,"-01-01",sep=""),format="%Y-%m-%d")]

# UAs over time

h.ua.t <- ggplot(ua.df,aes(x=year))+geom_histogram(binwidth=365)+
	 scale_x_date(breaks="year",labels=date_format("%Y"))+
	 xlab('Year')+
	 ylab('Urgent Actions')+theme_bw()+
	 theme(axis.text.x = element_text(angle=45,hjust=1))
ggsave(plot=h.ua.t,filename="../slides/figures/latest/h_ua_t.png",height=6,width=8)


# plot uas over time

# state.plot<-ggplot(all.sightings,aes(x=YearMonth,y=Sightings))+
# 	geom_line(aes(color="darkblue"))+
# 	facet_wrap(~State,nrow=10,ncol=5)+
# 	theme_bw()+
# 	scale_color_manual(values=c("darkblue"="darkblue"),guide="none")+
# 	scale_x_date(breaks="5 years",labels=date_format("%Y"))+
# 	xlab("Time")+ylab("Number of Sightings")+
# 	opts(title="Number of UFO sightings by Month-Year and US State (1990-2010)")

# ggsave(plot=state.plot,filename="./images/elw_ufo_sightings.pdf",width=14,height=8.5)

# browser()
