library(tidyverse)
library(clipr)
library(lubridate)

duck_time<-#read_clip_tbl()
  read_csv("Data/iNaturalist data/iNat/inat_RAW_Anatidae_Ducks_socal_2010_2025_with_timestamps_CLEAN.csv")

duck_time2<-duck_time %>%
  mutate(
    DateTime=lubridate::as_datetime(time_observed_at, tz = "America/Los_Angeles"),
    Date=lubridate::mdy(observed_on),
    DayOfWeek=weekdays(Date),
    MonthOfYear=month(Date),
    TimeOfDay=format(DateTime, '%H')
  )


####Time####
duck_time2_summary<-duck_time2 %>%
  as_tibble() %>%
  count(TimeOfDay) %>%
  filter(!is.na(TimeOfDay))



time_plot<-
  ggplot(data=duck_time2_summary, aes(x=TimeOfDay, y=n))+
  annotate(geom="rect",
           xmin="08", xmax="17", ymin= -Inf, ymax=Inf, fill="#fbb4ae80", color=NA)+
  geom_bar(stat="identity")+
  # geom_vline(xintercept=c("08","17"), color="red")+
  xlab("Time of day")+
  ylab("# observations")+
  theme_bw()

time_type_summary<-duck_time2_summary %>%
  mutate(Time_num=as.numeric(TimeOfDay),
         TimeType=case_when(Time_num %in% c(8:17)~"On",T~"Off")) %>%
  group_by(TimeType) %>%
  summarise(n=mean(n)) %>%
  mutate(n_tot=sum(n),
         pct_tot=100*n/n_tot)
time_type_summary$n[time_type_summary$TimeType=="On"]/time_type_summary$n[time_type_summary$TimeType=="Off"]

####Day of Week####
duck_time2_summary<-duck_time2 %>%
  as_tibble() %>%
  count(DayOfWeek) %>%
  filter(!is.na(DayOfWeek)) %>%
  mutate(DayOfWeek=factor(DayOfWeek, 
                          levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  ))
weekend_summary<-duck_time2_summary %>%
  mutate(DayType=case_when(DayOfWeek %in% c("Saturday","Sunday")~"Weekend",T~"Weekday")) %>%
  group_by(DayType) %>%
  summarize(n=mean(n))
weekend_summary$n[weekend_summary$DayType=="Weekend"]/weekend_summary$n[weekend_summary$DayType=="Weekday"]

day_plot<-ggplot(data=duck_time2_summary, 
                 aes(x=DayOfWeek, y=n))+
  # geom_bar(stat="identity")+
  annotate(geom="rect",
           xmin="Monday", xmax="Friday", ymin= -Inf, ymax=Inf, fill="#fbb4ae80", color=NA)+
  geom_bar(stat="identity")+
  # geom_vline(xintercept=c("Monday","Friday"), color="red")+
  xlab("Day of week")+
  ylab("# observations")+
  theme_bw()

####Month of Year####
duck_time2_summary<-duck_time2 %>%
  as_tibble() %>%
  count(MonthOfYear) %>%
  filter(!is.na(MonthOfYear))# %>%
# mutate(DayOfWeek=factor(DayOfWeek, 
# levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
# ))
month_summary<-duck_time2_summary %>%
  mutate(MonthType=case_when(
    MonthOfYear %in% c(12,1,2)~"Winter",
    MonthOfYear %in% c(3,4,5)~"Spring",
    MonthOfYear %in% c(6,7,8)~"Summer",
    MonthOfYear %in% c(9,10,11)~"Fall")) %>%
  group_by(MonthType) %>%
  summarize(n=mean(n))
# month_summary$n[month_summary$=="Summer"]/month_summary$n[month_summary$DayType=="Weekday"]

month_plot<-ggplot(data=duck_time2_summary, 
                   aes(x=MonthOfYear, y=n))+
  # geom_bar(stat="identity")+
  annotate(geom="rect",
           xmin=6.5, xmax=7.5, ymin= -Inf, ymax=Inf, fill="#fbb4ae80", color=NA)+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=c(1,4,7,10),
                     labels=c("Jan","Apr","Jul","Oct"))+
  # geom_vline(xintercept=c("Monday","Friday"), color="red")+
  xlab("Month of Year")+
  ylab("# observations")+
  theme_bw()

####Assembling plots into multipanel figure####

library(ggpubr)
day_time_plot<-ggarrange(day_plot, time_plot, ncol=1)
ggsave(day_time_plot, filename="NotForGit/Step4_1/day_time_plot.png",
       height=6, width=6.5)

month_day_time_plot<-ggarrange(month_plot, day_plot, time_plot, ncol=1)
ggsave(month_day_time_plot, filename="NotForGit/Step4_1/month_day_time_plot.png",
       height=7, width=6.5)


duck_time3<-duck_time2 %>%
  filter(!is.na(TimeOfDay)) %>%
  mutate(DayType=case_when(DayOfWeek %in% c("Saturday","Sunday")~"Weekend",
                           T~"Weekday"))

duck_time3 %>%
  count(DayType)

duck_time3 %>%
  count(DayType,TimeOfDay) %>%
  as_tibble() %>%
  ggplot(aes(x=TimeOfDay, y=n))+
  geom_bar(stat="identity")+
  facet_wrap(~DayType, ncol=1, scales="free_y")+
  geom_vline(xintercept=c("08","17"), color="red")



####All taxa for day of week and month of year####
all_taxa_df<-read_csv("NotForGit/Step1_1/all_taxa_df.csv")

all_taxa_df2<-all_taxa_df %>%
  mutate(
    Group3=case_when(Group=="Ducks"~"Ducks",
                     Group2=="Birds"~"Other birds",
                     T~Group2),
    # DateTime=lubridate::as_datetime(time_observed_at, tz = "America/Los_Angeles"),
    # Date=lubridate::ymd(observed_on),
    DayOfWeek=weekdays(Date),
    MonthOfYear=month(Date),
    # TimeOfDay=format(DateTime, '%H')
  )

all_taxa_summary<-all_taxa_df2 %>%
  select(Group3, MonthOfYear, user_id) %>%
  # distinct() %>%
  group_by(Group3, MonthOfYear) %>%
  tally()


month_plot<-ggplot(data=all_taxa_summary %>%
                     mutate(Group3=factor(Group3, 
                                          levels=c("Ducks","Other birds", "Fish","Herps","Invertebrates"))), 
                   aes(x=as.factor(MonthOfYear), y=n))+
  geom_bar(aes(fill=Group3), position=position_stack(), stat="identity")+  scale_fill_viridis_d(name="Taxon", option = "cividis")+
  scale_x_discrete(name="Month",
                   breaks=1:12,
                   labels=c("Jan","","",
                            "Apr","","",
                            "Jul","","",
                            "Oct","",""))+
  ylab("# observations")+
  theme_bw()
month_plot
ggsave(month_plot, filename="NotForGit/Step4_1/month_plot.png", width=4, height=3)


all_taxa_summary2<-all_taxa_df2 %>%
  select(user_id, MonthOfYear) %>%
  distinct() %>%
  group_by(MonthOfYear) %>%
  tally()


month_plot_users<-ggplot(data=all_taxa_summary2 , 
                   aes(x=as.factor(MonthOfYear), y=n))+
  geom_bar(stat="identity") +
  # geom_bar(aes(fill=Group3), position=position_stack(), stat="identity")+  scale_fill_viridis_d(name="Taxon", option = "cividis")+
  scale_x_discrete(name="",
                   breaks=1:12,
                   labels=c("Jan","","",
                            "Apr","","",
                            "Jul","","",
                            "Oct","",""))+
  scale_y_continuous(name="# unique users",
                     breaks=c(0,2500,5000,7500,10000))+
  theme_bw()
month_plot_users
ggsave(month_plot_users, filename="NotForGit/Step4_1/month_plot_users.png", width=4, height=3)



all_taxa_summary3<-all_taxa_df2 %>%
  # select(user_id, MonthOfYear) %>%
  # distinct() %>%
  group_by(MonthOfYear) %>%
  tally()


month_plot_observations<-ggplot(data=all_taxa_summary3 , 
                         aes(x=as.factor(MonthOfYear), y=n))+
  geom_bar(stat="identity") +
  # geom_bar(aes(fill=Group3), position=position_stack(), stat="identity")+  scale_fill_viridis_d(name="Taxon", option = "cividis")+
  scale_x_discrete(name="",
                   breaks=1:12,
                   labels=c("Jan","","",
                            "Apr","","",
                            "Jul","","",
                            "Oct","",""))+
  # scale_y_continuous(name="# unique users",
                     # breaks=c(0,2500,5000,7500,10000))+
  ylab("# observations")+
  theme_bw()
month_plot_observations
# ggsave(month_plot_users, filename="NotForGit/Step4_1/month_plot_users.png", width=4, height=3)

library(ggpubr)
seasonal_plots<-ggarrange(
  # month_plot_observations + theme(axis.text.x = element_blank())+ xlab(""),
  month_plot_users+ theme(axis.text.x=element_blank())+ xlab(""),
  month_plot+theme(legend.position = "bottom"),
  ncol=1,
  common.legend = T, legend = "bottom",
  labels=c("A","B")
)

ggsave(seasonal_plots, filename="NotForGit/Step4_1/seasonal_plots.png",
       width=6.5, height=5.5)
