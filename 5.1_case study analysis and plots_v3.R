library(tidyverse)
library(sf)

case_observation_join_summary2<-read_csv("NotForGit/Step5_0/case_observation_join_summary2.csv")
case_observation_join_annual2<-read_csv("NotForGit/Step5_0/case_observation_join_annual2.csv")
case_sf<-st_read("C:/Users/Raphaelm/Documents/ArcGIS/Projects/NBR/NBR.gdb",
                 layer="NBR_case_studies_Merge") 
case_pop_df<-#read_csv("Data/casestudy_buffers_population_summary.csv")
  read_csv("Data/casestudy_buffers_population_summary_v2.csv")
popdens_df<-read_csv("Data/stream_population_density_2020_wide.csv")
annual_segs<-readRDS("NotForGit/Step5_1/annual_segs.RDS") %>%
  filter(Group=="All taxa", Metric=="n_observers") #Can't import all of them
# smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
smc_flowlines100_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")

case_observation_join_summary2 %>%
  filter(Group=="All taxa", Metric=="n_observers") %>%
  select(Name, MetricResult_Dens )

#####Prep annual data set#####
ggplot(data=popdens_df, aes(x=pop_density_per_km2_5km))+
  geom_density()

#First, identify matching segments
case_df<-case_sf %>%
  as_tibble(  ) %>%
  select(-Shape) %>%
  # inner_join(case_pop_df %>% select(Name, popdens_5k=pop_density_per_km2_5km)) %>%
  inner_join(case_pop_df %>% select(Name, popdens_5k=pop_dens_5k )) %>%
  mutate(PopMin=0.75*popdens_5k,
         PopMax=1.25*popdens_5k)

case_segs_df<-lapply(1:nrow(case_df), function(i){
  popmin.i=case_df$PopMin[i]
  popmax.i=case_df$PopMax[i]
  name.i=case_df$Name[i]
  # yc.i=case_df$YearCompleted[i]
  case_df.i=case_df[i,]
  junk.i<-popdens_df %>%  
    select(Segment_ID=segment_id, pop_density_per_km2_5km) %>%
    filter(pop_density_per_km2_5km>=popmin.i,
           pop_density_per_km2_5km<popmax.i)  %>%
    mutate(Name=name.i)
  case_df.i %>% inner_join(junk.i)
}) %>% bind_rows()


case_segs_df<-lapply(1:nrow(case_df), function(i){
  # popmin.i=case_df$PopMin[i]
  # popmax.i=case_df$PopMax[i]
  popcase.i=case_df$popdens_5k[i]
  name.i=case_df$Name[i]
  # yc.i=case_df$YearCompleted[i]
  case_df.i=case_df[i,]
  junk.i<-popdens_df %>%  
    select(Segment_ID=segment_id, pop_density_per_km2_5km) %>%
    mutate(PopDiff=abs(pop_density_per_km2_5km-popcase.i)) %>%
    slice_min(PopDiff, prop=0.05) %>%
    mutate(Name=name.i)
  case_df.i %>% 
    inner_join(junk.i) %>%
    mutate(PopMin=min(pop_density_per_km2_5km),
           PopMax=max(pop_density_per_km2_5km),
           PopMin_pct=100*(popdens_5k-PopMin)/popdens_5k,
           PopMax_pct=100*(PopMax-popdens_5k)/popdens_5k,
           n_segs=length(junk.i$pop_density_per_km2_5km))
}) %>% bind_rows()

req_segs_df<-case_segs_df %>%
  select(Name,Segment_ID) %>% 
  distinct() %>%
  left_join(smc_flowlines100_sf %>%
              as_tibble() %>% 
              select(Segment_ID,SMCShed, TotLength_km, COMID, lengthkm2, SegLength_km,buff_area_km2))
all_mets<-case_observation_join_summary2 %>% select(Metric) %>% distinct()
all_groups<-case_observation_join_summary2 %>% select(Group) %>% distinct()
all_years<-tibble(Year=2010:2025)
req_segs_mets_df<-
  req_segs_df %>%
  crossing(all_groups, all_mets, all_years) %>%
  left_join(annual_segs) %>%
  mutate(MetricResult_Raw = case_when(is.na(MetricResult_Raw)~0,
                                      T~MetricResult_Raw),
         MetricResult_Dens = case_when(is.na(MetricResult_Dens)~0,
                                       T~MetricResult_Dens))
req_segs_mets_df %>% filter(Metric=="n_observers")

req_segs_mets_df_summary<-req_segs_mets_df %>%
  filter(SegLength_km>0.999,
         MetricResult_Dens>0) %>%
  group_by(Name, Group, Metric, Year) %>%
  summarize(q10=quantile(MetricResult_Dens, 0.1),
            q25=quantile(MetricResult_Dens, 0.25),
            q50=quantile(MetricResult_Dens, 0.5),
            q75=quantile(MetricResult_Dens, 0.75),
            q90=quantile(MetricResult_Dens, 0.9),
            # q10nz=quantile(MetricResult_Dens[MetricResult_Dens>0], 0.1, na.rm=T),
            # q25nz=quantile(MetricResult_Dens[MetricResult_Dens>0], 0.25, na.rm=T),
            # q50nz=quantile(MetricResult_Dens[MetricResult_Dens>0], 0.5, na.rm=T),
            # q75nz=quantile(MetricResult_Dens[MetricResult_Dens>0], 0.75, na.rm=T),
            # q90nz=quantile(MetricResult_Dens[MetricResult_Dens>0], 0.9, na.rm=T)
  ) %>%
  ungroup()

req_segs_mets_df_summary %>% filter(Metric=="n_observers")
req_segs_mets_df_summary$Metric %>% unique()
#######Make Plots#######
# 
# case_study_plot_no_context<-ggplot(data=case_observation_join_annual2 %>%
#                                      filter(Group=="All taxa", Metric=="n_observers"),
#                                    aes(x=Year, y=MetricResult_Dens)) +
#   geom_path() +
#   geom_vline(data=case_sf  %>%
#                as_tibble(),
#              aes(xintercept=YearCompleted, color="red")
#   )+
#   scale_color_manual(values="red", name="", labels=c("Year of completion"))+
#   facet_wrap(~Name, scales="free_y")+
#   ylab("NBR metric")+
#   scale_y_continuous(limits=c(0, NA))+expand_limits(y = 0)+
#   scale_x_continuous(limits=c(2009.5, 2025.5))
# ggsave(case_study_plot_no_context, filename="NotForGit/Step5_1/case_study_plot_no_context.png",
#        width=10, height=6)



case_study_plot_popdens<-
  ggplot(data=case_observation_join_annual2 %>%
           filter(Group=="All taxa", Metric=="n_observers"),
         aes(x=Year, y=MetricResult_Dens)) +
  geom_path(data=req_segs_mets_df_summary%>%
              filter(Group=="All taxa", Metric=="n_observers") %>%
              mutate(MetricResult_Dens=NA_real_),
            aes(y=q50), color="blue", linetype="dotted")+
  geom_ribbon(data=req_segs_mets_df_summary%>%
                filter(Group=="All taxa", Metric=="n_observers") %>%
                mutate(MetricResult_Dens=NA_real_),
              aes(ymin=q25, ymax=q75), alpha=0.75, fill="lightblue")+
  geom_path() +
  geom_vline(data=case_pop_df,#case_sf  %>%               as_tibble(),
             aes(xintercept=YearCompleted), color="red", linetype="dashed"
  )+
  facet_wrap(~Name, scales="free_y")+
  ylab("Density of observations")+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()

ggsave(case_study_plot_popdens, filename="NotForGit/Step5_1/case_study_plot_popdens.png",
       width=10.2, height=6)

####Selected examples
sel_proj<-c(#"Bowtie Parcel",
  "South LA Wetlands Park",
  # "Rice Creek",
  #"Chollas Creek Dechannelization",
  "Paradise Creek",
  # "Serrano Creek Park",
  # "Las Virgenes Phase 1",
  # "OCWD Weir Ponds",
  # "Arroyo Burro Open Space Acquisitition",
  "Mission Creek at Oak Park",
  "Oxford Retention Basin",
  "Sea Summit",
  "Crown Valley Gateway Project",
  "Dairy Forks Wetlands",
  "Galivan Basin",
  "Riverbend Community"
)

case_study_plot_popdens_selected<-
  ggplot(data=case_observation_join_annual2 %>%
           filter(Group=="All taxa", 
                  Metric=="n_observers",
                  Name %in% sel_proj),
         aes(x=Year, y=MetricResult_Dens)) +
  geom_path(data=req_segs_mets_df_summary%>%
              filter(Group=="All taxa", 
                     Metric=="n_observers",
                     Name %in% sel_proj) %>%
              mutate(MetricResult_Dens=NA_real_),
            aes(y=q50), color="blue", linetype="dotted")+
  geom_ribbon(data=req_segs_mets_df_summary%>%
                filter(Group=="All taxa", 
                       Metric=="n_observers",
                       Name %in% sel_proj) %>%
                mutate(MetricResult_Dens=NA_real_),
              aes(ymin=q25, ymax=q75), alpha=0.75, fill="lightblue")+
  geom_path() +
  geom_vline(data=case_pop_df %>%
               filter(Name %in% sel_proj),#case_sf  %>%               as_tibble(),
             aes(xintercept=YearCompleted), color="red", linetype="dashed"
  )+
  facet_wrap(~Name, scales="free_y", ncol=3)+
  ylab("Density of observers")+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()


ggsave(case_study_plot_popdens_selected, filename="NotForGit/Step5_1/case_study_plot_popdens_selected.png",
       width=6.5, height=6)


ggplot(data=case_observation_join_annual2 %>%
         filter(Group=="All taxa", Metric=="n_observers",
                Name=="Rice Creek"),
       aes(x=Year, y=MetricResult_Dens)) +
  geom_ribbon(data=req_segs_mets_df_summary%>%
                filter(Group=="All taxa", Metric=="n_observers",
                       Name=="Rice Creek") %>%
                mutate(MetricResult_Dens=NA_real_),
              aes(ymin=q25, ymax=q75), alpha=0.25)+
  geom_ribbon(data=req_segs_mets_df_summary%>%
                filter(Group=="All taxa", Metric=="n_observers",
                       Name=="Rice Creek") %>%
                mutate(MetricResult_Dens=NA_real_),
              aes(ymin=q10, ymax=q90), alpha=0.25)+
  # geom_path() +
  geom_vline(data=case_sf  %>% filter(Name=="Rice Creek") %>% as_tibble(),
             aes(xintercept=YearCompleted), color="red", linetype="dashed"
  )+
  # scale_color_manual(values="red", name="", labels=c("Year of completion"))+
  # facet_wrap(~Name, scales="free_y")+
  ylab("Density of observations")+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()


############

junk<-case_observation_join_annual2 %>%
  inner_join(
    case_df %>% select(Name, YearCompleted)
  ) %>%
  mutate(YearT= Year-YearCompleted)
junk2<-req_segs_mets_df_summary %>%
  inner_join(
    case_df %>% select(Name, YearCompleted)
  ) %>%
  mutate(YearT= Year-YearCompleted)
ggplot(data=junk %>% 
         filter(Group=="All taxa", Metric=="n_observers",
                !Name %in% c("Chollas Creek Dechannelization", "OCWD Weir Ponds")),
       aes(x=YearT, y=MetricResult_Dens))+
  geom_ribbon(data=junk2 %>% 
                filter(Group=="All taxa", Metric=="n_observers",
                       !Name %in% c("Chollas Creek Dechannelization", "OCWD Weir Ponds")),
              aes(ymin=q25, ymax=q75), y=NA, fill="#1f78b450")+
  geom_path(data=junk2 %>% 
              filter(Group=="All taxa", Metric=="n_observers",
                     !Name %in% c("Chollas Creek Dechannelization", "OCWD Weir Ponds")),
            aes(y=q50), color="#1f78b4", linetype="solid")+
  geom_path()+
  xlab("Years before/after project completion")+
  ylab("")+
  geom_vline(xintercept=0, linetype="dashed", color="red")+
  scale_y_continuous(limits=c(0, NA))+
  facet_wrap(~Name, scales="free_y")

#########
#####More broken-down Paradise Creek figures#####
pre_restoration_plot<-
  ggplot(data=case_observation_join_annual2 %>%
           filter(Name %in% c("Paradise Creek"),
                  Year<=2017) %>%
           filter(Group=="All taxa", Metric=="n_observers"),
         aes(x=Year, y=MetricResult_Dens)) +
  # geom_path(data=req_segs_mets_df_summary%>%
  #             filter(Name %in% c("Paradise Creek")) %>%
  #             filter(Group=="All taxa", Metric=="n_observers") %>%
  #             mutate(MetricResult_Dens=NA_real_),
  #           aes(y=q50), color="blue", linetype="dotted")+
  # geom_ribbon(data=req_segs_mets_df_summary%>%
  #               filter(Name %in% c("Paradise Creek")) %>%
  #               filter(Group=="All taxa", Metric=="n_observers") %>%
  #               mutate(MetricResult_Dens=NA_real_),
  #             aes(ymin=0, ymax=q75), alpha=0.75, fill="lightblue")+
  geom_path() +
  # geom_vline(data=case_sf  %>%               as_tibble() %>%
  # filter(Name %in% c( "Paradise Creek")),
  # aes(xintercept=YearCompleted), color="red", linetype="dashed"  )+
  # scale_color_manual(values="red", name="", labels=c("Year of completion"))+
  facet_wrap(~Name, scales="free_y")+
  scale_y_continuous(name="# of observers", limits=c(0,150))+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()

ggsave(pre_restoration_plot, filename="NotForGit/Step5_1/paradise_1.png",
       width=4, height=6)

ggsave(pre_restoration_plot+
         geom_vline(data=case_sf  %>%               as_tibble() %>%
                      filter(Name %in% c( "Paradise Creek")),
                    aes(xintercept=YearCompleted), color="red", linetype="dashed"  )+
         scale_color_manual(values="red", name="", labels=c("Year of completion"))
       , filename="NotForGit/Step5_1/paradise_1.5.png",
       width=4, height=6)

pre_restoration_plot2<-
  ggplot(data=case_observation_join_annual2 %>%
           filter(Name %in% c("Paradise Creek")) %>%
           filter(Group=="All taxa", Metric=="n_observers"),
         aes(x=Year, y=MetricResult_Dens)) +
  # geom_path(data=req_segs_mets_df_summary%>%
  #             filter(Name %in% c("Paradise Creek")) %>%
  #             filter(Group=="All taxa", Metric=="n_observers") %>%
  #             mutate(MetricResult_Dens=NA_real_),
  #           aes(y=q50), color="blue", linetype="dotted")+
  # geom_ribbon(data=req_segs_mets_df_summary%>%
  #               filter(Name %in% c("Paradise Creek")) %>%
  #               filter(Group=="All taxa", Metric=="n_observers") %>%
  #               mutate(MetricResult_Dens=NA_real_),
  #             aes(ymin=0, ymax=q75), alpha=0.75, fill="lightblue")+
  geom_path() +
  geom_vline(data=case_sf  %>%               as_tibble() %>%
               filter(Name %in% c( "Paradise Creek")),
             aes(xintercept=YearCompleted), color="red", linetype="dashed"  )+
  scale_color_manual(values="red", name="", labels=c("Year of completion"))+
  facet_wrap(~Name, scales="free_y")+
  scale_y_continuous(name="# of observers", limits=c(0,150))+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()

ggsave(pre_restoration_plot2, filename="NotForGit/Step5_1/paradise_2.png",
       width=4, height=6)

pre_restoration_plot3<-
  ggplot(data=case_observation_join_annual2 %>%
           filter(Name %in% c("Paradise Creek")) %>%
           filter(Group=="All taxa", Metric=="n_observers"),
         aes(x=Year, y=MetricResult_Dens)) +
  geom_path(data=req_segs_mets_df_summary%>%
              filter(Name %in% c("Paradise Creek")) %>%
              filter(Group=="All taxa", Metric=="n_observers") %>%
              mutate(MetricResult_Dens=NA_real_),
            aes(y=q50), color="blue", linetype="dotted")+
  geom_ribbon(data=req_segs_mets_df_summary%>%
                filter(Name %in% c("Paradise Creek")) %>%
                filter(Group=="All taxa", Metric=="n_observers") %>%
                mutate(MetricResult_Dens=NA_real_),
              aes(ymin=0, ymax=q75), alpha=0.75, fill="lightblue")+
  geom_path() +
  geom_vline(data=case_sf  %>%               as_tibble() %>%
               filter(Name %in% c( "Paradise Creek")),
             aes(xintercept=YearCompleted), color="red", linetype="dashed"  )+
  scale_color_manual(values="red", name="", labels=c("Year of completion"))+
  facet_wrap(~Name, scales="free_y")+
  scale_y_continuous(name="# of observers", limits=c(0,150))+
  scale_x_continuous(limits=c(2009.5, 2025.5))+
  theme_bw()

ggsave(pre_restoration_plot3, filename="NotForGit/Step5_1/paradise_3.png",
       width=4, height=6)

#####Table 5#####
#load thresholds
thresholds_df<-read_csv("NotForGit/Step3_0/NBR_empirical_thresholds.csv") %>%
  mutate(HighPop=`Above 75th precentile`)
case_df %>%
  filter(Name %in% sel_proj) %>%
  select(Name, popdens_5k) %>%
  inner_join(case_observation_join_summary2 %>%
               filter(Name %in% sel_proj,
                      Group=="All taxa",
                      Metric=="n_observers") %>%
               select(Name, MetricResult_Dens)
  ) %>%
  mutate(NBR_class=case_when(
    MetricResult_Dens>=thresholds_df$HighPop[thresholds_df$NBR_class.n==0.9]~"Very high",
    MetricResult_Dens>=thresholds_df$HighPop[thresholds_df$NBR_class.n==0.75]~"High",
    MetricResult_Dens>=thresholds_df$HighPop[thresholds_df$NBR_class.n==0.25]~"Moderate",
    MetricResult_Dens>0~"Low",
    T~"No NBR")) %>%
  arrange(Name) %>%
  write_clip()
