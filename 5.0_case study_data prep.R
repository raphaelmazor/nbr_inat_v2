library(tidyverse)
library(sf)


#####Import shapefiles#####
smc_sheds_sf_0<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_sheds_sf<-  st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2026/SMC_Sheds_Merge.shp")%>%
  st_transform(crs=st_crs(smc_sheds_sf_0))
rm(smc_sheds_sf_0)
smc_huc08_sf<-st_read("Shapefiles/HUCs/HUC08_SMCv2.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf)) 
smc_huc12_sf<-st_read("Shapefiles/HUCs/HUC12_SMC.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf)) 

huc12_dat<-read_csv("NotForGit/Step3_0/smc_streams_metrics_huc.csv")
huc12ave_dat<-read_csv("NotForGit/Step3_0/smc_streams_metrics_huc_average.csv")
all_metrics<-read_csv("NotForGit/Step3_0/all_mets_df_sub.csv")

smc_case_sf<-
  # st_read("Shapefiles/CaseStudies/Misc case studies/misc_case_studies_01132026_2.shp") %>%
  st_read("C:/Users/Raphaelm/Documents/ArcGIS/Projects/NBR/NBR.gdb",
          # layer='NBR_case_studies') %>%
          layer='NBR_case_studies_Merge') %>%
  st_transform(crs=st_crs(smc_sheds_sf)) %>%
  st_join(
    smc_sheds_sf %>%
      select(SMCShed=SMC_Name)) %>%
  mutate(SMCShed=case_when(Name=="Machado Lake"~"Dominguez Channel",
                           T~SMCShed)) %>%
  st_join(smc_huc08_sf %>%st_zm(drop=T),largest=T)%>%
  st_join(smc_huc12_sf %>%st_zm(drop=T) %>% select(HUC12, HUC12_name=NAME),largest=T)


# ggplot()+
#   geom_sf(data=smc_sheds_sf)+geom_sf(data=smc_case_sf, fill="pink", color="green")

smc_case_sf$case_peri_km=as.numeric(st_perimeter(smc_case_sf)/1000)
smc_case_sf$case_area_km2=as.numeric(st_area(smc_case_sf)/1000000)

# ggplot()+geom_sf(data=smc_case_sf)

#####Buffer case studies#####
#Maybe just use the case study footprint alone
# smc_case100m_sf<-  smc_case_sf %>%
#   st_zm(drop = T) %>%
#   st_buffer(dist=100) 
# smc_case100m_sf$buff_area_km2=as.numeric(st_area(smc_case100m_sf)/1000000)
# smc_case100m_sf$buffonly_area_km2 = smc_case100m_sf$buff_area_km2 - smc_case100m_sf$case_area_km2
# 
# ggplot()+geom_sf(data=smc_case100m_sf)+geom_sf(data=smc_case_sf)
#####Import iNat observations#####
all_taxa_df_orig<-read_csv("NotForGit/Step1_1/all_taxa_df.csv")
all_taxa_df<-all_taxa_df_orig %>%
  #Add aggregate groups
  # Group %in% c("All taxa","All Birds","All Invertebrates","All Herps", "Fish")
  bind_rows(
    all_taxa_df_orig %>% mutate(Group="All taxa"),
    all_taxa_df_orig %>% filter(Group2 =="Birds") %>% mutate(Group="All Birds"),
    all_taxa_df_orig %>% filter(Group2 =="Invertebrates") %>% mutate(Group="All Invertebrates"),
    all_taxa_df_orig %>% filter(Group2 =="Herps") %>% mutate(Group="All Herps"),
  ) %>%
  mutate(Obs_Date=paste(user_id, Date), sep="_")

all_taxa_sf<-all_taxa_df %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326) %>%
  st_transform(crs=st_crs(smc_sheds_sf))

#####Associate iNat observations#####

case_observation_join<-
  # smc_case100m_sf %>%
  smc_case_sf %>%
  st_join(all_taxa_sf, join=st_contains, left=FALSE)



#####Calculate metrics#####

case_observation_join_summary<-
  case_observation_join %>%
  group_by(Group,
           SMCShed, HUC8_simp, HUC12, HUC12_name,
           Name, case_peri_km, case_area_km2,
           # buff_area_km2, buffonly_area_km2
  ) %>%
  summarize(
    n_observations = sum(!is.na(user_id)),     n_observers = user_id %>% n_distinct(na.rm=T),    n_observer_dates = Obs_Date %>% n_distinct(na.rm=T),
    n_observations_m01 = sum(!is.na(user_id[Month==1])),      n_observers_m01 = user_id[Month==1] %>% n_distinct(na.rm=T),     n_observer_dates_m01 = Obs_Date[Month==1] %>% n_distinct(na.rm=T),
    n_observations_m02 = sum(!is.na(user_id[Month==2])),      n_observers_m02 = user_id[Month==2] %>% n_distinct(na.rm=T),     n_observer_dates_m02 = Obs_Date[Month==2] %>% n_distinct(na.rm=T),
    n_observations_m03 = sum(!is.na(user_id[Month==3])),      n_observers_m03 = user_id[Month==3] %>% n_distinct(na.rm=T),     n_observer_dates_m03 = Obs_Date[Month==3] %>% n_distinct(na.rm=T),
    n_observations_m04 = sum(!is.na(user_id[Month==4])),      n_observers_m04 = user_id[Month==4] %>% n_distinct(na.rm=T),     n_observer_dates_m04 = Obs_Date[Month==4] %>% n_distinct(na.rm=T),
    n_observations_m05 = sum(!is.na(user_id[Month==5])),      n_observers_m05 = user_id[Month==5] %>% n_distinct(na.rm=T),     n_observer_dates_m05 = Obs_Date[Month==5] %>% n_distinct(na.rm=T),
    n_observations_m06 = sum(!is.na(user_id[Month==6])),      n_observers_m06 = user_id[Month==6] %>% n_distinct(na.rm=T),     n_observer_dates_m06 = Obs_Date[Month==6] %>% n_distinct(na.rm=T),
    n_observations_m07 = sum(!is.na(user_id[Month==7])),      n_observers_m07 = user_id[Month==7] %>% n_distinct(na.rm=T),     n_observer_dates_m07 = Obs_Date[Month==7] %>% n_distinct(na.rm=T),
    n_observations_m08 = sum(!is.na(user_id[Month==8])),      n_observers_m08 = user_id[Month==8] %>% n_distinct(na.rm=T),     n_observer_dates_m08 = Obs_Date[Month==8] %>% n_distinct(na.rm=T),
    n_observations_m09 = sum(!is.na(user_id[Month==9])),      n_observers_m09 = user_id[Month==9] %>% n_distinct(na.rm=T),     n_observer_dates_m09 = Obs_Date[Month==9] %>% n_distinct(na.rm=T),
    n_observations_m10 = sum(!is.na(user_id[Month==10])),     n_observers_m10 = user_id[Month==10] %>% n_distinct(na.rm=T),    n_observer_dates_m10 = Obs_Date[Month==10] %>% n_distinct(na.rm=T),
    n_observations_m11 = sum(!is.na(user_id[Month==11])),     n_observers_m11 = user_id[Month==11] %>% n_distinct(na.rm=T),    n_observer_dates_m11 = Obs_Date[Month==11] %>% n_distinct(na.rm=T),
    n_observations_m12 = sum(!is.na(user_id[Month==12])),     n_observers_m12 = user_id[Month==12] %>% n_distinct(na.rm=T),    n_observer_dates_m12 = Obs_Date[Month==12] %>% n_distinct(na.rm=T),
  ) %>%
  ungroup() %>%
  pivot_longer(cols=starts_with("n_obs"), 
               names_to = "Metric", 
               values_to = "MetricResult_Raw") %>%
  mutate(MetricResult_Dens=MetricResult_Raw/case_area_km2)




# taxa<-all_taxa_df %>% select(Group) %>% distinct()
taxa<-tibble(Group=c("All taxa","All Birds","All Herps","Fish","All Invertebrates"))
all_cases<-smc_case_sf %>% as_tibble() %>% 
  # select(Name, SMCShed, HUC8_simp, HUC12, HUC12_name, case_peri_km, case_area_km2)
  select(Name, HUC12, HUC12_name)
all_metrics<-case_observation_join_summary %>% as_tibble() %>%  select(Metric) %>% distinct() %>% arrange(Metric)

case_observation_join_summary2<-
  crossing(taxa, all_metrics, all_cases) %>% left_join(
    case_observation_join_summary %>% 
      as_tibble() %>%
      select(-starts_with("Shape"))
  ) %>%
  mutate(MetricResult_Raw = case_when(is.na(MetricResult_Raw)~0, 
                                      T~MetricResult_Raw),
         MetricResult_Dens = case_when(is.na(MetricResult_Dens)~0, 
                                       T~MetricResult_Dens)
  )
# case_observation_join_summary2 %>% skimr::skim()
# case_observation_join_summary2 %>% count(Name) 

case_plot_dat<-case_observation_join_summary2 %>%
  as_tibble() %>%
  filter(Group=="All taxa",Metric=="n_observers")


huc12_plot_dat<-
  case_plot_dat %>% 
  select(HUC12, Name) %>%
  inner_join(
    huc12_dat %>%
      mutate(HUC12=as.character(HUC12)) )

# huc12_plot_dat %>% skimr::skim()

ggplot(data=huc12_plot_dat,
       aes(x=Name, y=MetricResult_Dens))+
  geom_boxplot()+
  geom_point(data=case_plot_dat, shape=21, fill="chartreuse")+
  scale_y_continuous(trans="sqrt")+
  coord_flip()

case_observation_join_summary2 %>% filter(is.na(MetricResult_Dens))

write_csv(case_observation_join_summary2 ,
          "NotForGit/Step5_0/case_observation_join_summary2.csv")


################################
#Annual analysis

case_observation_join_annual<-  case_observation_join %>%
  group_by(Group,  
           SMCShed, HUC8_simp, HUC12, HUC12_name,
           Name, Year,
           # Name_Year, 
           case_peri_km, case_area_km2,
           # buff_area_km2, buffonly_area_km2
  ) %>%
  summarize(
    n_observations = sum(!is.na(user_id)),     n_observers = user_id %>% n_distinct(na.rm=T),    n_observer_dates = Obs_Date %>% n_distinct(na.rm=T),
    n_observations_m01 = sum(!is.na(user_id[Month==1])),      n_observers_m01 = user_id[Month==1] %>% n_distinct(na.rm=T),     n_observer_dates_m01 = Obs_Date[Month==1] %>% n_distinct(na.rm=T),
    n_observations_m02 = sum(!is.na(user_id[Month==2])),      n_observers_m02 = user_id[Month==2] %>% n_distinct(na.rm=T),     n_observer_dates_m02 = Obs_Date[Month==2] %>% n_distinct(na.rm=T),
    n_observations_m03 = sum(!is.na(user_id[Month==3])),      n_observers_m03 = user_id[Month==3] %>% n_distinct(na.rm=T),     n_observer_dates_m03 = Obs_Date[Month==3] %>% n_distinct(na.rm=T),
    n_observations_m04 = sum(!is.na(user_id[Month==4])),      n_observers_m04 = user_id[Month==4] %>% n_distinct(na.rm=T),     n_observer_dates_m04 = Obs_Date[Month==4] %>% n_distinct(na.rm=T),
    n_observations_m05 = sum(!is.na(user_id[Month==5])),      n_observers_m05 = user_id[Month==5] %>% n_distinct(na.rm=T),     n_observer_dates_m05 = Obs_Date[Month==5] %>% n_distinct(na.rm=T),
    n_observations_m06 = sum(!is.na(user_id[Month==6])),      n_observers_m06 = user_id[Month==6] %>% n_distinct(na.rm=T),     n_observer_dates_m06 = Obs_Date[Month==6] %>% n_distinct(na.rm=T),
    n_observations_m07 = sum(!is.na(user_id[Month==7])),      n_observers_m07 = user_id[Month==7] %>% n_distinct(na.rm=T),     n_observer_dates_m07 = Obs_Date[Month==7] %>% n_distinct(na.rm=T),
    n_observations_m08 = sum(!is.na(user_id[Month==8])),      n_observers_m08 = user_id[Month==8] %>% n_distinct(na.rm=T),     n_observer_dates_m08 = Obs_Date[Month==8] %>% n_distinct(na.rm=T),
    n_observations_m09 = sum(!is.na(user_id[Month==9])),      n_observers_m09 = user_id[Month==9] %>% n_distinct(na.rm=T),     n_observer_dates_m09 = Obs_Date[Month==9] %>% n_distinct(na.rm=T),
    n_observations_m10 = sum(!is.na(user_id[Month==10])),     n_observers_m10 = user_id[Month==10] %>% n_distinct(na.rm=T),    n_observer_dates_m10 = Obs_Date[Month==10] %>% n_distinct(na.rm=T),
    n_observations_m11 = sum(!is.na(user_id[Month==11])),     n_observers_m11 = user_id[Month==11] %>% n_distinct(na.rm=T),    n_observer_dates_m11 = Obs_Date[Month==11] %>% n_distinct(na.rm=T),
    n_observations_m12 = sum(!is.na(user_id[Month==12])),     n_observers_m12 = user_id[Month==12] %>% n_distinct(na.rm=T),    n_observer_dates_m12 = Obs_Date[Month==12] %>% n_distinct(na.rm=T),
  ) %>%
  ungroup() %>%
  pivot_longer(cols=starts_with("n_obs"), 
               names_to = "Metric", 
               values_to = "MetricResult_Raw") %>%
  mutate(MetricResult_Dens=MetricResult_Raw/case_area_km2)


case_observation_join_annual2<-
  crossing(taxa, all_metrics, all_cases, Year=2010:2025) %>% 
  left_join(
    case_observation_join_annual %>% as_tibble()
  ) %>%
  mutate(MetricResult_Raw = case_when(is.na(MetricResult_Raw)~0, 
                                      T~MetricResult_Raw),
         MetricResult_Dens = case_when(is.na(MetricResult_Dens)~0, 
                                       T~MetricResult_Dens)
  )
#
annual_plot_dat<-case_observation_join_annual2 %>%
  filter(Group=="All taxa",Metric=="n_observers")

ggplot(data=annual_plot_dat, 
       aes(x=Year, y=MetricResult_Dens, color=Name))+
  geom_path()
# geom_smooth()

write_csv(case_observation_join_annual2 ,
          "NotForGit/Step5_0/case_observation_join_annual2.csv")

#Bring in annual metrics for segments in the HUCs

junk<-readRDS("NotForGit/Step2_1/stream_observation_join_summary_annual.RDS") %>% 
  as_tibble() 

junk2<-junk %>%
  filter(Group=="All taxa") %>%
  select(-geometry)

huc_seg_xwalk<-huc12_plot_dat %>%
  select(HUC12, Segment_ID) %>%
  distinct()
case_huc12_xwalk<-case_plot_dat %>% select(Name, HUC12) %>% distinct()

junk3<- junk2 %>%
  inner_join(huc_seg_xwalk) %>%
  inner_join(case_huc12_xwalk) 


spine_df<-  crossing(Group=c("All taxa"),#, "All Birds","All herps", "Fish","All Invertebrates"),
                     all_metrics, 
                     all_cases, 
                     Year=2010:2025)

annual_seg_data<-spine_df %>%
  left_join(junk3) %>%
  mutate(MetricResult_Raw = case_when(is.na(MetricResult_Raw)~0, 
                                      T~MetricResult_Raw),
         MetricResult_Dens = case_when(is.na(MetricResult_Dens)~0, 
                                       T~MetricResult_Dens)
  )

annual_seg_data_summary<-annual_seg_data %>%
  group_by(Group, Metric, HUC12, Name, Year) %>%
  summarize(
    q10=quantile(MetricResult_Dens, 0.1),
    q25=quantile(MetricResult_Dens, 0.25),
    q50=quantile(MetricResult_Dens, 0.5),
    q75=quantile(MetricResult_Dens, 0.75),
    q90=quantile(MetricResult_Dens, 0.90)
  ) %>%
  ungroup()



ggplot(data=annual_seg_data_summary %>%
         filter(Metric=="n_observers",
                Group=="All taxa"), 
       aes(x=Year, y=q50))+
  geom_ribbon(aes(ymin=q10, ymax=q90), alpha=0.25)+
  geom_ribbon(aes(ymin=q25, ymax=q75), alpha=0.25)+
  geom_path(linetype="dotted")+
  geom_path(data=annual_plot_dat %>%
              filter(Metric=="n_observers",
                     Group=="All taxa"), 
            aes(y=MetricResult_Dens),
            linetype="solid", color="blue")+
  facet_wrap(~Name, scales="free_y")+
  geom_vline(data=
               smc_case_sf %>% as_tibble(), aes(xintercept=YearCompleted),
             linetype="dashed", color="red")+
  coord_cartesian(xlim = c(2009.5, 2025.5))+
  ylab("NBR activity")


