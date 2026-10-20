library(tidyverse)
library(sf)

############Import shapefiles############
smc_sheds_sf_0<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_sheds_sf<-  st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2026/SMC_Sheds_Merge.shp")%>%
  st_transform(crs=st_crs(smc_sheds_sf_0))
my_bb<-st_bbox(smc_sheds_sf)
rm(smc_sheds_sf_0)
smc_flowlines100m_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")
# smc_wb100m_sf<-readRDS("NotForGit/Step1_2/smc_wb100m_sf.RDS")
smc_wb_sf<-readRDS("NotForGit/Step1_2/smc_wb_sf_modified.RDS")
smc_flow_wb_100m_sf<-readRDS("NotForGit/Step1_2/smc_flow_wb_100m_sf.RDS")

mx_sf<-st_read("S:/Mexico/BajaMexico.shp") %>%st_transform(crs=st_crs(smc_sheds_sf))
smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
counties_sf<-st_read("Shapefiles/SoCalCounties/SoCal_Counties.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))

############Import observations############
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
  #
  mutate(Obs_Date=paste(user_id, Date), sep="_") %>%
  group_by(user_id) %>%
  mutate(n_userobs=length(user_id),
         n_userdates=Obs_Date %>% unique() %>% length(),
         n_years_active=Year %>% unique() %>% length(),
         year_span=max(Year)-min(Year)+1,
         year_of_observation=Year-min(Year)) %>%
  ungroup() %>%
  mutate(UserClass = case_when(n_userdates<=1~"One day",
                               n_userdates<=10~"2 to 10 days",
                               # n_userdates<=50~"11 to 50 days",
                               n_userdates<=100~"11 to 100 days",
                               n_userdates<=1000~"101 to 1000 days",
                               T~">1000 days"))

all_taxa_df$UserClass=factor(all_taxa_df$UserClass, levels=c("One day",
                                                             "2 to 10 days",
                                                             # "11 to 50 days",
                                                             "11 to 100 days",
                                                             "101 to 1000 days",
                                                             ">1000 days"))

all_taxa_sf<-all_taxa_df %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326) %>%
  st_transform(crs=st_crs(smc_sheds_sf))

group_group2_df<-all_taxa_df %>% select(Group2, Group) %>% distinct()
write_csv(group_group2_df, "NotForGit/Step2_1/group_group2_df.csv")
#Plot everything to confirm

all_observations_map<-
  ggplot()+
  geom_sf(data=counties_sf, fill="gray75")+
  # geom_sf(data=smc_sheds_sf, fill="white")+
  # geom_sf(data=smc_flowlines_sf, color="blue")+
  # geom_sf(data=smc_wb_sf, fill="lightblue", color=NA)+
  # geom_sf(data=smc_flowlines_sf, aes(color=SMC_Shed))
  geom_sf(data=all_taxa_sf %>%
            filter(!Group %in% c("All taxa","All Birds","All Herps","All Invertebrates")),
          size=0.25, alpha=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray50")+
  theme_bw()+
  coord_sf(xlim=st_bbox(smc_sheds_sf)[c(1,3)],
           ylim=st_bbox(smc_sheds_sf)[c(2,4)])+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank())
# geom_sf(data=all_taxa_sf, size=0.5, alpha=0.25,
# aes(color=year_of_observation))+
# scale_color_viridis_c()
# facet_wrap(~Group)
ggsave(all_observations_map, filename="NotForGit/Step2_1/all_observations_map.png",
       width=6, height=4)

# all_observations_map+facet_wrap(~Group2)
my_bb2<-smc_sheds_sf %>% filter(SMC_Name=="Los Angeles") %>% st_bbox()
# ggplot()+geom_sf(data=smc_flow_wb_100m_sf)+coord_sf(xlim=my_bb2[c(1,3)], ylim=my_bb2[c(2,4)])

close_observations_sf<-all_taxa_sf %>%
  filter(!Group %in% c("All taxa","All Birds","All Herps","All Invertebrates")) %>%
  # st_filter(smc_flowlines100m_sf)
  st_filter(smc_flow_wb_100m_sf)

n_distinct(close_observations_sf$user_id)

close_observations_sf %>%
  as_tibble() %>%
  count(Group2) %>%
  mutate(Pct=n/nrow(close_observations_sf)) %>%
  arrange(-n)

taxa_counts_df<-close_observations_sf %>%
  as_tibble() %>%
  count(Group2, Group) %>%
  group_by(Group2) %>%
  mutate(GroupTotal=sum(n),
         Pct=n/GroupTotal) %>%
  ungroup() %>%
  arrange(-GroupTotal,-n) 
write_csv(taxa_counts_df, "NotForGit/Step2_1/taxa_counts_df.csv")

taxa_counts_df$Group.f=factor(taxa_counts_df$Group, levels=rev(taxa_counts_df$Group))


taxa_counts_labels=taxa_counts_df %>%
  filter(Group %in% c("Ducks","Herons","Gulls", "Shorebirds",
                      "Fish",
                      "Frogs","Turtles","Newts","Dragonflies","Crayfish"))

taxa_count_stacked_bar<-ggplot(taxa_counts_df, aes(x=Group2, y=100*n/GroupTotal, fill=Group.f ))+
  geom_bar(position=position_stack(), stat="identity",
           color="white")+
  geom_text(data=taxa_counts_labels,position=position_stack(), 
            aes(label=Group),
            vjust= 1.5,
            stat="identity")+
  scale_fill_discrete(name="Taxon")+
  xlab("Assemblage")+ylab("% of group total")+
  theme_bw()+
  theme(legend.position = "none")
ggsave(taxa_count_stacked_bar, filename="NotForGit/Step2_1/taxa_count_stacked_bar.png",
       height=4, width=5)

taxa_count_stacked_bar2<-ggplot(taxa_counts_df, aes(x=Group2, y=n, fill=Group.f ))+
  geom_bar(position=position_stack(), stat="identity",
           color="white")+
  scale_fill_discrete(name="Taxon")+
  xlab("Assemblage")+ylab("# observations")+
  theme_bw()+
  theme(legend.position = "none")
ggsave(taxa_count_stacked_bar2, filename="NotForGit/Step2_1/taxa_count_stacked_bar2.png",
       height=4, width=5)

close_observations_map<-
  all_observations_map + 
  geom_sf(data=close_observations_sf,
          color="red",
          size=0.25)

close_observations_map<-ggplot()+
  geom_sf(data=counties_sf, fill="white")+
  geom_sf(data=smc_sheds_sf)+
  # geom_sf(data=smc_flowlines100m_sf, fill="lightblue", color=NA)+
  # geom_sf(data=smc_flow_wb_100m_sf, fill="lightblue", color=NA)+
  geom_sf(data=smc_flowlines_sf, color="slateblue")+
  geom_sf(data=smc_wb_sf, color="slateblue", fill="lightblue")+
  geom_sf(data=all_taxa_sf , color="gray", alpha=0.5,
          size=0.25)+
  geom_sf(data=close_observations_sf,
          color="red",
          size=0.75)+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  # guides(color=guide_legend(override.aes=list(size=1.5)))+
  # facet_wrap(~Group2)+
  theme(axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank())


ggsave(close_observations_map, filename="NotForGit/Step2_1/close_observations_map.png",
       width=6, height=4)


la_shed_sf<-smc_sheds_sf %>% filter(SMC_Name=="Los Angeles")
ggsave(  all_observations_map + 
           geom_sf(data=smc_flowlines_sf, color="slateblue")+
           geom_sf(data=smc_wb_sf, color="slateblue", fill="lightblue")+
           geom_sf(data=close_observations_sf,
                   color="red",
                   size=0.25) +
         coord_sf(xlim=c(st_bbox(la_shed_sf)$xmin,
                         st_bbox(la_shed_sf)$xmax),
                  ylim=c(st_bbox(la_shed_sf)$ymin,
                         st_bbox(la_shed_sf)$ymax),
         ), filename="NotForGit/Step2_1/close_observations_map_closeup.png",
       width=6, height=4)

saveRDS(all_observations_map, "NotForGit/Step2_1/all_observations_map.RDS")
saveRDS(close_observations_map, "NotForGit/Step2_1/close_observations_map.RDS")

close_observations_closeup_map<-all_observations_map + 
  geom_sf(data=smc_flowlines_sf, color="slateblue")+
  geom_sf(data=close_observations_sf,
          color="red",
          size=0.25) +
  coord_sf(xlim=c(st_bbox(la_shed_sf)$xmin,
                  st_bbox(la_shed_sf)$xmax),
           ylim=c(st_bbox(la_shed_sf)$ymin,
                  st_bbox(la_shed_sf)$ymax))

saveRDS(close_observations_closeup_map, "NotForGit/Step2_1/close_observations_closeup_map.RDS")
close_observations_closeup_map<-readRDS("NotForGit/Step2_1/close_observations_closeup_map.RDS")
library(ggpubr)
queried_data_maps<-ggarrange(all_observations_map,close_observations_closeup_map,
                             labels="AUTO")
ggsave(queried_data_maps, filename="NotForGit/Step2_1/queried_data_maps.png",
       width=6.5, height=4
       )

########################Tally counts########################
# stream_observation_join<-
#   smc_flow_wb_100m_sf %>%
#   st_join(all_taxa_sf, join=st_contains, left=FALSE)

stream_observation_join<-
  smc_flowlines100m_sf %>%
  st_join(all_taxa_sf, join=st_contains, left=FALSE)
  # st_join(all_taxa_sf, join=st_contains, left=TRUE)


# wb_observation_join<-
#   smc_wb100m_sf %>%
#   st_join(all_taxa_sf, join=st_contains, left=FALSE)
# case_observation_join<-
#   smc_case100m_sf %>%
#   st_join(all_taxa_sf, join=st_contains, left=FALSE)

# group2group_xwalk<-stream_observation_join %>% select(Group, Group2) %>% distinct()

stream_observation_join_summary<-
  stream_observation_join %>%
  group_by(
    Group,
    # Group2, Group,
    SMCShed, TotLength_km, 
    COMID, lengthkm2,
    Segment_ID, SegLength_km,
    # Unit_ID, Unit_Length_km, Unit_Area_km2,
    buff_area_km2,# buffonly_area_km2
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
  mutate(MetricResult_Dens=MetricResult_Raw/buff_area_km2)


stream_observation_join_summary_annual<-
  stream_observation_join %>%
  mutate(Seg_Year=paste(Segment_ID, Year, sep="_")) %>%
  group_by(
    # Group2, Group,
    Group,
    SMCShed, TotLength_km, 
    COMID, lengthkm2, 
    Segment_ID, Seg_Year, Year, SegLength_km, buff_area_km2) %>%
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
  mutate(MetricResult_Dens=MetricResult_Raw/buff_area_km2)

saveRDS(stream_observation_join_summary_annual, "NotForGit/Step2_1/stream_observation_join_summary_annual.RDS")

####revisitors analysis####
revisits_df<-stream_observation_join %>%
  as_tibble() %>% select(-geometry) %>%
  group_by(user_id,
           Segment_ID) %>%
  summarize(
    n_dates= Date %>% n_distinct(na.rm=T),
    n_years= Year %>% n_distinct(na.rm=T)
  ) %>%
  ungroup() #%>%

revisitors_df<-revisits_df %>%
  filter(n_dates>1) %>%
  group_by(Segment_ID) %>%
  summarise(n_revisitors=user_id %>% n_distinct(na.rm=T)) %>%
  ungroup()

annual_revisitors_df<-revisits_df %>%
  filter(n_years>1) %>%
  group_by(Segment_ID) %>%
  summarise(n_annual_revisitors=user_id %>% n_distinct(na.rm=T)) %>%
  ungroup()

stream_revisitors_summary<-
  # stream_observation_join_summary %>%
  # select(-Group, -Group2, -Group_sci) %>%
  # distinct() %>%
  smc_flowlines100m_sf %>%
  left_join(revisitors_df) %>%
  left_join(annual_revisitors_df) %>%
  mutate(n_revisitors=replace_na(n_revisitors, 0),
         n_annual_revisitors=replace_na(n_annual_revisitors, 0))
write_csv(stream_revisitors_summary %>%
            as_tibble() %>%
            select(-geometry),
          "NotForGit/Step2_1/stream_revisitors_summary.csv")

ggplot()+
  geom_sf(data=counties_sf, fill="white")+
  geom_sf(data=stream_revisitors_summary %>%
            filter(n_annual_revisitors==0), color="gray", size=0.5, alpha=0.5)+
  geom_sf(data=stream_revisitors_summary %>%
            filter(n_annual_revisitors>0) %>%
            mutate(cat_annual_rev=case_when(#n_annual_revisitors==0~0,
              n_annual_revisitors<=5~5,
              n_annual_revisitors<=10~10,
              T~100)),
          aes(color=cat_annual_rev %>% as.factor()), size=1)+
  scale_color_viridis_d()


# 
# wb_observation_join_summary<-
#   wb_observation_join %>%
#   group_by(#Group2, Group, Group_sci,
#     Group,
#     SMCShed, 
#     Permanent_, wb_peri_km, wb_area_km2, buff_area_km2, buffonly_area_km2) %>%
#   summarize(
#     n_observations = sum(!is.na(user_id)),     n_observers = user_id %>% n_distinct(na.rm=T),    n_observer_dates = Obs_Date %>% n_distinct(na.rm=T),
#     n_observations_m01 = sum(!is.na(user_id[Month==1])),      n_observers_m01 = user_id[Month==1] %>% n_distinct(na.rm=T),     n_observer_dates_m01 = Obs_Date[Month==1] %>% n_distinct(na.rm=T),
#     n_observations_m02 = sum(!is.na(user_id[Month==2])),      n_observers_m02 = user_id[Month==2] %>% n_distinct(na.rm=T),     n_observer_dates_m02 = Obs_Date[Month==2] %>% n_distinct(na.rm=T),
#     n_observations_m03 = sum(!is.na(user_id[Month==3])),      n_observers_m03 = user_id[Month==3] %>% n_distinct(na.rm=T),     n_observer_dates_m03 = Obs_Date[Month==3] %>% n_distinct(na.rm=T),
#     n_observations_m04 = sum(!is.na(user_id[Month==4])),      n_observers_m04 = user_id[Month==4] %>% n_distinct(na.rm=T),     n_observer_dates_m04 = Obs_Date[Month==4] %>% n_distinct(na.rm=T),
#     n_observations_m05 = sum(!is.na(user_id[Month==5])),      n_observers_m05 = user_id[Month==5] %>% n_distinct(na.rm=T),     n_observer_dates_m05 = Obs_Date[Month==5] %>% n_distinct(na.rm=T),
#     n_observations_m06 = sum(!is.na(user_id[Month==6])),      n_observers_m06 = user_id[Month==6] %>% n_distinct(na.rm=T),     n_observer_dates_m06 = Obs_Date[Month==6] %>% n_distinct(na.rm=T),
#     n_observations_m07 = sum(!is.na(user_id[Month==7])),      n_observers_m07 = user_id[Month==7] %>% n_distinct(na.rm=T),     n_observer_dates_m07 = Obs_Date[Month==7] %>% n_distinct(na.rm=T),
#     n_observations_m08 = sum(!is.na(user_id[Month==8])),      n_observers_m08 = user_id[Month==8] %>% n_distinct(na.rm=T),     n_observer_dates_m08 = Obs_Date[Month==8] %>% n_distinct(na.rm=T),
#     n_observations_m09 = sum(!is.na(user_id[Month==9])),      n_observers_m09 = user_id[Month==9] %>% n_distinct(na.rm=T),     n_observer_dates_m09 = Obs_Date[Month==9] %>% n_distinct(na.rm=T),
#     n_observations_m10 = sum(!is.na(user_id[Month==10])),     n_observers_m10 = user_id[Month==10] %>% n_distinct(na.rm=T),    n_observer_dates_m10 = Obs_Date[Month==10] %>% n_distinct(na.rm=T),
#     n_observations_m11 = sum(!is.na(user_id[Month==11])),     n_observers_m11 = user_id[Month==11] %>% n_distinct(na.rm=T),    n_observer_dates_m11 = Obs_Date[Month==11] %>% n_distinct(na.rm=T),
#     n_observations_m12 = sum(!is.na(user_id[Month==12])),     n_observers_m12 = user_id[Month==12] %>% n_distinct(na.rm=T),    n_observer_dates_m12 = Obs_Date[Month==12] %>% n_distinct(na.rm=T),
#   ) %>%
#   ungroup() %>%
#   pivot_longer(cols=starts_with("n_obs"), 
#                names_to = "Metric", 
#                values_to = "MetricResult_Raw") %>%
#   mutate(MetricResult_Dens=MetricResult_Raw/buffonly_area_km2)
# 
# case_observation_join_summary<-
#   case_observation_join %>%
#   group_by(Group2, Group, Group_sci,
#            SMCShed, 
#            Name, case_peri_km, case_area_km2, buff_area_km2, buffonly_area_km2) %>%
#   summarize(
#     n_observations = sum(!is.na(user_id)),     n_observers = user_id %>% n_distinct(na.rm=T),    n_observer_dates = Obs_Date %>% n_distinct(na.rm=T),
#     n_observations_m01 = sum(!is.na(user_id[Month==1])),      n_observers_m01 = user_id[Month==1] %>% n_distinct(na.rm=T),     n_observer_dates_m01 = Obs_Date[Month==1] %>% n_distinct(na.rm=T),
#     n_observations_m02 = sum(!is.na(user_id[Month==2])),      n_observers_m02 = user_id[Month==2] %>% n_distinct(na.rm=T),     n_observer_dates_m02 = Obs_Date[Month==2] %>% n_distinct(na.rm=T),
#     n_observations_m03 = sum(!is.na(user_id[Month==3])),      n_observers_m03 = user_id[Month==3] %>% n_distinct(na.rm=T),     n_observer_dates_m03 = Obs_Date[Month==3] %>% n_distinct(na.rm=T),
#     n_observations_m04 = sum(!is.na(user_id[Month==4])),      n_observers_m04 = user_id[Month==4] %>% n_distinct(na.rm=T),     n_observer_dates_m04 = Obs_Date[Month==4] %>% n_distinct(na.rm=T),
#     n_observations_m05 = sum(!is.na(user_id[Month==5])),      n_observers_m05 = user_id[Month==5] %>% n_distinct(na.rm=T),     n_observer_dates_m05 = Obs_Date[Month==5] %>% n_distinct(na.rm=T),
#     n_observations_m06 = sum(!is.na(user_id[Month==6])),      n_observers_m06 = user_id[Month==6] %>% n_distinct(na.rm=T),     n_observer_dates_m06 = Obs_Date[Month==6] %>% n_distinct(na.rm=T),
#     n_observations_m07 = sum(!is.na(user_id[Month==7])),      n_observers_m07 = user_id[Month==7] %>% n_distinct(na.rm=T),     n_observer_dates_m07 = Obs_Date[Month==7] %>% n_distinct(na.rm=T),
#     n_observations_m08 = sum(!is.na(user_id[Month==8])),      n_observers_m08 = user_id[Month==8] %>% n_distinct(na.rm=T),     n_observer_dates_m08 = Obs_Date[Month==8] %>% n_distinct(na.rm=T),
#     n_observations_m09 = sum(!is.na(user_id[Month==9])),      n_observers_m09 = user_id[Month==9] %>% n_distinct(na.rm=T),     n_observer_dates_m09 = Obs_Date[Month==9] %>% n_distinct(na.rm=T),
#     n_observations_m10 = sum(!is.na(user_id[Month==10])),     n_observers_m10 = user_id[Month==10] %>% n_distinct(na.rm=T),    n_observer_dates_m10 = Obs_Date[Month==10] %>% n_distinct(na.rm=T),
#     n_observations_m11 = sum(!is.na(user_id[Month==11])),     n_observers_m11 = user_id[Month==11] %>% n_distinct(na.rm=T),    n_observer_dates_m11 = Obs_Date[Month==11] %>% n_distinct(na.rm=T),
#     n_observations_m12 = sum(!is.na(user_id[Month==12])),     n_observers_m12 = user_id[Month==12] %>% n_distinct(na.rm=T),    n_observer_dates_m12 = Obs_Date[Month==12] %>% n_distinct(na.rm=T),
#   ) %>%
#   ungroup() %>%
#   pivot_longer(cols=starts_with("n_obs"), 
#                names_to = "Metric", 
#                values_to = "MetricResult_Raw") %>%
#   mutate(MetricResult_Dens=MetricResult_Raw/buffonly_area_km2)
# 


all_metrics = stream_observation_join_summary %>%
  as_tibble() %>%
  select(Metric) %>%
  distinct()
write_csv(all_metrics, "NotForGit/Step2_1/all_metrics.csv")



#####Make a "spine" of all comids and taxa#####
# taxa<-all_taxa_df %>% select(Group, Group2, Group_sci) %>% distinct() #
taxa<-all_taxa_df %>% select(Group) %>% distinct() #
all_streams<-#smc_flowlines_sf %>% as_tibble() %>%   select(COMID, Segment_ID) %>% distinct()
  smc_flowlines100m_sf %>% as_tibble() %>%   select(SMCShed,   TotLength_km,    COMID, lengthkm2, Segment_ID, SegLength_km, buff_area_km2 ) %>% distinct()
# all_wbs<-#smc_wb100m_sf %>% as_tibble() %>% select(Permanent_)  %>% distinct( )
#   smc_wb100m_sf %>% as_tibble() %>% select(Permanent_, wb_peri_km, wb_area_km2, buff_area_km2, buffonly_area_km2)  %>% distinct( )
# all_cases<-smc_case100m_sf %>% as_tibble() %>% select(Name, SMCShed, case_peri_km, case_area_km2, buff_area_km2, buffonly_area_km2)  %>% distinct( )

# all_streams_taxa<-all_streams %>% crossing(taxa)
# all_wb_taxa<-all_wbs %>% crossing(taxa)
# all_cases_taxa<-all_cases %>% crossing(taxa)


stream_observation_join_summary2<-
  all_streams %>%
  crossing(taxa) %>%
  crossing(all_metrics) %>%
  left_join(
    stream_observation_join_summary %>%
      as_tibble() %>%
      select(Group, 
             Group2, 
             COMID, Segment_ID, starts_with("Metric"))
  ) %>%
  mutate(#MetricResult_Raw=replace_na(0),
    #MetricResult_Dens=replace_na(0)
    MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
    MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens),
  )

stream_observation_join_summary2$MetricResult_Raw %>% is.na() %>% sum()


# #This code is too big to run. Skip for now
# stream_observation_join_summary2_annual<-
#   all_streams %>%
#   # crossing(taxa, all_metrics, Year=2010:2025) %>%
#   crossing(Group=c("All taxa","All Birds","All Herps","Fish","Invertebrates"), all_metrics, Year=2010:2025) %>%
#   left_join(
#     stream_observation_join_summary_annual %>%
#       as_tibble() %>%
#       select(Group, Year,
#              # Group2, 
#              COMID, Segment_ID, starts_with("Metric"))
#   ) %>%
#   mutate(#MetricResult_Raw=replace_na(0),
#     #MetricResult_Dens=replace_na(0)
#     MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
#     MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens),
#   )
# 

# wb_observation_join_summary2<-
#   all_wbs %>%
#   crossing(taxa) %>%
#   crossing(all_metrics) %>%
#   left_join(
#     wb_observation_join_summary %>%
#       as_tibble() %>%
#       select(Group, #Group2, 
#              Permanent_,  
#              starts_with("Metric")) 
#   ) %>%
#   # mutate(MetricResult_Raw=replace_na(0),
#   # MetricResult_Dens=replace_na(0))
#   mutate(#MetricResult_Raw=replace_na(0),
#     #MetricResult_Dens=replace_na(0)
#     MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
#     MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens),
#   )

# case_observation_join_summary2<-
#   all_cases %>%
#   crossing(taxa) %>%
#   crossing(all_metrics) %>%
#   left_join(
#     case_observation_join_summary %>%
#       as_tibble() %>%
#       select(Group, 
#              # Group2, 
#              Name,  
#              starts_with("Metric")) 
#   ) %>%
#   # mutate(MetricResult_Raw=replace_na(0),
#   # MetricResult_Dens=replace_na(0))
#   mutate(#MetricResult_Raw=replace_na(0),
#     #MetricResult_Dens=replace_na(0)
#     MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
#     MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens),
#   )
#Confirm that no COMIDs were lost
setdiff(stream_observation_join_summary$COMID, smc_flowlines_sf$COMID)
setdiff(smc_flowlines_sf$COMID, stream_observation_join_summary$COMID) #These comids had no observations within the buffer
setdiff(stream_observation_join_summary$Segment_ID, smc_flowlines_sf$Segment_ID)
setdiff(smc_flowlines_sf$Segment_ID, stream_observation_join_summary$Segment_ID) #These comids had no observations within the buffer


#Save
saveRDS(stream_observation_join_summary,"NotForGit/Step2_1/stream_observation_join_summary.RDS")
# saveRDS(stream_observation_join_summary2_annual,"NotForGit/Step2_1/stream_observation_join_summary2_annual.RDS")
# saveRDS(wb_observation_join_summary,"NotForGit/Step2_1/wb_observation_join_summary.RDS")
# saveRDS(case_observation_join_summary,"NotForGit/Step2_1/case_observation_join_summary.RDS")


##########Shortcut#########
#Open
stream_observation_join_summary<-readRDS("NotForGit/Step2_1/stream_observation_join_summary.RDS")
wb_observation_join_summary<-readRDS("NotForGit/Step2_1/wb_observation_join_summary.RDS")
case_observation_join_summary<-readRDS("NotForGit/Step2_1/case_observation_join_summary.RDS")



# all_streams %>%
#   crossing(taxa) %>%
#   crossing(all_metrics)

stream_observation_join_summary_df<-all_streams %>%
  crossing(taxa) %>%
  crossing(all_metrics) %>%
  left_join(stream_observation_join_summary %>%
              as_tibble() %>%
              select(-geometry)) %>%
  mutate(MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
         MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens))

# stream_observation_join_summary_df$MetricResult_Dens %>% is.na() %>% sum()

stream_observation_join_summary_df2<-stream_observation_join_summary_df

stream_observation_join_summary_df$MetricResult_Dens %>% is.na() %>% sum()

# setdiff(stream_observation_join_summary_df$Segment_ID, smc_flowlines_sf$Segment_ID)
# setdiff(smc_flowlines_sf$Segment_ID, stream_observation_join_summary_df$Segment_ID )
write_csv(stream_observation_join_summary_df,"NotForGit/Step2_1/stream_observation_join_summary_df.csv")

wb_observation_join_summary_df<-all_wbs %>%
  crossing(taxa) %>%
  crossing(all_metrics) %>%
  left_join(wb_observation_join_summary %>%
              as_tibble() %>%
              select(-geometry)) %>%
  mutate(MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
         MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens))
# 
# wb_observation_join_summary_df2<-bind_rows(
#   wb_observation_join_summary_df,
#   wb_observation_join_summary_df %>%
#     group_by(Permanent_, wb_peri_km, wb_area_km2, buff_area_km2,buffonly_area_km2 ,
#              Group="All taxa",
#              Group2="All taxa",
#              Group_sci="All taxa",
#              Metric) %>%
#     summarise(MetricResult_Raw=sum(MetricResult_Raw),
#               MetricResult_Dens=sum(MetricResult_Dens)) %>%
#     ungroup()
# )   %>%
#   bind_rows(
#     wb_observation_join_summary_df %>%
#       filter(Group2!="Fish") %>%
#       group_by(Permanent_, wb_peri_km, wb_area_km2, buff_area_km2,buffonly_area_km2 ,
#                Group=paste("All",Group2),
#                Group2,
#                Group_sci=Group2,
#                Metric) %>%
#       summarise(MetricResult_Raw=sum(MetricResult_Raw),
#                 MetricResult_Dens=sum(MetricResult_Dens)) %>%
#       ungroup()
#   )

write_csv(wb_observation_join_summary_df,"NotForGit/Step2_1/wb_observation_join_summary_df.csv")

case_observation_join_summary_df<-all_cases %>%
  crossing(taxa) %>%
  crossing(all_metrics) %>%
  left_join(case_observation_join_summary %>%
              as_tibble() %>%
              select(-geometry)) %>%
  mutate(MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
         MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens))

# case_observation_join_summary_df2<-bind_rows(
#   case_observation_join_summary_df,
#   case_observation_join_summary_df %>%
#     group_by(Name, case_peri_km, case_area_km2, buff_area_km2,buffonly_area_km2 ,
#              Group="All taxa",
#              Group2="All taxa",
#              Group_sci="All taxa",
#              Metric) %>%
#     summarise(MetricResult_Raw=sum(MetricResult_Raw),
#               MetricResult_Dens=sum(MetricResult_Dens)) %>%
#     ungroup() %>%
#     bind_rows(
#       case_observation_join_summary_df %>%
#         filter(Group2!="Fish") %>%
#         group_by(Name, case_peri_km, case_area_km2, buff_area_km2,buffonly_area_km2 ,
#                  Group=paste("All",Group2),
#                  Group2,
#                  Group_sci=Group2,
#                  Metric) %>%
#         summarise(MetricResult_Raw=sum(MetricResult_Raw),
#                   MetricResult_Dens=sum(MetricResult_Dens)) %>%
#         ungroup()
#     )
# )  


write_csv(case_observation_join_summary_df,"NotForGit/Step2_1/case_observation_join_summary_df.csv")

#######################################
plot_lines<-smc_flowlines_sf %>%
  left_join(stream_observation_join_summary_df2 %>%
              filter(Metric=="n_observers",
                     Group %in% c("All taxa","All Birds","All Invertebrates","All Herps", "Fish")
              )) %>%
  mutate(#Group2.f=factor(Group2, levels=c("All taxa","Birds", "Herps","Fish","Invertebrates")),
    BinDens=case_when(MetricResult_Dens==0~0,
                      MetricResult_Dens<=10~10,
                      MetricResult_Dens<=50~50,
                      T~100) %>%
      as_factor()
  ) %>%
  st_zm(drop = T)
plot_lines$Group2.f<-factor(plot_lines$Group, 
                            levels=c("All taxa","All Birds", "All Herps",
                                     "Fish","All Invertebrates"))

major_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines,
          aes(color=MetricResult_Dens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=c(0,1, 10,50,100),
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(major_taxa_maps+
         facet_wrap(~Group2.f, nrow=2)+
         theme(legend.position = "bottom"), 
       filename="NotForGit/Step2_1/major_taxa_maps.png",
       height=4.5,width=9.75)
ggsave(major_taxa_maps+
         facet_wrap(~Group2.f, ncol=2)+
         theme(legend.position = "bottom"), 
       filename="NotForGit/Step2_1/major_taxa_maps_long.png",
       height=8,width=6.5)


all_taxa_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines %>%
            filter(Group=="All taxa"),# %>% filter(MetricResult_Raw>0),
          aes(color=MetricResult_Dens), size=0.25,
          # aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=c(0,1, 10,50,100),
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(all_taxa_map+
         ggtitle("All Taxa")+
         theme(legend.position = "bottom"),
       filename="NotForGit/Step2_1/all_taxa_map.png",
       height=6.5,width=9)
ggsave(all_taxa_map+
         ggtitle("All Taxa")+
         theme(legend.position = "bottom"), 
       filename="NotForGit/Step2_1/all_taxa_map_portrait.png",
       height=4.7,width=6.5)

la_closeup_map<-all_taxa_map+
  # geom_sf(data=smc_sheds_sf %>% 
            # filter(SMC_Name!="Los Angeles"), color="white", fill="gray75")+
  coord_sf(xlim=st_bbox(smc_sheds_sf %>% filter(SMC_Name=="Los Angeles"))[c(1,3)],
           ylim=st_bbox(smc_sheds_sf %>% filter(SMC_Name=="Los Angeles"))[c(2,4)])

ggsave(la_closeup_map+
         ggtitle("All Taxa")+
         theme(legend.position = "bottom"), 
       filename="NotForGit/Step2_1/all_taxa_map_LA.png",
       height=6,width=6.5)

########
# maps for individual taxa
breaks_scale1<-c(0,1, 10, 50, 100)
breaks_scale2<-c(0,1,  5, 10, 50)
breaks_scale3<-c(0,1,  5, 10, 25)

birds_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines %>%
            filter(Group=="All Birds"),# %>% filter(MetricResult_Raw>0),
          aes(color=MetricResult_Dens), size=0.25,
          # aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=breaks_scale1,
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(birds_map+
         ggtitle("All Birds")+
         theme(legend.position = "bottom"),
       filename="NotForGit/Step2_1/birds_map.png",
       height=6.5,width=9)


fish_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines %>%
            filter(Group=="Fish"),# %>% filter(MetricResult_Raw>0),
          aes(color=MetricResult_Dens), size=0.25,
          # aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=breaks_scale3,
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(fish_map+
         ggtitle("Fish")+
         theme(legend.position = "bottom"),
       filename="NotForGit/Step2_1/fish_map.png",
       height=6.5,width=9)


herps_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines %>%
            filter(Group=="All Herps"),# %>% filter(MetricResult_Raw>0),
          aes(color=MetricResult_Dens), size=0.25,
          # aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=breaks_scale3,
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(herps_map+
         ggtitle("Reptiles and Amphibians")+
         theme(legend.position = "bottom"),
       filename="NotForGit/Step2_1/herps_map.png",
       height=6.5,width=9)


inverts_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines %>%
            filter(Group=="All Invertebrates"),# %>% filter(MetricResult_Raw>0),
          aes(color=MetricResult_Dens), size=0.25,
          # aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=breaks_scale3,
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(inverts_map+
         ggtitle("Invertebrates")+
         theme(legend.position = "bottom"),
       filename="NotForGit/Step2_1/inverts_map.png",
       height=6.5,width=9)

########

library(ggpubr)
all_taxa_and_closeup<-
  ggarrange(all_taxa_map,la_closeup_map,
            ncol=1, 
            labels="AUTO",
            common.legend = TRUE, legend="bottom")
ggsave(all_taxa_and_closeup, filename="NotForGit/Step2_1/all_taxa_and_closeup.png",
       height=8, width=6.5)

plot_lines2<-smc_flowlines_sf %>%
  left_join(stream_observation_join_summary_df2 %>%
              filter(#str_detect(Metric,"n_observer_dates"),
                str_detect(Metric,"_m"),
                Group %in% c("All taxa","All Birds","All Invertebrates","All Herps", "Fish")
              )) %>%
  mutate(#Group2.f=factor(Group2, levels=c("All taxa","Birds", "Herps","Fish","Invertebrates")),
    Month=str_sub(Metric, -2,-1) %>% as.numeric(),
    BinDens=case_when(MetricResult_Dens==0~0,
                      MetricResult_Dens<=10~10,
                      MetricResult_Dens<=50~50,
                      T~100) %>%
      as_factor()
  ) %>%
  st_zm(drop = T)

gc()

plot_lines2$Month_a<-month.abb[plot_lines2$Month] %>%
  factor(levels=month.abb)

monthly_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines2  %>% filter(Group=="All taxa"),
          aes(color=MetricResult_Dens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=c(0,1, 10,50),
                     palette=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+

  facet_wrap(~Month_a)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(monthly_taxa_maps, filename="NotForGit/Step2_1/monthly_taxa_maps.png",
       height=6,width=10)

gc()

birds_monthly_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=plot_lines2  %>% filter(Group %in% c("Kingfishers", "Ducks", "Herons", "Dippers", "Loons", "Gulls", 
                                                    "Pelicans", "Grebes", "Rails", "Stilts", "Shorebirds", "Cormorants", 
                                                    "All Birds")),
  aes(color=MetricResult_Dens), size=0.25,
  key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  # geom_sf(data=smc_shedssup_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=c(0,1, 10,50),
                     palette=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  facet_wrap(~Month_a)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(birds_monthly_taxa_maps, filename="NotForGit/Step2_1/birds_monthly_taxa_maps.png",
       height=6,width=10)
