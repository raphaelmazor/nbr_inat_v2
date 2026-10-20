library(tidyverse)
library(sf)

# smc_streams_metrics<-read_csv("NotForGit/Step2_1/stream_observation_join_summary_df.csv") 



smc_streams_metrics<-read_csv("NotForGit/Step2_1/smc_streams_metrics_shortlist.csv")   
popdens_df<-read_csv("Data/stream_population_density_2020_wide.csv") %>%
  select(-stream_rowid ) %>%
  rename(Segment_ID=segment_id)

smc_streams_metrics2<-smc_streams_metrics %>%
  filter(Group=="All taxa") %>%
  filter(!str_detect(Metric, "_m")) %>%
  transmute(Segment_ID,
            Metric,
            MetricResult_Dens )  %>%
  pivot_wider(names_from=Metric, values_from = MetricResult_Dens )

smc_streams_metrics2_raw<-smc_streams_metrics %>%
  filter(Group=="All taxa") %>%
  filter(!str_detect(Metric, "_m")) %>%
  transmute(Segment_ID,
            Metric,
            MetricResult_Raw )  %>%
  pivot_wider(names_from=Metric, values_from = MetricResult_Raw )%>%
  select(Segment_ID, n_observers_raw=n_observers, n_observations_raw=n_observations, n_observer_dates_raw=n_observer_dates)

smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
smc_flowlines100m_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")
counties_sf<-st_read("Shapefiles/SoCalCounties/SoCal_Counties.shp") %>%
  st_transform(crs=st_crs(smc_flowlines_sf))
chapman_sf<-tibble(
  site="Chapman",
  lat= 33.794232,
  lon = -117.851849) %>%
  st_as_sf(coords=c("lon","lat"),
           crs=4326) %>%
  st_transform(crs=st_crs(smc_flowlines_sf))

chapman_buffkm_sf<-st_buffer(chapman_sf,
                           75000) #75 km from chapman



smc_flowlines_sf_val_full<-smc_flowlines_sf %>%
  select(
    COMID, Segment_ID, SegLength_km, HUC8_simp,
  ) %>%
  distinct() %>%
  filter(HUC8_simp!="406") %>%
  st_filter(chapman_buffkm_sf) %>%
  # #add location data
  # st_join(counties_sf %>% select(County=NAME_PCASE), left=T, largest=T) %>%
  # st_join(chapman_buffkm_sf, left=T, largest=T) %>%
  #add population/stratification data
  left_join(
    popdens_df %>%
      select(Segment_ID,
             popden_1k=pop_density_per_km2_1km,
             popden_5k=pop_density_per_km2_5km)  ) %>%
  left_join(smc_streams_metrics2) %>%
  mutate(
    Pop_Class=case_when(popden_5k<69~"Low", #bottom quartile of chapman region
                        popden_5k<524~"Mod", #median of chapman region
                        T~"High"),
    Obs_Class=case_when(n_observers==0~"Low",
                        n_observers<=8.6~"Mod", #Median non-zero of chapman region
                        T~"High"),
    Stratum = case_when(Pop_Class=="Low" & Obs_Class=="Low"~"LowPop_LowObs",
                        Pop_Class=="Low" & Obs_Class=="High"~"LowPop_HighObs",
                        Pop_Class=="High" & Obs_Class=="Low"~"HighPop_LowObs",
                        Pop_Class=="High" & Obs_Class=="High"~"HighPop_HighObs",
                        T~"OTHER"),
    Stratum2=paste0(Pop_Class,"Pop_", Obs_Class,"Obs"),
  ) 

study_area_map<-ggplot()+geom_sf(data=smc_flowlines_sf, color="gray")+
  geom_sf(data=smc_flowlines_sf_val_full, color="red")+
  theme_void()
ggsave(study_area_map, filename="NotForGit/Step4_1/study_area_map.png", width=10, height=6)


smc_flowlines_sf_val_full %>% as_tibble() %>% count(Stratum)

#Regional quantiles
quartiles_df<-  smc_streams_metrics %>%
  filter(Group=="All taxa",
         Metric=="n_observers",
         SegLength_km >0.999) %>%
  select(Segment_ID, n_observers=MetricResult_Dens) %>%
  inner_join(popdens_df %>%
               select(Segment_ID,
                      popden_5k=pop_density_per_km2_5km)  )%>%
  pivot_longer(cols=c(n_observers, popden_5k)) %>%
  group_by(name) %>%
  summarise(
    q25=quantile(value, .25),
    q50=quantile(value, .5),
    q75=quantile(value, .75),
    q25nz=quantile(value[value>0], .25),
    q50nz=quantile(value[value>0], .5),
    q75nz=quantile(value[value>0], .75)) %>%
  ungroup()

study_area_strata<-ggplot()+
  geom_sf(data=smc_flowlines_sf, color="gray")+
  geom_sf(data=smc_flowlines_sf_val_full, aes(color=Stratum))+
  # scale_color_viridis_d(  )+
  scale_color_manual(values=c("#ca0020","#f4a582","#0571b0","#92c5de", "yellow"))+
  theme_void()
ggsave(study_area_strata, filename="NotForGit/Step4_1/study_area_strata.png",
       width=10, height=6)

ggsave(study_area_strata +
         geom_sf(data=smc_flowlines100m_sf_exp, color="black", fill="black",size=3),
       filename="NotForGit/Step4_1/study_area_strata_selected.png",
       width=10, height=6)

#chapman quantiles
quartiles_chap_df<-  
  smc_streams_metrics %>%
  filter(Segment_ID %in% smc_flowlines_sf_val_full$Segment_ID) %>%
  filter(Group=="All taxa",
         Metric=="n_observers",
         SegLength_km >0.999) %>%
  select(Segment_ID, n_observers=MetricResult_Dens) %>%
  inner_join(popdens_df %>%
               select(Segment_ID,
                      popden_5k=pop_density_per_km2_5km)  ) %>%
  pivot_longer(cols=c(n_observers, popden_5k)) %>%
  group_by(name) %>%
  summarise(
    q25=quantile(value, .25),
    q33=quantile(value, .33),
    q50=quantile(value, .5),
    q67=quantile(value, .67),
    q75=quantile(value, .75),
    q25nz=quantile(value[value>0], .25),
    q50nz=quantile(value[value>0], .5),
    q75nz=quantile(value[value>0], .75)
    
    ) %>%
  ungroup()

#

# smc_flowlines_sf_exp %>% 
#   as_tibble()%>% 
#   filter(SegLength_km>.999,
#          Stratum!="OTHER") %>%
#   count(County, Stratum) %>%
#   pivot_wider(names_from = Stratum, values_from = n, values_fill = 0)

smc_flowlines_sf_val_full %>% as_tibble() %>% count(Stratum)
set.seed(20260610)
samp_df<-smc_flowlines_sf_val_full %>%
  as_tibble() %>%
  filter(SegLength_km>.999,
         !HUC8_simp %in% c("316","406")) %>%
  group_by(Stratum) %>%
  slice_sample(n=40) %>%
  ungroup()
samp_df_chap<-samp_df %>%
  group_by(Stratum) %>%
  slice_head(n=20) %>%
  ungroup()
samp_df <- samp_df %>%
  mutate(AssignedTo=case_when(Segment_ID %in% samp_df_chap$Segment_ID~"Chapman",
                              T~"UCI"))

samp_df %>%
  as_tibble() %>%
  group_by(Stratum, AssignedTo) %>%
  tally()
intersect(samp_df$Segment_ID[samp_df$AssignedTo=="Chapman"],
        samp_df$Segment_ID[samp_df$AssignedTo=="UCI"])

smc_flowlines_sf_val_full<-smc_flowlines_sf_val_full  %>%
  mutate(SELECTED=(Segment_ID %in% samp_df$Segment_ID)) %>%
  left_join(samp_df %>%
              as_tibble() %>%
              select(Segment_ID, AssignedTo))

smc_flowlines_sf_val_full %>%
  as_tibble() %>%
  filter(SELECTED) %>%
  count(Segment_ID) %>% 
  filter(n>1)

smc_flowlines_sf_full<-smc_flowlines_sf %>%
  left_join(
    smc_flowlines_sf_val_full %>% 
      as_tibble() %>%
      select(Segment_ID, popden_5k,n_observers,
             Pop_Class, Obs_Class, Stratum, SELECTED, AssignedTo)
  )

st_write(smc_flowlines_sf_full,
         "NotForGit/Step4_1/smc_flowlines_sf_full.shp" )


smc_flowlines100m_sf_exp<-smc_flowlines100m_sf %>%
  select(
    COMID, Segment_ID, SegLength_km, buff_area_km2,
  ) %>%
  left_join(
    smc_flowlines_sf_val_full %>% 
      as_tibble() %>%
      select(Segment_ID, popden_5k,n_observers,
             Pop_Class, Obs_Class, Stratum, SELECTED, AssignedTo)
  ) %>%
  filter(SELECTED)
ggplot()+geom_sf(data=smc_flowlines100m_sf_exp)


st_write(smc_flowlines100m_sf_exp,
         "NotForGit/Step4_1/smc_flowlines100m_sf_exp.shp" )
# st_write(smc_flowlines100m_sf_exp%>%
# mutate(SELECTED=(Segment_ID %in% samp_df$Segment_ID)) %>%
# filter(SELECTED),
# "NotForGit/Misc exports/smc_flowlines100m_sf_exp.kml" )
write_csv(smc_flowlines100m_sf_exp%>%
            # st_join(counties_sf %>% select(County=NAME_PCASE)) %>%
            as_tibble() %>% select(-geometry),
          "NotForGit/Step4_1/smc_flowlines_selected.csv")


selected_segments_export<-smc_flowlines_sf_full%>%
            filter(SELECTED) %>%
    select(COMID, Segment_ID, GNIS_NAME,SegLength_km,
           SWAMP3CODE, HUC8_simp, HUC12, HUC12_name,
           popden_5k, n_observers, Pop_Class, Obs_Class, Stratum, AssignedTo) %>%
    st_transform(crs=4326)
selected_segments_export <- selected_segments_export %>%
  bind_cols(
    st_coordinates(selected_segments_export) %>%
      as_tibble() %>%
      group_by(L1) %>%
      slice_head(n=1) %>%
      ungroup() %>%
      transmute(Lat1=Y, Lon1=X)
  ) %>%
  bind_cols(
    st_coordinates(selected_segments_export) %>%
      as_tibble() %>%
      group_by(L1) %>%
      slice_tail(n=1) %>%
      ungroup() %>%
      transmute( Lat2=Y, Lon2=X)
  ) %>%
  as_tibble() %>% 
  select(-geometry) %>%
  inner_join(smc_streams_metrics2_raw)
write_csv( selected_segments_export,"NotForGit/Step4_1/smc_flowlines_selected.csv")


all_taxa_df<-read_csv("NotForGit/Step1_1/all_taxa_df.csv") %>%
  select(user_id, user_login, observed_on, Year, Month, latitude, longitude) %>%
  distinct() 



chapman_taxa_sf<-read_csv("NotForGit/Step1_1/all_taxa_df.csv") %>%
  select(user_id, user_login, observed_on, Year, Month, latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326, remove=F) %>%
  st_transform(crs=st_crs(chapman_buffkm_sf)) %>%
  st_filter(chapman_buffkm_sf)
st_write(chapman_taxa_sf, "NotForGit/Step4_1/chapman_taxa_sf.shp")
chapman_taxa_df<-chapman_taxa_sf %>% 
  st_join(counties_sf %>%
            select(County=NAME_PCASE)) %>%
  as_tibble() %>% select(-geometry)
chapman_taxa_df%>% count(County)
write_csv(chapman_taxa_df %>% filter(County=="Los Angeles"), "NotForGit/Step4_1/all_taxa_split/chapman_taxa_LosAngeles.csv")
write_csv(chapman_taxa_df %>% filter(County!="Los Angeles"), "NotForGit/Step4_1/all_taxa_split/chapman_taxa_NotLosAngeles.csv")


sel_segs_buff<-read_csv("NotForGit/Step1_1/all_taxa_df.csv") %>%
  select(user_id, user_login, observed_on, Year, Month, latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326, remove=F) %>%
  st_transform(crs=st_crs(smc_flowlines100m_sf_exp)) %>%
  st_filter(smc_flowlines100m_sf_exp %>%
              st_buffer(1000))
st_write(sel_segs_buff, "NotForGit/Step4_1/sel_segs_buff.shp")

st_write(sel_segs_buff[1:9999,], "NotForGit/Step4_1/sel_segs_buff_pt1.shp")
st_write(sel_segs_buff[10000:19999,], "NotForGit/Step4_1/sel_segs_buff_pt2.shp")
st_write(sel_segs_buff[20000:28192,], "NotForGit/Step4_1/sel_segs_buff_pt3.shp")
