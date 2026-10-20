library(tidyverse)
library(sf)

############Import shapefiles############
smc_sheds_sf<-st_read("Shapefiles/SMCSheds2009/SMCSheds2009.shp")
smc_shedssup_sf<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009_SupplementalSheds.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))
smc_flowlines100m_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")
smc_wb100m_sf<-readRDS("NotForGit/Step1_2/smc_wb100m_sf.RDS")
smc_case100m_sf<-readRDS("NotForGit/Step1_2/smc_case100m_sf.RDS")

# smc_flowlines_sf<-st_read("Shapefiles/rivers_split_1000m/rivers_split_1000m.shp") %>%
# st_transform(crs=st_crs(smc_sheds_sf))
smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")

all_ambient_sf<-  bind_rows(smc_flowlines100m_sf,
                            smc_wb100m_sf)


############Import observations############
all_taxa_df_orig<-read_csv("NotForGit/Step1_1/all_taxa_df.csv") %>%
  mutate(Obs_Date=paste(user_id, Date, sep="_"),
         YM=paste(Year, Month, sep="_"),
         YM_date=paste(Year,Month,"01", sep="-") %>% ymd()) 

all_taxa_sf<-all_taxa_df_orig %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326) %>%
  st_transform(crs=st_crs(smc_sheds_sf))

all_taxa_df<-all_taxa_df_orig %>%
  bind_rows(
  all_taxa_df_orig %>% mutate(Group="All taxa", Group2="All taxa", Group_sci="All taxa"),
  all_taxa_df_orig %>% filter(Group2 =="Birds") %>% mutate(Group="All Birds", Group2="All Birds", Group_sci="All Birds"),
  all_taxa_df_orig %>% filter(Group2 =="Invertebrates") %>% mutate(Group="All Invertebrates", Group2="All Invertebrates", Group_sci="All Invertebrates"),
  all_taxa_df_orig %>% filter(Group2 =="Herps") %>% mutate(Group="All Herps", Group2="All Herps", Group_sci="All Herps"),
) 


########################Tally counts by month and year########################

stream_observation_join<-
  smc_flowlines100m_sf %>%
  st_join(all_taxa_sf, join=st_contains, left=FALSE)
# wb_observation_join<-
#   smc_wb100m_sf %>%
#   st_join(all_taxa_sf, join=st_contains, left=FALSE)
# case_observation_join<-
#   smc_case100m_sf %>%
#   st_join(all_taxa_sf, join=st_contains, left=FALSE)


stream_observation_join_summary<-
  stream_observation_join %>%
  as_tibble() %>%
  # saveRDS("NotForGit/Step2_3/stream_observation_join_TEMP.RDS")
  group_by(Group2, Group, Group_sci,
           Year, Month, YM, YM_date,
           SMCShed, TotLength_km, 
           COMID, lengthkm2, 
           Segment_ID, SegLength_km, buff_area_km2) %>%
  summarize(
    n_observations = sum(!is.na(user_id)),     
    n_observers = user_id %>% n_distinct(na.rm=T),    
    n_observer_dates = Obs_Date %>% n_distinct(na.rm=T),  ) %>%
  ungroup() %>%
  pivot_longer(cols=starts_with("n_obs"), 
               names_to = "Metric", 
               values_to = "MetricResult_Raw") %>%
  mutate(MetricResult_Dens=MetricResult_Raw/buff_area_km2)
saveRDS(stream_observation_join_summary, "NotForGit/Step2_3/Stream_Obs_Summary.Rda")
stream_observation_join_summary<-readRDS("NotForGit/Step2_3/Stream_Obs_Summary.Rda")
stream_observation_join_summary




all_metrics = stream_observation_join_summary %>%
  as_tibble() %>%
  select(Metric) %>%
  distinct()

all_times = #stream_observation_join %>% select(Year, Month, YM, YM_date) %>% distinct()
  tibble(Year=2010:2025)

#####Make a "spine" of all comids and taxa and times#####
taxa<-all_taxa_df %>% select(Group, Group2, Group_sci) %>% distinct() #
all_streams<-#smc_flowlines_sf %>% as_tibble() %>%   select(COMID, Segment_ID) %>% distinct()
  smc_flowlines100m_sf %>% as_tibble() %>%   select(SMCShed, TotLength_km,    COMID, lengthkm2, Segment_ID, SegLength_km, buff_area_km2 ) %>% distinct()
# all_wbs<-#smc_wb100m_sf %>% as_tibble() %>% select(Permanent_)  %>% distinct( )
#   smc_wb100m_sf %>% as_tibble() %>% select(Year, Month, YM, YM_date, Permanent_, wb_peri_km, wb_area_km2, buff_area_km2, buffonly_area_km2)  %>% distinct( )
# all_cases<-smc_case100m_sf %>% as_tibble() %>% select(Year, Month, YM, YM_date, Name, SMCShed, case_peri_km, case_area_km2, buff_area_km2, buffonly_area_km2)  %>% distinct( )

# all_streams_taxa<-all_streams %>% crossing(taxa)
# all_wb_taxa<-all_wbs %>% crossing(taxa)
# all_cases_taxa<-all_cases %>% crossing(taxa)


stream_observation_join_summary2<-
  all_streams %>%
  # crossing(taxa) %>%
  crossing(all_times) %>%
  crossing(all_metrics) %>%
  left_join(
    stream_observation_join_summary %>%
      as_tibble() %>%
      select(COMID, Segment_ID, Year, #Month, YM, YM_date, 
             starts_with("Metric"))
  ) %>%
  mutate(#MetricResult_Raw=replace_na(0),
    #MetricResult_Dens=replace_na(0)
    MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
    MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens),
  )

stream_observation_join_summary2$MetricResult_Raw %>% is.na() %>% sum()

#Save
# saveRDS(stream_observation_join_summary,"NotForGit/Step2_1/stream_observation_join_summary.RDS")
# stream_observation_join_summary<-readRDS("NotForGit/Step2_1/stream_observation_join_summary.RDS")
# saveRDS(wb_observation_join_summary,"NotForGit/Step2_1/wb_observation_join_summary.RDS")
# saveRDS(case_observation_join_summary,"NotForGit/Step2_1/case_observation_join_summary.RDS")

# all_streams %>%
#   crossing(taxa) %>%
#   crossing(all_metrics)

stream_observation_join_summary_df<-all_streams %>%
  crossing(taxa) %>%
  crossing(all_metrics) %>%
  left_join(stream_observation_join_summary %>%
              as_tibble() #%>%
              # select(-geometry)
            ) %>%
  mutate(MetricResult_Raw=case_when(is.na(MetricResult_Raw)~0, T~MetricResult_Raw),
         MetricResult_Dens=case_when(is.na(MetricResult_Dens)~0, T~MetricResult_Dens))

# stream_observation_join_summary_df$MetricResult_Dens %>% is.na() %>% sum()

# stream_observation_join_summary_df2<-bind_rows(
#   stream_observation_join_summary_df,
#   stream_observation_join_summary_df %>%
#     group_by(SMCShed, TotLength_km, COMID, lengthkm2, Segment_ID, SegLength_km, buff_area_km2,
#              Group="All taxa",
#              Group2="All taxa",
#              Group_sci="All taxa",
#              Metric) %>%
#     summarise(MetricResult_Raw=sum(MetricResult_Raw),
#               MetricResult_Dens=sum(MetricResult_Dens)) %>%
#     ungroup()
# )  %>%
#   bind_rows(
#     stream_observation_join_summary_df %>%
#       filter(Group2!="Fish") %>%
#       group_by(SMCShed, TotLength_km, COMID, lengthkm2, Segment_ID, SegLength_km, buff_area_km2,
#                Group=paste("All",Group2),
#                Group2,
#                Group_sci=Group2,
#                Metric) %>%
#       summarise(MetricResult_Raw=sum(MetricResult_Raw),
#                 MetricResult_Dens=sum(MetricResult_Dens)) %>%
#       ungroup()
#   )
stream_observation_join_summary_df$MetricResult_Dens %>% is.na() %>% sum()

# setdiff(stream_observation_join_summary_df$Segment_ID, smc_flowlines_sf$Segment_ID)
# setdiff(smc_flowlines_sf$Segment_ID, stream_observation_join_summary_df$Segment_ID )
write_csv(stream_observation_join_summary2,
          "NotForGit/Step2_3/stream_observation_join_summary2_ANNUAL.csv")

plot_dat<-stream_observation_join_summary %>%
  group_by(Year, Metric) %>%
  summarise(MetricResult_Raw=sum(MetricResult_Raw))
obs_v_year_plot<-ggplot(data=plot_dat,
                        aes(x=Year, y=MetricResult_Raw))+
  geom_path()+
  ylab("")+
  facet_wrap(~Metric)+
  theme_bw()
ggsave(obs_v_year_plot, filename="NotForGit/Step2_3/all_taxa_v_time.png", width=10, height=6)

obs_v_year_plot2<-ggplot(data=plot_dat,
                        aes(x=Year, y=MetricResult_Raw))+
  geom_path(aes(color=Metric, linetype=Metric))+
  scale_color_brewer(palette="Set2",
                     labels=c("# observations","# observer-dates","# observers"))+
  scale_linetype_discrete(labels=c("# observations","# observer-dates","# observers"))+
  ylab("")+
  # facet_wrap(~Metric)+
  theme_bw()
ggsave(obs_v_year_plot2, filename="NotForGit/Step2_3/all_taxa_v_time2.png", width=6.5, height=4)



plot_dat<-stream_observation_join_summary %>%
  group_by(Year, Metric) %>%
  summarise(MetricResult_Raw=sum(MetricResult_Raw))
nobservers_v_year_plot<-ggplot(data=plot_dat %>% filter(Metric=="n_observers"),
                        aes(x=Year, y=MetricResult_Raw))+
  geom_path()+
  ylab("# observers")+
  # facet_wrap(~Metric)+
  theme_bw(base_size = 16)
ggsave(nobservers_v_year_plot, filename="NotForGit/Step2_3/nobservers_v_year_plot.png", width=5, height=5)


#######################################
library(ggpubr)
all_observations_map<-readRDS("NotForGit/Step2_1/all_observations_map.RDS")
close_observations_map<-readRDS("NotForGit/Step2_1/close_observations_map.RDS")
close_observations_map_closeup<-readRDS("NotForGit/Step2_1/close_observations_closeup_map.RDS")

ggtop<-ggarrange(all_observations_map, close_observations_map_closeup,
                 ncol=2)
ggboth<-ggarrange(ggtop, obs_v_year_plot, nrow=2)

ggsave(ggboth, filename="NotForGit/Step2_3/queried_data.png",
       height=6.5, width=6.5)



#######################################


plot_lines<-smc_flowlines_sf %>%
  left_join(stream_observation_join_summary_df2 %>%
              filter(Metric=="n_observer_dates",
                     Group %in% c("All taxa","All Birds","All Invertebrates","All Herps", "Fish")
              )) %>%
  mutate(Group2.f=factor(Group2, levels=c("All taxa","Birds", "Herps","Fish","Invertebrates")),
         BinDens=case_when(MetricResult_Dens==0~0,
                           MetricResult_Dens<=10~10,
                           MetricResult_Dens<=50~50,
                           T~100) %>%
           as_factor()
  ) %>%
  st_zm(drop = T) #%>%
# group_by(SMCShed, COMID, Segment_ID, Metric,
# Group="All taxa") %>%
# summarise(MetricResult_Raw=sum(MetricResult_Raw),
# MetricResult_Dens=sum(MetricResult_Dens)) %>%
# ungroup()

# plot_lines$Group2.f<-factor(plot_lines$Group2, levels=c("All taxa","Birds", "Herps","Fish","Invertebrates"))

counties_sf<-st_read("Shapefiles/SoCalCounties/SoCal_Counties.shp")

major_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray25")+
  
  # geom_sf(data=smc_flowlines_sf,
  #         color="#2b547e",
  #         size=0.25)+
  geom_sf(data=plot_lines,# %>% filter(MetricResult_Raw>0),
          aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=smc_shedssup_sf, fill=NA, color="white", lwd=0.25)+
  scale_color_manual(breaks=c(0,10,50,100),
                     labels=c("0","10","50","100+"),
                     # begin=0.2,
                     values=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observer-dates\nper sqkm") +
  # scale_color_viridis_b(breaks=c(0,10,50,100),
  #                       labels=c(0,10,50,100),
  #                       begin=0.2,
  #                       option="plasma",
  #                       # show.limits = TRUE,
  #                       name ="Observer-dates\n per sqkm") +
  facet_wrap(~Group2.f)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(major_taxa_maps, filename="NotForGit/Step2_1/major_taxa_maps.png",
       height=3*1.5,width=6.5*1.5)

all_taxa_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray25")+
  
  # geom_sf(data=smc_flowlines_sf,
  #         color="#2b547e",
  #         size=0.25)+
  geom_sf(data=plot_lines %>%
            filter(Group=="All taxa"),# %>% filter(MetricResult_Raw>0),
          aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=smc_shedssup_sf, fill=NA, color="white", lwd=0.25)+
  scale_color_manual(breaks=c(0,10,50,100),
                     labels=c("0","10","50","100+"),
                     # begin=0.2,
                     values=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observer-dates\nper sqkm") +
  # scale_color_viridis_b(breaks=c(0,10,50,100),
  #                       labels=c(0,10,50,100),
  #                       begin=0.2,
  #                       option="plasma",
  #                       # show.limits = TRUE,
  #                       name ="Observer-dates\n per sqkm") +
  facet_wrap(~Group2.f)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="lightblue"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(all_taxa_map, filename="NotForGit/Step2_1/all_taxa_map.png",
       height=3*1.5,width=6.5*1.5)


plot_lines2<-smc_flowlines_sf %>%
  left_join(stream_observation_join_summary_df2 %>%
              filter(#str_detect(Metric,"n_observer_dates"),
                str_detect(Metric,"_m"),
                Group %in% c("All taxa","All Birds","All Invertebrates","All Herps", "Fish")
              )) %>%
  mutate(Group2.f=factor(Group2, levels=c("All taxa","Birds", "Herps","Fish","Invertebrates")),
         Month=str_sub(Metric, -2,-1) %>% as.numeric(),
         BinDens=case_when(MetricResult_Dens==0~0,
                           MetricResult_Dens<=10~10,
                           MetricResult_Dens<=50~50,
                           T~100) %>%
           as_factor()
  ) %>%
  st_zm(drop = T)

plot_lines2$Month_a<-month.abb[plot_lines2$Month] %>%
  factor(levels=month.abb)

monthly_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray25")+
  geom_sf(data=plot_lines2  %>% filter(Group=="All taxa"),
          aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=smc_shedssup_sf, fill=NA, color="white", lwd=0.25)+
  scale_color_manual(breaks=c(0,10,50,100),
                     labels=c("0","10","50","100+"),
                     # begin=0.2,
                     values=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observer-dates\nper sqkm") +
  # scale_color_viridis_b(breaks=c(0,10,50,100),
  #                       labels=c(0,10,50,100),
  #                       begin=0.2,
  #                       option="plasma",
  #                       # show.limits = TRUE,
  #                       name ="Observer-dates\n per sqkm") +
  facet_wrap(~Month_a)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(monthly_taxa_maps, filename="NotForGit/Step2_1/monthly_taxa_maps.png",
       height=6,width=10)

birds_monthly_taxa_maps<-ggplot()+
  geom_sf(data=counties_sf, fill="gray25")+
  geom_sf(data=plot_lines2  %>% filter(Group2=="Birds"),
          aes(color=BinDens), size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=smc_shedssup_sf, fill=NA, color="white", lwd=0.25)+
  scale_color_manual(breaks=c(0,10,50,100),
                     labels=c("0","10","50","100+"),
                     # begin=0.2,
                     values=c("#2b547e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observer-dates\nper sqkm") +
  # scale_color_viridis_b(breaks=c(0,10,50,100),
  #                       labels=c(0,10,50,100),
  #                       begin=0.2,
  #                       option="plasma",
  #                       # show.limits = TRUE,
  #                       name ="Observer-dates\n per sqkm") +
  facet_wrap(~Month_a)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf)[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf)[c(2,4)])
ggsave(birds_monthly_taxa_maps, filename="NotForGit/Step2_1/birds_monthly_taxa_maps.png",
       height=6,width=10)
