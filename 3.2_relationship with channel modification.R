library(tidyverse)
library(sf)

############Import shapefiles and data############
smc_sheds_sf_0<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_sheds_sf<-  st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2026/SMC_Sheds_Merge.shp")%>%
  st_transform(crs=st_crs(smc_sheds_sf_0))
# st_transform(crs=26911)
rm(smc_sheds_sf_0)
smc_huc12_sf<-st_read("Shapefiles/HUCs/HUC12_SMC.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))
smc_streams_metrics<-read_csv("NotForGit/Step2_1/smc_streams_metrics_shortlist.csv")
smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS") %>%
  mutate(HUC12=case_when(    COMID=="22547535"~"180703020201",    T~HUC12),
         HUC12_name=case_when(    COMID=="22547535"~"Upper Cahuilla Creek",    T~HUC12_name),)
counties_sf<-st_read("Shapefiles/SoCalCounties/SoCal_Counties.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))

pop_df<-read_csv("Data/stream_population_density_2020_wide.csv")

chan_df<-read_csv("Data/smc_chaneng_06032026.csv")
chan_df2<-chan_df %>%
  select(Segment_ID=nearest_segment_id, class, class_2, class_3 ) %>%
  distinct() %>%
  mutate(class_3=factor(class_3, levels=c("NAT","SB","HB")))

############
smc_metrics_chan_df<-smc_streams_metrics %>%
  left_join(    chan_df2 ) %>%
  inner_join(pop_df %>%
               select(Segment_ID=segment_id,
                      starts_with("pop_density_per_km2")))

ggplot(data=smc_metrics_chan_df %>%
         filter(Group=="All taxa",Metric=="n_observers",
                !is.na(class)),
       aes(x=class_3, y=MetricResult_Dens))+
  geom_boxplot()+
  geom_point(position=position_jitter(width=0.2, height=0))+
  scale_y_sqrt()


mod_dat<-smc_metrics_chan_df %>%
  filter(Group=="All taxa",Metric=="n_observers",
         !is.na(class))

nbr_vs_chan_type_pop<-ggplot(data=mod_dat, aes(x=pop_density_per_km2_5km,
                         y=MetricResult_Dens))+
  geom_point(size=1.5, aes(shape=class_3, fill=class_3))+
  geom_smooth(aes(color=class_3))+
  scale_shape_manual(values=c(21,24,22),
                     name="Channel type",
                     labels=c("Natural","Soft bottom","Hard bottom"))+
  scale_color_manual(values=c("#4daf4a","#ff7f00", "#e41a1c"),
                     name="Channel type",
                     labels=c("Natural","Soft bottom","Hard bottom")
                     )+
  scale_fill_manual(values=c("#4daf4a","#ff7f00", "#e41a1c"),
                     name="Channel type",
                     labels=c("Natural","Soft bottom","Hard bottom")
  )+
  scale_x_continuous("Population density within 5 km\npeople per square-km",
                     # expand=c(0,0)
                     )+
  scale_y_continuous("# observers per square-km",
                     # expand=c(0,0)
                     trans="sqrt",
                     )+
  theme_bw()+
  coord_cartesian(ylim = c(0,2510))

ggsave(nbr_vs_chan_type_pop, filename="NotForGit/Step3_2/nbr_vs_chan_type_pop.png",
       width=6.5, height=4)


ggsave(nbr_vs_chan_type_pop+
         facet_wrap(~class_3, ncol=1, scales="free_y")+
         theme(legend.position = "bottom"), filename="NotForGit/Step3_2/nbr_vs_chan_type_pop_alt.png",
       width=4.5, height=6)


mod_dat2<-smc_metrics_chan_df %>%
  filter(Group=="All taxa",Metric %in% c("n_observers","n_observations","n_observer_dates"),
         !is.na(class))


nbr_vs_chan_type_pop2<-ggplot(data=mod_dat2, aes(x=pop_density_per_km2_5km,
                                               y=MetricResult_Dens))+
  geom_point(size=1.5, aes(shape=class_3, fill=class_3))+
  geom_smooth(aes(color=class_3))+
  scale_shape_manual(values=c(21,24,22),
                     name="Channel type",
                     labels=c("Natural","Soft bottom","Hard bottom"))+
  scale_color_manual(values=c("#4daf4a","#ff7f00", "#e41a1c"),
                     name="Channel type",
                     labels=c("Natural","Soft bottom","Hard bottom")
  )+
  scale_fill_manual(values=c("#4daf4a","#ff7f00", "#e41a1c"),
                    name="Channel type",
                    labels=c("Natural","Soft bottom","Hard bottom")
  )+
  scale_x_continuous("Population density within 5 km\npeople per square-km",
                     # expand=c(0,0)
  )+
  scale_y_continuous("# observers per square-km",
                     # expand=c(0,0)
                     trans="sqrt"
  )+
  theme_bw()+
  # coord_cartesian(ylim = c(0,3000))+
  # facet_wrap(~Metric, nrow=1, scales="free_y")+
  # facet_grid(class_3~Metric, scales="free")+
  theme(legend.position = "bottom")

ggsave(nbr_vs_chan_type_pop2+facet_wrap(~Metric, nrow=1, scales="free_y"), 
       filename="NotForGit/Step3_2/nbr_vs_chan_type_pop2.png",
       width=6.5, height=4.5)
ggsave(nbr_vs_chan_type_pop2+facet_grid(class_3~Metric, scales="free"), filename="NotForGit/Step3_2/nbr_vs_chan_type_pop2_alt.png",
       width=6.5, height=6.5)


#Try to develop a GAM model?
# junk<-gam(MetricResult_Dens~s(pop_density_per_km2_5km+class_3, bs = "cs", k=5),
#     # gam(MetricResult_Dens~s(CSCI_scape, bs = "cs", k=5),
#     method="REML",
#     data=mod_dat)
# plot(junk)
# summary(junk)
# 
# predict(junk, type="response")
# junk2<-mod_dat %>% 
#   mutate(Predicted=predict(junk, type="response")) %>%
#   arrange(pop_density_per_km2_5km)
# ggplot(junk2, aes(x=pop_density_per_km2_5km, y=Predicted, color=class_3))+
#   geom_point()



mod_dat %>%
  slice_max(MetricResult_Dens)
smc_flowlines_sf %>%
  filter(Segment_ID=="22515822_5") %>%
  st_transform(crs=4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  slice_tail()
  

#########
#Model all 3 responses
mod_mets<-c("n_observations","n_observer_dates","n_observers")



Sys.time()
all_mets_sub_mods_gam<-lapply(mod_mets, function(i){
  # group.i=all_mets_df_sub$Group[i]
  met.i=all_mets_df_sub$Metric[i]
  # print(paste(i, group.i, met.i))
  dat.i=mod_dat %>% filter(Metric==met.i)
  gam(MetricResult_Dens~s(pop_density_per_km2_5km+class_3, bs = "cs", k=5),
      # gam(MetricResult_Dens~s(CSCI_scape, bs = "cs", k=5),
      method="REML",
      data=dat.i)
})

library(qgam)
my_qgam<-
  qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
       qu=0.5,
       data=mod_dat)
