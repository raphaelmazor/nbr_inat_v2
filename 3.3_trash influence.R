library(tidyverse)
library(sf)
smc_sheds_sf_0<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_sheds_sf<-  st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2026/SMC_Sheds_Merge.shp")%>%
  st_transform(crs=st_crs(smc_sheds_sf_0))
my_bb<-st_bbox(smc_sheds_sf)
# st_transform(crs=26911)
rm(smc_sheds_sf_0)
smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS") %>%
  mutate(HUC12=case_when(    COMID=="22547535"~"180703020201",    T~HUC12),
         HUC12_name=case_when(    COMID=="22547535"~"Upper Cahuilla Creek",    T~HUC12_name),)
mx_sf<-st_read("S:/Mexico/BajaMexico.shp") %>%st_transform(crs=st_crs(smc_sheds_sf))
counties_sf<-st_read("Shapefiles/SoCalCounties/SoCal_Counties.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))



lustations_df<-read_csv("Data/lustations_df.csv")
trash_tally_df<-#read_csv("Data/trash_tally_df.csv") %>%
  read_csv("Data/trash_with_nearest_segment.csv")# %>%
#   mutate(masterid=case_when(
#   masterid=="408CGCS13"~"408WE0654",
#   stationid=="412BCDN1"~"412BCDN1",
#   stationid=="412BCDN2"~"412BCDN2",
#   stationid=="412BCUP"~"412BCUP",
#   stationid=="412LAGEP"~"412LAGEP",
#   stationid=="SMC016169"~"SMC16169",
#   T~masterid)) %>%
# transmute(masterid, sampledate=mdy(sampledate), 
#           total_debris_count  ) %>%
#   distinct() %>%
# left_join(
#   lustations_df %>% 
#     group_by(masterid) %>% slice_head(n=1) %>% ungroup() %>%
#     select(masterid, latitude, longitude)
# )

trash_tally_df2<-trash_tally_df %>% 
  group_by(Segment_ID) %>%
  summarize(Trash=mean(Trash_mean)) %>%
  ungroup()

smc_streams_metrics<-read_csv("NotForGit/Step2_1/smc_streams_metrics_shortlist.csv") 

trash_metrics_df<-smc_streams_metrics %>%
  inner_join(trash_tally_df2)

ggplot(data=trash_metrics_df %>%
         filter(Group=="All taxa",
                Metric %in% c("n_observers","n_observations","n_observer_dates")),
       aes(x=MetricResult_Dens, y=Trash))+
  geom_point()+
  geom_smooth()+
  scale_x_sqrt()+scale_y_sqrt()+
  coord_cartesian(ylim=c(0,400))+
  facet_wrap(~Metric, scales="free_x")

trash_vs_nbr_plot<-
  ggplot(data=trash_metrics_df %>%
           filter(Group=="All taxa",
                  SegLength_km>0.999,
                  Metric %in% c("n_observers","n_observations","n_observer_dates")),
         aes(y=MetricResult_Dens, x=Trash))+
  geom_point(size=.75)+
  geom_smooth()+
  scale_x_sqrt(name="Mean # debris items")+
  scale_y_sqrt(name="Metric value (all taxa)")+
  coord_cartesian(xlim=c(0,400))+
  facet_wrap(~Metric, scales="free_y")+
  theme_bw()
ggsave(trash_vs_nbr_plot, filename="NotForGit/Step3_3/trash_vs_nbr_plot.png",
       width=6.5, height=3)

trash_vs_nbr_plot_1metric<-
  ggplot(data=trash_metrics_df %>%
           filter(Group=="All taxa",
                  SegLength_km>0.999,
                  Metric %in% c("n_observer_dates")),
         aes(y=MetricResult_Dens, x=Trash))+
  geom_point(size=.75)+
  geom_smooth()+
  scale_x_sqrt(name="Mean # debris items")+
  scale_y_sqrt(name="Density of observer-dates\n(all taxa)")+
  # coord_cartesian(xlim=c(0,400))+
  # facet_wrap(~Metric, scales="free_y")+
  theme_bw()
ggsave(trash_vs_nbr_plot_1metric, filename="NotForGit/Step3_3/trash_vs_nbr_plot_1metric.png",
       width=3.5, height=3)


ggplot(data=trash_metrics_df %>%
         filter(Group=="All taxa",
                SegLength_km>0.999,
                Metric %in% c("n_observer_dates")),
       aes(x=MetricResult_Dens, y=Trash))+
  geom_point(size=.75)+
  geom_smooth()+
  scale_y_sqrt(name="Mean # debris items")+
  scale_x_sqrt(name="Density of observer-dates\n(all taxa)")+
  # coord_cartesian(xlim=c(0,1000))+
  # facet_wrap(~Metric, scales="free_y")+
  theme_bw()

trash_metrics_df %>%
  filter(Group=="All taxa",
         Metric %in% c("n_observers"),
         MetricResult_Dens>500,
         Trash==0)

trash_tally_df %>% filter(Segment_ID=="20331434_1") 

flowlines_trash_sf<-smc_flowlines_sf %>%
  left_join(trash_tally_df2)

flowlines_trash_sf_100<-st_buffer(flowlines_trash_sf, dist=100)

smc_flowlines_sf %>%
  filter(Segment_ID=="20331434_1") %>%
  select(HUC12)
# 180703040705 

ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=flowlines_trash_sf_100 %>% filter(!is.na(Trash)), 
          aes(fill=Trash))+
  scale_fill_binned(
    name="Trash]n(buffers)",
    breaks=c(0,1, 10,50,100),
    palette=c("#440154ff","#39568cff","#2a788eff","#22a884ff","#7ad151ff"))+
  geom_sf(data=smc_flowlines_sf %>%
            inner_join(
              smc_streams_metrics %>%
                filter(Group=="All taxa", Metric=="n_observers") %>%
                select(Segment_ID, MetricResult_Dens),),
          aes(color=MetricResult_Dens),
          # key_glyph="path"
          
  )+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  scale_color_binned(breaks=c(0,1, 10,100,200),
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Recreation\n(lines)",
                     
                     # guide = guide_legend(
                     #   override.aes = list(
                     #     linewidth = 1,
                     #     linetype = 1))
  )+
  
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        # panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank())+
  coord_sf(xlim=st_bbox(smc_flowlines_sf %>% filter(HUC12=="180703040705"))[c(1,3)],
           ylim=st_bbox(smc_flowlines_sf%>% filter(HUC12=="180703040705"))[c(2,4)])


###Are important NBR hotspots affected by trash?
nbr_hotspots_df<-read_csv("NotForGit/Step3_0/nbr_hotspots_alltaxa.csv")

nbr_hotspots_df2<-nbr_hotspots_df %>%
  filter(Metric=="n_observer_dates") %>%
  left_join(trash_tally_df2) %>%
  mutate(NBR_class=factor(
    NBR_class,
    levels=c("No NBR","Low NBR","Moderate NBR","High NBR","Very high NBR")))

nbr_class_vs_trash<-
  ggplot(nbr_hotspots_df2, aes(x=NBR_class , y=Trash))+
  geom_boxplot(outlier.colour = NA, fill="gray")+
  geom_point(position=position_jitter(height=0, width=0.2), size=0.5)+
  scale_x_discrete(name="Population-adjusted NBR level\n(observation-dates of all taxa)",
                   labels=c("None","Low","Moderate","High","Very high")
                   )+
  scale_y_sqrt(name="Mean # debris items")+
  theme_bw()
ggsave(nbr_class_vs_trash, filename="NotForGit/Step3_3/nbr_class_vs_trash.png",
       height=4, width=5)

pop_class_vs_trash<-
  ggplot(nbr_hotspots_df2, aes(x=Pop_class , y=Trash))+
  geom_boxplot(outlier.colour = NA, fill="gray")+
  geom_point(position=position_jitter(height=0, width=0.2), size=0.5)+
  xlab("Population density class (quartile)")+
  scale_y_sqrt(name="Mean # debris items")+
  theme_bw()

pop_class_vs_trash_cont<-
  ggplot(nbr_hotspots_df2 %>%
           filter(SegLength_km>0.999), 
         aes(x=pop_density_per_km2_5km , y=Trash))+
  geom_point(size=.75)+
  geom_smooth()+
  scale_x_continuous("Population density (within 5 km)",
                     limits=c(0,5500)
                     # expand = expansion(mult = c(0, 0.08)) 
  )+
  scale_y_sqrt(name="Mean # debris items")+
  theme_bw()



ggsave(trash_vs_nbr_plot_1metric, filename="NotForGit/Step3_3/trash_vs_nbr_plot_1metric.png",
       width=3.5, height=3)

plot_dat<-nbr_hotspots_df2 %>% pivot_longer(cols=ends_with("class")) %>%
  mutate(name2=case_when(name=="NBR_class"~"Population-adjusted NBR level",
                         name=="Pop_class"~"Population density class (quartile)"),
         value=factor(value,
                      levels=c("No NBR","Low NBR","Moderate NBR","High NBR","Very high NBR",
                               "0_25","25_50","50_75","75_100"))
  )
levels(plot_dat$value)<-
  c("None","Low","Moderate","High","Very high",
    "1st","2nd","3rd","4th")

class_v_trash<-
  ggplot(data=plot_dat, aes(x=value, y=Trash))+
  geom_boxplot(outlier.colour = NA, fill="gray")+
  geom_point(position=position_jitter(height=0, width=0.2), size=0.5)+
  facet_wrap(~name2, scales="free_x")+
  theme_bw()+
  xlab("")+
  scale_y_sqrt(name="Mean # debris items")
ggsave(class_v_trash, filename="NotForGit/Step3_3/class_v_trash.png",
       width=6.5, height=4)

plot_dat2<-nbr_hotspots_df2 %>% 
  pivot_longer(cols=c(MetricResult_Dens, pop_density_per_km2_5km)) %>%
  mutate(name2=case_when(name=="MetricResult_Dens"~"# observer-dates/km2",
                         T~"Population density within 5 km"))
  
metric_v_trash<-ggplot(data=plot_dat2 %>%
         filter(SegLength_km>0.999), aes(x=value, y=Trash))+
  geom_point(size=0.75)+
  geom_smooth()+
  facet_wrap(~name2)+
  theme_bw()+
  scale_x_sqrt(name="", limits=c(0,5500), breaks=c(0,1000,2500,5000))+
  scale_y_sqrt(name="Mean # debris items")
ggsave(metric_v_trash, filename="NotForGit/Step3_3/metric_v_trash.png",
       width=6.5, height=4)

# metric_v_trash_alt<-
  ggplot(data=plot_dat2 %>%
         filter(SegLength_km>0.999,
                name2=="# observer-dates/km2"), aes(y=value, x=Trash))+
  geom_point(size=0.75)+
  geom_smooth()+
  # facet_wrap(~name2)+
  theme_bw()+
  scale_y_sqrt(name="", limits=c(0,5500), breaks=c(0,1000,2500,5000))+
  scale_x_sqrt(name="Mean # debris items")

library(ggpubr)

trash_boxplots<-ggarrange( pop_class_vs_trash, 
                           nbr_class_vs_trash), 
                           ncol=2, align="hv")
trash_scaterplots<-ggarrange(pop_class_vs_trash_cont, trash_vs_nbr_plot_1metric, ncol=2, align="hv")

ggsave(trash_boxplots, filename="NotForGit/Step3_3/trash_boxplots.png",
       height=3.5, width=6.5)
ggsave(trash_scaterplots, filename="NotForGit/Step3_3/trash_scaterplots.png",
       height=3.5, width=6.5)
