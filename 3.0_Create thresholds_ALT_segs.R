library(tidyverse)
library(sf)

############Import shapefiles############
smc_sheds_sf_0<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_sheds_sf<-  st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2026/SMC_Sheds_Merge.shp")%>%
  st_transform(crs=st_crs(smc_sheds_sf_0))
my_bb<-st_bbox(smc_sheds_sf)
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
popdens_seg_df<-read_csv("Data/stream_population_density_2020_wide.csv") %>%
  select(-stream_rowid ) %>%
  rename(Segment_ID=segment_id)

mx_sf<-st_read("S:/Mexico/BajaMexico.shp") %>%st_transform(crs=st_crs(smc_sheds_sf))

#Plot popdens 
pop_dens_plot_dat=smc_flowlines_sf %>%
  left_join(popdens_seg_df %>%
              select(Segment_ID, contains("density")))

pop_dens_seg_map<-ggplot()+
  geom_sf(data=counties_sf, color=NA, fill="gray50")+
  geom_sf(data=mx_sf, color="black", fill="gray50")+
  geom_sf(data=pop_dens_plot_dat, 
          aes(color=pop_density_per_km2_5km ))+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_fill_viridis_c(name="Population density\npeople per square km",
  # trans="sqrt")+
  scale_color_binned(name="Population density\npeople per square km",
                     breaks=c(0,1,10,100,1000,5000),
                     palette="viridis")+
  # palette=viridisLite::viridis(6))+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="black")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))
ggsave(pop_dens_seg_map, filename="NotForGit/Step3_0/pop_dens_map_segs.png",
       height=5.5, width=10)
ggsave(pop_dens_seg_map, filename="NotForGit/Step3_0/pop_dens_map_portrait_segs.png",
       height=5.5*.65, width=10*(.65))

# 
##############
#Empirical approach with bins
mod_dat<-smc_streams_metrics %>%
  left_join(popdens_seg_df) %>%
  filter(SegLength_km>0.999)

pop_quantiles_df<-mod_dat %>%
  filter(Group=="All taxa", Metric=="n_observations") %>%
  summarize(pop_q10=quantile(pop_density_per_km2_5km,0.1),
            pop_q20=quantile(pop_density_per_km2_5km,0.2),
            pop_q25=quantile(pop_density_per_km2_5km,0.25),
            pop_q30=quantile(pop_density_per_km2_5km,0.3),
            pop_q40=quantile(pop_density_per_km2_5km,0.4),
            pop_q50=quantile(pop_density_per_km2_5km,0.5),
            pop_q60=quantile(pop_density_per_km2_5km,0.6),
            pop_q70=quantile(pop_density_per_km2_5km,0.7),
            pop_q75=quantile(pop_density_per_km2_5km,0.75),
            pop_q80=quantile(pop_density_per_km2_5km,0.8),
            pop_q90=quantile(pop_density_per_km2_5km,0.9))
# 


junk<-mod_dat %>%
  left_join(popdens_seg_df %>%
              select(Segment_ID, pop_density_per_km2_5km)) %>%
  mutate(Pop_class=case_when(
    pop_density_per_km2_5km<pop_quantiles_df$pop_q25~"0_25",
    pop_density_per_km2_5km<pop_quantiles_df$pop_q50~"25_50",
    pop_density_per_km2_5km<pop_quantiles_df$pop_q75~"50_75",
    T~"75_100"),
  ) %>%
  group_by(Pop_class, Group, Metric) %>%
  summarize(
    
    nbr_q10=quantile(MetricResult_Dens[MetricResult_Dens>=0],.1),
    nbr_q25=quantile(MetricResult_Dens[MetricResult_Dens>=0],.25),
    nbr_q50=quantile(MetricResult_Dens[MetricResult_Dens>=0],.5),
    nbr_q75=quantile(MetricResult_Dens[MetricResult_Dens>=0],.75),
    nbr_q90=quantile(MetricResult_Dens[MetricResult_Dens>=0],.9),
    
    nbr_q10nz=quantile(MetricResult_Dens[MetricResult_Dens>0],.1),
    nbr_q25nz=quantile(MetricResult_Dens[MetricResult_Dens>0],.25),
    nbr_q50nz=quantile(MetricResult_Dens[MetricResult_Dens>0],.5),
    nbr_q75nz=quantile(MetricResult_Dens[MetricResult_Dens>0],.75),
    nbr_q90nz=quantile(MetricResult_Dens[MetricResult_Dens>0],.9),
  ) #%>% filter(Metric=="n_observations")  

junk2<-junk %>%
  select(Group, Metric, Pop_class, contains("nz")) %>%
  pivot_longer(cols=contains("nz")) %>%
  mutate(Pop_class.n=case_when(Pop_class=="0_25"~0.25,
                               Pop_class=="25_50"~0.5,
                               Pop_class=="50_75"~0.75,
                               T~1),
         NBR_class.n=case_when(str_detect(name,"q10")~0.1,
                               str_detect(name,"q25")~0.25,
                               str_detect(name,"q50")~0.5,
                               str_detect(name,"q75")~0.75,
                               T~0.9),
         MaxPopDens=case_when(Pop_class.n==0.25~pop_quantiles_df$pop_q25,
                              Pop_class.n==0.5~pop_quantiles_df$pop_q50,
                              Pop_class.n==0.75~pop_quantiles_df$pop_q75,
                              T~Inf
         ))
write_csv(junk2, "NotForGit/Step3_0/pop_dens_empirical_thresholds.csv")
# pop_dens_empirical_thresholds_formatted<-junk2 %>%
#   ungroup() %>%
#   transmute(Group, Metric,
#             `Population density quartile`=case_when(Pop_class.n==0.25~"1st",
#                                                     Pop_class.n==0.5~"2nd",
#                                                     Pop_class.n==0.75~"3rd",
#                                                     T~"4th"
#                                                     ),
#             `Population density (people/km2)`=round(MaxPopDens,1),
#             # `NBR quantile`=NBR_class.n,
#             NBR_class.n,
#             # `NBR threshold`=round(value,1)
#             value=round(value,1)
#             ) %>%
#   pivot_wider(names_from=NBR_class.n, values_from = value)

junk2 %>%
  filter(Group=="All taxa", Metric=="n_observers") %>%
  mutate(MaxPopDens=round(MaxPopDens,1),
         ColLab=case_when(
           Pop_class=="0_25"~paste0("Below 25th percentile (",MaxPopDens,")"),
           Pop_class=="25_50"~paste0("Below 50th percentile (",MaxPopDens,")"),
           Pop_class=="50_75"~paste0("Below 75th percentile (",MaxPopDens,")"),
           T~"Above 75th precentile")) %>%
  ungroup() %>%
  select(ColLab, NBR_class.n, value) %>%
  pivot_wider(names_from=ColLab, values_from = value) %>%
  write_csv("NotForGit/Step3_0/NBR_empirical_thresholds.csv")



plot_dat<-junk2 %>%
  filter(Group=="All taxa", Metric=="n_observers",
         name!="nbr_q50nz") %>%
  mutate(value=case_when(name=="nbr_q10nz"~0,T~value),
         NBR_class.n=case_when(name=="nbr_q10nz"~0,T~NBR_class.n),)
empirical_thresholds_plot<-ggplot(data=plot_dat,
                                  aes(x=Pop_class, y=value, color=NBR_class.n %>% as.factor()))+
  geom_path(aes(group=NBR_class.n))+
  geom_point()+
  scale_color_brewer(palette="Set1", name="NBR threshold")+
  ylab("NBR threshold")+
  xlab("Population density quartile")+
  ggtitle("All taxa",subtitle = "# observers")+
  coord_cartesian(ylim=c(0,250))
ggsave(empirical_thresholds_plot, filename="NotForGit/Step3_0/empirical_thresholds_plot.png",
       height=4, width=6)

ggplot(data=junk2 %>%
         filter(Group=="All taxa", Metric=="n_observers"),
       aes(x=NBR_class.n, y=value, color=Pop_class))+
  geom_path()+geom_point()+
  ylab("Threshold")+
  scale_x_continuous(breaks=c(0,0.1,0.25,0.5,0.75,0.9,1),
                     name="NBR metric percentile")+
  ggtitle("All taxa",subtitle = "# observers")


smc_streams_metrics2<-smc_streams_metrics %>%
  left_join(popdens_seg_df %>%
              select(Segment_ID, pop_density_per_km2_5km)) %>%
  mutate(Pop_class=case_when(
    pop_density_per_km2_5km<pop_quantiles_df$pop_q25~"0_25",
    pop_density_per_km2_5km<pop_quantiles_df$pop_q50~"25_50",
    pop_density_per_km2_5km<pop_quantiles_df$pop_q75~"50_75",
    T~"75_100")  ) %>%
  inner_join(
    junk %>%
      select(Pop_class, Group, Metric, contains("nz"))
  ) %>%
  mutate(NBR_class=case_when(
    MetricResult_Dens==0~"No NBR",
    MetricResult_Dens<nbr_q25nz ~"Low NBR",
    MetricResult_Dens<nbr_q75nz ~"Moderate NBR",
    MetricResult_Dens<nbr_q90nz ~"High NBR",
    T~"Very high NBR"),
    NBR_class=factor(NBR_class,
                     levels=c("No NBR","Low NBR","Moderate NBR", "High NBR","Very high NBR")))

nbr_hotspots_alltaxa<-smc_streams_metrics2 %>%
  filter(Group=="All taxa",
         Metric %in% c("n_observers","n_observations","n_observer_dates")
  ) %>%
  select(Segment_ID, COMID, SegLength_km,
         Group, Metric, MetricResult_Dens,
         pop_density_per_km2_5km,
         Pop_class,NBR_class, starts_with("nbr")  ) 

write_csv(nbr_hotspots_alltaxa,"NotForGit/Step3_0/nbr_hotspots_alltaxa.csv")


smc_flowlines_sf2<-smc_flowlines_sf %>%
  left_join(smc_streams_metrics2 %>%
              filter(Group=="All taxa", Metric=="n_observers") %>%
              select(Segment_ID, Group, Metric, MetricResult_Dens,
                     pop_density_per_km2_5km, Pop_class,
                     NBR_class,contains("nz")
              ))
st_write(smc_flowlines_sf2,"NotForGit/Step3_0/Shapefiles/smc_flowlines_nbr.shp")


nbr_segments_map_all_taxa<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=NBR_class),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_size_manual(
  #   name="NBR activity\n(relative to\npopulation density)",
  #   values=c(0.25,0.25,0.25,0.25,0.25)
  # )+
  scale_color_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
    )
  )+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))
ggsave(nbr_segments_map_all_taxa, filename="NotForGit/Step3_0/nbr_segments_map_all_taxa_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(nbr_segments_map_all_taxa, filename="NotForGit/Step3_0/nbr_segments_map_all_taxa_portrait.png",
       height=5.5*.65, width=10*(.65))


nbr_segments_map_all_taxa_faceted<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2 %>% select(-NBR_class, -Pop_class), 
          color="gray"
  )+
  geom_sf(data=smc_flowlines_sf2 ,
          aes(color=NBR_class),
          key_glyph = 'rect')+
  facet_wrap(~Pop_class)+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
    )
  )+
  geom_sf(data=counties_sf, fill=NA, color="black")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))
ggsave(nbr_segments_map_all_taxa_faceted, filename="NotForGit/Step3_0/nbr_segments_map_all_taxa_faceted.png",
       # height=5.5, width=10)
       height=5.5,width=10)

####SMC summary###
smc_segments_summary<-
  smc_streams_metrics2 %>%
  group_by(SMCShed="South Coast", Group, Metric, NBR_class    ) %>%
  summarise(
    Class_length = sum(SegLength_km)) %>%
  mutate(TotLength_km=25601.09) %>%
  ungroup() %>%
  bind_rows(
    
    smc_streams_metrics2 %>%
      group_by(SMCShed, Group, Metric, NBR_class,TotLength_km    ) %>%
      summarise(Class_length = sum(SegLength_km)) %>%
      ungroup() 
  ) %>%
  
  
  mutate(Pct_Length=100*Class_length/TotLength_km,
         SMCShed = factor(SMCShed,
                          levels=c(
                            "South Coast",
                            "Santa Barbara Coast",
                            "Ventura","Santa Clara","Calleguas",
                            "Santa Monica Bay", "Los Angeles","San Gabriel",
                            "Dominguez Channel",
                            "Lower Santa Ana", "Middle Santa Ana","Upper Santa Ana", "San Jacinto",
                            "San Juan", "Northern San Diego","Central San Diego","Mission Bay and San Diego River","Southern San Diego",
                            "Northern Channel Islands","Southern Channel Islands"
                          ) %>% rev()
         ))
smcshed_segments_stacked_bar<-
  ggplot(data=smc_segments_summary %>%
           filter(Group=="All taxa", Metric=="n_observers"), 
         aes(x=SMCShed , y=Pct_Length))+
  geom_bar(aes(fill=NBR_class), position=position_stack(), stat="identity")+
  scale_fill_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
    )  )+
  coord_flip()+
  theme_bw()+
  xlab("")+ylab("% stream-length")
ggsave(smcshed_segments_stacked_bar, filename="NotForGit/Step3_0/smcshed_segments_stacked_bar.png",
       height=6, width=6.5)

smc_segments_summary %>%
  filter(Group=="All taxa", Metric=="n_observers") %>%
  mutate(NBR2 = case_when(NBR_class %in% c("High NBR", "Very high NBR")~"HOVH",
                          NBR_class %in% c("No NBR", "Low NBR")~"NOLH",
                          T~"Mod")) %>%
  group_by(SMCShed,NBR2) %>%
  summarise(Pct_Length=sum(Pct_Length)) %>%
  filter(NBR2=="HOVH") %>%
  arrange(-Pct_Length)

####Make combined plot to show impact of pop adjustments####

#Ventura
ven_bb<-smc_sheds_sf%>% 
  filter(SMC_Name=="Ventura") %>%
  st_bbox()

ven_nbr_hotspots_adjusted_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=NBR_class),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
    )
  )+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(ven_bb[c(1,3)]),
           ylim=c(ven_bb[c(2,4)]))


ven_nbr_hotspots_unadjusted_map<-
  ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=MetricResult_Dens),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_binned(breaks=c(0,1, 10,50,100),
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(ven_bb[c(1,3)]),
           ylim=c(ven_bb[c(2,4)]))

library(ggpubr)

ventura_dual_map<-ggarrange(ven_nbr_hotspots_unadjusted_map,
                            ven_nbr_hotspots_adjusted_map,
                            ncol=1)
ggsave(ventura_dual_map,
       filename="NotForGit/Step3_0/ventura_dual_map.png",
       width=6.5, height=8)


#la
lar_bb<-smc_sheds_sf%>% 
  filter(SMC_Name=="Los Angeles") %>%
  # filter(SMC_Name=="San Gabriel") %>%
  st_bbox()

lar_nbr_hotspots_adjusted_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=NBR_class),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
    )
  )+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(lar_bb[c(1,3)]),
           ylim=c(lar_bb[c(2,4)]))


lar_nbr_hotspots_unadjusted_map<-
  ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=MetricResult_Dens),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_binned(breaks=c(0,1, 10,50,100),
                     palette=c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921"),
                     # show.limits = TRUE,
                     name ="Observers\nper sqkm")+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "bottom")+
  coord_sf(xlim=c(lar_bb[c(1,3)]),
           ylim=c(lar_bb[c(2,4)]))

losangeles_dual_map<-ggarrange(lar_nbr_hotspots_unadjusted_map,
                               lar_nbr_hotspots_adjusted_map,
                               ncol=1)
ggsave(losangeles_dual_map,
       filename="NotForGit/Step3_0/losangeles_dual_map.png",
       width=6.5, height=8)

unadj_map<-ggarrange(ven_nbr_hotspots_unadjusted_map,lar_nbr_hotspots_unadjusted_map,
                     ncol=2, common.legend = T, legend="bottom")
unadj_map2<-annotate_figure(unadj_map,
                top = text_grob("NBR (unadjusted for population)", 
                                color = "black", 
                                face = "bold", 
                                size = 12))
adj_map<-ggarrange(ven_nbr_hotspots_adjusted_map,lar_nbr_hotspots_adjusted_map,
                   ncol=2, common.legend = T, legend="bottom")
adj_map2<-annotate_figure(adj_map,
                top = text_grob("NBR (adjusted for population)", 
                                color = "black", 
                                face = "bold", 
                                size = 12))

la_ven_quad_map<-ggarrange(  unadj_map2,adj_map2, nrow=2)
ggsave(la_ven_quad_map,
       filename="NotForGit/Step3_0/la_ven_quad_map.png",
       width=6.5, height=8)

# 
# ##################END##################
# all_mets_df_sub <- mod_dat %>%
#   select(Group, Metric) %>%
#   distinct() %>%
#   # filter(Metric=="n_observations", Group=="All taxa")
#   filter(Metric %in% c("n_observers","n_observations","n_observer_dates"),
#          Group=="All taxa")
# # 
# 
# library(qgam)
# 
# gc()
# all_mets_sub_mods_gam_q50<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
#        qu=0.5,
#        data=dat.i,
#        multicore = TRUE,   # Enables parallel processing
#        ncores = 3)
# })
# saveRDS(all_mets_sub_mods_gam_q50, "NotForGit/Step3_0/all_mets_sub_mods_gam_q50.RDS")
# all_mets_sub_mods_gam_q50<-readRDS("NotForGit/Step3_0/all_mets_sub_mods_gam_q50.RDS")
# 
# gc()
# all_mets_sub_mods_gam_q25<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
#        # method="REML",
#        qu=0.25,
#        data=dat.i)
# })
# saveRDS(all_mets_sub_mods_gam_q25, "NotForGit/Step3_0/all_mets_sub_mods_gam_q25.RDS")
# all_mets_sub_mods_gam_q25<-readRDS("NotForGit/Step3_0/all_mets_sub_mods_gam_q25.RDS")
# 
# gc()
# all_mets_sub_mods_gam_q75<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
#        # method="REML",
#        qu=0.75,
#        data=dat.i)
# })
# saveRDS(all_mets_sub_mods_gam_q75, "NotForGit/Step3_0/all_mets_sub_mods_gam_q75.RDS")
# all_mets_sub_mods_gam_q75<-readRDS("NotForGit/Step3_0/all_mets_sub_mods_gam_q75.RDS")
# 
# gc()
# all_mets_sub_mods_gam_q10<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
#        # method="REML",
#        qu=0.10,
#        data=dat.i)
# })
# saveRDS(all_mets_sub_mods_gam_q10, "NotForGit/Step3_0/all_mets_sub_mods_gam_q10.RDS")
# all_mets_sub_mods_gam_q10<-readRDS("NotForGit/Step3_0/all_mets_sub_mods_gam_q10.RDS")
# 
# gc()
# all_mets_sub_mods_gam_q90<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   qgam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
#        # method="REML",
#        qu=0.90,
#        data=dat.i)
# })
# saveRDS(all_mets_sub_mods_gam_q90, "NotForGit/Step3_0/all_mets_sub_mods_gam_q90.RDS")
# all_mets_sub_mods_gam_q90<-readRDS("NotForGit/Step3_0/all_mets_sub_mods_gam_q90.RDS")
# # all_mets_sub_mods_gam_huc12_predicted<-lapply(1:nrow(all_mets_df_sub), function(i){
# #   group.i=all_mets_df_sub$Group[i]
# #   met.i=all_mets_df_sub$Metric[i]
# #   print(paste(i, group.i, met.i))
# #   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
# #   mod25.i=all_mets_sub_mods_gam_huc12_q25[[i]]
# #   mod75.i=all_mets_sub_mods_gam_huc12_q75[[i]]
# #   mod.i=all_mets_sub_mods_gam_huc12[[i]]
# #   dat.i$Predicted25=predict(mod25.i, newdata=dat.i, type="response") 
# #   dat.i$Predicted50=predict(mod.i, newdata=dat.i, type="response")
# #   dat.i$Predicted75=predict(mod75.i, newdata=dat.i, type="response")
# #   dat.i
# # }) %>% bind_rows()
# 
# all_mets_sub_mods_gam_huc12_predicted<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   # dat.i %>% as_tibble() %>% group_by(HUC12) %>% tally() %>% filter(n>1) %>% print()
#   mod25.i=all_mets_sub_mods_gam_huc12_q25[[i]]
#   mod75.i=all_mets_sub_mods_gam_huc12_q75[[i]]
#   mod50.i=all_mets_sub_mods_gam_huc12_q50[[i]]
#   mod10.i=all_mets_sub_mods_gam_huc12_q10[[i]]
#   mod90.i=all_mets_sub_mods_gam_huc12_q90[[i]]
#   dat.i$Predicted10=predict(mod10.i, newdata=dat.i, type="response") %>% as.numeric()
#   dat.i$Predicted25=predict(mod25.i, newdata=dat.i, type="response") %>% as.numeric()
#   # browser()
#   dat.i$Predicted50=predict(mod50.i, newdata=dat.i, type="response") %>% as.numeric()
#   dat.i$Predicted75=predict(mod75.i, newdata=dat.i, type="response") %>% as.numeric()
#   dat.i$Predicted90=predict(mod90.i, newdata=dat.i, type="response") %>% as.numeric()
#   dat.i
# }) %>% bind_rows()
# 
# # all_mets_sub_mods_gam_huc12_predicted %>% print(width=Inf)
# 
# 
# plot_dat<-all_mets_sub_mods_gam_huc12_predicted %>%
#   filter(Group=="All taxa", Metric=="n_observers") %>%
#   arrange(popdens_huc12_2024) %>%
#   mutate(Predicted10=case_when(Predicted10<0~0, T~Predicted10))
# 
# pop_huc12_plot<-
#   ggplot(data=plot_dat, aes(x=popdens_huc12_2024, y=Predicted50))+
#   geom_point(aes(y=MeanMetric_Dens), size=0.5)+
#   geom_ribbon(aes(ymin=Predicted10, ymax=Predicted90), alpha=0.25)+
#   geom_ribbon(aes(ymin=Predicted25, ymax=Predicted75), alpha=0.25)+
#   geom_path(color="blue")+
#   scale_y_sqrt("# observers per sq-km\nAll taxa")+
#   # scale_x_sqrt()+
#   theme_bw()+
#   xlab("Population density (HUC12)\nPeople per sq-km")
# ggsave(pop_huc12_plot, filename="NotForGit/Step3_0/pop_huc12_plot.png",
#        width=6.5, height=4)
# 
# plot_labels_df<-tibble(
#   x=c(4500, 4500, 4500,4500,4500,4500), 
#   y=c(1, 10,30,75,200,350), 
#   label=c("Very low","Low", "Mod. low", "Mod. high","High","Very high")
# )
# pop_huc12_plot_labeled<-pop_huc12_plot +
#   geom_text(data=plot_labels_df, aes(x=x,y=y, label=label))
# ggsave(pop_huc12_plot_labeled, filename="NotForGit/Step3_0/pop_huc12_plot_labeled.png",
#        width=6.5, height=4)
# 
# plot_dat_nonmonthly<-all_mets_sub_mods_gam_huc12_predicted %>%
#   filter(!str_detect(Metric, "_m")) %>%
#   arrange(popdens_huc12_2024)
# 
# plot_dat_nonmonthly$Group=factor(plot_dat_nonmonthly$Group, 
#                                  levels=c("All taxa","All Birds","All Herps","Fish","All Invertebrates"))
# # levels(plot_dat_nonmonthly$Group)[5]<-"All Invertebrates"
# # ggplot(data=plot_dat_nonmonthly, aes(x=popdens_huc12_2024))+
# #   # geom_ribbon(aes(ymin=Predicted25, ymax=Predicted75),alpha=0.25)+
# #   # geom_ribbon(aes(ymin=Predicted10, ymax=Predicted90),alpha=0.25)+
# #   geom_path(aes(y=Predicted50), color="blue")+
# #   facet_grid(Group~Metric, scales="free_y")
# 
# modeled_responses_qt50<-ggplot(data=plot_dat_nonmonthly, aes(x=popdens_huc12_2024))+
#   # geom_ribbon(aes(ymin=Predicted25, ymax=Predicted75),alpha=0.25)+
#   # geom_ribbon(aes(ymin=Predicted10, ymax=Predicted90),alpha=0.25)+
#   geom_path(aes(y=Predicted50, color=Metric, linetype=Metric))+
#   scale_color_brewer(palette="Set2",
#                      labels=c("# observations","# observer-dates","# observers"))+
#   scale_linetype_discrete(labels=c("# observations","# observer-dates","# observers"))+
#   facet_wrap(~Group, scales="free_y")+
#   theme_bw()+
#   theme(legend.position="bottom")+
#   xlab("Population density (HUC12)\nPeople per sq-km")+
#   ylab("Predicted median metric value")
# ggsave(modeled_responses_qt50, filename="NotForGit/Step3_0/modeled_responses_qt50.png",
#        width=6.5, height=4.5)
# 
# 
# # 
# # all_mets_sub_mods_qgam_summary<-all_mets_df_sub %>%
# #   bind_cols(
# #     lapply(all_mets_sub_mods_gam_huc12_q10, function(x){
# #       broom::glance(x) %>%        rename_with(~paste0(., "_q10"))
# #     }) %>%      bind_rows(),
# #     lapply(all_mets_sub_mods_gam_huc12_q25, function(x){
# #       broom::glance(x) %>%    select(-npar, -nobs) %>%    rename_with(~paste0(., "_q25"))
# #     }) %>%      bind_rows(),
# #     lapply(all_mets_sub_mods_gam_huc12_q50, function(x){
# #       broom::glance(x) %>%     select(-npar, -nobs) %>%     rename_with(~paste0(., "_q50"))
# #     }) %>%      bind_rows(),
# #     lapply(all_mets_sub_mods_gam_huc12_q75, function(x){
# #       broom::glance(x) %>%       select(-npar, -nobs) %>%   rename_with(~paste0(., "_q75"))
# #     }) %>%      bind_rows(),
# #     lapply(all_mets_sub_mods_gam_huc12_q90, function(x){
# #       broom::glance(x) %>%      select(-npar, -nobs) %>%    rename_with(~paste0(., "_q90"))
# #     }) %>%      bind_rows(),
# #   )
# 
# which(all_mets_df_sub$Group=="All taxa" & 
#         all_mets_df_sub$Metric=="n_observer_dates_m11")
# which(all_mets_sub_mods_qgam_summary$p.value<0)
# 
# junk<-all_mets_sub_mods_gam_huc12_q10[[42]]
# broom::glance(junk)
# broom::tidy(junk)
# junk2<-summary(junk)
# junk2$s.pv
# 
# all_mets_sub_mods_qgam_summary<-
#   all_mets_df_sub %>%
#   mutate(qt=0.1)    %>%
#   bind_cols(
#     lapply(all_mets_sub_mods_gam_huc12_q10, function(x){
#       x1=broom::glance(x) 
#       x2=broom::tidy(x)
#       bind_cols(x1, x2)
#     }) %>%  bind_rows()) %>%
#   bind_rows(
#     all_mets_df_sub %>%
#       mutate(qt=0.25)    %>%
#       bind_cols(
#         lapply(all_mets_sub_mods_gam_huc12_q25, function(x){
#           x1=broom::glance(x) 
#           x2=broom::tidy(x)
#           bind_cols(x1, x2)
#         }) %>% bind_rows()),
#     all_mets_df_sub %>%
#       mutate(qt=0.5)    %>%
#       bind_cols(
#         lapply(all_mets_sub_mods_gam_huc12_q50, function(x){
#           x1=broom::glance(x) 
#           x2=broom::tidy(x)
#           bind_cols(x1, x2)
#         }) %>% bind_rows()),
#     all_mets_df_sub %>%
#       mutate(qt=0.75)    %>%
#       bind_cols(
#         lapply(all_mets_sub_mods_gam_huc12_q75, function(x){
#           x1=broom::glance(x) 
#           x2=broom::tidy(x)
#           bind_cols(x1, x2)
#         }) %>% bind_rows()),
#     all_mets_df_sub %>%
#       mutate(qt=0.90)    %>%
#       bind_cols(
#         lapply(all_mets_sub_mods_gam_huc12_q90, function(x){
#           x1=broom::glance(x) 
#           x2=broom::tidy(x)
#           bind_cols(x1, x2)
#         }) %>% bind_rows())
#   )
# write_csv(all_mets_sub_mods_qgam_summary, "NotForGit/Step3_0/all_mets_sub_mods_qgam_summary.csv")
# 
# all_mets_sub_mods_qgam_summary %>%
#   filter(Group=="All taxa") %>% 
#   clipr::write_clip()
# 
# all_mets_sub_mods_qgam_summary %>%
#   group_by(qt) %>%
#   summarize(tot_mods=length(qt),
#             better_mods=sum(adj.r.squared>=.))
# 
# all_mets_sub_mods_qgam_summary %>%
#   filter(adj.r.squared>=0.2)%>%
#   print(width=Inf)
# 
# all_mets_sub_mods_qgam_summary  %>%
#   filter(p.value<0.00005)%>%
#   print(width=Inf)
# 
# #How many had
# 
# 
# #####Apply thresholds#####
# all_mets_sub_mods_gam_huc12_predicted<-
#   all_mets_sub_mods_gam_huc12_predicted %>%
#   mutate(
#     huc_hotspot=case_when(
#       MeanMetric_Dens > Predicted90~"Hot spot",
#       # MeanMetric_Dens > Predicted50~"Moderately high",
#       MeanMetric_Dens > Predicted25~"Typical",
#       T~"Cold spot",
#     ),
#     huc_hotspot=factor(huc_hotspot, levels=c("Hot spot", "Typical","Cold spot")),
#     huc_performance = 
#       case_when(
#         MeanMetric_Dens > Predicted90~"Very high",
#         MeanMetric_Dens > Predicted75~"High",
#         MeanMetric_Dens > Predicted50~"Moderately high",
#         MeanMetric_Dens > Predicted25~"Moderately low",
#         # MeanMetric_Dens > Predicted25~"Expected",
#         MeanMetric_Dens > Predicted10~"Low",
#         T~"Very low"),
#     huc_performance= factor(huc_performance,levels=c("Very low","Low", 
#                                                      "Moderately low","Moderately high",
#                                                      # "Expected",
#                                                      "High","Very high")))
# 
# 
# all_mets_sub_mods_gam_huc12_predicted %>%
#   filter(Group=="All taxa", Metric=="n_observers",
#          
#          huc_performance=="Very high") %>%
#   select(HUC12, HUC12_name, MeanMetric_Dens) %>%
#   inner_join(
#     smc_flowlines_sf %>%
#       as_tibble() %>%
#       select(HUC12, HUC8_simp) %>%
#       distinct()
#   ) %>%
#   inner_join(popdens_df) %>%
#   clipr::write_clip()
# 
# map_huc_dat<-smc_huc12_sf %>%
#   left_join(all_mets_sub_mods_gam_huc12_predicted %>%
#               filter(Group=="All taxa", Metric=="n_observers") %>%
#               select(HUC12, MeanMetric_Dens, starts_with("Predi"),
#                      huc_performance, huc_hotspot)) %>%
#   mutate(
#     huc_performance = case_when(
#       is.na(huc_performance) ~ "Very low",
#       T~huc_performance),
#     huc_hotspot=case_when(is.na(huc_hotspot)~"Cold spot",
#                           T~huc_hotspot),
#     
#     huc_performance= factor(huc_performance,levels=c("Very low","Low", 
#                                                      "Moderately low","Moderately high",
#                                                      # "Expected",
#                                                      "High","Very high")))
# 
# 
# 
# map_hucs_NBR<-ggplot()+
#   geom_sf(data=counties_sf, color=NA)+
#   geom_sf(data=map_huc_dat %>%
#             st_intersection(smc_sheds_sf), 
#           aes(fill=huc_performance), color=NA)+
#   geom_sf(data=smc_sheds_sf, fill=NA)+
#   scale_fill_viridis_d(name="NBR activity\nwithin HUC12\n(relative to\npopulation density)",
#                        option="inferno",
#                        begin=0.2, end=0.9
#                        # labels=c("")
#   )+
#   geom_sf(data=counties_sf, fill=NA, color="black")+
#   theme_bw()+
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank())+
#   coord_sf(xlim=c(my_bb[c(1,3)]),
#            ylim=c(my_bb[c(2,4)]))
# ggsave(map_hucs_NBR, filename="NotForGit/Step3_0/map_hucs_NBR_landscape.png",
#        height=5.5, width=10)
# ggsave(map_hucs_NBR, filename="NotForGit/Step3_0/map_hucs_NBR_portrait.png",
#        height=5.5*.65, width=10*(.65))
# 
# 
# map_huc_dat2<-
#   smc_flowlines_sf %>%
#   # filter(Group=="All taxa", Metric=="n_observers")
#   left_join(map_huc_dat %>% as_tibble() %>%
#               select(HUC12,  huc_performance))
# 
# 
# map_hucs_flowlines_NBR<-ggplot()+
#   geom_sf(data=counties_sf, color=NA)+
#   # geom_sf(data=map_huc_dat %>% st_intersection(smc_sheds_sf), 
#   # aes(fill=huc_performance), color=NA)+
#   geom_sf(data=smc_sheds_sf)+
#   geom_sf(data=map_huc_dat2,
#           aes(color=huc_performance),
#           size=0.5)+
#   scale_color_viridis_d(name="NBR activity\nwithin HUC12\n(relative to\npopulation density)",
#                         option="inferno",
#                         begin=0.2, end=0.9)+
#   geom_sf(data=smc_sheds_sf, fill=NA)+
#   geom_sf(data=counties_sf, fill=NA, color="black")+
#   theme_bw()+
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank())+
#   coord_sf(xlim=c(my_bb[c(1,3)]),
#            ylim=c(my_bb[c(2,4)]))
# ggsave(map_hucs_flowlines_NBR, filename="NotForGit/Step3_0/map_hucs_flowlines_NBR_landscape.png",
#        height=5.5, width=10)
# ggsave(map_hucs_flowlines_NBR, filename="NotForGit/Step3_0/map_hucs_flowlines_NBR_portrait.png",
#        height=5.5*.65, width=10*(.65))
# 
# smc_flowlines_sf[43,"Segment_ID"]
# map_huc_dat %>% as_tibble() %>% filter(Segment_ID=="17562532_1")
# map_huc_dat %>% as_tibble() %>% group_by(HUC12) %>% tally() %>% filter(n>1)
# # 
# 
# #######Stacked bar#######
# huc12_lengths_df<-smc_flowlines_sf %>%
#   as_tibble() %>%
#   group_by(HUC12, HUC12_name, SMCShed) %>%
#   summarize(TotLength_HUC12_km = sum(SegLength_km)) %>%
#   ungroup()
# 
# huc12_lengths_df$SMCShed2=
#   factor(huc12_lengths_df$SMCShed,
#          levels=c(
#            "Santa Barbara Coast",
#            "Ventura","Santa Clara", "Calleguas","Santa Monica Bay", "Dominguez Channel", "Los Angeles","San Gabriel",
#            "Lower Santa Ana","Middle Santa Ana","Upper Santa Ana", "San Jacinto",
#            "San Juan","Northern San Diego","Central San Diego","Mission Bay and San Diego River","Southern San Diego",
#            "Northern Channel Islands","Southern Channel Islands"
#            
#          ))
# 
# stack_bar_df<-map_huc_dat%>%
#   as_tibble() %>%
#   inner_join(huc12_lengths_df) %>%
#   select(HUC12, HUC12_name, SMCShed,SMCShed2,
#          TotLength_HUC12_km,
#          huc_performance
#   ) %>%
#   group_by(SMCShed) %>%
#   mutate(
#     TotLen_km=sum(TotLength_HUC12_km)
#   ) %>%
#   group_by(SMCShed, SMCShed2, TotLen_km, huc_performance) %>%
#   summarize(NBR_len_km = sum(TotLength_HUC12_km )) %>%
#   ungroup() 
# 
# 
# stack_bar_plot<-ggplot(data=stack_bar_df, aes(x=SMCShed2, y=100*NBR_len_km/TotLen_km , fill=huc_performance))+
#   geom_bar(position=position_stack(), stat="identity")+
#   scale_fill_viridis_d(name="NBR activity\nwithin HUC12\n(relative to\npopulation density)",
#                        option="inferno",
#                        begin=0.2, end=0.9)+
#   scale_x_discrete(limits=rev, name="Watershed")+
#   ylab("% stream length")+
#   coord_flip()+
#   theme_bw()
# ggsave(stack_bar_plot, filename="NotForGit/Step3_0/stack_bar_plot.png",
#        height=6, width=6.5)
# 
# ###########Export shapefile###########
# 
# 
# smc_flowlines_sf3<-smc_flowlines_sf %>%
#   left_join(
#     smc_streams_metrics2 %>%
#               filter(Group=="All taxa", 
#                      Metric %in% c("n_observer_dates")) %>%
#               select(Segment_ID, pop_density_per_km2_5km, Pop_class,
#                      Group, Metric, MetricResult_Dens,
#                      NBR_class
#               )
#  )
# st_write(smc_flowlines_sf3,"NotForGit/Step3_0/Shapefiles/smc_nbr_obsdates.shp")
