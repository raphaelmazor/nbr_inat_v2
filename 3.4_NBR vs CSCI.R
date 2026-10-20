library(tidyverse)
library(sf)
nbr_hotspots_alltaxa<-read_csv("NotForGit/Step3_0/nbr_hotspots_alltaxa.csv")
scape_df<-read_csv("C:/Users/Raphaelm/Documents/Repositories/SMC_misc/Shapefiles/scape.csv") %>%
  select(COMID, CSCI=qt50, Ref10) %>%
  mutate(CSCI_class=case_when(CSCI<0.63~"Very likely altered",
                              CSCI<0.79~"Likely altered",
                              CSCI<0.92~"Possibly altered",
                              CSCI>=0.92~"Likely intact") %>% 
           factor(levels=c("Very likely altered","Likely altered","Possibly altered","Likely intact")),
         CSCI_class2=case_when(CSCI<0.63~"Very likely altered",
                               CSCI<0.79~"Likely altered",
                               CSCI>=0.79~"Likely intact") %>%
           factor(levels=c("Very likely altered","Likely altered","Likely intact"))
  )

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

mx_sf<-st_read("S:/Mexico/BajaMexico.shp") %>%st_transform(crs=st_crs(smc_sheds_sf))


smc_flowlines_sf2<-smc_flowlines_sf %>%
  left_join(nbr_hotspots_alltaxa %>%
              filter(Group=="All taxa", Metric=="n_observers") %>%
              select(Segment_ID, Group, Metric, MetricResult_Dens,
                     pop_density_per_km2_5km, Pop_class,
                     NBR_class,contains("nz")
              )) %>%
  mutate(NBR_class=factor(NBR_class, 
                          levels=c("No NBR","Low NBR","Moderate NBR","High NBR", "Very high NBR")),
         NBR_class2=case_when(NBR_class=="Moderate NBR"~"Low NBR",
                              NBR_class=="High NBR"~"Moderate NBR",
                              NBR_class=="Very high NBR"~"High NBR",
                              T~NBR_class) %>%
           factor(levels=c("No NBR","Low NBR","Moderate NBR","High NBR")),
         NBR_class3=case_when(NBR_class=="No NBR"~"Low",
                              NBR_class=="Low NBR"~"Mod",
                              # NBR_class=="Very high NBR"~"High NBR",
                              T~"High") %>%
           factor(levels=c("Low","Mod","High")),
           ) %>%
  left_join(scape_df) %>%
  mutate(ComboClass = case_when(CSCI>=0.79 & NBR_class2 %in% c("Low NBR","Moderate NBR","High NBR")~"Both",
                                CSCI<0.79 & NBR_class2 %in% c("Low NBR","Moderate NBR","High NBR")~"NBR only",
                                CSCI>=0.79 & !NBR_class2 %in% c("Low NBR","Moderate NBR","High NBR")~"Aquatic life only",
                                T~"Neither") %>% 
           factor(levels=c("Both","NBR only","Aquatic life only", "Neither"))
  )
# # nbr_map<-
#   ggplot()+
#   geom_sf(data=counties_sf, fill="gray50")+
#   geom_sf(data=mx_sf, fill="gray50")+
#   geom_sf(data=smc_flowlines_sf2, 
#           aes(color=NBR_class),
#           size=0.25,
#           key_glyph = 'rect')+
#   geom_sf(data=smc_sheds_sf, fill=NA)+
#   scale_color_manual(
#     name="NBR activity\n(relative to\npopulation density)",
#     values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")
#     )
#   )+
#   geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
#   geom_sf(data=counties_sf, fill=NA, color="gray25")+
#   theme_bw()+
#   theme(panel.grid = element_blank(),
#         axis.text=element_blank(),
#         axis.ticks=element_blank(),
#         panel.background = element_rect(fill="lightblue"),
#         strip.background = element_blank(),
#         legend.position = "bottom")+
#   coord_sf(xlim=c(my_bb[c(1,3)]),
#            ylim=c(my_bb[c(2,4)]))

nbr_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=NBR_class2),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_manual(
    name="NBR activity\n(relative to\npopulation density)",
    values=c(c("#2b547e","#46327e","#ed7953","#f0f921")
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


ggsave(nbr_map, filename="NotForGit/Step3_4/nbr_map_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(nbr_map, filename="NotForGit/Step3_4/nbr_map_portrait.png",
       height=5.5*.65, width=10*(.65))

csci_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=CSCI_class),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_color_manual(    name="NBR activity\n(relative to\npopulation density)",
  #   values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")    )  )+
  scale_color_manual(    name="CSCI class",
                         values=c(c("#2b547e","#46327e","#ed7953","#f0f921")    ),
                         na.translate=FALSE)+
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

ggsave(csci_map, filename="NotForGit/Step3_4/csci_map_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(csci_map, filename="NotForGit/Step3_4/csci_map_portrait.png",
       height=5.5*.65, width=10*(.65))

library(ggpubr)

two_panel_map<-ggarrange(nbr_map+
                           theme(legend.position = "right")+
                           ggtitle("Human use"), 
                         csci_map+
                           theme(legend.position = "right")+
                           ggtitle("Aquatic life"), ncol=1)
ggsave(two_panel_map, filename="NotForGit/Step3_4/two_panel_map.png",
       height=5.5*.65*2, width=10*(.65))

######Bivariate map######
library(biscale)

mydf<-smc_flowlines_sf2 %>%
  bi_class(x=NBR_class2, y=CSCI_class, dim=4)

bivariate_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=mydf, 
          aes(color=bi_class),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  bi_scale_color(dim=4, pal="BlueOr")+
  # scale_color_manual(    name="CSCI class",
                         # values=c(c("#2b547e","#46327e","#ed7953","#f0f921")    ),
                         # na.translate=FALSE)+
  geom_sf(data=smc_sheds_sf, fill=NA, color="gray75", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_void()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "none"
        )+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))
bivariate_map
bivar_legend<-bi_legend(pal="BlueOr", dim=4, xlab="NBR", ylab="CSCI")
library(cowplot)

bivariate_map_plot<-ggdraw() +
  draw_plot(bivariate_map, 0, 0, 1, 1) +
  draw_plot(bivar_legend+theme(plot.background = element_rect(fill="lightblue")),
            0.02, .02, 0.3, 0.3)
ggsave(bivariate_map_plot, filename="NotForGit/Step3_4/bivariate_map_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(bivariate_map_plot, filename="NotForGit/Step3_4/bivariate_map_portrait.png",
       height=5.5*.65, width=10*(.65))


#####Panel Plot#####
combo_panel_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, size=0.25, color="gray70") +
  geom_sf(data=smc_flowlines_sf2, color="#e41a1c",
          # aes(color=ComboClass),
          size=0.25,
          # key_glyph = 'rect'
          )+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_color_manual(
    # name="NBR activity\n(relative to\npopulation density)",
    # values=c("#e41a1c")  )+
  facet_wrap(~ComboClass)+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "none")+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))

ggsave(combo_panel_map, filename="NotForGit/Step3_4/combo_panel_map_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(combo_panel_map+ggtitle("Streams that support aquatic life vs. human uses"), filename="NotForGit/Step3_4/combo_panel_map_portrait.png",
       height=5.5*.65, width=10*(.65))


combo_2panel_map<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, size=0.25, color="gray70") +
  geom_sf(data=smc_flowlines_sf2, color="#e41a1c",
          # aes(color=ComboClass),
          size=0.25,
          # key_glyph = 'rect'
  )+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_color_manual(
  # name="NBR activity\n(relative to\npopulation density)",
  # values=c("#e41a1c")  )+
  facet_wrap(~ComboClass)+
  geom_sf(data=smc_sheds_sf, fill=NA, color="white", lwd=0.25)+
  geom_sf(data=counties_sf, fill=NA, color="gray25")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill="lightblue"),
        strip.background = element_blank(),
        legend.position = "none")+
  coord_sf(xlim=c(my_bb[c(1,3)]),
           ylim=c(my_bb[c(2,4)]))

ggsave(combo_panel_map, filename="NotForGit/Step3_4/combo_panel_map_landscape.png",
       # height=5.5, width=10)
       height=5.5,width=10)
ggsave(combo_panel_map+ggtitle("Streams that support aquatic life vs. human uses"), filename="NotForGit/Step3_4/combo_panel_map_portrait.png",
       height=5.5*.65, width=10*(.65))



ggplot(smc_flowlines_sf2 %>% as_tibble() %>%
         filter(CSCI>0), 
       aes(x=CSCI, y=MetricResult_Dens))+
  geom_point(size=0.5)+
  geom_smooth()+
  scale_x_sqrt()+scale_y_sqrt()

#########3 classes##########


nbr_map3<-  ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=NBR_class3),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  scale_color_manual(
    name="Support",
    values=c("#2b547e","#46327e","#ed7953"),
             labels=c("Low","Mod","High")
  
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
nbr_map3

csci_map3<-ggplot()+
  geom_sf(data=counties_sf, fill="gray50")+
  geom_sf(data=mx_sf, fill="gray50")+
  geom_sf(data=smc_flowlines_sf2, 
          aes(color=CSCI_class2),
          size=0.25,
          key_glyph = 'rect')+
  geom_sf(data=smc_sheds_sf, fill=NA)+
  # scale_color_manual(    name="NBR activity\n(relative to\npopulation density)",
  #   values=c(c("#2b547e","#46327e","#9c179e","#ed7953","#f0f921")    )  )+
  scale_color_manual(    name="Support",
                         values=c(c("#2b547e","#46327e","#ed7953")    ),
                         labels=c("Low","Mod","High"),
                         na.translate=FALSE)+
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

csci_map3

map3classes<-
  ggarrange(csci_map3 +
              ggtitle("Aquatic life"), 
            nbr_map3+ 
              ggtitle("Human use"), ncol=1,
            common.legend = T,
            legend ="right")

ggsave(map3classes, filename="NotForGit/Step3_4/map3classes.png",
       height=5.5*.65*2, width=10*(.65))
