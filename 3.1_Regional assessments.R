library(tidyverse)
library(sf)

############Import shapefiles############
smc_sheds_sf<-st_read("S:/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp")
smc_huc12_sf<-st_read("Shapefiles/HUCs/HUC12_SMC.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))


smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
# smc_flowlines100m_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")
smc_wb_sf<-readRDS("NotForGit/Step1_2/smc_wb_sf_modified.RDS")
# smc_wb100m_sf<-readRDS("NotForGit/Step1_2/smc_wb100m_sf.RDS")
smc_case_sf<-readRDS("NotForGit/Step1_2/smc_case_sf_modified.RDS")
# smc_case100m_sf<-readRDS("NotForGit/Step1_2/smc_case100m_sf.RDS")


############Load stream and wb metrics############
smc_streams_metrics<-read_csv("NotForGit/Step2_1/stream_observation_join_summary_df.csv")
# smc_wb_metrics<-read_csv("NotForGit/Step2_1/wb_observation_join_summary_df.csv")

smc_streams_metrics2 <-
  smc_streams_metrics %>%
  inner_join(
    smc_flowlines_sf %>%
      as_tibble() %>%
      select(Segment_ID, HUC12, HUC8=HUC8_simp)
  )

# smc_flowlines_sf %>%
#   as_tibble() %>%
#   select(Segment_ID, HUC12, HUC8=HUC8_simp) %>%
#   group_by(Segment_ID) %>%
#   tally() %>%
#   filter(n>1)

############Calculate quantiles############

smc_streams_metrics_quantiles<-smc_streams_metrics %>%
  group_by(Group2, Group, Metric) %>%
  summarise(d_q50=quantile(MetricResult_Dens,0.5),
            d_q75=quantile(MetricResult_Dens,0.75),
            d_q90=quantile(MetricResult_Dens,0.9),
            d_q95=quantile(MetricResult_Dens,0.95),
            d_qMax=quantile(MetricResult_Dens,1)) %>%
  ungroup() %>%
  inner_join(
    smc_streams_metrics %>%
      filter(MetricResult_Dens>0) %>%
      group_by(Group2, Group, Metric) %>%
      summarise(dwp_q50=quantile(MetricResult_Dens,0.5),
                dwp_q75=quantile(MetricResult_Dens,0.75),
                dwp_q90=quantile(MetricResult_Dens,0.9),
                dwp_q95=quantile(MetricResult_Dens,0.95)) %>%
      ungroup()
  ) %>%
  inner_join(
    smc_streams_metrics %>%
      filter(MetricResult_Dens>0,
             SegLength_km>=.5) %>%
      group_by(Group2, Group, Metric) %>%
      summarise(dwp500_q50=quantile(MetricResult_Dens,0.5),
                dwp500_q75=quantile(MetricResult_Dens,0.75),
                dwp500_q90=quantile(MetricResult_Dens,0.9),
                dwp500_q95=quantile(MetricResult_Dens,0.95)) %>%
      ungroup()
  ) %>%
  inner_join(
    smc_streams_metrics %>%
      filter(
        SegLength_km>=.5) %>%
      group_by(Group2, Group, Metric) %>%
      summarise(d500_q50=quantile(MetricResult_Dens,0.5),
                d500_q75=quantile(MetricResult_Dens,0.75),
                d500_q90=quantile(MetricResult_Dens,0.9),
                d500_q95=quantile(MetricResult_Dens,0.95)) %>%
      ungroup()
  )

smc_streams_metrics_quantiles %>% skimr::skim_without_charts()

smc_streams_metrics3<-smc_streams_metrics2 %>% 
  left_join(
    smc_streams_metrics_quantiles %>%
      select(Group, Group2, Metric,
             Q50=dwp500_q50,Q75=dwp500_q75,Q90=dwp500_q90)
  ) %>%
  mutate(
    Class.n=case_when(MetricResult_Dens==0~0,
                      MetricResult_Dens<Q50~1,
                      MetricResult_Dens<Q75~2,
                      MetricResult_Dens<Q90~3,
                      T~4
    )
  )

huc12_summary<-smc_streams_metrics3 %>%
  group_by(SMCShed, TotLength_km, HUC12, Group, Group2, Metric) %>%
  summarise(
    HUC12_length_km=sum(SegLength_km),
    Pct_Class4 = sum(SegLength_km[Class.n==4])/HUC12_length_km,
    Pct_Class34 = sum(SegLength_km[Class.n>=3])/HUC12_length_km,
    Pct_Class234 = sum(SegLength_km[Class.n>=2])/HUC12_length_km,
    Pct_Class1234 = sum(SegLength_km[Class.n>=1])/HUC12_length_km,
  ) %>% 
  ungroup()


shed_summary<-smc_streams_metrics3 %>%
  group_by(SMCShed, TotLength_km, Group, Group2, Metric, Class.n) %>%
  summarise(
    Sum_Class_length=sum(SegLength_km)
  ) %>% 
  ungroup() 

plot_dat<-shed_summary %>%
  mutate(Pct_Class = Sum_Class_length/TotLength_km * 100) %>%
  mutate(Class=factor(Class.n, levels=0:4, labels=c("None",
                                                    "<50th %tile",
                                                    "50th to 75th %tile",
                                                    "75th to 90th %tile",
                                                    ">90th %tile")),
         Sheds_ns=factor(SMCShed,
                         levels=c("Santa Barbara Coast",
                                  "Ventura","Santa Clara","Calleguas", "Santa Monica Bay",
                                  "Dominguez Channel",
                                  "Los Angeles","San Gabriel",
                                  "Lower Santa Ana","Middle Santa Ana", "Upper Santa Ana", "San Jacinto",
                                  "San Juan", "Northern San Diego", "Central San Diego", "Mission Bay and San Diego River","Southern San Diego",
                                  "Northern Channel Islands","Southern Channel Islands") %>% rev()
                         ))

shed_stacked_bars_plot<-ggplot(plot_dat %>%
         filter(Group=="All taxa", Metric=="n_observers"),
       aes(x=Sheds_ns , y=Pct_Class, fill=Class))+
  geom_bar(position=position_stack(), stat="identity")+
  coord_flip()+
  theme_bw()+
  xlab("")+ylab("% stream-length")+
  scale_fill_viridis_d(direction=-1, name="Level of NBR activity")
ggsave(shed_stacked_bars_plot, filename="NotForGit/Step3_1/shed_stacked_bars_plot.png", height=6.5, width=6.5)



my_grp<-"All taxa"
my_met<-"n_observer_dates"
huc12_summary_medians<-huc12_summary %>%
  filter(Group==my_grp,
         Metric==my_met) %>%
  group_by(SMCShed) %>%
  summarise(
    n_hucs=length(Pct_Class1234),
    Med_1234=quantile(Pct_Class1234,0.5),
    Med_234=quantile(Pct_Class234,0.5),
    Med_34=quantile(Pct_Class34,0.5),
    Med_4=quantile(Pct_Class4,0.5)
    ) %>%
  arrange(Med_1234)

smcshed_huc12_alltaxa_boxplot<-ggplot(data=
         huc12_summary %>%
         filter(Group==my_grp,
                Metric==my_met) %>%
         # mutate(SMCShed.f=factor(SMCShed, 
         #                         levels=c("Santa Barbara Coast", "Northern Channel Islands",
         #                                  "Ventura","Santa Clara", "Calleguas",
         #                                  "Santa Monica Bay","Dominguez Channel", "Los Angeles","San Gabriel","Southern Channel Islands",
         #                                  "Lower Santa Ana","Middle Santa Ana","Upper Santa Ana","San Jacinto",
         #                                  "San Juan","Northern San Diego","Central San Diego","Mission Bay and San Diego River", "Southern San Diego"
         #                         ) %>% rev())) %>%
         
         mutate(SMCShed.f=factor(SMCShed, 
                                 levels=huc12_summary_medians$SMCShed))  %>%
         pivot_longer(cols=starts_with("Pct_Class")) %>%
           mutate(name=factor(name, 
                              levels=c("Pct_Class4","Pct_Class34","Pct_Class234","Pct_Class1234"),
                              labels=c(">90th",">75th",">50th","Any level")
                              )), 
       aes(x=SMCShed.f, y=value, fill=name))+
  scale_fill_viridis_d(#labels=c("Any level",">50th",">75th",">90th") %>% rev(),
                       begin=.2,
                       name="Level of support")+
  geom_boxplot(outlier.shape = 21, outlier.size = .75)+
  coord_flip()+
  # facet_wrap(~name)+
  xlab("")+
  ylab("% of stream-length\nsupporting NBR\nper HUC12")+
  ggtitle(paste(my_grp), subtitle = "# of observer-dates")+
  theme_bw()
ggsave(smcshed_huc12_alltaxa_boxplot, 
       filename="NotForGit/Step3_1/smcshed_huc12_alltaxa_boxplot.png",
       height=6.5, width=6.5)



huc12_summary_medians

smc_shed_summary<-smc_streams_metrics3 %>%
  group_by(SMCShed, TotLength_km, Group, Group2, Metric) %>%
  summarise(
    HUC12_length_km=sum(SegLength_km),
    Pct_Class4 = sum(SegLength_km[Class.n==4])/HUC12_length_km,
    Pct_Class34 = sum(SegLength_km[Class.n>=3])/HUC12_length_km,
    Pct_Class234 = sum(SegLength_km[Class.n>=2])/HUC12_length_km,
    Pct_Class1234 = sum(SegLength_km[Class.n>=1])/HUC12_length_km,
  ) %>% 
  ungroup() %>%
  inner_join(
    huc12_summary %>%
      group_by(SMCShed, Group, Metric) %>%
      summarise(
        n_hucs=length(Pct_Class1234),
        Med12_1234=quantile(Pct_Class1234,0.5),
        Med12_234=quantile(Pct_Class234,0.5),
        Med12_34=quantile(Pct_Class34,0.5),
        Med12_4=quantile(Pct_Class4,0.5)
      ) %>%
      ungroup()

  ) 

write_csv(smc_shed_summary, "NotForGit/Step3_1/smc_shed_summary.csv")

