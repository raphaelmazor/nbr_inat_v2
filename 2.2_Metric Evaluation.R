library(tidyverse)
library(sf)

# smc_streams_metrics<-read_csv("NotForGit/Step2_1/stream_observation_join_summary_df.csv")

#All the metrics is too unwieldy. Now just doing a short list of 5 groups
# smc_streams_metrics<-smc_streams_metrics %>%
  # filter(Group %in% c("All taxa", "All Birds","All Herps","All Invertebrates","Fish"))
# write_csv(smc_streams_metrics, "NotForGit/Step2_1/smc_streams_metrics_shortlist.csv")
smc_streams_metrics<-read_csv("NotForGit/Step2_1/smc_streams_metrics_shortlist.csv")

popdens_df<-#read_csv("stream_population_density_2020_wide.csv") %>%
  read_csv("Data/stream_population_density_2020_wide.csv") %>%
  select(-stream_rowid ) %>%
  rename(Segment_ID=segment_id)
scape_df<-st_read("S:/SCAPE/SCAPE_Subsets/SCAPE_SMC.shp") %>%
  as_tibble() %>%
  select(COMID, CSCI_scape=qt50)
  # transmute(COMID=as.character(COMID), qt50)

mod_dat<-smc_streams_metrics %>%
  left_join(popdens_df) %>%
  filter(SegLength_km>0.5) #%>%
# saveRDS(mod_dat,"NotForGit/Step2_2/mod_dat.RDS")
# mod_dat<-readRDS("NotForGit/Step2_2/mod_dat.RDS")

# mod_dat2<-smc_streams_metrics %>%
#   left_join(scape_df) %>%
#   group_by(COMID, Group, Metric, CSCI_scape, lengthkm2) %>%
#   summarize(MetricResult_Dens = weighted.mean(MetricResult_Dens, na.rm=T, w=SegLength_km)) %>%
#   filter(lengthkm2>0.5) %>%
#   ungroup()
# saveRDS(mod_dat2,"NotForGit/Step2_2/mod_dat2.RDS")
# readRDS("NotForGit/Step2_2/mod_dat2.RDS")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode_nonzero <- function(v) {
  v=v[which(v!=0)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

all_mets_df<-mod_dat %>%
  select(Group, 
         # Group2, 
         Metric, MetricResult_Dens) %>%
  group_by(Group, #Group2, 
           Metric) %>%
  summarise(n_unique_values=n_distinct(MetricResult_Dens),
            modal_value=getmode(MetricResult_Dens),
            pct_dom = sum(MetricResult_Dens==modal_value)/length(MetricResult_Dens),
            modal_value_nonzero=getmode_nonzero(MetricResult_Dens),
            pct_dom_nonzero = sum(MetricResult_Dens==modal_value_nonzero)/sum(MetricResult_Dens!=0),
            n_zero=sum(MetricResult_Dens==0),
            n_nonzero=sum(MetricResult_Dens!=0),
            pct_zero=sum(MetricResult_Dens==0)/length(MetricResult_Dens)
  ) %>%
  ungroup()

#####Metric evaluations#####
#####Select metrics that have acceptable "range"
all_mets_df_sub<-all_mets_df %>%
  filter(n_unique_values>=10,
         n_nonzero>=50,
  )

#How many were eliminated?
#None from the shortlist, 261 (?) from the full list
all_mets_df %>% nrow()
all_mets_df_sub %>% nrow()
all_mets_df %>%   filter(n_unique_values<10) %>%   nrow()
all_mets_df %>%   filter(n_nonzero<50) %>%   nrow()
all_mets_df %>%   filter(n_nonzero<50| n_unique_values<10) %>%   nrow()


gc()
#########
#####Responsiveness#####
####GAMs####
library(mgcv)
Sys.time()
all_mets_sub_mods_gam<-lapply(1:nrow(all_mets_df_sub), function(i){
  group.i=all_mets_df_sub$Group[i]
  met.i=all_mets_df_sub$Metric[i]
  print(paste(i, group.i, met.i))
  dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
  gam(MetricResult_Dens~s(pop_density_per_km2_5km, bs = "cs", k=5),
  # gam(MetricResult_Dens~s(CSCI_scape, bs = "cs", k=5),
      method="REML",
      data=dat.i)
  })
Sys.time()
saveRDS(all_mets_sub_mods_gam, file="NotForGit/Step2_2/all_mets_sub_mods_gam.RDS")
all_mets_sub_mods_gam<-readRDS("NotForGit/Step2_2/all_mets_sub_mods_gam.RDS")

plot(all_mets_sub_mods_gam[[105]])

#This next step is very slow and also give you the wrong information
# Sys.time()
# all_mets_sub_mods_gam_predinterval<-  lapply(1:length(all_mets_sub_mods_gam), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
#   gam.i=all_mets_sub_mods_gam[[i]]
#   Xp<-predict(gam.i, type="lpmatrix")
#   br <- rmvn(10000,coef(gam.i),vcov(gam.i)) ## 10000 replicate param. vectors
#   yr<- Xp %*% t(br)
#   pi <- apply(yr, 1, quantile, probs=c(.1,.25,.5,.75,.9), type=9)
#   dat.i %>% bind_cols(t(pi) %>% as.data.frame())
# })
# saveRDS(all_mets_sub_mods_gam_predinterval, file="NotForGit/Step2_2/all_mets_sub_mods_gam_predinterval.RDS")
# all_mets_sub_mods_gam_predinterval<-readRDS("NotForGit/Step2_2/all_mets_sub_mods_gam_predinterval.RDS")
# Sys.time()
#Try qgam
# library(qgam)
# all_mets_sub_mods_qgam<-lapply(1:nrow(all_mets_df_sub), function(i){
#   group.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   print(paste(i, group.i, met.i))
#   dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i) %>%
#     slice_sample(n=100)
#   qg_75=qgam(MetricResult_Dens~s(PopDen2010Cat,bs = "cs", k=5),
#       qu=0.75,
#       data=dat.i)
#   qg_50=qgam(MetricResult_Dens~s(PopDen2010Cat,bs = "cs", k=5),
#              qu=0.5,
#              data=dat.i)
#   qg_25=qgam(MetricResult_Dens~s(PopDen2010Cat,bs = "cs", k=5),
#              qu=0.25,
#              data=dat.i)
#   list(qg_25, qg_50,qg_75)
# })

#Model one
gam_i<-all_mets_sub_mods_gam[[which(all_mets_df_sub$Group=="All taxa" & all_mets_df_sub$Metric=="n_observers")]]
plot(gam_i)

####Piecewise regression####

library(segmented)
all_mets_sub_mods_segmented<-lapply(1:nrow(all_mets_df_sub), function(i){
  group.i=all_mets_df_sub$Group[i]
  met.i=all_mets_df_sub$Metric[i]
  print(paste(i, group.i, met.i))
  dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
  # dat.i=smc_streams_metrics %>% filter(Group==group.i, Metric==met.i)
  #Initial linear fit
  lm.i=lm(MetricResult_Dens~pop_density_per_km2_5km, data=dat.i)
  fit_seg=segmented(lm.i, 
                    psi=1000) #300 for streamcat data, 1000 for popden_5k
})
saveRDS(all_mets_sub_mods_segmented, file="NotForGit/Step2_2/all_mets_sub_mods_segmented.RDS")
all_mets_sub_mods_segmented<-readRDS("NotForGit/Step2_2/all_mets_sub_mods_segmented.RDS")

all_mets_sub_mods_segmented_predinterval<-  lapply(1:nrow(all_mets_df_sub), function(i){
  group.i=all_mets_df_sub$Group[i]
  met.i=all_mets_df_sub$Metric[i]
  print(paste(i, group.i, met.i))
  dat.i=mod_dat %>% filter(Group==group.i, Metric==met.i)
  mod.i=all_mets_sub_mods_segmented[[i]]
  break.i=mod.i$psi[2]
  dat.j = dat.i %>%
    transmute(pop_density_per_km2_5km=case_when(pop_density_per_km2_5km>break.i~break.i, 
                                      T~pop_density_per_km2_5km))
  pred.j_50 = predict(mod.i, newdata=dat.j, interval="prediction", level=0.5) %>%
    as_tibble() %>% 
    dplyr::select(Predicted=fit, Predicted_25=lwr, Predicted_75=upr)
  pred.j_80 = predict(mod.i, newdata=dat.j, interval="prediction", level=0.8) %>%
    as_tibble() %>% 
    dplyr::select(Predicted_10=lwr, Predicted_90=upr)
  dat.i %>% bind_cols(pred.j_50) %>% bind_cols(pred.j_80)
}) %>%
  bind_rows()

saveRDS(all_mets_sub_mods_segmented_predinterval, file="NotForGit/Step2_2/all_mets_sub_mods_segmented_predinterval.RDS")
all_mets_sub_mods_segmented_predinterval<-readRDS("NotForGit/Step2_2/all_mets_sub_mods_segmented_predinterval.RDS")

all_mets_sub_mods_segmented_predinterval %>%
  mutate(NBR_class=
           case_when(is.na(MetricResult_Dens)~"Cannot be assessed",
                     MetricResult_Dens>=Predicted_90~"Above 90",
                     MetricResult_Dens>=Predicted_75~"Above 75",
                     MetricResult_Dens>=Predicted~"Above 50",
                     MetricResult_Dens>=Predicted_25~"Above 25",
                     MetricResult_Dens>=Predicted_10~"Above 10",
                     MetricResult_Dens<Predicted_10~"Below 10",
                     T~"OTHER"
           ),
         NBR_class.f=factor(NBR_class, levels=c("Above 90","Above 75","Above 50","Above 25","Above 10")))

#Alterative modeling approaches
#qgam: very slow
#piece-wise regression

# 
# library(qgam) #Too slow


# #EXTRACT FITTED VALUES TO USE AS THRESHOLDS
# mod_preds_df<-lapply(1:nrow(all_mets_df_sub ), function(i){
#   grp.i=all_mets_df_sub$Group[i]
#   met.i=all_mets_df_sub$Metric[i]
#   mod.i=all_mets_sub_mods_gam[[i]]
#   new_df<-streamcat_df %>% 
#     filter(COMID %in% smc_streams_metrics$COMID) %>%
#     select(PopDen2010Cat, COMID)
#   new_df$Predicted=predict(mod.i, newdata=new_df)
#   new_df %>% select(-PopDen2010Cat)
# }) %>%
#   bind_rows()
detach("package:segmented")

all_mets_sub_mods_segmented_predinterval2<-all_mets_sub_mods_segmented_predinterval %>%
  mutate(NBR_class=case_when(MetricResult_Dens>Predicted_90~5,
                             MetricResult_Dens>Predicted_75~4,
                             MetricResult_Dens>Predicted   ~3,
                             MetricResult_Dens>Predicted_25~2,
                             MetricResult_Dens>Predicted_10~1,
                             T~0
  ))


write_csv(all_mets_sub_mods_segmented_predinterval2, "NotForGit/Step2_2/all_mets_sub_mods_segmented_predinterval.csv")

plot_dat<-all_mets_sub_mods_segmented_predinterval2 %>%
  filter(Metric=="n_observers",
         Group=="All taxa") 

# 
# junk_mod<-all_mets_sub_mods_gam[[2]]
# xxx<-predict(junk_mod, newdata=streamcat_df,
#              interval="prediction")
# 
# junk2<-smc_streams_metrics %>%
#   left_join(    mod_preds_df  )



library(broom)
all_mets_sub_mods_gam_summary<-all_mets_df_sub %>%
  bind_cols(
    lapply(all_mets_sub_mods_gam, function(x){
      glance(x)
    }) %>%
      bind_rows()
  )

all_mets_sub_mods_seg_summary<-all_mets_df_sub %>%
  bind_cols(
    lapply(all_mets_sub_mods_segmented, function(x){
      glance(x)
    }) %>%
      bind_rows()
  )

write_csv(left_join(all_mets_df, all_mets_sub_mods_gam_summary), "NotForGit/Step2_2/all_mets_sub_mods_gam_summary.csv")
write_csv(left_join(all_mets_df, all_mets_sub_mods_seg_summary), "NotForGit/Step2_2/all_mets_sub_mods_seg_summary.csv")
all_mets_sub_mods_gam_summary<-read_csv("NotForGit/Step2_2/all_mets_sub_mods_gam_summary.csv") %>%
  filter(!is.na(AIC))



mod_dat %>%
  select(Group, Metric, PopDen2010Cat) %>%
  distinct()

