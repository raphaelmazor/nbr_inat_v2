library(tidyverse)

######Import data######
selected_segments<-read_csv("NotForGit/Step4_1/smc_flowlines_selected.csv")
fieldval_df<-read_csv("NotForGit/Step4_2/fieldval_metrics_df.csv")


mydf<-selected_segments %>%
  inner_join(fieldval_df)

nbr_val_plot<-ggplot(data=mydf, 
       aes(x=npm_people_nbr, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  theme_bw()+
  ylab("# observers in iNaturalist")+
  xlab("# of people per minute engaged in NBR\nin field observations")
ggsave(nbr_val_plot, filename="NotForGit/Step4_3/nbr_val_plot.png", height=4, width=6)

leisure_val_plot<-ggplot(data=mydf, 
                     aes(x=npm_people_leisure, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  theme_bw()+
  ylab("# observers in iNaturalist")+
  xlab("# of people per minute engaged in leisure\nin field observations")
ggsave(leisure_val_plot, filename="NotForGit/Step4_3/leisure_val_plot.png", height=4, width=6)

attention_val_plot<-ggplot(data=mydf, 
                         aes(x=npm_people_attention, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  theme_bw()+
  ylab("# observers in iNaturalist")+
  xlab("# of people people per minute paying attention to\nwater, wildlife, or the streambed\nin field observations")
ggsave(attention_val_plot, filename="NotForGit/Step4_3/attention_val_plot.png", height=4, width=6)


residents_val_plot<-ggplot(data=mydf, 
                           aes(x=npm_people_resident, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  theme_bw()+
  ylab("# observers in iNaturalist")+
  xlab("# of people per minute residing in asssessment area\n(including unhoused)\nduring field observations")+
  coord_cartesian(ylim=c(0,22))
ggsave(residents_val_plot, filename="NotForGit/Step4_3/residents_val_plot.png", height=4, width=6)


####Long format####

fieldval_df_long<-fieldval_df %>%
  pivot_longer(cols=starts_with("npm_"), names_to = "fv_metric", values_to = "fv_value")

mydf_long<-selected_segments %>%
  inner_join(fieldval_df_long %>%
               filter(!str_detect(fv_metric,"nd")))
                      

field_val_plot<-ggplot(mydf_long, 
       aes(x=fv_value, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  facet_wrap(~fv_metric, scales="free", ncol=3)+
  theme_bw()+
  xlab("Field validation metric")+
  ylab("# observers in iNaturalist")+
  theme(legend.position="bottom",
        legend.direction = "vertical")+
  guides(color = guide_legend(ncol = 3)) 
ggsave(field_val_plot, filename="NotForGit/Step4_3/field_val_plot.png",
       # width=10, height=7
       width=6.5, height=9
       )

field_val_plot_subset<-ggplot(mydf_long %>%
                                filter(fv_metric %in% 
                                         c("npm_people","npm_people_attention","npm_people_nbr", 
                                           "npm_people_resident")), 
                       aes(x=fv_value, y=n_observers_raw))+
  geom_point(aes(color=Stratum))+
  scale_color_brewer(palette="Paired")+
  geom_smooth(method=lm)+
  facet_wrap(~fv_metric, scales="free")+
  theme_bw()+
  xlab("Field validation metric\n(people per minute)")+
  ylab("# observers in iNaturalist")+
  coord_cartesian(ylim=c(0,22))
ggsave(field_val_plot_subset, filename="NotForGit/Step4_3/field_val_plot_subset.png",
       width=6.5, height=6)


field_val_cor_table<-mydf_long %>%
  group_by(fv_metric) %>%
  summarize(
    # Pearson_r=cor(fv_value, n_observers_raw, method="pearson"),
    Spearman_rho=cor(fv_value, n_observers_raw, method="spearman"),
    Spearman_rho_p=cor.test(fv_value, n_observers_raw, estimate="rho")$p.value
  ) %>%
  ungroup() %>%
  mutate(#rsq=Pearson_r^2,
         rhosq=Spearman_rho^2) %>%
  arrange(-rhosq)

write_csv(field_val_cor_table, "NotForGit/Step4_3/field_val_cor_table.csv")
#####summary stats####

#Number of visits
fieldval_df %>% nrow()

#Number of visits per stratum
mydf %>% count(Stratum)


#Earliest and lattest visits
summary(fieldval_df$visit_time)
summary(fieldval_df$visit_date)

#Number without people
mydf %>%
  group_by(n_people>0) %>%
  tally()
