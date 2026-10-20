
######Import field data######
field_df<-read_csv("Data/Field validation data/nbr_field_validation_0.csv")
field_df2<-field_df %>%
  filter(segment_id!="111111_1") %>%
  transmute(
    parentglobalid=globalid,
    #Fix possible typos
    Segment_ID=case_when(
      segment_id=="225329990_3"~"22532990_3", #Extra 9?
      segment_id=="22525731_2"~"22525731_1", #wrong segment? #2 wasn't selected
      T~segment_id) ,
    visit_date=mdy_hms(assessment_date),
    visit_time=site_visit_time,
    weather_current,
    weather_current_notes,
    weather_suitable=weather_suitable_notes=="yes",
    weather_suitable_notes=paste(weather_suitable_describe,weather_suitable_notes),
    reach_accessibility_left=reach_length_left,
    reach_accessibility_left_numeric=case_when(reach_accessibility_left=="none"~0,
                                               reach_accessibility_left=="lt25"~1,
                                               reach_accessibility_left=="m25_100"~2,
                                               reach_accessibility_left=="m100_500"~3,
                                               reach_accessibility_left=="m500plus"~4,
                                               T~NA_real_),
    reach_accessibility_right=reach_length_right,
    
    reach_accessibility_right_numeric=case_when(reach_accessibility_right=="none"~0,
                                                reach_accessibility_right=="lt25"~1,
                                                reach_accessibility_right=="m25_100"~2,
                                                reach_accessibility_right=="m100_500"~3,
                                                reach_accessibility_right=="m500plus"~4,
                                                T~NA_real_),
    reach_accessibility_max=case_when(reach_accessibility_left_numeric>=reach_accessibility_right_numeric~reach_accessibility_left_numeric, T~reach_accessibility_right_numeric),
    access_rating,
    access_rating_numeric=case_when(access_rating=="none"~0,
                                    access_rating=="poor"~1,
                                    access_rating=="moderate"~2,
                                    access_rating=="good"~3,
                                    T~NA_real_),
    access_deterrents,
    access_deterrents_other,
    access_deterrents_notes,
    parking,	parking_notes,
    hydro_conditions,	hydro_notes,	
    water_visibility,
    water_visibility_numeric=case_when(water_visibility=="not_visible"~0,
                                       water_visibility=="yes_lt50"~1,
                                       water_visibility=="yes_50plus"~2,
                                       T~NA_real_),
    water_visibility_notes,
    water_access,	
    water_access_numeric=case_when(water_access=="direct"~0,
                                   water_access=="within_5"~1,
                                   water_access=="5_to_10"~2,
                                   water_access=="not_within_10"~3,
                                   T~NA_real_),
    water_access_notes,
    wq_concerns,	wq_concerns_other,	wq_concerns_notes,
    p1_start_time,	p1_stop_time,	p1_length=p1_stop_time-p1_start_time,
    p1_start_lat,	p1_start_long,	p1_notes,
    p2_start_time,	p2_stop_time,	p2_length=p2_stop_time-p2_start_time,
    p2_start_lat,	p2_start_long,	p2_notes,
    p3_start_time,	p3_stop_time,	p3_length=p3_stop_time-p3_start_time,
    p3_start_lat,	p3_start_long,	p3_notes,
    longitude=x, latitude=y,
    total_observation_time=p1_length+p2_length+p3_length,
  )
units(field_df2$total_observation_time)<-"mins"

p1_df<-read_csv("Data/Field validation data/p1_groups_1.csv") %>% 
  mutate(Period="P1", g_notes=p1_notes)
p2_df<-read_csv("Data/Field validation data/p2_groups_2.csv") %>% 
  mutate(Period="P2", g_notes=p2_notes)
p3_df<-read_csv("Data/Field validation data/p3_groups_3.csv") %>%
  mutate(Period="P3", g_notes=p3_notes)
groups_df<-bind_rows(p1_df, p2_df, p3_df) %>%
  transmute(
    Period,
    parentglobalid, 
    globalid,
    g_size,
    g_size_numeric=case_when(
      g_size=="10plus"~11,
      T~as.numeric(g_size)),
    g_pets=case_when(g_pets=="y"~1,
                     g_pets=="n"~0,
                     T~NA_real_
    ),
    g_activity=case_when(
      g_activity=="s"~"Stationary",
      g_activity=="w"~"Walking",
      g_activity=="r"~"Active",
      g_activity=="v"~"Vehicle",
      T~NA_character_),
    g_purpose=case_when(
      g_purpose=="p"~"Passing through",
      g_purpose=="l"~"Leisure",
      g_purpose=="r"~"Resident",
      g_purpose=="e"~"Employment",
      T~NA_character_),
    g_proximity,
    g_proximity_numeric=case_when(g_proximity=="water"~0,
                                  g_proximity=="within_5"~1,
                                  g_proximity=="5_to_10"~2,
                                  g_proximity=="over_10"~0,
                                  T~NA_real_),
    g_attention,
    g_nbr, 
    g_notes
  ) 

######Calculate metrics######

groups_metrics_df<-groups_df %>%
  group_by(parentglobalid) %>%
  summarize(
    n_groups=length(globalid),
    n_people=sum(g_size_numeric, na.rm=T),
    n_people_walking=sum(g_size_numeric[g_activity=="Walking"], na.rm=T),
    n_people_stationary=sum(g_size_numeric[g_activity=="Stationary"], na.rm=T),
    n_people_vehicle=sum(g_size_numeric[g_activity=="Vehicle"], na.rm=T),
    n_people_activity_nd=sum(g_size_numeric[is.na(g_activity)], na.rm=T),
    
    n_people_pets=sum(g_size_numeric[g_pets==1], na.rm=T),
    n_people_pets_nd=sum(g_size_numeric[is.na(g_pets)], na.rm=T),
    
    n_people_leisure=sum(g_size_numeric[g_purpose=="Leisure"], na.rm=T),
    n_people_passing=sum(g_size_numeric[g_purpose=="Passing through"], na.rm=T),
    n_people_resident=sum(g_size_numeric[g_purpose=="Resident"], na.rm=T),
    n_people_employment=sum(g_size_numeric[g_purpose=="Employment"], na.rm=T),
    n_people_purpose_nd=sum(g_size_numeric[is.na(g_purpose)], na.rm=T),
    
    n_people_in_water=sum(g_size_numeric[g_proximity_numeric==0], na.rm=T),
    n_people_in_near_water=sum(g_size_numeric[g_proximity_numeric<=1], na.rm=T),
    n_people_mean_proximity=weighted.mean(g_proximity_numeric, w=g_size_numeric, na.rm=T),
    n_people_prox_nd=sum(g_size_numeric[is.na(g_proximity)], na.rm=T),
    
    n_people_attention=sum(g_size_numeric[g_attention=="y"], na.rm=T),
    n_people_attention_nd=sum(g_size_numeric[g_attention=="nd"], na.rm=T),
    
    n_people_nbr=sum(g_size_numeric[g_nbr=="y"], na.rm=T),
    n_people_nbr_nd=sum(g_size_numeric[g_nbr=="nd"], na.rm=T)
    
  ) %>%
  ungroup()

#Assume all "missing" parentglobalIDs have zero observations
miss_pid=tibble(
  parentglobalid=setdiff(field_df2$parentglobalid, groups_metrics_df$parentglobalid)
)
all_mets=setdiff(names(groups_metrics_df),"parentglobalid")

missing_data=miss_pid %>%
  crossing(name=all_mets) %>%
  mutate(value=0) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0)



groups_metrics_df2<-groups_metrics_df %>% 
  bind_rows(missing_data)

skimr::skim(groups_metrics_df2)



######Assemble data######

fieldval_metrics_df<-field_df2 %>%
  inner_join(groups_metrics_df2) %>%
  pivot_longer(cols=starts_with("n")) %>%
  mutate(value=value/as.numeric(total_observation_time),
         name=str_replace(name,"n_", "npm_")) %>%
  pivot_wider(names_from = name, values_from = value)

write_csv(fieldval_metrics_df, "NotForGit/Step4_2/fieldval_metrics_df.csv")

# 
# selected_segments<-read_csv("NotForGit/Step4_1/smc_flowlines_selected.csv")
# setdiff(fieldval_metrics_df$Segment_ID, selected_segments$Segment_ID)
# field_df %>%
#   filter(segment_id == "22525731_2") %>%
#   select(p1_start_lat, p1_start_long) %>% print(width=Inf)


#####
groups_df %>% filter(g_nbr=="y") %>%
  select(g_notes) %>%
  distinct()

field_df %>%
  filter(globalid %in% groups_df$parentglobalid[groups_df$g_nbr=="y"]) %>%
  select("p3_notes")
