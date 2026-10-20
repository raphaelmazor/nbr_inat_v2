library(tidyverse)
library(sf)
# library(units)


############Import shapefiles############
smc_sheds_sf<-st_read("Shapefiles/SMCSheds2009/SMCSheds2009.shp") 
smc_huc08_sf<-st_read("Shapefiles/HUCs/HUC08_SMCv2.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf)) 
smc_huc12_sf<-st_read("Shapefiles/HUCs/HUC12_SMC.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf)) 

############Streams############
smc_flowlines_sf<-st_read("Shapefiles/rivers_split_1000m/rivers_split_1000m.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf))
duplicated(smc_flowlines_sf %>%
             as_tibble() %>%
             select(Segment_ID)) %>%
  sum()

smc_flowlines_sf$SegLength_km=as.numeric(st_length(smc_flowlines_sf)/1000)
smc_flowlines_sf<- smc_flowlines_sf %>%
  st_zm(drop=T) %>%
  group_by(SMCShed) %>%
  mutate(TotLength_km=sum(SegLength_km)) %>%
  ungroup() %>%
  st_join(smc_huc08_sf %>%st_zm(drop=T),largest=T) %>%
  st_join(smc_huc12_sf %>%st_zm(drop=T) %>% select(HUC12, HUC12_name=NAME) ,largest=T)

duplicated(smc_flowlines_sf %>%
             as_tibble() %>%
             select(Segment_ID)) %>% sum()

saveRDS(smc_flowlines_sf, "NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
st_write(smc_flowlines_sf,"Shapefiles/rivers_split_1000m_modified.shp")

smc_flowlines100m_sf<-
  smc_flowlines_sf %>%
  st_zm(drop = T) %>%
  st_buffer(dist=100) 
# smc_flowlines100m_sf$buff_area_km2=st_area(smc_flowlines100m_sf)/1000000
smc_flowlines100m_sf$buff_area_km2=as.numeric(st_area(smc_flowlines100m_sf)/1000000)
# units::set_units('km2')

smc_flowlines100m_sf<-smc_flowlines100m_sf %>%
  select(SMCShed, TotLength_km,
         # GNIS_NAME,
         COMID, lengthkm2,
         Segment_ID, SegLength_km, buff_area_km2) %>%
  mutate(Type="Flowlines",Type2="Ambient")


saveRDS(smc_flowlines100m_sf, "NotForGit/Step1_2/smc_flowlines100m_sf.RDS")

############Waterbodies############
smc_wb_sf<-st_read("S:/SMCBasefiles/Frames/smc_nhd_wb_sb.shp") %>%
  st_transform(crs=st_crs(smc_sheds_sf)) %>%
  group_by(SMCShed) %>%
  mutate(TotArea_km2=sum(AreaSqKm)) %>% 
  ungroup() %>%
  st_join(smc_huc08_sf %>%st_zm(drop=T),largest=T)%>%
  st_join(smc_huc12_sf %>%st_zm(drop=T) %>% select(HUC12, HUC12_name=NAME),largest=T)

smc_wb_sf$wb_peri_km=as.numeric(st_perimeter(smc_wb_sf)/1000)
smc_wb_sf$wb_area_km2=as.numeric(st_area(smc_wb_sf)/1000000)


smc_wb100m_sf<-
  smc_wb_sf %>%
  st_zm(drop = T) %>%
  st_buffer(dist=100) 


smc_wb100m_sf$buff_area_km2=as.numeric(st_area(smc_wb100m_sf)/1000000)
smc_wb100m_sf$buffonly_area_km2 = smc_wb100m_sf$buff_area_km2 - smc_wb100m_sf$wb_area_km2


smc_wb100m_sf<-smc_wb100m_sf %>%
  select(SMCShed, TotArea_km2 ,
         # GNIS_Name,
         Permanent_, wb_peri_km, 
         wb_area_km2, buff_area_km2, buffonly_area_km2) %>%
  mutate(Type="Waterbodies",Type2="Ambient")

saveRDS(smc_wb100m_sf, "NotForGit/Step1_2/smc_wb100m_sf.RDS")
saveRDS(smc_wb_sf, "NotForGit/Step1_2/smc_wb_sf_modified.RDS")


# ggplot()+
#   geom_sf(data=smc_flowlines_sf, color="blue", alpha=0.5)+
#   geom_sf(data=smc_wb_sf, color="blue", fill="lightblue")

# ############Case studies############
# smc_case_sf<-st_read("Shapefiles/CaseStudies/Misc case studies/misc_case_studies_01132026_2.shp") %>%
#   st_transform(crs=st_crs(smc_sheds_sf)) %>% 
#   st_join(
#     smc_sheds_sf %>%
#       select(SMCShed=SMC_Name)) %>%
#   mutate(SMCShed=case_when(Name=="Machado Lake"~"Dominguez Channel",
#                            T~SMCShed)) %>%
#   st_join(smc_huc08_sf %>%st_zm(drop=T),largest=T)%>%
#   st_join(smc_huc12_sf %>%st_zm(drop=T) %>% select(HUC12, HUC12_name=NAME),largest=T)
# 
# smc_case_sf$case_peri_km=as.numeric(st_perimeter(smc_case_sf)/1000)
# smc_case_sf$case_area_km2=as.numeric(st_area(smc_case_sf)/1000000)
# 
# 
# smc_case100m_sf<-
#   smc_case_sf %>%
#   st_zm(drop = T) %>%
#   st_buffer(dist=100) 
# 
# 
# smc_case100m_sf$buff_area_km2=as.numeric(st_area(smc_case100m_sf)/1000000)
# smc_case100m_sf$buffonly_area_km2 = smc_case100m_sf$buff_area_km2 - smc_case100m_sf$case_area_km2
# 
# 
# smc_case100m_sf<-smc_case100m_sf %>%
#   select(SMCShed, #TotArea_km2 ,
#          # GNIS_Name,
#          Name, case_peri_km, 
#          case_area_km2, buff_area_km2, buffonly_area_km2) %>%
#   mutate(Type="Case study",Type2="Case study")
# 
# saveRDS(smc_case100m_sf, "NotForGit/Step1_2/smc_case100m_sf.RDS")
# saveRDS(smc_case_sf, "NotForGit/Step1_2/smc_case_sf_modified.RDS")


############Combined Streams and Waterbodies############

# smc_sheds_sf<-st_read("Shapefiles/SMCSheds2009/SMCSheds2009.shp")
smc_flowlines_sf<-readRDS("NotForGit/Step1_2/rivers_split_1000m_modified.RDS")
smc_flowlines100m_sf<-readRDS("NotForGit/Step1_2/smc_flowlines100m_sf.RDS")
smc_wb_sf<-readRDS("NotForGit/Step1_2/smc_wb_sf_modified.RDS")
smc_wb100m_sf<-readRDS("NotForGit/Step1_2/smc_wb100m_sf.RDS")

duplicated(smc_flowlines_sf %>%
             as_tibble() %>%
             select(Segment_ID)) %>% sum()
duplicated(smc_flowlines100m_sf %>%
             as_tibble() %>%
             select(Segment_ID)) %>% sum()
duplicated(smc_wb_sf %>%
             as_tibble() %>%
             select(Permanent_)) %>% sum()
duplicated(smc_wb100m_sf %>%
             as_tibble() %>%
             select(Permanent_)) %>% sum()
setdiff(smc_flowlines_sf$Segment_ID, smc_flowlines100m_sf$Segment_ID)
setdiff(smc_flowlines100m_sf$Segment_ID, smc_flowlines_sf$Segment_ID)
setdiff(smc_wb_sf$Permanent_, smc_wb100m_sf$Permanent_)
setdiff(smc_wb100m_sf$Permanent_, smc_wb_sf$Permanent_)


smc_flow_wb_sf<-
  smc_flowlines_sf %>%
  transmute(
    Original_ID = as.character(COMID),
    GNIS_Name=GNIS_NAME,
    Unit_ID=Segment_ID,
    Unit_Length_km = SegLength_km,
    Unit_Area_km2=NA_real_,
    SMCShed,  HUC8_simp, HUC12, HUC12_name,
    Type="Flowline",
  ) %>%
  bind_rows(
    smc_wb_sf %>%
      transmute(
        Original_ID=as.character(Permanent_),GNIS_Name,
        Unit_ID=as.character(Permanent_),
        Unit_Length_km = wb_peri_km,
        Unit_Area_km2=wb_area_km2,
        SMCShed,  HUC8_simp, HUC12, HUC12_name,
        Type="Waterbody"
      )
  )

duplicated(smc_flow_wb_sf %>%
             as_tibble() %>%
             select(Unit_ID)) %>% sum()

smc_flow_wb_100m_sf<-
  smc_flowlines100m_sf %>%
  inner_join(
    smc_flowlines_sf %>%
      as_tibble() %>%
      select(
        Segment_ID, HUC8_simp, HUC12, HUC12_name
      ) ) %>%
  transmute(
    Original_ID = as.character(COMID),
    Unit_ID=Segment_ID,
    Unit_Length_km = SegLength_km,
    Unit_Area_km2=NA_real_,
    buff_area_km2,buffonly_area_km2=buff_area_km2,
    SMCShed,  HUC8_simp, HUC12, HUC12_name,
    Type="Flowline"
  ) %>%
  bind_rows(
    smc_wb100m_sf %>%
      inner_join(
        smc_wb_sf %>%
          as_tibble() %>%
          select(
            Permanent_, HUC8_simp, HUC12, HUC12_name
          )
      ) %>%
      transmute(
        Original_ID=as.character(Permanent_),
        Unit_ID=as.character(Permanent_),
        Unit_Length_km = wb_peri_km,
        Unit_Area_km2=wb_area_km2,
        buff_area_km2,buffonly_area_km2,
        SMCShed, HUC8_simp, HUC12, HUC12_name,
        Type="Waterbody"
      )
  )

duplicated(smc_flow_wb_100m_sf %>%
             as_tibble() %>%
             select(Unit_ID)) %>% sum()


saveRDS(smc_flow_wb_100m_sf, "NotForGit/Step1_2/smc_flow_wb_100m_sf.RDS")
saveRDS(smc_flow_wb_sf, "NotForGit/Step1_2/smc_flow_wb_sf.RDS")
