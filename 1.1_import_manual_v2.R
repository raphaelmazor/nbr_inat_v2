library(tidyverse)

#####Kingfishers####
alcedinidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Alcedinidae_Kingfishers_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Kingfishers",Group2="Birds", Group_sci="Alcedinidae")

#####Ducks####
anatidae_df<-
  # readRDS("Data/iNaturalist data//iNat/inat_RAW_Anatidae_Ducks_socal_2010_2025_FIXED.rds") %>%
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Anatidae_Ducks_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Ducks",Group2="Birds", Group_sci="Anatidae")


#####Herons####
ardeidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Ardeidae_Herons_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Herons",Group2="Birds", Group_sci="Arediedae")

#####Dippers####
cinclidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Cinclidae_Dippers_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Dippers",Group2="Birds", Group_sci="Cinclidae")

#####Loons####
gaviidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Gaviidae_Loons_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Loons",Group2="Birds", Group_sci="Gaviidae")

#####Gulls and terns####
laridae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Laridae_Gulls_and_terns_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Gulls",Group2="Birds", Group_sci="Laridae")

#####Pelicans####
pelecanidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Pelecanidae_Pelicans_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Pelicans",Group2="Birds", Group_sci="Pelecanidae")

#####Grebes####
podicipedidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Podicipedidae_Grebes_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Grebes",Group2="Birds", Group_sci="Podicipedidae")

#####Rails####
rallidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Rallidae_Rails_and_coots_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Rails",Group2="Birds", Group_sci="Rallidae")

#####Stilts and avocets####
recurvirostridae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Recurvirostridae_Stilts_and_avocets_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Stilts",Group2="Birds", Group_sci="Recurvirostridae")

#####Shorebirds####
scolopacidae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Scolopacidae_Plovers_and_sandpipers_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Shorebirds",Group2="Birds", Group_sci="Scolopacidae")

#####Cormorants####
suliformes_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Suliformes_Cormorants_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Cormorants",Group2="Birds", Group_sci="Suliformes")

#####Frogs####
anura_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Anura_Frogs_socal_2010_2025.rds") %>%
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Frogs",Group2="Herps", Group_sci="Anura")

#####Salamanders####
salamandridae_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Salamandridae_Newts_socal_2010_2025.rds") %>%  
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Newts",Group2="Herps", Group_sci="Salamandridae")


#####Turtles####
testudines_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Testudines_Turtles_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Turtles",Group2="Herps", Group_sci="Testudines")


#####Garter snakes####
thamnophis_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Thamnophis_Garter_snakes_socal_2010_2025.rds") %>%
  filter(!captive,
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Garter snakes",Group2="Herps", Group_sci="Thamnophis")


#####Fish####
fish_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Actinopterygii_Fish_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Fish",Group2="Fish", Group_sci="Actinopterygii")

#####Mayflies####
mayflies_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Ephemeroptera_Mayflies_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Mayflies",Group2="Invertebrates", Group_sci="Ephemeroptera")

#####Stoneflies####
stoneflies_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Plecoptera_Stoneflies_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Stoneflies",Group2="Invertebrates", Group_sci="Plecoptera")

#####Caddisflies####
caddisflies_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Trichoptera_Caddisflies_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Caddisflies",Group2="Invertebrates", Group_sci="Trichoptera")

#####Dragonflies####
odonata_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Odonata_Dragonflies_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Dragonflies",Group2="Invertebrates", Group_sci="Odonata")

#####Crayfish####
crayfish_df<-
  readRDS("Data/iNaturalist data//iNat/iNat_allobvs/inat_RAW_Astacoidea_Crayfish_socal_2010_2025.rds") %>% 
  filter(!captive, 
         !obscured #T/F. T when obscured for any reason
  ) %>%
  mutate(Group="Crayfish",Group2="Invertebrates", Group_sci="Astacoidea")

#####Assemble all taxa into a single spreadsheet####

all_taxa_df<-
  bind_rows(alcedinidae_df,anatidae_df,ardeidae_df,
            cinclidae_df,gaviidae_df,
            laridae_df,pelecanidae_df,podicipedidae_df,rallidae_df,
            recurvirostridae_df, scolopacidae_df, suliformes_df,
            anura_df, salamandridae_df, testudines_df, thamnophis_df,
            fish_df,
            mayflies_df, stoneflies_df, caddisflies_df, odonata_df, crayfish_df
            )    %>%
  transmute(
    user_id=user.id,user_login=user.login,
    observed_on,
    Date=observed_on,
    Year=year(Date), Month=month(Date),
    latitude=Lat, longitude=Long,
    Group, Group2,Group_sci, requested_taxon, requested_common_name
    
    # taxon=taxon.name
  )

#Review for data artifacts and other strange patterns
library(sf)
all_taxa_sf<-all_taxa_df %>%
  st_as_sf(coords=c("longitude","latitude"),
           crs=4326)

ggplot()+geom_sf(data=all_taxa_sf, size=0.5)

#Export
write_csv(all_taxa_df, "NotForGit/Step1_1/all_taxa_df.csv")


