#### Compare 1970 SMSA and 2010 CBSA D and xPx
#### Hypersegregation Project

#libraries
library(tidyverse)
library(haven)
library(OasisR)

#setwd to Hyperseg dirs
setwd("R:/Project/Hypersegregation/Paper 1/Data")

#load in NCDB to extract CBSAs with
ncdb_vanilla <- read_dta("NCDB_1970_2010.dta")
ncdb_interp <- read_dta("wide_tract_metro_interp_2015.dta")

#load in OMB definition file to use as crosswalk
msa1970 <- read_csv("OMB MSA definitions/71mfips.csv") #delimited county-MSA data for
msa1980 <- read_csv("OMB MSA definitions/80mfips.csv") #each census year
msa1990 <- read_csv("OMB MSA definitions/90mfips.csv")
msa2000 <- read_csv("OMB MSA definitions/99mfips.csv")

#not in operator
'%!in%' <- function(x,y)!('%in%'(x,y))

#### A. Reshape crosswalk tables a little -------------------------------------

msa1970 <- msa1970 %>% #wipe out the cases for the MSA alone --- they are coded
  filter(!is.na(STATE)) %>% #as missing STATE IDs
  mutate(TOWN = X6) %>%
  select(STATE, COUNTY, TOWN, SMSA70) %>% #remove other variables from data frame
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY),
         TOWN = str_pad(TOWN, 5, side = "left", pad = "0")) %>%
  filter(!is.na(SMSA70)) %>%
  group_by(STATE, COUNTY, FIPS, TOWN) %>%
  filter((length(unique(SMSA70)) != 1 & !is.na(TOWN)) |
           length(unique(SMSA70)) == 1) %>%
  summarize(MSA70 = unique(SMSA70)) 

msa1980 <- msa1980 %>%
  filter(!is.na(STATE)) %>%
  mutate(TOWN = X8) %>%
  select(STATE, COUNTY, TOWN, SMSA80) %>%
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY),
         TOWN = str_pad(TOWN, 5, side = "left", pad = "0")) %>%
  filter(!is.na(SMSA80)) %>%
  group_by(STATE, COUNTY, FIPS, TOWN) %>%
  filter((length(unique(SMSA80)) != 1 & !is.na(TOWN)) |
           length(unique(SMSA80)) == 1) %>%
  summarize(MSA80 = unique(SMSA80)) 

msa1990 <- msa1990 %>%
  filter(!is.na(STATE)) %>%
  mutate(TOWN = X12) %>%
  select(STATE, COUNTY, TOWN, PMSA90, CMSA90) %>%
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY),
         TOWN = str_pad(TOWN, 5, side = "left", pad = "0")) %>%
  filter(!is.na(CMSA90)) %>%
  group_by(STATE, COUNTY, FIPS, TOWN) %>%
  filter((length(unique(PMSA90)) != 1 & !is.na(TOWN)) |
           length(unique(PMSA90)) == 1) %>%
  summarize(MSA90 = ifelse(is.na(unique(PMSA90)), unique(CMSA90), unique(PMSA90))) 

msa2000 <- msa2000 %>%
  filter(!is.na(STATE)) %>%
  mutate(TOWN = X12) %>%
  select(STATE, COUNTY, TOWN, PMSA99, CMSA99) %>%
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY),
         TOWN = str_pad(TOWN, 5, side = "left", pad = "0")) %>%
  filter(!is.na(CMSA99)) %>%
  group_by(STATE, COUNTY, FIPS, TOWN) %>%
  filter((length(unique(PMSA99)) != 1 & !is.na(TOWN)) |
           length(unique(PMSA99)) == 1) %>%
  summarize(MSA00 = ifelse(is.na(unique(PMSA99)), unique(CMSA99), unique(PMSA99))) 

ncdb_cw <- ncdb_vanilla %>%
  filter(CBSA != "") %>%
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY),
         TOWN = str_pad(COUSUB, 5, side = "left", pad = "0"))

#join MSA vals to NCDB cases outside of New England by COUNTY X STATE
ncdb_nonNE <- left_join(ncdb_cw %>% filter(STATE %!in% c("09", "23", "25", "33", "44")),
                        msa1970 %>% filter(STATE %!in% c("09", "23", "25", "33", "44")) %>% select(-TOWN))

ncdb_nonNE <- left_join(ncdb_nonNE,
                        msa1980 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% select(-TOWN))

ncdb_nonNE <- left_join(ncdb_nonNE,
                        msa1990 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% select(-TOWN))

ncdb_nonNE <- left_join(ncdb_nonNE,
                        msa2000 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% select(-TOWN))

#join MSA vals to NCDB cases inside New England by COUNTY X STATE X TOWN
ncdb_NE <- left_join(ncdb_cw %>% filter(STATE %in% c("09", "23", "25", "33", "44")),
                     msa1970 %>% filter(STATE %in% c("09", "23", "25", "33", "44")))

ncdb_NE <- left_join(ncdb_NE,
                     msa1980 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

ncdb_NE <- left_join(ncdb_NE,
                     msa1990 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

ncdb_NE <- left_join(ncdb_NE,
                     msa2000 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

#join NE and non-NE tables together
ncdb_cw <- bind_rows(ncdb_nonNE, ncdb_NE) %>%
  arrange(STATE, COUNTY)

#### B. Compute segregation measures from NCDB --------------------------------

ncdb_cw <- ncdb_cw %>%
  ungroup() %>%
  distinct(GEO2010, COUNTY, STATE, TOWN, MSA70, MSA80, MSA90, MSA00, CBSA) #really just need tract ID + MSA IDs

ncdb_interp$GEO2010 <- ncdb_interp$geo2010

ncdb <- inner_join(ncdb_cw, ncdb_interp)

seg_1970msa <- ncdb %>%
  filter(!is.na(MSA70)) %>%
  group_by(MSA70) %>%
  summarize(MSA70_D_1970 = (.5) * sum(abs(tnhbt1970/sum(tnhbt1970) - tnhwt1970/sum(tnhwt1970)))) %>%
  mutate_at(.vars = vars(starts_with("MSA70_D")),
            .funs = function(x){ifelse(is.nan(x), NA, x)})

seg_1980msa <- ncdb %>%
  filter(!is.na(MSA80)) %>%
  group_by(MSA80) %>%
  summarize(MSA80_D_1980 = (.5) * sum(abs(tnhbt1980/sum(tnhbt1980) - tnhwt1980/sum(tnhwt1980)))) %>%
  mutate_at(.vars = vars(starts_with("MSA80_D")),
            .funs = function(x){ifelse(is.nan(x), NA, x)})

seg_1990msa <- ncdb %>%
  filter(!is.na(MSA90)) %>%
  group_by(MSA90) %>%
  summarize(MSA90_D_1990 = (.5) * sum(abs(tnhbt1990/sum(tnhbt1990) - tnhwt1990/sum(tnhwt1990)))) %>%
  mutate_at(.vars = vars(starts_with("MSA90_D")),
            .funs = function(x){ifelse(is.nan(x), NA, x)})

seg_2000msa <- ncdb %>%
  filter(!is.na(MSA00)) %>%
  group_by(MSA00) %>%
  summarize(MSA00_D_2000 = (.5) * sum(abs(tnhbt2000/sum(tnhbt2000) - tnhwt2000/sum(tnhwt2000)))) %>%
  mutate_at(.vars = vars(starts_with("MSA00_D")),
            .funs = function(x){ifelse(is.nan(x), NA, x)})

seg_2010cbsa <- ncdb %>%
  filter(!is.na(CBSA)) %>%
  group_by(CBSA) %>%
  summarize(CBSA_D_1970 = (.5) * sum(abs(tnhbt1970/sum(tnhbt1970) - tnhwt1970/sum(tnhwt1970))),
            CBSA_D_1980 = (.5) * sum(abs(tnhbt1980/sum(tnhbt1980) - tnhwt1980/sum(tnhwt1980))),
            CBSA_D_1990 = (.5) * sum(abs(tnhbt1990/sum(tnhbt1990) - tnhwt1990/sum(tnhwt1990))),
            CBSA_D_2000 = (.5) * sum(abs(tnhbt2000/sum(tnhbt2000) - tnhwt2000/sum(tnhwt2000))),
            CBSA_D_2010 = (.5) * sum(abs(tnhbt2010/sum(tnhbt2010) - tnhwt2010/sum(tnhwt2010)))) %>%
  mutate_at(.vars = vars(starts_with("CBSA_D")),
            .funs = function(x){ifelse(is.nan(x), NA, x)})

seg_cw <- ncdb_cw %>%
  distinct(STATE, COUNTY, TOWN, MSA70, MSA80, MSA90, MSA00, CBSA)

seg_D <- left_join(seg_cw, seg_1970msa)
seg_D <- left_join(seg_D, seg_1980msa)
seg_D <- left_join(seg_D, seg_1990msa)
seg_D <- left_join(seg_D, seg_2000msa)
seg_D <- left_join(seg_D, seg_2010cbsa)            

glimpse(seg_D)

seg_D <- seg_D %>%
  filter(!is.na(MSA70)) #restrict to MSAs that existed in 1970

#NB: remaining SMSA70 rows with NAs for 1970 have 0 population counts, likely a suppression issue

cor.test(seg_D$MSA70_D_1970, seg_D$CBSA_D_1970)
cor.test(seg_D$MSA80_D_1980, seg_D$CBSA_D_1980)
cor.test(seg_D$MSA90_D_1990, seg_D$CBSA_D_1990)
cor.test(seg_D$MSA00_D_2000, seg_D$CBSA_D_2000)

seg_D %>% 
  select(ends_with("1970"), ends_with("1980"), ends_with("1990"), ends_with("2000")) %>% 
  summary

mean(seg_D$MSA70_D_1970-seg_D$CBSA_D_1970, na.rm = T)
mean(seg_D$MSA80_D_1980-seg_D$CBSA_D_1980, na.rm = T)
mean(seg_D$MSA90_D_1990-seg_D$CBSA_D_1990, na.rm = T)
mean(seg_D$MSA00_D_2000-seg_D$CBSA_D_2000, na.rm = T)

ggplot(seg_D, aes(x = MSA70_D_1970, y = CBSA_D_1970)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggplot(seg_D, aes(x = MSA80_D_1980, y = CBSA_D_1980)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggplot(seg_D, aes(x = MSA90_D_1990, y = CBSA_D_1990)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

ggplot(seg_D, aes(x = MSA00_D_2000, y = CBSA_D_2000)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))
