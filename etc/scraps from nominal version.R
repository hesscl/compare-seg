## Transform OMB and NCDB tables

The next step is to transform the starting tables to the structure needed. For each OMB table, we would like a row to uniquely identify counties and to denote what metropolitan area code was tied to the county at a specific point in time. Unfortunately, New England states can have multiple MSAs to a county, so because of this situation we level the data to capture all unique county subdivisions. Outside of New England there can be multiple county subdivisions to a county, but definitions of MSAs do not vary within a county (i.e. a county has at most 1 valid MSA code).

The process for a given period follows these steps:
  
  1. remove completely blank rows (denoted by not having a state value)
2. name the COUSUB variable (first valid line of each table has no value since not in New England)
3. pad zeroes on the ID codes for comparability
4. remove county subdivisions that did not have a MSA code for the specified year
5. group table by unique STATE X COUNTY X COUSUB combinations. In NE this varies by STATE X COUNTY X COUSUB, outside NE the table varies by STATE X COUNTY only
6. filter rows where: 
  + multiple SMSAXX codes exist and there is no town denoted (these are heading rows for each MSA in New England) OR
+ there's only one MSA value among the group (this is all of the counties outside of New England)
7. return the unique value found for each STATE X COUNTY X COUSUB combination (length 1 in each combination)

```{r Transform data 1970/1980, warning=FALSE}
#1970 SMSA definitions
msa1970 <- msa1970 %>% #wipe out the cases for the MSA alone --- they are coded
filter(!is.na(STATE)) %>% #as missing STATE IDs
mutate(COUSUB = X6) %>% #establish colname for COUSUB field
select(STATE, COUNTY, COUSUB, SMSA70) %>% #remove other variables from data frame
mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"), #pad ID strings with 0s as needed
COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
COUSUB = str_pad(COUSUB, 5, side = "left", pad = "0"),
FIPS = paste0(STATE, COUNTY)) %>%
filter(!is.na(SMSA70)) %>% #remove rows with no valid MSA code
group_by(STATE, COUNTY, FIPS, COUSUB) %>% #group by unique State X Cty/Cty X Town combinations
filter((length(unique(SMSA70)) != 1 & !is.na(COUSUB)) | #if two or more SMSA codes for the combination, keep those that correspond to a COUSUB in NE
length(unique(SMSA70)) == 1) %>% #and keep all rows where there's only one SMSA code for the combination (i.e. one county, one SMSA)
summarize(MSA70 = unique(SMSA70)) #for each combination, the summary value is the unique (i.e. only) code present

#1980 SMSA definitions
msa1980 <- msa1980 %>%
  filter(!is.na(STATE)) %>%
  mutate(COUSUB = X8) %>%
  select(STATE, COUNTY, COUSUB, SMSA80) %>%
  mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
         COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
         COUSUB = str_pad(COUSUB, 5, side = "left", pad = "0"),
         FIPS = paste0(STATE, COUNTY)) %>%
  filter(!is.na(SMSA80)) %>%
  group_by(STATE, COUNTY, FIPS, COUSUB) %>%
  filter((length(unique(SMSA80)) != 1 & !is.na(COUSUB)) |
           length(unique(SMSA80)) == 1) %>%
  summarize(MSA80 = unique(SMSA80))
```

For the 1990 and 2000 OMB delineations the process needs to deal with OMB's creation of _consolidated metropolitan statistical areas (CMSA)_. Large metropolitan areas were assigned a single CMSA code covering multiple distinct housing and employment markets. The 1990 and 2000 metropolitan units most comparable to the 1970 and 1980 _standard metropolitan statistical areas (SMSA)_ are called the _primary metropolitan statistical areas (PMSA)_ that exist within CMSAs. Thus, for the following code we process the data more or less the same up to the point of using `summarize()` to assign the unique MSA code observed for the STATE X COUNTY X COUSUB combination. 

If there is no consolidated MSA for a given county, the delineation table notes a value in the same column as it does when there is despite meaning something different (it calls it MSA/CMSA codes as a column name). This means for the 1990 and 2000 data aggregation with `summarize()`, we now need to conditionally assign either the PMSA code or the CMSA code. The column of the OMB file for CMSA has MSA values for all counties and county subdivisions that do not have a higher-oreder CMSA. When a higher-order CMSA exists, a separate field indicates the PMSA (otherwise missing as `NA`). `ifelse()` has three arguments---the first is the expression to test, the second is the value to assign to cases where the expression was `TRUE`, the last is the value to assign where the expression was `FALSE`. Accordingly, `ifelse(is.na(unique(PMSA90)), unique(CMSA90), unique(PMSA90)))` assigns the unique CMSA code in 1990 to cases where the value for PMSA in 1990 was missing, and otherwise uses the PMSA code if not missing.

```{r Transform data 1990/2000, warning=FALSE}
#1990 PMSA definitions
msa1990 <- msa1990 %>%
filter(!is.na(STATE)) %>%
mutate(COUSUB = X12) %>%
select(STATE, COUNTY, COUSUB, PMSA90, CMSA90) %>%
mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
COUSUB = str_pad(COUSUB, 5, side = "left", pad = "0"),
FIPS = paste0(STATE, COUNTY)) %>%
filter(!is.na(CMSA90)) %>%
group_by(STATE, COUNTY, FIPS, COUSUB) %>%
filter((length(unique(PMSA90)) != 1 & !is.na(COUSUB)) |
length(unique(PMSA90)) == 1) %>%
summarize(MSA90 = ifelse(is.na(unique(PMSA90)), unique(CMSA90), unique(PMSA90))) 

#2000 PMSA definitions
msa2000 <- msa2000 %>%
filter(!is.na(STATE)) %>%
mutate(COUSUB = X12) %>%
select(STATE, COUNTY, COUSUB, PMSA99, CMSA99) %>%
mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
COUSUB = str_pad(COUSUB, 5, side = "left", pad = "0"),
FIPS = paste0(STATE, COUNTY)) %>%
filter(!is.na(CMSA99)) %>%
group_by(STATE, COUNTY, FIPS, COUSUB) %>%
filter((length(unique(PMSA99)) != 1 & !is.na(COUSUB)) |
length(unique(PMSA99)) == 1) %>%
summarize(MSA00 = ifelse(is.na(unique(PMSA99)), unique(CMSA99), unique(PMSA99)))
```

After munging the OMB delineation files, we do some light cleaning to the vanilla NCDB object (i.e. the NCDB as it comes from Geolytics) to remove tracts outside of CBSAs and to pad FIPS IDs to match the same structure as in other tables. This census tract unit table will be used to identify what MSA a given tract was in at a specific point in time based on STATE X COUNTY or STATE X COUNTY X COUSUB combinations (if New England).

```{r Crosswalk}
#Crosswalk table of tracts with accompanying higher level IDs
ncdb_cw <- ncdb_vanilla %>%
filter(CBSA != "") %>%
mutate(STATE = str_pad(STATE, 2, side = "left", pad = "0"),
COUNTY = str_pad(COUNTY, 3, side = "left", pad = "0"),
COUSUB = str_pad(COUSUB, 5, side = "left", pad = "0"),
FIPS = paste0(STATE, COUNTY))
```

<br>
<hr>
<br>

## Compile the crosswalk table

There are two sets of joins here. The first is for all tracts outside of New England and results in `ncdb_nonNE`, the second is for all New England tracts and creates `ncdb_NE`. Note how the filter function that is piped onto each object used in `left_join` handles this split by using `%in%` or `%!in%` matches made to the set of New England states with MSAs at the point in time. To ensure that the joins outside of New England are made according to STATE X COUNTY matches, we compute the join only after removing the COUSUB field from the table of segregation measures for each decade via `select(-COUSUB)`. The tracts from the NCDB may only match one set of values in the segregation table, so the resulting table of `bind_rows(ncdb_nonNE, ncdb_NE)` has the same number of observations as the `ncdb_cw` object we start with.

> **NB: There are some code changes for New England county subdivsions (Stamford CT, ) **

```{r Compile transformed objects, warning=FALSE}
start <- nrow(ncdb_cw)

#join MSA vals to NCDB cases outside of New England by COUNTY X STATE
ncdb_nonNE <- left_join(ncdb_cw %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")),
msa1970 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% 
select(-COUSUB))

ncdb_nonNE <- left_join(ncdb_nonNE %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")),
msa1980 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% 
select(-COUSUB)) 

ncdb_nonNE <- left_join(ncdb_nonNE %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")),
msa1990 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% 
select(-COUSUB)) 

ncdb_nonNE <- left_join(ncdb_nonNE %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")),
msa2000 %>% filter(STATE %!in% c("09", "23", "25", "33", "44", "50")) %>% 
select(-COUSUB)) 

#join MSA vals to NCDB cases inside New England by COUNTY X STATE X COUSUB
ncdb_NE <- left_join(ncdb_cw %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")),
msa1970 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

ncdb_NE <- left_join(ncdb_NE %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")),
msa1980 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

ncdb_NE <- left_join(ncdb_NE %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")),
msa1990 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

ncdb_NE <- left_join(ncdb_NE %>%  filter(STATE %in% c("09", "23", "25", "33", "44", "50")),
msa2000 %>% filter(STATE %in% c("09", "23", "25", "33", "44", "50")))

#join NE and non-NE tables together
ncdb_cw <- bind_rows(ncdb_nonNE, ncdb_NE) %>%
arrange(STATE, COUNTY)

end <- nrow(ncdb_cw)

start == end
```
