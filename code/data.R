# Load data --------------------------------------------------------------------

a <- paste0(dir_data, sapply(strsplit(x = tolower(locations), split = ".", fixed = TRUE), "[[", 2), ".csv") 

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  temp <- strsplit(x = a[i], split = "/")
  temp <- gsub(pattern = "\\.csv", replacement = "", x = temp[[1]][length(temp[[1]])])
  assign(x = paste0(temp, "0"), value = b)
}


# Wrangle data ----------------------------------------------------------------

# Spatial data

# Bering Sea Slope
reg_dat_bss = akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "EPSG:3338")
reg_dat_bss$survey.area <- reg_dat_bss$survey.area %>%
  dplyr::mutate(
    SRVY = "BSS", 
    color = alpha(colour = "grey50", 0.7), 
    SURVEY = "Bering Sea Slope") 

# Bering Sea (NBS and EBS)
reg_dat_bs = akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "EPSG:3338")
survey_reg_col <- gray.colors(length(unique(reg_dat_bs$survey.area$SURVEY))+2)
survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
reg_dat_bs$survey.area <- reg_dat_bs$survey.area %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      SURVEY == "EBS_SHELF" ~ "EBS", 
      SURVEY == "NBS_SHELF" ~ "NBS"), 
    color = alpha(colour = survey_reg_col, 0.7), 
    SRVY_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea") )

# NBS Bering Sea
reg_dat_nbs = akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "EPSG:3338")
reg_dat_nbs$survey.area <- reg_dat_nbs$survey.area %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      SURVEY == "EBS_SHELF" ~ "EBS", 
      SURVEY == "NBS_SHELF" ~ "NBS"), 
    color = alpha(colour = survey_reg_col, 0.7)[2], 
    SRVY_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea") )


# NBS Bering Sea (NBS and EBS)
reg_dat_ebs = akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "EPSG:3338")
reg_dat_ebs$survey.area <- reg_dat_ebs$survey.area %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      SURVEY == "EBS_SHELF" ~ "EBS", 
      SURVEY == "NBS_SHELF" ~ "NBS"), 
    color = alpha(colour = survey_reg_col, 0.7)[1], 
    SRVY_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea") )

# AI
reg_dat_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
reg_dat_ai$survey.area <- reg_dat_ai$survey.area %>%
  dplyr::mutate(
    SRVY = "AI", 
    color = alpha(colour = "grey80", 0.7), 
    SURVEY = "Aleutian Islands") 

# CPUE data
cpue_data <- dplyr::bind_rows(
  nbs_cpue0 %>% 
    dplyr::mutate(SRVY = "NBS", 
                  cpue_kgha = wgtcpue/100)  %>% 
    dplyr::select(SRVY, year, species_code, hauljoin, cpue_kgha, number_fish ), 
  ebsshelf_cpue0 %>% # EBS CPUE data
    dplyr::mutate(SRVY = "EBS", 
                  cpue_kgha = wgtcpue/100) %>% 
    dplyr::select(SRVY, year, species_code, hauljoin, cpue_kgha, number_fish ), 
  cpue0 %>%
    dplyr::mutate(SRVY = "AI") %>% # AI CPUE data
    dplyr::rename(cpue_kgha = wgtcpue) %>% 
    dplyr::select(SRVY, year, species_code, hauljoin, cpue_kgha, number_fish ) ) %>% 
  # define 'standard' tows
  dplyr::left_join(
    x = ., 
    y = haul0, 
    by = "hauljoin") %>% 
  dplyr::filter(abundance_haul == "Y" &
                  performance >= 0 &
                  !(is.null(stationid))) %>% 
  dplyr::select(SRVY, year, species_code, cpue_kgha, number_fish, stationid, start_latitude, start_longitude) #%>% 
# dplyr::mutate(common_name = dplyr::case_when( # normally, I'd left_join to RACEBASE.SPEICES, but I'm cutting down on downloads here
#   species_code == 10110 ~ "arrowtooth flounder", 
#   species_code == 30576 ~ "shortraker rockfish")) %>% 
# dplyr::filter(!is.na(common_name)) 

# > head(cpue_data)
# # A tibble: 6 × 7
# SRVY   year species_code cpue_kgha stationid start_latitude start_longitude
# <chr> <dbl>        <dbl>     <dbl> <chr>              <dbl>           <dbl>
#   1 EBS    2005          455     0     H-15                57.3           -159.
# 2 EBS    2005        50161     0     H-15                57.3           -159.
# 3 EBS    2005        71001     0     H-15                57.3           -159.

# comp data
comp_data <- dplyr::bind_rows(
  # length comps
  sizecomp_nbs_stratum0 %>% # NBS CPUE data
    dplyr::filter(stratum == 999999) %>% 
    dplyr::mutate(SRVY = "NBS", 
                  comp = "length") %>% 
    dplyr::rename(value = length) %>% 
    dplyr::select(SRVY, species_code, year, value, comp, males, females, unsexed), 
  sizecomp_ebs_plusnw_stratum0 %>% # EBS CPUE data
    dplyr::filter(stratum == 999999) %>% 
    dplyr::mutate(SRVY = "EBS", 
                  comp = "length") %>% 
    dplyr::rename(value = length) %>% 
    dplyr::select(SRVY, species_code, year, value, comp, males, females, unsexed), 
  sizecomp_total0 %>% # AI CPUE data
    dplyr::filter(summary_area == 999) %>% 
    dplyr::mutate(SRVY = "AI", 
                  comp = "length") %>% 
    dplyr::rename(value = length) %>% 
    dplyr::select(SRVY, species_code, year, value, comp, males, females, unsexed)) %>% 
  
  tidyr::pivot_longer( # rearrange data
    data = ., 
    cols = c("males", "females", "unsexed"), 
    names_to = "sex", 
    values_to = "pop") %>% 
  dplyr::bind_rows(
    ., 
    # age comps
    dplyr::bind_rows(
      agecomp_ebs_plusnw_stratum0 %>% 
        dplyr::mutate(SRVY = "EBS") %>% 
        dplyr::filter(stratum == 999999), 
      agecomp_nbs_stratum0 %>% 
        dplyr::mutate(SRVY = "NBS") %>% 
        dplyr::filter(stratum == 999999), 
      agecomp_total0 %>% 
        dplyr::rename(#SRVY = survey, 
                      year = survey_year, 
                      meanlen = mean_length, 
                      sdev = standard_deviation)  %>% 
        dplyr::mutate(SRVY = "AI"))  %>% 
      dplyr::rename(value = age, 
                    pop = agepop ) %>% 
      dplyr::select(SRVY, species_code, year, value, sex, pop) %>% 
      dplyr::mutate(comp = "age", 
                    sex = dplyr::case_when(
                      sex == 1 ~ "males", 
                      sex == 2 ~ "females", 
                      sex == 3 ~ "unsexed")) ) %>%
  dplyr::filter(pop > 0 &
                  value >= 0) %>% 
  dplyr::mutate(sex = str_to_sentence(sex), # this will assure the order of appearance in the plot
                sex = factor(sex, 
                             levels = c("Unsexed", "Males", "Females", "Immature females", "Mature females"), 
                             labels = c("Unsexed", "Males", "Females", "Immature females", "Mature females"),
                             ordered = TRUE)) %>% 
  dplyr::arrange(sex)

# > head(comp_data)
# # A tibble: 6 × 6
# SRVY  species_code  year length sex       pop
# <chr>        <dbl> <dbl>  <dbl> <ord>   <dbl>
#   1 EBS            420  2000   1890 Unsexed 98445
# 2 EBS            420  2014   1240 Unsexed 27993
# 3 EBS            420  2014   1380 Unsexed 27993
