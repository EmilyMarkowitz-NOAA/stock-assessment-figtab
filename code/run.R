#' ---
#' title: 'Shotwell and Bryan EBS ATF and AI Shortraker Figures for SAFE Reports'
#' Issue: https://github.com/afsc-gap-products/data-requests/issues/41
#' Google drive: https://drive.google.com/drive/folders/1cvPy-_CgjiSsPy9Yxj38GXcTt2uUMmOP
#' Based on code found in https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report
#' author: 'E. H. Markowitz'
#' start date: 2022-10-26
#' End date: 2022-10-31
#' ---

# Source help files ------------------------------------------------------------

# Load functions
source("./code/functions.R") # based on functions from https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/code/functions.R

# Download data
dir_data <- "./data/"

locations <- c( # data pulled from Oracle for these figures: 
  "RACEBASE.HAUL",
  
  # CPUE
  "EBSSHELF.EBSSHELF_CPUE", # "HAEHNR.CPUE_EBS_PLUSNW", 
  "NBSSHELF.NBS_CPUE", # "HAEHNR.CPUE_NBS", 
  "AI.CPUE", 
  # "HOFFJ.CPUE_EBSSLOPE_POS", -> needs to be peer reviewed
  
  # Age comps
  "HAEHNR.AGECOMP_EBS_PLUSNW_STRATUM",
  "HAEHNR.AGECOMP_NBS_STRATUM", 
  "AI.AGECOMP_TOTAL", 
  # We currently do not know where BSS age comp data are/were ever made?
  
  # size comp
  "HAEHNR.sizecomp_nbs_stratum",
  "HAEHNR.sizecomp_ebs_plusnw_stratum", 
  "AI.SIZECOMP_TOTAL"
  # "HOFFJ.SIZECOMP_EBSSLOPE" -> needs to be peer reviewed
)

# source("./code/data_dl.R") # run annually

# Wrangle data
source("./code/data.R")

# Knowns and requests ----------------------------------------------------------
# yrs <- c(2018:2022) # years for distribution plots

maxyr <-2022

# Requests: 
# Rephrased requests as data frame so I can loop through each ask: 

## 1.) Eastern Bering Sea: ATF 1992-2022
### CPUE plots 3 row x 2 col plus NBS and BSS (one with coldpool outline in red or yellow, one without)
### M and F side by side length comps EBS only
### M and F side by side age comps EBS only
### M and F side by side length comps BSS only
### M and F side by side age comps BSS only
comb <- data.frame(
  # data
  SRVY = c("EBS", "EBS_NBS", "EBS_NBS_BSS", "BSS"), 
  common_abrv = "atf",
  common_name = "arrowtooth flounder",
  species_code = 10110, 
  year_start = 1991, 
  year_end = maxyr, 
  # plots
  bubble = FALSE, 
  bubble_coldpool = FALSE, 
  cpue = c(TRUE, TRUE, TRUE, FALSE), 
  cpue_coldpool = c(TRUE, TRUE, TRUE, FALSE), 
  length_comp = c(TRUE, FALSE, FALSE, TRUE) , 
  age_comp = c(TRUE, FALSE, FALSE, TRUE) )

## 2.) Eastern Bering Sea: KAM 1991-2022 (although note from Lyle Britt that high confidence in speciation did not start until 1992 for ATF and KAM)
### CPUE plots 3 row x 2 col plus NBS and BSS (one with coldpool outline in red or yellow, one without)
### M and F side by side length comps EBS only
### M and F side by side age comps EBS only
### M and F side by side length comps BSS only
### M and F side by side age comps BSS only
comb <- dplyr::bind_rows(
  comb, 
  data.frame(
    # data
    SRVY = c("EBS", "EBS_NBS", "EBS_NBS_BSS", "BSS"), 
    common_abrv = "kam",
    common_name = "Kamchatka flounder",
    species_code = 10112, 
    year_start = 1991, 
    year_end = maxyr, 
    # plots
    bubble = FALSE, 
    bubble_coldpool = FALSE, 
    cpue = c(TRUE, TRUE, TRUE, FALSE), 
    cpue_coldpool = c(TRUE, TRUE, TRUE, FALSE), 
    length_comp = c(TRUE, FALSE, FALSE, TRUE) , 
    age_comp = c(TRUE, FALSE, FALSE, TRUE) )
)

## 3.) Aleutian Islands Survey: ATF 1991-2022
### Bubble plots 3 row x 1 col
### CPUE plots 3 row x 1 col
### M and F side by side length comps
### M and F side by side age comps

comb <- dplyr::bind_rows(
  comb, 
  data.frame(
    # data
    SRVY = "AI", 
    common_abrv = "atf",
    common_name = "arrowtooth flounder",
    species_code = 10110, 
    year_start = 1991, 
    year_end = maxyr, 
    # plots
    bubble = TRUE, 
    bubble_coldpool = FALSE, 
    cpue = TRUE, 
    cpue_coldpool = FALSE, 
    length_comp = TRUE, 
    age_comp = TRUE)
)

## 4.) Aleutian Islands Survey: KAM 1991-2022
### Bubble plots 3 row x 1 col
### CPUE plots 3 row x 1 col
### M and F side by side length comps
### M and F side by side age comps

comb <- dplyr::bind_rows(
  comb, 
  data.frame(
    # data
    SRVY = "AI", 
    common_abrv = "kam",
    common_name = "Kamchatka flounder",
    species_code = 10112, 
    year_start = 1991, 
    year_end = maxyr, 
    # plots
    bubble = TRUE, 
    bubble_coldpool = FALSE, 
    cpue = TRUE, 
    cpue_coldpool = FALSE, 
    length_comp = TRUE, 
    age_comp = TRUE)
)

## 5.) Aleutian Islands Survey: SR 1991-2022
### Bubble plots 3 row x 1 col
### CPUE plots 3 row x 1 col
### Totally forgot we already have the ridges plot for Shortraker lengths, so all good there!

comb <- dplyr::bind_rows(
  comb, 
  data.frame(
    # data
    SRVY = "AI", 
    common_abrv = "sr",
    common_name = "shortracker rockfish",
    species_code = 30576, 
    year_start = 1991, 
    year_end = maxyr, 
    # plots
    bubble = TRUE, 
    bubble_coldpool = FALSE, 
    cpue = TRUE, 
    cpue_coldpool = FALSE, 
    length_comp = FALSE, 
    age_comp = FALSE)
)

## 6.) Eastern Bering Sea Slope (BSS): SR 2002-2016
### CPUE plots 3 row x 2 col, not sure how this will look, but might need an outline of EBS and NBS survey just for reference? This may be a really odd one to do, so no worries if it doesn't work.

comb <- dplyr::bind_rows(
  comb, 
  data.frame(
    # data
    SRVY = c("BSS", "BSS_EBS_NBS"), 
    common_abrv = "sr",
    common_name = "shortracker rockfish",
    species_code = 30576, 
    year_start = 1991, 
    year_end = maxyr, 
    # plots
    bubble = TRUE, 
    bubble_coldpool = TRUE, 
    cpue = TRUE, 
    cpue_coldpool = TRUE, 
    length_comp = FALSE, 
    age_comp = FALSE)
)

# What does comb look like? 
# > comb
# SRVY common_abrv          common_name species_code year_start year_end bubble bubble_coldpool  cpue cpue_coldpool length_comp age_comp
# 1          EBS         atf  arrowtooth flounder        10110       1991     2022  FALSE           FALSE  TRUE          TRUE        TRUE     TRUE
# 2      EBS_NBS         atf  arrowtooth flounder        10110       1991     2022  FALSE           FALSE  TRUE          TRUE       FALSE    FALSE
# 3  EBS_NBS_BSS         atf  arrowtooth flounder        10110       1991     2022  FALSE           FALSE  TRUE          TRUE       FALSE    FALSE
# 4          BSS         atf  arrowtooth flounder        10110       1991     2022  FALSE           FALSE FALSE         FALSE        TRUE     TRUE
# 5          EBS         kam   Kamchatka flounder        10112       1991     2022  FALSE           FALSE  TRUE          TRUE        TRUE     TRUE
# 6      EBS_NBS         kam   Kamchatka flounder        10112       1991     2022  FALSE           FALSE  TRUE          TRUE       FALSE    FALSE
# 7  EBS_NBS_BSS         kam   Kamchatka flounder        10112       1991     2022  FALSE           FALSE  TRUE          TRUE       FALSE    FALSE
# 8          BSS         kam   Kamchatka flounder        10112       1991     2022  FALSE           FALSE FALSE         FALSE        TRUE     TRUE
# 9           AI         atf  arrowtooth flounder        10110       1991     2022   TRUE           FALSE  TRUE         FALSE        TRUE     TRUE
# 10          AI         kam   Kamchatka flounder        10112       1991     2022   TRUE           FALSE  TRUE         FALSE        TRUE     TRUE
# 11          AI          sr shortracker rockfish        30576       1991     2022   TRUE           FALSE  TRUE         FALSE       FALSE    FALSE
# 12         BSS          sr shortracker rockfish        30576       1991     2022   TRUE            TRUE  TRUE          TRUE       FALSE    FALSE
# 13 BSS_EBS_NBS          sr shortracker rockfish        30576       1991     2022   TRUE            TRUE  TRUE          TRUE       FALSE    FALSE

# What can I actually do:
comb <- comb %>% 
  dplyr::filter(!grepl(pattern = "BSS", x = SRVY) ) # I can't do anything with BSS data

# Figures ----------------------------------------------------------------------

# Loop through each request (documented in the comb data.frame)
for (i in 1:nrow(comb)) {
  
  # Define loop specific knowns
  comb0 <- comb[i,]
  SRVY1 <- SRVY <- unlist(strsplit(x = comb0$SRVY, split = "_", fixed = TRUE))
  spp_code <- comb0$species_code
  common_abrv <- comb0$common_abrv
  common_name <- comb0$common_name
  filename00 <- tolower(paste0("figure_",common_abrv,"_",comb0$SRVY))
  print(filename00)
  
  # Length and Age Comp plots
  if (comb0$length_comp | comb0$age_comp) {

    # wrangle data for this request
    table_raw <- comp_data  %>% 
      dplyr::filter(SRVY %in% SRVY & 
                      species_code %in% spp_code & 
                      year >= comb0$year_start & 
                      year <= comb0$year_end) %>% 
      dplyr::group_by(sex, SRVY, species_code, year, comp, value) %>% 
      dplyr::summarise(pop = sum(pop, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    # M and F Length Comps
    if (comb0$length_comp) {
      fig_type <- "comp_length"
      
      # Males
      figure1 <- plot_sizecomp(
        sizecomp0 = table_raw %>% 
          dplyr::filter(comp == "length" & 
                          sex == "Males") %>% 
          dplyr::rename(length = value), 
        spp_code = spp_code,
        spp_print = "Male",
        type = "fork lengths", 
        unit = "cm",
        ridgeline = TRUE,
        print_n = FALSE) +
        # Modify output figure
        ggtitle("Males") + 
        ylab("Survey years")
      
      # Females 
      figure2 <- plot_sizecomp(
        sizecomp0 = table_raw %>% 
          dplyr::filter(comp == "length" & 
                          sex == "Females") %>% 
          dplyr::rename(length = value), 
        spp_code = spp_code,
        spp_print = "Female",
        type = "fork lengths", 
        unit = "cm",
        ridgeline = TRUE,
        print_n = FALSE) +
        # Modify output figure
        ggtitle("Females") + 
        ylab("")
      
      # Layout figures
      figure <- cowplot::plot_grid(figure1, # Males
                                   figure2, # Females
                                   ncol = 2, nrow = 1, 
                                   rel_widths = c(0.5, 0.5), 
                                   rel_heights = c(1,1))
      
      figure <- cowplot::plot_grid(cowplot::ggdraw() + # Title
                                     cowplot::draw_text(text = paste0(SRVY, " ", common_name, " length comps"), 
                                                        fontface = 'bold'), 
                                   figure, # Males and Females plotted next to each other
                                   ncol = 1, nrow = 2, 
                                   rel_widths = c(1, 1), 
                                   rel_heights = c(0.1,0.9))
      
      # save figures (from functions.R)
      save_figures(dir_data = dir_data, 
                   filename0 = paste0(filename00, "_", fig_type), 
                   figure = figure)
    }
    
    # M and F Age comps
    if (comb0$age_comp) {
      fig_type <- "comp_age"
      
      # Males
      figure1 <- plot_sizecomp( # Males
        sizecomp0 = table_raw %>% 
          dplyr::filter(comp == "age" & 
                          sex == "Males") %>% 
          dplyr::rename(length = value), 
        spp_code = spp_code,
        spp_print = "Male",
        type = "age", 
        unit = "years",
        ridgeline = TRUE,
        print_n = FALSE) +
        # Modify output figure
        ggtitle("Males") + 
        ylab("Survey years")
      
      # Females 
      figure2 <- plot_sizecomp(
        sizecomp0 = table_raw %>% 
          dplyr::filter(comp == "age" & 
                          sex == "Females") %>% 
          dplyr::rename(length = value), 
        spp_code = spp_code,
        spp_print = "Female",
        type = "age", 
        unit = "years",
        ridgeline = TRUE,
        print_n = FALSE) +
        # Modify output figure
        ggtitle("Females") + 
        ylab("")
      
      # Layout figures
      figure <- cowplot::plot_grid(figure1, # Males
                                   figure2, # Females
                                   ncol = 2, nrow = 1, 
                                   rel_widths = c(0.5, 0.5), 
                                   rel_heights = c(1,1))
      
      figure <- cowplot::plot_grid(cowplot::ggdraw() + # Title
                                     cowplot::draw_text(text = paste0(SRVY, " ", common_name, " age comps"), 
                                                        fontface = 'bold'), 
                                   figure, # Males and Females plotted next to each other
                                   ncol = 1, nrow = 2, 
                                   rel_widths = c(1, 1), rel_heights = c(0.1,0.9))
      
      # save figures (from functions.R)
      save_figures(dir_data = dir_data, 
                   filename0 = paste0(filename00, "_", fig_type), 
                   figure = figure)
    }
    
    
  }
  
  
  if (comb0$cpue | comb0$cpue_coldpool | comb0$bubble) {
    
    # Define mapping information
    if (sum(SRVY %in% c("EBS", "NBS")) == 2) {
      reg_dat <- reg_dat_bs
      region <- "bs.all"
      fig_per_row <- 3
      fig_per_col <- 2
    } else if (SRVY == "EBS") {
      reg_dat <- reg_dat_ebs
      region <- "bs.south"
      fig_per_row <- 3
      fig_per_col <- 2
    } else if (SRVY == "NBS") {
      reg_dat <- reg_dat_nbs
      region <- "bs.north"
      fig_per_row <- 3
      fig_per_col <- 2
    } else if (SRVY == "BSS") {
      reg_dat <- reg_dat_bss
      region <- "bss"
      fig_per_row <- 1
      fig_per_col <- 3
    } else if (SRVY == "AI") {
      reg_dat <- reg_dat_ai
      region <- "ai"
      fig_per_row <- 3
      fig_per_col <- 1
    }
    
    # wrangle data for this request
    table_raw0 <- cpue_data %>% 
      dplyr::filter(SRVY %in% SRVY1 & 
                      species_code %in% spp_code & 
                      year >= comb0$year_start & 
                      year <= comb0$year_end)
    
    set.breaks <- set_breaks(dat = table_raw0 %>% 
                               dplyr::filter(cpue_kgha > 0), 
                             var = "cpue_kgha")
    
    # The years that need to be plotted: 
    combyrs <- sort(unique(table_raw0$year))
    num_fig <- ceiling(x = length(combyrs) / (fig_per_row*fig_per_col))
    
    # Loop through each sub-figure
    counter <- 0
    for (ii in 1:num_fig) {
      
      # Advance to the next set of years
      counter_new <- counter+(fig_per_row*fig_per_col)
      yrs <- combyrs[(counter+1) : ifelse(length(combyrs) < counter_new, length(combyrs), counter_new) ]
      yrs <- yrs[yrs %in% combyrs]
      print(paste0(min(yrs), "-", max(yrs)))
      
      # Define figure height depending on # of years to plot
      height <- dplyr::case_when(
        length(yrs) == 1 ~ 4, 
        length(yrs) < fig_per_row * fig_per_col ~ 6,
        TRUE ~ 8)

      # wrangle data for this request's sub figure
      table_raw <- table_raw0 %>% 
        dplyr::filter(year %in% yrs)
      
      # Plot CPUE figures
      if (comb0$cpue) {
        fig_type <- "cpue"
        
        figure <- plot_idw_xbyx(
          yrs = yrs,
          dat = table_raw, 
          lat = "start_latitude",
          lon = "start_longitude",
          var = "cpue_kgha",
          year = "year",
          grid = "extrapolation.grid",
          reg_dat = reg_dat, 
          region = region, 
          key.title = paste0(paste0(SRVY, collapse = " and "), " ", common_name, "\nWeight CPUE (kg/ha)"),
          set.breaks = set.breaks, 
          row0 = fig_per_row, 
          legend_srvy_reg = FALSE)
        
        # save figures (from functions.R)
        save_figures(dir_data = dir_data, 
                     filename0 = paste0(filename00,"_", fig_type, "_", min(yrs),"-",max(yrs)), 
                     figure = figure, 
                     height = height)
      }
      
      # Plot CPUE figures with coldpool
      if (comb0$cpue_coldpool) {
        fig_type <- "cpue_coldpool"
        
        figure <- plot_idw_xbyx(
          yrs = yrs,
          dat = table_raw, 
          lat = "start_latitude",
          lon = "start_longitude",
          var = "cpue_kgha",
          year = "year",
          grid = "extrapolation.grid",
          reg_dat = reg_dat, 
          region = region, 
          key.title = paste0(paste0(SRVY, collapse = " and "), " ", common_name, "\nWeight CPUE (kg/ha)"),
          set.breaks = set.breaks, 
          row0 = fig_per_row, 
          legend_srvy_reg = FALSE, 
          plot_coldpool = TRUE)
        
        # save figures (from functions.R)
        save_figures(dir_data = dir_data, 
                     filename0 = paste0(filename00,"_", fig_type, "_", min(yrs),"-",max(yrs)), 
                     figure = figure, 
                     height = height)
        
      }
      
      # Plot bubble plots figures
      if (comb0$bubble) {
        
        fig_type <- "bubble"
        
        figure <- plot_pa_xbyx(
          yrs = yrs,
          dat = table_raw %>% 
            dplyr::filter(cpue_kgha > 0),
          lat = "start_latitude",
          lon = "start_longitude",
          year = "year",
          reg_dat = reg_dat,
          key.title = paste0(paste0(SRVY, collapse = " and "), " ", common_name, "\n"),
          row0 = fig_per_row,
          plot_bubble = TRUE)
        
        # save figures (from functions.R)
        save_figures(dir_data = dir_data, 
                     filename0 = paste0(filename00,"_", fig_type, "_", min(yrs),"-",max(yrs)), 
                     figure = figure, 
                     height = height)
        
      }
      
      # Advance for next set of years 
      counter <- counter_new
      
    }
    
  }
  
}


# SAVE METADATA ----------------------------------------------------------------
rmarkdown::render(paste0("./README.Rmd"),
                  output_dir = "./",
                  output_file = paste0("README.md"))


