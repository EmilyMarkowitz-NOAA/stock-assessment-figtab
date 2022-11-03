#' ---
#' title: 'Shotwell and Bryan EBS ATF and AI Shortraker Figures for SAFE Reports'
#' Issue: https://github.com/afsc-gap-products/data-requests/issues/41
#' Google drive: https://drive.google.com/drive/folders/1cvPy-_CgjiSsPy9Yxj38GXcTt2uUMmOP
#' Based on code found in https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report
#' author: 'E. H. Markowitz'
#' start date: 2022-10-26
#' End date: 2022-10-31
#' ---

# source("./code/run.R")
# 1

# Source help files ------------------------------------------------------------

# Load functions
source("./code/functions.R") # based on functions from https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/code/functions.R

# Download data
dir_data <- "./data/"
dir_out <- "./output/"
dir_fig <- paste0(dir_out, Sys.Date(), "/")
dir.create(dir_fig)

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
googledrive::drive_auth()
1
googledrive::drive_download(file = googledrive::as_id("https://docs.google.com/spreadsheets/d/12MsI5Ro9f9FOjib5REOJAg2ZXLH1MJJBC48T8e73QcA/edit#gid=0"), 
                            # type = "", 
                            overwrite = TRUE, 
                            path = paste0(dir_fig, "requests.xlsx"))

comb <- readxl::read_xlsx(path = paste0(dir_fig, "requests.xlsx"), 
                          sheet = "Requests", skip = 3) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(common_abrv = dplyr::case_when(
    common_name == "arrowtooth flounder" ~ "atf",
    common_name == "Kamchatka flounder" ~ "kam", 
    common_name == "shortracker rockfish" ~ "sr",
  TRUE ~"other"), 
  species_code = dplyr::case_when(
    common_name == "arrowtooth flounder" ~ 10110,
    common_name == "Kamchatka flounder" ~ 10112, 
    common_name == "shortracker rockfish" ~ 30576,
  TRUE ~ 0) ) %>% 
  dplyr::rename(SRVY = srvy) %>%
    # What can I actually do:
  dplyr::filter(!grepl(pattern = "BSS", x = SRVY) )  # %>% # I can't do anything with BSS data
  # dplyr::mutate( # remove plots that can't be plotted across multiple survey areas
    # cpue_coldpool = ifelse(grepl(pattern = "AI", x = SRVY), FALSE, cpue_coldpool) )
    # across(.cols = ends_with("comp"), .fns = ifelse, grepl(pattern = "_", x = SRVY), FALSE, .),
    # across(.cols = ends_with("coldpool"), .fns = ifelse, grepl(pattern = "_", x = SRVY), FALSE, .), 
    # across(.cols = ends_with("comp"), .fns = ifelse, grepl(pattern = "AI", x = SRVY), FALSE, .),
    # across(.cols = ends_with("coldpool"), .fns = ifelse, grepl(pattern = "AI", x = SRVY), FALSE, .))

# What does comb look like? 
# > comb
# # A tibble: 14 × 16
# yr_requested requestor report srvy        common_name         yr_start yr_end length_comp age_comp bubble bubble_coldpool cpue  cpue_coldpool notes common_abrv species_code
# <dbl> <chr>     <chr>  <chr>       <chr>                   <dbl>  <dbl> <lgl>       <lgl>    <lgl>  <lgl>           <lgl> <lgl>         <lgl> <chr>              <dbl>
#   1         2022 Shotwell  SAFE   EBS         arrowtooth flounder      1991   2022 TRUE        TRUE     FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 2         2022 Shotwell  SAFE   EBS_NBS     arrowtooth flounder      1991   2022 FALSE       FALSE    FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 3         2022 Shotwell  SAFE   EBS_NBS_BSS arrowtooth flounder      1991   2022 FALSE       FALSE    FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 4         2022 Shotwell  SAFE   BSS         arrowtooth flounder      1991   2022 TRUE        TRUE     FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 5         2022 Shotwell  SAFE   AI          arrowtooth flounder      1991   2022 TRUE        TRUE     FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 6         2022 Bryan     SAFE   EBS         Kamchatka flounder       1991   2022 TRUE        TRUE     FALSE  FALSE           TRUE  TRUE          NA    kam                10112
# 7         2022 Bryan     SAFE   EBS_NBS     Kamchatka flounder       1991   2022 FALSE       FALSE    FALSE  FALSE           TRUE  TRUE          NA    kam                10112
# 8         2022 Bryan     SAFE   EBS_NBS_BSS Kamchatka flounder       1991   2022 FALSE       FALSE    FALSE  FALSE           TRUE  TRUE          NA    kam                10112
# 9         2022 Bryan     SAFE   BSS         Kamchatka flounder       1991   2022 TRUE        TRUE     FALSE  FALSE           FALSE FALSE         NA    kam                10112
# 10         2022 Bryan     SAFE   AI          arrowtooth flounder      1991   2022 TRUE        TRUE     FALSE  FALSE           TRUE  TRUE          NA    atf                10110
# 11         2022 Shotwell  SAFE   AI          shortracker rockfish     1991   2022 TRUE        TRUE     TRUE   TRUE            TRUE  TRUE          NA    sr                 30576
# 12         2022 Shotwell  SAFE   EBS_NBS_BSS shortracker rockfish     1991   2022 FALSE       FALSE    TRUE   TRUE            TRUE  TRUE          NA    sr                 30576
# 13         2022 Shotwell  SAFE   EBS_NBS     shortracker rockfish     1991   2022 FALSE       FALSE    TRUE   TRUE            TRUE  TRUE          NA    sr                 30576
# 14         2022 Shotwell  SAFE   BSS         shortracker rockfish     1991   2022 TRUE        TRUE     TRUE   TRUE            TRUE  TRUE          NA    sr                 30576


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
      dplyr::filter(SRVY %in% SRVY1 & 
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
      save_figures(dir_fig = dir_fig, 
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
      save_figures(dir_fig = dir_fig, 
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
    
    if (nrow(table_raw0) != 0){
    
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
        save_figures(dir_fig = dir_fig, 
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
        save_figures(dir_fig = dir_fig, 
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
        save_figures(dir_fig = dir_fig, 
                     filename0 = paste0(filename00,"_", fig_type, "_", min(yrs),"-",max(yrs)), 
                     figure = figure, 
                     height = height)
        
      }
      
      # Advance for next set of years 
      counter <- counter_new
      
    }
    }
  }
  
}


# SAVE METADATA ----------------------------------------------------------------
rmarkdown::render(paste0("./README.Rmd"),
                  output_dir = "./",
                  output_file = paste0("README.md"))


