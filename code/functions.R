# Load libaries and funcitons --------------------------------------------------

PKG <- c(
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion
  
  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  
  
  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  # "cowplot",
  "png",
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
  "viridis", 
  
  # other tidyverse
  "plyr",
  "dplyr",
  "googledrive",
  "magrittr",
  "readr",
  "tidyr",
  
  # Text Management
  "stringr",
  "readtext",
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  # "coldpool", # devtools::install_github("sean-rohan-noaa/coldpool")
  
  
  # Spatial
  "sf",
  "rlist", 
  "jsonlite", 
  "prettymapr",
  "rgdal", 
  "rosm", 
  "shadowtext", 
  "ggspatial", 
  "digest", 
  "ggsn",
  "rgdal", 
  "ps", 
  "backports", 
  "callr", 
  "labeling", 
  "gstat", 
  "magrittr", 
  "raster", 
  "reshape", 
  "stars",
  "grid", 
  
  # Tables
  "officer", 
  "flextable")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

find_units <- function(unit = "", unt = "", dat, divby = NULL){
  
  # x <- ifelse(unit == "", "s", paste0(" ", unit))
  x <- unit#ifelse(unit != "", paste0(" ", unit), unit)
  x_ <- ifelse(unt =="", "", unt)
  
  # find appropriate units
  
  if (is.null(divby)) {
    min_val <- min(dat, na.rm = TRUE)
    min_val1 <- NMFSReports::xunits(value = min_val, words = TRUE)
  } else {
    min_val <- divby
    min_val1 <- NMFSReports::xunits(value = divby, words = TRUE)
  }
  
  # unit_word <- ""
  # unit_wrd <- ""
  if (min_val<1e3) {
    divby <- 1
    unit_word <- ifelse(unit == "", "", paste0(" (", x, ")"))
    unit_wrd <- paste0("", x_)
  } else if (min_val<1e6) {
    divby <- 1e3
    unit_word <- paste0(" (thousand",
                        ifelse(unit == "", "s", paste0(" ", unit)),
                        ")" )
    unit_wrd <- paste0("K", x_)
  } else if (grepl(pattern = "million", x = min_val1)) {
    divby <- 1e6
    unit_word <- paste0(" (million",
                        ifelse(unit == "", "s", paste0(" ", unit)),
                        ")")
    unit_wrd <- paste0("M", x_)
  } else if (grepl(pattern = "billion", x = min_val1)) {
    divby <- 1e9
    unit_word <- paste0(" (billion",
                        ifelse(unit == "", "s", paste0(" ", unit)),
                        ")")
    unit_wrd <- paste0("B", x_)
  } else if (grepl(pattern = "trillion", x = min_val1)) {
    divby <- 1e12
    unit_word <- paste0(" (trillion",
                        ifelse(unit == "", "s", paste0(" ", unit)),
                        ")")
    unit_wrd <- paste0("T", x_)
  }
  
  
  return(list("divby" = divby, 
              "unit_word" = unit_word, 
              "unit_wrd" = unit_wrd))
}


set_breaks <- function(dat, var) {
  set.breaks0 <- classInt::classIntervals(var = as.numeric(unlist(dat[,var]))[as.numeric(unlist(dat[,var])) != 0], 
                                          n = 5, style = "jenks")$brks
  set.breaks <- c()
  
  for (i in 1:length(set.breaks0)) {
    
    if (i == length(set.breaks0)) {
      set.breaks<-c(set.breaks, ceiling(x = set.breaks0[i])) #Inf)# #round(set.breaks0[i], digits = 0))
    } else if (i == 1) {
      set.breaks<-c(set.breaks, 0)
    } else {    
      set.breaks <- c(set.breaks, 
                      plyr::round_any(x = set.breaks0[i], 
                                      accuracy = ifelse(max(set.breaks0[i])>300, 100, 
                                                        ifelse(max(set.breaks0[i])>100, 50, 
                                                               ifelse(max(set.breaks0[i])>20, 10, 
                                                                      1))), 
                                      f = ceiling))    
    }
  }
  set.breaks <- unique(set.breaks)
  
  return(set.breaks)
}


save_figures <- function(dir_data, filename0, figure, height = 8, width = 6.5) {
  
  ggsave(filename = paste0(dir_data, "figs/", filename0, ".png"), 
         plot = figure, 
         device = "png", 
         bg = "white", 
         height = height, 
         width = width)
  
  # ggsave(filename = paste0(dir_data, "figs/", filename0, ".pdf"), 
  #        plot = figure, 
  #        device = "pdf",
  #        height = 8, width = 6.5)
}


#' plot_size_comp
#'
#' @param sizecomp0 data.frame with these columns: "year", "taxon", "SRVY", "species_code", "sex", "pop", "length"   
#' @param length_data0 data.frame of sampled lengths
#' @param spp_code numeric. 
#' @param spp_print string. 
#' @param print_n TRUE/FALSE. Default = FALSE. Will print n = number of the species counted. 
#' @param ridgeline plot as ridgeline? Default = FALSE. 
#'
#' @return
#' @export
#'
#' @examples
plot_sizecomp <- function(sizecomp0,
                          length_data0 = NULL,
                          spp_code, 
                          spp_print, 
                          type = "length", 
                          print_n = FALSE, 
                          ridgeline = FALSE, 
                          unit = NULL){
  
  table_raw <- sizecomp0 %>%
    # dplyr::mutate(sex = stringr::str_to_title(
    #   gsub(pattern = "_", replacement = " ", x = sex, fixed = TRUE))) %>% 
    dplyr::arrange(year, SRVY, sex, length) %>% 
    dplyr::mutate(year = factor(
      x = year,
      levels = as.character(sort(unique(year))),
      labels = as.character(sort(unique(year))),
      ordered = TRUE))
  
  # find appropriate units
  a<-find_units(unit = "", unt = "", dat = max(table_raw$pop, na.rm = TRUE))
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  pop_unit <- divby
  pop_unit_word <- unit_word
  
  if (is.null(unit)){
    # mm vs cm
    len_unit_word <- ifelse(!grepl(pattern = " crab", x = spp_print, ignore.case = TRUE), 
                            #report_spp$taxon[jj] =="fish", 
                            # max(table_raw$length)-min(table_raw$length)>45, 
                            "cm", "mm")
  } else {
    len_unit_word <- unit
  }
  table_raw <- table_raw %>%
    dplyr::mutate(pop = pop/pop_unit, 
                  length = round(
                    x = length*ifelse(len_unit_word == "mm", 10, 1), digits = 0)) #%>%
  len_unit_axis <- ifelse(max(table_raw$length)-min(table_raw$length)>150, 50, 
                          ifelse(max(table_raw$length)-min(table_raw$length)>45, 10, 5))
  
  # figure 
  if (!ridgeline) { # facet plot without ridgeline
    
    # if (length(unique(table_raw$SRVY))>1) {
    
    figure <- ggplot(data = table_raw,
                     mapping = aes(x = length,
                                   y = pop,
                                   group = SRVY,
                                   fill = sex)) +
      geom_bar(position="stack", stat="identity", na.rm = TRUE) +
      scale_fill_viridis_d(direction = -1, 
                           option = "mako",
                           begin = .2,
                           end = .6,
                           na.value = "transparent") +
      guides(fill=guide_legend(title="")) +
      scale_y_continuous(name = paste0(spp_print, #"\n
                                       " population",pop_unit_word), 
                         breaks = function(pop) unique(floor(pretty(seq(0, (max(pop) + 1) * 1.1))))) +
      scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word, ")")), 
                         breaks = function(length) unique(floor(pretty(seq(0, (max(length) + 1) * 1.1))))) +
      facet_grid(year ~ SRVY,
                 scales = "free_x") 
    
    # } else {
    # 
    # figure <- ggplot(data = table_raw,
    #                  mapping = aes(x = length,
    #                                y = pop,
    #                                fill = sex))+
    #   geom_bar(position="stack", stat="identity", na.rm = TRUE) +
    #   scale_fill_viridis_d(direction = -1, 
    #                        option = "mako",
    #                        begin = .2,
    #                        end = .6,
    #                        na.value = "transparent") +
    #   guides(fill=guide_legend(title="")) +
    #   scale_y_continuous(name = paste0(spp_print, "\npopulation",pop_unit_word), 
    #                      breaks = function(pop) unique(floor(pretty(seq(0, (max(pop) + 1) * 1.1))))) +
    #   scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word, ")")), 
    #                      breaks = function(length) unique(floor(pretty(seq(0, (max(length) + 1) * 1.1))))) +
    #   facet_grid(year ~ .,
    #              scales = "free_x")  
    # }
    figure <- figure +
      guides(
        fill = guide_legend(title.position = "top", 
                            # label.position = "bottom",
                            title.hjust = 0.5,
                            nrow = 1
        )) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = NA,
                                    colour = "grey20"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        legend.background = element_rect(colour = "transparent", 
                                         fill = "transparent"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.position = "bottom",
        legend.box = "horizontal" # "horizontal" "vertical"
      )
    
    
  } else {
    table_raw1 <- table_raw %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(year, #SRVY, 
                      length) %>% 
      dplyr::summarise(pop = sum(pop, na.rm = TRUE))
    
    temp <- setdiff(as.numeric(paste(min(table_raw1$year, na.rm = TRUE))):as.numeric(paste(max(table_raw1$year, na.rm = TRUE))), 
                    as.numeric(paste(unique(table_raw1$year))))
    if (length(temp)>0) {
      table_raw1 <- rbind.data.frame(
        data.frame(year = temp,
                   # SRVY = , 
                   length = 0, 
                   pop = 0), 
        table_raw1)
    }
    
    table_raw1 <- table_raw1 %>% 
      dplyr::arrange(desc(year)) #%>% 
    # dplyr::mutate(
    #   year = as.numeric(paste(table_raw$year)), 
    #   year =  factor(
    # x = year,
    # levels = as.character(sort(unique(year), decreasing = TRUE)),
    # labels = as.character(sort(unique(year), decreasing = TRUE)),
    # ordered = TRUE))
    
    table_raw1$year <- as.numeric(paste(table_raw1$year))
    table_raw1$year <- factor(
      x = table_raw1$year,
      levels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      labels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      ordered = TRUE)
    
    figure <- ggplot(data = table_raw1, 
                     mapping = aes(x = length, 
                                   y = year, 
                                   fill = length, 
                                   height = pop/mean(pop, na.rm = TRUE))) +
      ggridges::geom_ridgeline_gradient() +
      scale_fill_viridis_c(name = length, option = "G") +
      ylab(paste0(spp_print, #"\n
                  " population across years")) +
      xlab(stringr::str_to_sentence(paste0(type," (", len_unit_word, ")"))) +
      theme(legend.position = "none", 
            panel.grid.major.x = element_line(colour = "grey80"))
  }
  
  figure <- figure + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey80"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 9),
          legend.key = element_rect(colour = "transparent",
                                    fill = "transparent"),
          axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10))
  
  
  if (print_n & !is.null(length_data0)) {
    
    dat_text  <- data.frame(
      label = paste0(c("# measured: ", rep_len(x = "", length.out = (length(unique(length_data0$year))-1))),   
                     length_data0 %>% 
                       dplyr::mutate(year = as.character(year)) %>% 
                       dplyr::ungroup() %>% 
                       dplyr::group_by((year)) %>% 
                       dplyr::summarise(frequency = formatC(x = sum(frequency, na.rm = TRUE), 
                                                            digits = 0, big.mark = ",", format = "f")) %>% 
                       dplyr::select(frequency) %>% 
                       unlist()))
    
    dat_text$label <- gsub("\\s", " ", formatC(x = dat_text$label)) #, width=max(nchar(dat_text$label))))
    
    figure <- 
      tag_facet(p = figure, 
                x = Inf, y = Inf, 
                hjust = 1.25,
                tag_pool = dat_text$label, 
                open = "", close = "")
  }
  
  return(figure)
}

plot_idw_xbyx <- function(
    yrs, 
    dat, 
    lat,
    lon,
    var,
    year,
    key.title = "", 
    grid = "extrapolation.grid",
    set.breaks = "auto", #seq(from = -2, to = 20, by = 2),
    grid.cell = c(20000, 20000), # c(0.02, 0.02), for lat-lon 
    row0 = 2, 
    reg_dat,
    region, 
    dist_unit = "nm", # nautical miles
    col_viridis = "mako", 
    plot_stratum = FALSE, 
    plot_coldpool = FALSE, 
    legend_srvy_reg = TRUE) {
  
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  dat <- dat %>%
    dplyr::rename(year = as.character(year), 
                  LATITUDE = as.character(lat), 
                  LONGITUDE = as.character(lon), 
                  CPUE_KGHA = as.character(var)) %>% 
    dplyr::select(year, LATITUDE, LONGITUDE, CPUE_KGHA, SRVY) %>% 
    dplyr::mutate(year = as.numeric(year), 
                  CPUE_KGHA = as.numeric(CPUE_KGHA), 
                  LATITUDE = as.numeric(LATITUDE), 
                  LONGITUDE = as.numeric(LONGITUDE)) %>% 
    dplyr::filter(year %in% yrs)
  
  figure <- ggplot()
  if (nrow(dat) != 0) {
    if (set.breaks[1] =="auto") {
      set.breaks <- set_breaks(dat = dat, var = "CPUE_KGHA")
    }
    
    # Select data and make plot
    for (ii in length(yrs):1) {
      
      temp <- dat %>% 
        dplyr::filter(year == yrs[ii]) 
      
      region0 <- region
      # reg_dat0 <- reg_dat
      if (length(unique(temp$SRVY))==1 & # is this species in only 1 of the 2 survey areas
          length(unique(dat$SRVY))>1) { # (when SRVY == NEBS)?
        region0 <- ifelse(unique(temp$SRVY) == "EBS", "bs.south", "bs.north")
      }
      
      temp1 <- make_idw_map(x = temp, # Pass data as a data frame
                            region = region0, 
                            extrap.box = sf::st_bbox(reg_dat$survey.area), 
                            out.crs = as.character(reg_dat$crs)[1],
                            set.breaks = set.breaks,
                            grid.cell = grid.cell, 
                            key.title = key.title, 
                            use.survey.bathymetry = FALSE)
      
      temp0 <- temp1[grid][[1]]  
      
      if (ii == length(yrs)) {
        stars_list <- temp0
        names(stars_list)[names(stars_list) == "var1.pred"] <- paste0("y", yrs[ii])  
      } else {
        stars_list$temp <- temp0$var1.pred
        names(stars_list)[names(stars_list) == "temp"] <- paste0("y", yrs[ii])   
      }
    }
    
    # https://rpubs.com/michaeldorman/646276
    stars_list <- stars_list %>% 
      dplyr::select(names(stars_list)[substr(start = 1, stop = 1, x = names(stars_list)) == "y"])
    names(stars_list)<-gsub(pattern = "y", replacement = "", x = names(stars_list))
    stars_list = stars::st_redimension(stars_list)
    names(stars_list) = "value"
    
    figure <- figure +
      geom_stars(data = stars_list) 
  }
  
  if (plot_coldpool) {
    temp_break <- 2 # 2*C
    
    if (sum(dat$SRVY %in% "EBS")>0) {
      cp <- coldpool::ebs_bottom_temperature
    } else if (unique(dat$SRVY) %in% "NBS") {
      cp <- coldpool::nbs_ebs_bottom_temperature
    }
    
    temp <- c()
    outline <- c()
    for (i in 1:length(yrs)){
      #   temp <- c(temp, which(grepl(pattern = yrs[i], x = names(cp))))
      # }
      temp <- which(grepl(pattern = yrs[i], x = names(cp)))
      
      cp0 <- cp[[temp]]#[[which(grepl(x = names(cp), pattern = 2019))]] # cp[[temp[2]]]
      values(cp0)[values(cp0) <= temp_break] <- 1
      values(cp0)[values(cp0) > temp_break] <- NA
      pp <- rasterToPolygons(x = cp0, na.rm = TRUE, dissolve=TRUE)
      
      outline <- rbind(outline, 
                       pp %>% 
                         sp::geometry(obj = .) %>% 
                         sf::st_as_sf(x = .) %>% 
                         dplyr::mutate(new_dim  = yrs[i]))
      
    }
    
    figure <- figure +
      geom_sf(data = outline %>%
                sf::st_cast(x = ., to = "MULTIPOLYGON"), 
              size = 1, 
              fill = NA, # alpha(colour = "red", alpha = 0.3),
              color = alpha(colour = "red", alpha = 0.3))
    # fill = alpha(colour = "yellow", alpha = 0.3), 
    # color = alpha(colour = "yellow", alpha = 0.3))
  }
  
  if (length(yrs) == 0) {
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(mapping = aes(x = mean(reg_dat$lon.breaks), 
                                       y = mean(reg_dat$lat.breaks), 
                                       label = "No data was available\nfor this species in this\nregion for this year."), 
                         fontface="bold")
  } else if (length(yrs) == 1) {
    figure <- figure +
      coord_sf() +
      ggtitle(maxyr) +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  } else if (length(yrs)>0) {
    figure <- figure +
      facet_wrap( ~ new_dim, nrow = row0) +
      coord_sf()# coord_equal() 
  }
  
  if (plot_stratum) {
    figure <- figure +
      geom_sf(data = reg_dat$survey.strata,
              color = "grey50",
              size = 0.1,
              # alpha = 0,
              fill = NA)
  }
  
  figure <- figure +
    geom_sf(data = reg_dat$akland,
            color = NA,
            fill = "grey50")  +  
    # geom_sf(data = reg_dat$graticule,
    #         color = "grey80",
    #         alpha = 0.2) +
    ggplot2::scale_y_continuous(name = "", #"Latitude", 
                                limits = reg_dat$plot.boundary$y,
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "", #"Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) + 
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 100,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = dplyr::case_when(row0 ==1 & length(yrs)>4 ~ 0.07,
                                              row0 == 1 ~ 0.04, 
                                              row0 == 2 ~ 0.06, 
                                              TRUE ~ 0.05),  # ifelse(row0 > 1, 0.08, 0.04), #ifelse(row0 == 1, 0.04, ifelse(row0 == 2, 0.06, 0.05)),  # ifelse(row0 > 1, 0.08, 0.04),
                   height = ifelse(row0 == 1, 0.02, ifelse(row0 == 2, 0.04, 0.04)),  # ifelse(row0 > 1, 0.04, 0.02),
                   st.bottom = FALSE, #ifelse(row0 <= 2, TRUE, FALSE),
                   st.size = dplyr::case_when(row0 ==1 & length(yrs) > 4 ~ 1.5,
                                              row0 == 1 & length(yrs) > 3 ~ 2, 
                                              row0 == 1 ~ 3, 
                                              row0 == 2 ~ 2.25, 
                                              TRUE ~ 2)) # ifelse(row0 == 1, 3, ifelse(row0 == 2, 2.25, 2))
  
  # if (length(length(reg_dat$survey.area$color))>1 ) {
  figure <- figure +
    geom_sf(data = reg_dat$survey.area, 
            mapping = aes(color = SURVEY), 
            fill = NA, 
            shape = NA, 
            size = ifelse(row0 > 2, 0.25, 0.75),
            show.legend = legend_srvy_reg) +
    scale_color_manual(
      name = " ",
      values = reg_dat$survey.area$color,
      breaks = rev(reg_dat$survey.area$SURVEY),
      labels = rev((reg_dat$survey.area$SRVY))) +
    ggplot2::guides(
      size = guide_legend(override.aes = list(size = 10)), 
      fill = guide_legend(order = 1, 
                          title.position = "top", 
                          label.position = "bottom",
                          title.hjust = 0.5, 
                          override.aes = list(color = NA), 
                          nrow = 1), 
      color = guide_legend(order = 2, 
                           label.position = "right",
                           override.aes = list(size = 2), 
                           title.hjust = 0.5,
                           nrow = 2)) 
  
  if (grid == "continuous.grid") {
    figure <- figure + 
      scale_fill_viridis_c(option = col_viridis, 
                           #limits = range(set.breaks),
                           na.value = "transparent", 
                           breaks = set.breaks,
                           labels = set.breaks)  + 
      guides(fill=guide_colourbar(title = key.title, 
                                  title.position = "top", 
                                  title.hjust = 0.5))
    
  } else if (grid == "extrapolation.grid") {
    # temp <- factor(x = temp0$var1.pred, levels = levels(temp0$var1.pred), labels = levels(temp0$var1.pred), ordered = T)
    figure <- figure +
      scale_fill_manual(
        name = key.title, 
        na.value = "transparent",
        limits = levels(temp0$var1.pred), 
        values=c("gray90", 
                 viridis::mako(
                   direction = -1, 
                   n = (length(levels(temp0$var1.pred))-1), # temp1$n.breaks,
                   begin = 0,
                   end = 0.80)), 
        breaks = levels(temp0$var1.pred), 
        labels = levels(temp0$var1.pred))      
  }
  
  figure <- figure +
    #set legend position and vertical arrangement
    theme( 
      # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      axis.text = element_text(size = ifelse(length(yrs)>4 & row0 == 1, 6, 8)),
      
      strip.background = element_blank(), 
      strip.text = element_text(size = 10, face = "bold"), 
      # legend.title = element_text(size = 12), #, vjust = .5, hjust = .3),
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      # legend.title.align = 0,#.1, 
      legend.position = "bottom", 
      # legend.box.just = "center",
      # legend.key.width = unit(.5, "in"), 
      legend.box = "horizontal") # ifelse(length(length(reg_dat$survey.area$color)>1), "horizontal", "vertical"))
  
  return(figure)
  
}

plot_pa_xbyx <- function(
    yrs, 
    dat, 
    lat,
    lon,
    year,
    key.title = "", 
    row0 = 2, 
    reg_dat,
    dist_unit = "nm", # nautical miles
    col_viridis = "mako", 
    plot_coldpool = FALSE, 
    plot_stratum = FALSE, 
    plot_bubble = FALSE) {
  
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  
  if (plot_bubble){
    dat0 <- dat %>%
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) %>% 
      dplyr::select(year, lat, lon, cpue_kgha) %>% 
      dplyr::mutate(year = as.numeric(year), 
                    latdd = as.numeric(lat), 
                    londd = as.numeric(lon), 
                    cpue_kgha = as.numeric(cpue_kgha))
    d <- dat0[,c("londd", "latdd", "year", "cpue_kgha")]
  } else {
    dat0 <- dat %>%
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) %>% 
      dplyr::select(year, lat, lon) %>% 
      dplyr::mutate(year = as.numeric(year), 
                    latdd = as.numeric(lat), 
                    londd = as.numeric(lon))
    d <- dat0[,c("londd", "latdd", "year")]
  }
  
  
  coordinates(d) <- c("londd", "latdd")
  sp::proj4string(d) <- CRS("+proj=longlat +datum=WGS84") 
  dd <- data.frame(sp::spTransform(d, CRS(as.character(reg_dat$crs)[1])))
  # dd <- as(res, "SpatialPoints") ## For a SpatialPoints object rather than a SpatialPointsDataFrame
  
  figure <- ggplot() +
    geom_sf(data = reg_dat$akland,
            color = NA,
            fill = "grey50") #+
  # geom_sf(data = reg_dat$graticule,
  #         color = "grey80",
  #         alpha = 0.2)
  
  # if (length(length(reg_dat$survey.area$color))>1 ) {
  if (plot_bubble) {
    figure <- figure   + 
      geom_point(data = dd, 
                 mapping = aes(x = londd, y = latdd,
                               size = cpue_kgha,
                               group = as.factor(year)), 
                 color = mako(n = 1, begin = .25, end = .75),
                 shape = 16,
                 # size = 1.5,
                 show.legend = TRUE,
                 na.rm = TRUE) +
      scale_size_continuous(
        name = paste0(key.title, "weight CPUE (kg/ha)"), 
        range = c(1,4))
  } else {
    figure <- figure   + 
      geom_point(data = dd, 
                 mapping = aes(x = londd, y = latdd,
                               # shape = key.title,
                               group = as.factor(year)), 
                 color = mako(n = 1, begin = .25, end = .75),
                 shape = 16,
                 size = 1.5,
                 show.legend = TRUE,
                 na.rm = TRUE)    
  }
  
  figure <- figure  +
    geom_sf(data = reg_dat$survey.area, # %>% 
            # dplyr::filter(SRVY %in% SRVY1), 
            mapping = aes(color = SURVEY), 
            fill = NA, 
            shape = NA, 
            size = ifelse(row0 > 2, 0.25, 0.75),
            show.legend = TRUE) +
    scale_color_manual(
      name = "", # key.title,
      values = reg_dat$survey.area$color,
      breaks = rev(reg_dat$survey.area$SURVEY), 
      labels = rev((reg_dat$survey.area$SRVY)))
  # } else {
  #   figure <- figure   + 
  #     geom_point(data = dd, 
  #                mapping = aes(x = londd, y = latdd, 
  #                              shape = key.title,
  #                              group = as.factor(year)), 
  #                color = mako(n = 1, begin = .25, end = .75),
  #                # shape = 16,
  #                size = 2,
  #                show.legend = TRUE,
  #                na.rm = TRUE) 
  # }
  
  # if (plot_coldpool) {
  #   temp_break <- 2 # 2*C
  #   
  #   if (unique(dat$SRVY) %in% "EBS") {
  #     cp <- coldpool::ebs_bottom_temperature
  #   } else if (unique(dat$SRVY) %in% "NBS") {
  #     cp <- coldpool::nbs_ebs_bottom_temperature
  #   }
  #   
  #   coords <- raster::coordinates(cp)
  #   
  #   for(i in 1:length(yrs)) {
  #     sel_layer_df <- data.frame(x = coords[,1],
  #                                y = coords[,2],
  #                                temperature = cp@data@values[,i])
  #     sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
  #     sel_layer_df$year <- yrs[i]
  #     
  #     if(i == 1) {
  #       bt_year_df <- sel_layer_df
  #     } else{
  #       bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
  #     }
  #   }
  #   
  #   figure <- figure +
  #     ggplot2::geom_tile(data = bt_year_df %>%
  #                          dplyr::filter(temperature <= temp_break), #%>% 
  #                          # dplyr::rename(new_dim = year),
  #                        aes(x = x,
  #                            y = y, 
  #                            group = year),
  #                        fill = "magenta", 
  #                        alpha = 0.25, 
  #                        show.legend = FALSE)
  #   
  # }  
  
  if (plot_coldpool) {
    temp_break <- 2 # 2*C
    
    if (sum(dat$SRVY %in% "EBS")>0) {
      cp <- coldpool::ebs_bottom_temperature
    } else if (unique(dat$SRVY) %in% "NBS") {
      cp <- coldpool::nbs_ebs_bottom_temperature
    }
    
    temp <- c()
    outline <- c()
    for (i in 1:length(yrs)){
      #   temp <- c(temp, which(grepl(pattern = yrs[i], x = names(cp))))
      # }
      temp <- which(grepl(pattern = yrs[i], x = names(cp)))
      
      cp0 <- cp[[temp]]#[[which(grepl(x = names(cp), pattern = 2019))]] # cp[[temp[2]]]
      values(cp0)[values(cp0) <= temp_break] <- 1
      values(cp0)[values(cp0) > temp_break] <- NA
      pp <- rasterToPolygons(x = cp0, na.rm = TRUE, dissolve=TRUE)
      
      outline <- rbind(outline, 
                       pp %>% 
                         sp::geometry(obj = .) %>% 
                         sf::st_as_sf(x = .) %>% 
                         dplyr::mutate(new_dim  = yrs[i]))
      
    }
    
    figure <- figure +
      geom_sf(data = outline %>%
                sf::st_cast(x = ., to = "MULTIPOLYGON"), 
              size = 1, 
              fill = NA, # alpha(colour = "red", alpha = 0.3),
              color = alpha(colour = "red", alpha = 0.3))
    # fill = alpha(colour = "yellow", alpha = 0.3), 
    # color = alpha(colour = "yellow", alpha = 0.3))
  }
  
  if (length(yrs) == 0) { # if there is no data to plot
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(mapping = aes(x = mean(reg_dat$lon.breaks), 
                                       y = mean(reg_dat$lat.breaks), 
                                       label = "No data was available\nfor this species in this\nregion for this year."), 
                         fontface="bold")
  } else if (length(yrs)>1) { # if there is data to plot
    figure <- figure +
      facet_wrap( ~ year, nrow = row0) +
      coord_sf() # coord_equal() 
  }
  
  
  if (plot_stratum) {
    
    figure <- figure +
      geom_sf(data = reg_dat$survey.strata,
              color = "grey50",
              size = 0.1,
              # alpha = 0,
              fill = NA)
  }
  
  # lon_break <- reg_dat$lon.breaks
  # lat_break <- reg_dat$lat.breaks
  # lon_label <- reg_dat$lon.label
  # lat_label <- reg_dat$lat.label
  # if (length(yrs) > 6) { # are there a lot of plots on the page? In which case we'd want detail
  #   lon_break <- lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_break))]
  #   lat_break <- lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_break))]
  #   lon_label <- lon.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lon_label))]
  #   lat_label <- lat.breaks[rep_len(x = c(FALSE, TRUE), length.out = length(lat_label))]
  # }
  
  figure <- figure +
    ggplot2::scale_y_continuous(name = "", #"Latitude", 
                                limits = reg_dat$plot.boundary$y,
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "", #"Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) + 
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 100,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = dplyr::case_when(row0 ==1 & length(yrs)>4 ~ 0.07,
                                              row0 == 1 ~ 0.04, 
                                              row0 == 2 ~ 0.06, 
                                              TRUE ~ 0.05),  # ifelse(row0 > 1, 0.08, 0.04), #ifelse(row0 == 1, 0.04, ifelse(row0 == 2, 0.06, 0.05)),  # ifelse(row0 > 1, 0.08, 0.04),
                   height = ifelse(row0 == 1, 0.02, ifelse(row0 == 2, 0.04, 0.04)),  # ifelse(row0 > 1, 0.04, 0.02),
                   st.bottom = FALSE, #ifelse(row0 <= 2, TRUE, FALSE),
                   st.size = dplyr::case_when(row0 ==1 & length(yrs) > 4 ~ 1.5,
                                              row0 == 1 & length(yrs) > 3 ~ 2, 
                                              row0 == 1 ~ 3, 
                                              row0 == 2 ~ 2.25, 
                                              TRUE ~ 2)) 
  if (plot_bubble) {
    figure <- figure +
      guides(
        size = guide_legend(order = 1, 
                            title.position = "top", 
                            label.position = "top",
                            title.hjust = 0.5, 
                            nrow = 1), 
        color = guide_legend(order = 2, 
                             label.position = "right",
                             title.hjust = 0.5,
                             nrow = 1)) 
  } else {
    figure <- figure +
      guides(
        color = guide_legend(title = key.title, 
                             title.position = "top",  
                             label.position = "right",
                             title.hjust = 0.5,
                             nrow = 1)) 
  }
  
  figure <- figure +
    theme(  #set legend position and vertical arrangement
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      axis.text = element_text(size = ifelse(length(yrs)>4 & row0 == 1, 6, 8)),
      strip.background = element_blank(), 
      strip.text = element_text(size = 10, face = "bold"), 
      # legend.title = ,element_blank(),
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = "bottom",
      legend.box = "horizontal")# ifelse(plot_bubble, "vertical", "horizontal"))
  
  
  return(figure)
  
}