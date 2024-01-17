


SEDAR_plot_domain_den_by_year <- function(dataset, species, present = FALSE, title = NULL) {
  
  a <-  getDomainDensity(dataset, species, when_present = present) %>%
    mutate( SE   = sqrt(var),
            plot_yr = YEAR,
            YEAR = as_factor(YEAR)) %>% 
    mutate(survey = case_when(
      REGION %in% c("PRICO") & plot_yr < 2014 ~ "Pre NCRMP",
      REGION %in% c("PRICO") & plot_yr == 2014 ~ "Belt-Island-Wide",
      REGION %in% c("PRICO") & plot_yr > 2015 ~ "NCRMP",
      REGION %in% c("STTSTJ", "STX") & plot_yr < 2012 ~ "Pre NCRMP",
      REGION %in% c("STTSTJ", "STX") & plot_yr %in% c(2012, 2013,2014,2015) ~ "Belt-Island-Wide",
      REGION %in% c("STTSTJ", "STX") & plot_yr > 2015 ~ "NCRMP")) %>% 
    mutate(survey = as_factor(survey)) %>% 
    mutate(survey = factor(survey, levels = c("Pre NCRMP", "Belt-Island-Wide", "NCRMP")))

  yupper = max(a$density + a$SE)
  
  ggplot(a, aes(x=YEAR, y=density, group = 1)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25,
                  size = 0.5) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'Pre NCRMP')) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'Belt-Island-Wide')) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'NCRMP')) +
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) +
    xlab("Year") + ylab("Density")
  
}

SEDAR_plot_domain_den_by_year_exploited <- function(dataset, species, present = FALSE, title = NULL, length = NULL) {
  a <-  getDomainDensity(dataset, species, when_present = present, length_bins = length) %>%
    mutate( SE   = sqrt(var),
            plot_yr = YEAR,
            YEAR = as_factor(YEAR)) %>%
    mutate(survey = case_when(
      REGION %in% c("PRICO") & plot_yr < 2014 ~ "Pre NCRMP",
      REGION %in% c("PRICO") & plot_yr == 2014 ~ "Belt-Island-Wide",
      REGION %in% c("PRICO") & plot_yr > 2015 ~ "NCRMP",
      REGION %in% c("STTSTJ", "STX") & plot_yr < 2012 ~ "Pre NCRMP",
      REGION %in% c("STTSTJ", "STX") & plot_yr %in% c(2012, 2013,2014,2015) ~ "Belt-Island-Wide",
      REGION %in% c("STTSTJ", "STX") & plot_yr > 2015 ~ "NCRMP")) %>% 
    mutate(survey = as_factor(survey)) %>% 
    mutate(survey = factor(survey, levels = c("Pre NCRMP", "Belt-Island-Wide", "NCRMP"))) %>% 
    filter(if(!is.null(length)) length_class == paste(">= ", length, sep = "") else TRUE)
  
  yupper = max(a$density + a$SE)
  
  ggplot(a, aes(x=YEAR, y=density, group = 1)) + 
    geom_line(size=1) + geom_point(size=3) + 
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE),
                  width = 0.25,
                  size = 0.5) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'Pre NCRMP')) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'Belt-Island-Wide')) +
    geom_mark_ellipse(aes(fill = survey, filter = survey == 'NCRMP')) +
    ggtitle(title) + 
    theme_Publication(base_size = 20) +
    theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1)) +
    xlab("Year") + ylab("Density")
  
}