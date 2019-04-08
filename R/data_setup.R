library(tidyverse)
library(countrycode)

# Custom country codes (defined in <https://github.com/fsolt/swiid/raw/master/R/cc_swiid.R>)
download.file("https://github.com/fsolt/swiid/raw/master/data/cc_swiid.rda", "data/cc_swiid.rda")
load("data/cc_swiid.rda")
body(countrycode)[[2]] <- substitute(
    if (is.null(custom_dict) | as.list(match.call())[["custom_dict"]] == "cc_swiid") {
        if (origin == "country.name") {
            origin <- "country.name.en"
        }
        if (destination == "country.name") {
            destination <- "country.name.en"
        }
        if (origin %in% c("country.name.en", "country.name.de")) {
            origin <- paste0(origin, ".regex")
            origin_regex <- TRUE
        }
        else {
            origin_regex <- FALSE
        }
    }
)

format_lis_ginis <- function(x) {
    paste0("https://raw.githubusercontent.com/fsolt/topcoding/master/data-raw/LISSY/", 
           x, ".txt") %>%
        readLines() %>% 
        str_subset("^gini,\\D{2}\\d{2}h,.*") %>%
        paste(collapse = "\n") %>% 
        read_csv(col_names = FALSE) %>%
        transmute(country = str_extract(X2, "\\D{2}") %>%
                      toupper() %>% 
                      str_replace("UK", "GB") %>% 
                      countrycode("iso2c", "swiid.name", custom_dict = cc_swiid),
                  year = ifelse(str_extract(X2, "\\d{2}") %>% as.numeric() > 50,
                                str_extract(X2, "\\d{2}") %>% as.numeric() + 1900,
                                str_extract(X2, "\\d{2}") %>% as.numeric() + 2000),
                  gini = (str_trim(X4) %>% as.numeric()),
                  gini_se = (str_trim(X5) %>% as.numeric()),
                  top_coded = str_detect(X3, "10")) %>% 
        filter(!gini == 0) %>% 
        arrange(country, year)
}

format_lis_obs <- function(x) {
    cat(x)
    paste0("https://raw.githubusercontent.com/fsolt/topcoding/master/data-raw/LISSY/", 
           x, ".txt") %>%
        readLines() %>% 
        str_subset('^(\\s+)?\\[\\d+\\]\\s+"[^L].*') %>% 
        str_replace('\\[\\d+\\]\\s+', "") %>% 
        str_replace('"over"\\s+', "") %>% 
        str_replace_all('"', "") %>%
        str_trim() %>% 
        str_replace_all("(\\d)\\s+", "\\1,") %>% 
        str_replace("\\s+", " ") %>% 
        paste(collapse = "\n") %>% 
        paste("\n") %>% 
        read_delim(col_names = c("ccyy", "over_obs"), delim = " ") %>%
        separate_rows(over_obs, sep = ",") %>% 
        transmute(country = str_extract(ccyy, "\\D{2}") %>%
                      toupper() %>%
                      str_replace("UK", "GB") %>%
                      countrycode("iso2c", "swiid.name", custom_dict = cc_swiid),
                  year = ifelse(str_extract(ccyy, "\\d{2}") %>% as.numeric() > 50,
                                str_extract(ccyy, "\\d{2}") %>% as.numeric() + 1900,
                                str_extract(ccyy, "\\d{2}") %>% as.numeric() + 2000),
                  over = over_obs)
}

lis_files <- c("au", "at", "be", "br", "ca", "cl", "cn", "co", "cz", "dk",
               "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
               "in", "ie", "il", "it", "jp", "lt", "lu", "mx", "nl", "no", "pa", "py", 
               "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es", "se", 
               "ch", "tw", "uk", "us", "uy") # add "tn" when LIS releases data

ginis_compared <- lis_files %>% 
    map_df(format_lis_ginis) %>% 
    select(-gini_se) %>% 
    mutate(top_coded = if_else(top_coded, "gini_tc", "gini")) %>% 
    spread(key = top_coded, value = gini) %>% 
    left_join(lis_files %>% 
                  map_df(format_lis_ginis) %>% 
                  select(-gini) %>% 
                  mutate(top_coded = if_else(top_coded, "gini_tc_se", "gini_se")) %>% 
                  spread(key = top_coded, value = gini_se),
              by = c("country", "year")) %>% 
    select(country, year, gini_tc, gini_tc_se, gini, gini_se) %>% 
    mutate(diff = round(gini - gini_tc, 4) * 100,
           diff_se = round(sqrt(gini_tc_se^2 + gini_se^2), 4) * 100,
           problem = diff > 2 * diff_se) %>% 
    arrange(-diff)

write_csv(ginis_compared, "data/ginis_compared.csv")

over_obs <- lis_files %>% 
    map_df(format_lis_obs)

write_csv(over_obs, "data/over_obs.csv")
