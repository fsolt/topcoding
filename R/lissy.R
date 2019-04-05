* user = lis_user
* password = lis_password
* package = R
* project = LIS

library(dplyr)

## Define functions
gini <- function(df, x) {
    df1 <- df[!is.na(df[[x]]), ]
    x <- as.vector(df1[[x]])
    weight <- df1$wt
    
    ox <- order(x)
    x <- as.vector(x)[ox]
    weight <- as.vector(weight)[ox] / sum(weight) 
    p <- cumsum(weight)
    nu <- cumsum(weight * x)
    n <- length(nu)
    nu <- nu / nu[n]
    res <- round((sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])), digits = 4)
    return(res)
}

wNtile <- function(var, wgt, split) {
    x <- var[order(var)]
    y <- wgt[order(var)]
    z <- cumsum(y) / sum(y)
    cop <- rep(NA, length(split)) 
    for (i in 1:length(cop)) {
        cop[i] <- x[Find(function(h) z[h] > split[i], seq_along(z))]
    }
    return(cop)
}

topBottom <- function(var, botline, topline) {
    tb <- ifelse(var < botline, botline, var)
    tb <- ifelse(tb > topline, topline, tb)
    return(tb)
}

setups <- function(df) {
    botline <- 0
    topline10 <- 10 * wNtile(df$dhi, df$hpopwgt, 0.5)
    
    df$disp_sqrt10 <- topBottom(df$dhi, botline, topline10) / (df$nhhmem ^ 0.5)
    df$disp_sqrt <- ifelse(df$dhi < botline, botline, df$dhi) / (df$nhhmem ^ 0.5) 
    df$disp_sqrt_med <- round(df$disp_sqrt / wNtile(df$dhi, df$hpopwgt, 0.5), 1)

    df$market_sqrt10 <- topBottom(ifelse(!is.na(df$hitp), (df$factor + df$hitp), df$factor), botline, topline10) / (df$nhhmem ^ 0.5)
    df$market_sqrt <- ifelse(ifelse(!is.na(df$hitp), (df$factor + df$hitp), df$factor) < 0, botline, ifelse(!is.na(df$hitp), (df$factor + df$hitp), df$factor)) / (df$nhhmem ^ 0.5)
    
    return(df)
}

boot_gini_se <- function(data, var, reps=100) {
    data <- data[!is.na(data[[var]]), ]
    resamples <- lapply(1:reps, function(i) dplyr::sample_n(data, size = nrow(data), replace=TRUE))
    r_stat <- lapply(resamples, function(x) gini(x, var))
    std_err <- round(sqrt(var(unlist(r_stat))), digits = 4)
    return(std_err)   
}

# For testing at home:
# read.LIS <- function(data_file, labels, vars, subset) {
#   require(dplyr)
#   data_file <- stringr::str_replace(data_file, "h", "ih.dta")
#   df <- haven::read_dta(data_file)[, vars] %>%
#     filter(eval(parse(text = subset), .))
#   if (!labels) {
#     df <- df %>% dplyr::mutate_all(funs(as.numeric))
#   }
#   return(df)
# }

get_ginis <- function(cc, reps = 100) {
    set.seed(324)
    ccs <- c("au", "at", "be", "br", "ca", "cl", "cn", "co", "cz", "dk", 
             "do", "eg", "ee", "fi", "fr", "de", "ge", "gr", "gt", "hu", "is", 
             "in", "ie", "il", "it", "jp", "lu", "lt", "mx", "nl", "no", "pa",  
             "py", "pe", "pl", "ro", "ru", "rs", "sk", "si", "za", "kr", "es",  
             "se", "ch", "tw", "uk", "us", "uy")
    cc <- tolower(cc)
    if (!cc %in% ccs) {
        stop("Please specify a LIS country in iso2c format")
    }
    last_year_yy <- as.numeric(format(Sys.Date(), "%Y")) - 2001
    yy <- as.character(c(c(67, 69, 71, 73:75, 78:99), paste0("0", 0:9), c(10:last_year_yy)))
    
    datasets <- paste0(rep(cc, each = length(yy)), rep(yy, times = length(cc)), "h")
    vars <- c("dhi", "hi", "factor", "hitp", "hc", "hpopwgt", "nhhmem", "nhhmem13", "grossnet")
    
    v <- c("disp_sqrt10", "disp_sqrt")
    
    for (ccyy in datasets) {
        cat("")
        df <- try(read.LIS(ccyy, labels = FALSE, vars = vars), silent = TRUE)
        if (!class(df)[1] == "try-error") {
            mean_dhi <- mean(df$dhi, na.rm = TRUE)
            if (!is.nan(mean_dhi) & !mean_dhi == 0) {
                df <- setups(df)
                cat(paste(ccyy,
                          nrow(df %>% 
                                   filter(!is.na(disp_sqrt10))),
                          nrow(df %>% 
                                   filter(!is.na(disp_sqrt) & disp_sqrt > 10 * wNtile(df$dhi, df$hpopwgt, 0.5))),
                          10 * wNtile(df$dhi, df$hpopwgt, 0.5)),
                    sep = "\n")
                print(summary(df %>% 
                                  pull(disp_sqrt10)))
                print(summary(df %>% 
                                  filter(disp_sqrt > 10 * wNtile(df$dhi, df$hpopwgt, 0.5)) %>% 
                                  pull(disp_sqrt)))
                print(c("over", ccyy, df %>%
                          filter(disp_sqrt_med > 10) %>% 
                          pull(disp_sqrt_med) %>% 
                            sort()))
                for (var in v) {
                    if (!is.na(mean(df[[var]], na.rm = TRUE))) {
                        cat(paste("gini",
                                  ccyy, 
                                  var, 
                                  gini(df, var),
                                  boot_gini_se(df, var, reps = reps),
                                  df$grossnet[1],
                                  sep = ","), sep = "\n")
                    }
                }
            }
        }
    }
}

# Call
get_ginis("CCODE", reps = 500)
