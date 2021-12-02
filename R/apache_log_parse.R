

#' @importFrom tidyr extract
#' @importFrom tidyr separate
#' @importFrom stringr str_c
#' @importFrom stringr str_remove_all
#' @importFrom tibble enframe
#' @importFrom lubridate dmy_hms
NULL


#' Parse log file string in data frame
#'
#' @title regex_parse
#' @param log_df Data frame with log
#' @param log_col Column name string with log
#' @return log_df_out Data frame with parsed columns
regex_parse <- function(log_df, log_col){

  # set log column names
  nm <- c("remote_ip",    # 1
          "log_nm",       # 2
          "remote_usr",   # 3
          "dttm",         # 4
          "request",      # 5
          "status",       # 6
          "bytes_sent",   # 7
          "referer",      # 8
          "ua_string")    # 9

  # set regex for each column
  re <- stringr::str_c("(\\S+)\\s+",                 # 1 remote_ip
              "(\\S+)\\s+",                 # 2 log_nm
              "(\\S+)\\s+",                 # 3 remote_usr
              "(\\[[^\\[\\]]+\\])\\s+",     # 4 dttm
              "(\"[^\"]*\")\\s+",           # 5 request
              "(\\d+)\\s+",                 # 6 status
              "(\\d+)\\s+",                 # 7 bytes_sent
              "(\"[^\"]+\")\\s+",           # 8 referer
              "(\"[^\"]+\")")               # 9 ua_string

  # extract columns with regex
  log_df_out <- tidyr::extract(log_df, log_col, nm, re)

  rm(nm, re)

  return(log_df_out)
}


format_log_date <- function(log_df){

  log_df$dttm <- stringr::str_remove_all(log_df$dttm, "[\\[\\]]")

  log_df <- tidyr::separate(log_df, dttm, into = c("dttm","tz"), sep = " ")

  log_df$dttm <- lubridate::dmy_hms(log_df$dttm)

  return(log_df)
}

#' Parse Apache-style access log file
#'
#' @title apache_log_parse
#' @param log_file String name/path of log
#' @return log_df Data frame with parsed log columns
#' @export
apache_log_parse <- function(log_file){

  # read in log file to data frame
  log_ch_str <- readLines(log_file)
  log_df <- tibble::enframe(log_ch_str)
  log_df <- log_df[,-1]

  # parse value column of data frame
  log_df <- regex_parse(log_df, "value")

  # format date column of data frame
  log_df <- format_log_date(log_df)

  return(log_df)

}


