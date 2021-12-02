
# --
# Apache access log parse
# --

library(lubridate)
library(tidyverse)

alog <- readLines("log/20210903_access.log")
alog <- enframe(alog)
alog <- alog[,-1]

# regex log parse ---------------------------------------------------------

nm <- c("remote_ip",    # 1
        "log_nm",       # 2
        "remote_usr",   # 3
        "dttm",         # 4
        "request",      # 5
        "status",       # 6
        "bytes_sent",   # 7
        "referer",      # 8
        "ua_string")    # 9

re <- str_c("(\\S+)\\s+",                 # 1 remote_ip
            "(\\S+)\\s+",                 # 2 log_nm      
            "(\\S+)\\s+",                 # 3 remote_usr
            "(\\[[^\\[\\]]+\\])\\s+",     # 4 dttm
            "(\"[^\"]*\")\\s+",           # 5 request
            "(\\d+)\\s+",                 # 6 status
            "(\\d+)\\s+",                 # 7 bytes_sent
            "(\"[^\"]+\")\\s+",           # 8 referer
            "(\"[^\"]+\")")               # 9 ua_string

blog <- extract(alog, value, nm, re)

rm(nm, re)

# format date time --------------------------------------------------------

blog$dttm <- str_remove_all(blog$dttm, "[\\[\\]]")

blog <- blog %>% 
  separate(dttm, into = c("dttm","tz"), sep = " ")

blog$dttm <- dmy_hms(blog$dttm)

# graph hour counts -------------------------------------------------------

# create hour data frame to graph
hr <- hour(blog$dttm)
hr <- enframe(hr)
names(hr) <- c("id","hour")

# format hour as a factor
hr$hour <- str_c(hr$hour, ":00 am")
hr$hour_fctr <- factor(hr$hour, levels = unique(hr$hour))

# plot hits per hour
ggplot(hr, aes(hour_fctr)) +
  geom_bar() +
  ggtitle("Hits per hour") + 
  xlab("hour")
