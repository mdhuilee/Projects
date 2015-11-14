 
# Database with R



```r
# setup
opts_chunk$set(tidy = TRUE, cache = TRUE, autodep = TRUE, message = FALSE)
```



```r
library(pitchRx)
library(dplyr)
library(RSQLite)
library(DBI)
library(ggplot2)

db <- src_sqlite("pitchfx.sqlite3", create = T)

files <- c("inning/inning_all.xml", "players.xml")
scrape(start = "2013-06-01", end = "2013-06-01", suffix = files, connect = db$con)
```

```
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_arimlb_chnmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_bosmlb_nyamlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_chamlb_oakmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_cinmlb_pitmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_detmlb_balmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_houmlb_anamlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_kcamlb_texmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_lanmlb_colmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_milmlb_phimlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_nynmlb_miamlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_seamlb_minmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_sfnmlb_slnmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_sfnmlb_slnmlb_2/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_tbamlb_clemlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_tormlb_sdnmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_wasmlb_atlmlb_1/players.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_arimlb_chnmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_bosmlb_nyamlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_chamlb_oakmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_cinmlb_pitmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_detmlb_balmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_houmlb_anamlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_kcamlb_texmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_lanmlb_colmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_milmlb_phimlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_nynmlb_miamlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_seamlb_minmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_sfnmlb_slnmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_sfnmlb_slnmlb_2/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_tbamlb_clemlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_tormlb_sdnmlb_1/inning/inning_all.xml 
## http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_01/gid_2013_06_01_wasmlb_atlmlb_1/inning/inning_all.xml
```

```
## NULL
```

```r
# dat2 <- scrape(start = '2013-06-01', end = '2013-06-01', suffix = files)

# db <- src_sqlite('pitchfx.sqlite3')

pitch_dt <- tbl(db, "pitch")
umpire_dt <- tbl(db, "umpire")
atbat_dt <- tbl(db, "atbat")
# pitch2_dt <- dat2[['pitch']] umpire_dt <- dat[['umpire']]


atbat <- select(atbat_dt, gameday_link, num, stand, p_throws, inning, pitcher_name)

pitch_umpire <- umpire_dt %>% filter(position == "home") %>% inner_join(pitch_dt, 
    by = "gameday_link") %>% distinct()

# outside_zone
out_zn <- pitch_umpire %>% filter((px < -0.85 | px > 0.85 | pz < sz_bot | pz > 
    sz_top), des %in% c("Called Strike", "Ball")) %>% mutate(cs_ind = as.numeric(des == 
    "Called Strike")) %>% select(position, name, id.x, des, cs_ind, px, pz, 
    sz_top, sz_bot, num, count, gameday_link) %>% collect()

# proportion of pitches that are 'called strikes' among all pitches outside
# the strikezone.

cs_out_zn_all <- out_zn %>% summarize(sum = sum(cs_ind), n = n()) %>% collect() %>% 
    mutate(cs_out_zn = sum/n)

cs_out_zn_all
```

```
## Source: local data frame [1 x 3]
## 
##     sum     n cs_out_zn
##   (int) (int)     (dbl)
## 1   189  1604 0.1178304
```

```r
# cs_out_zn <- sum(out_zn$cs_ind) / dim(out_zn)[1] cs_out_zn

cs_out_zn_count <- out_zn %>% group_by(count) %>% summarize(sum = sum(cs_ind), 
    n = n()) %>% collect() %>% mutate(cs_out_zn = sum/n)

cs_out_zn_count
```

```
## Source: local data frame [12 x 4]
## 
##    count   sum     n   cs_out_zn
##    (chr) (int) (int)       (dbl)
## 1    0-0    85   495 0.171717172
## 2    0-1    17   235 0.072340426
## 3    0-2     1   112 0.008928571
## 4    1-0    30   175 0.171428571
## 5    1-1    16   176 0.090909091
## 6    1-2     9   133 0.067669173
## 7    2-0     8    46 0.173913043
## 8    2-1    10    74 0.135135135
## 9    2-2     8    89 0.089887640
## 10   3-0     2    13 0.153846154
## 11   3-1     2    26 0.076923077
## 12   3-2     1    30 0.033333333
```

```r
ggplot(data = cs_out_zn_count, aes(x = count, y = cs_out_zn)) + geom_point() + 
    ylab("Probalilities by count") + theme_bw()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
