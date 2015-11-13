
# title: "Package demo"

This document is to demonstrate how to use R package `NFLvegas` made by Hui Li to extract NFL vegas line information from website <pro-football-reference.com>.
 

```r
# library(devtools) install_github('NFLvegas', 'mdhuilee')
library("NFLvegas")

nfl_vegas(from = 2011, to = 2011, obs = 5)
```

```
## ##------ Fri Nov 13 13:28:43 2015 ------##
## ##------ Fri Nov 13 13:28:49 2015 ------##
```

```
##   year season       gameid            favorite spread
## 1 2011   2011 201109080gnb   Green Bay Packers   -5.0
## 2 2011   2011 201109110cle    Cleveland Browns   -7.0
## 3 2011   2011 201109110crd   Arizona Cardinals   -7.0
## 4 2011   2011 201109110ram Philadelphia Eagles   -4.0
## 5 2011   2011 201109110sfo San Francisco 49ers   -6.0
```
 
