
## Summary
* Demonstrate using Zillow official API to extract information
* Alternatively, scrape Zillow website to get richer information and geocoding the address
* Visualize relationship between real estate location and value in a map


##1. Using API 
<http://www.zillow.com/howto/api/GetSearchResults.htm>


```r
library(knitr)
opts_chunk$set(tidy = TRUE, cache=TRUE, autodep=TRUE, message=FALSE)

library(httr)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(knitr)
library(xml2)
library(formattable)
sample <- GET("http://www.zillow.com/webservice/GetSearchResults.htm", 
              query = list('zws-id' = "X1-ZWz1f063o0dzij_7yjzz", 
                           address = "2114 Bigelow Ave",
                           citystatezip = "Seattle, WA"))

result <- read_xml(content(sample, "text"))

zpid <- result %>% html_node("zpid")%>%html_text()
amount <- result %>% html_node("amount")%>%html_text()
low <- result %>% html_node("low")%>%html_text()
high <- result %>% html_node("high")%>%html_text()
valueChange30Day <- result %>% html_node("valueChange")%>%html_text()
kable(data.frame(zpid,amount,low,high,valueChange30Day))
```



|zpid     |amount  |low     |high    |valueChange30Day |
|:--------|:-------|:-------|:-------|:----------------|
|48749425 |1425219 |1339706 |1539237 |144              |



##2.  Explore Zillow data using rvest

Hui adapted from https://raw.githubusercontent.com/notesofdabbler/blog_notesofdabbler/master/learn_rvest/exploreZillow_w_rvest.R
 

```r
# here the search is filtered to just homes for sale
# if there are less filters in search, the code will need to be modified since the css might be different
# for different types of results (eg. homes for sales vs new homes vs homes for rent)
url="http://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/"

# get list of houses for sales that appears on the page
houselist <- url%>%html()%>%html_nodes("article")
```

```
## Warning: 'html' is deprecated.
## Use 'read_html' instead.
## See help("Deprecated")
```

```r
# Extract zillow id for each listing
zpid <- houselist %>% html_attr("id") %>% str_replace_all("zpid_", "")

# get the address for each listing

staddrlink <- houselist%>%html_node(".property-address a")%>%html_attr("href")
straddr <- sapply(strsplit(staddrlink,"/"),function(x) x[3])
straddr <- str_replace_all(straddr, "-", " ")

lat_lon <- geocode(straddr, source = "google")

lotsqft <- houselist %>% html_node(".lot-size") %>% html_text() %>% str_replace_all(",", "")
lotsqft <- sapply(lotsqft, function(x) {
        num <- as.numeric(str_extract_all(x, "(\\d*\\.)?\\d+"))
        if (!is.na(num) & str_detect(x, "(ac|acre)")) {
            num <- num * 43560
        }
        num
}
)

yrbuilt <- houselist%>%html_node(".built-year")%>%html_text()
yrbuilt <- as.numeric(str_extract_all(yrbuilt, "\\d+"))

price <- houselist%>%html_node(".price-large")%>%html_text()%>%gsub("[\\$a-zA-Z,]","",.)%>%as.numeric()

# house parameters (number of beds, baths, house area)
houseparams <- houselist%>%html_node(".property-data")%>%html_text()
houseparamsSplit <- strsplit(houseparams,", ")
## get number of beds
numbeds <- sapply(houseparamsSplit,function(x) as.numeric(strsplit(x[1]," ")[[1]][1]))
## get number of baths
numbaths <- sapply(houseparamsSplit,function(x) as.numeric(strsplit(x[1]," ")[[1]][4]))

housesqft <- sapply(houseparamsSplit,function(x) strsplit(x[1]," ")[[1]][7]) %>%
    str_replace_all(",", "") %>% as.numeric()

#houseData <- data.frame(zpid,price,yrbuilt,numbeds,numbaths,housesqft,lotsqft,straddr)
houseData <- data.frame(zpid,price,yrbuilt,numbeds,numbaths,housesqft,lotsqft,straddr,lat_lon)

formattable(houseData, list(
  housesqft = formatter("span", 
    style = x ~ style(color = "white", background = "green", 
      padding.right = sprintf("%.0fpx", 4 + 76 * normalize(x)),
      padding.left = "4px",
      border.radius = "4px")),
    price = formatter("span", 
    style = x ~ style(color = "white", background = "green", 
      padding.right = sprintf("%.0fpx", 4 + 76 * normalize(x)),
      padding.left = "4px",
      border.radius = "4px"))
  # ,
#   grade = formatter("span",
#     style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
#   test1_score = color_bar("pink", 0.2),
#   test2_score = color_bar("pink", 0.2),
#   final_score = formatter("span",
#     style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
#     x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
#   registered = formatter("span", 
#     style = x ~ style(color = ifelse(x, "green", "red")),
#     x ~ icontext(ifelse(x, "leaf", "alarm"), ifelse(x, "ok", "No")))
), align = c("l","l","l","l","l","l","l","l","l"))
```


|zpid       |price                                                                                                                   |yrbuilt |numbeds |numbaths |housesqft                                                                                                             |lotsqft |straddr                                      |lon       |lat      |
|:----------|:-----------------------------------------------------------------------------------------------------------------------|:-------|:-------|:--------|:---------------------------------------------------------------------------------------------------------------------|:-------|:--------------------------------------------|:---------|:--------|
|85444228   |<span style="color: white; background: green; padding-right: 31px; padding-left: 4px; border-radius: 4px">135000</span> |2002    |2       |2.0      |<span style="color: white; background: green; padding-right: 9px; padding-left: 4px; border-radius: 4px">1173</span>  |4791.0  |1112 Lincoln Park East Dr Greenwood IN 46142 |-86.16448 |39.63262 |
|2100587200 |<span style="color: white; background: green; padding-right: 80px; padding-left: 4px; border-radius: 4px">375000</span> |2013    |4       |2.5      |<span style="color: white; background: green; padding-right: 80px; padding-left: 4px; border-radius: 4px">4074</span> |15246.0 |Brentford Ln Greenwood IN 46143              |-86.14453 |39.58283 |
|85442093   |<span style="color: white; background: green; padding-right: 4px; padding-left: 4px; border-radius: 4px">10</span>      |1800    |2       |1.0      |<span style="color: white; background: green; padding-right: 8px; padding-left: 4px; border-radius: 4px">1114</span>  |7840.0  |627 Forest Ave Greenwood IN 46143            |-86.09644 |39.61269 |
|85439013   |<span style="color: white; background: green; padding-right: 20px; padding-left: 4px; border-radius: 4px">79900</span>  |1960    |3       |1.0      |<span style="color: white; background: green; padding-right: 4px; padding-left: 4px; border-radius: 4px">960</span>   |7840.0  |611 Park Dr Greenwood IN 46143               |-86.10177 |39.62611 |
|94434235   |<span style="color: white; background: green; padding-right: 4px; padding-left: 4px; border-radius: 4px">30</span>      |2007    |4       |3.5      |<span style="color: white; background: green; padding-right: 76px; padding-left: 4px; border-radius: 4px">3900</span> |9147.0  |597 Timeless Run Greenwood IN 46143          |-86.04300 |39.60632 |
|85448555   |<span style="color: white; background: green; padding-right: 32px; padding-left: 4px; border-radius: 4px">139900</span> |1977    |3       |3.0      |<span style="color: white; background: green; padding-right: 25px; padding-left: 4px; border-radius: 4px">1819</span> |27442.8 |59 Idleway Ct Greenwood IN 46142             |-86.16611 |39.61024 |
|85463712   |<span style="color: white; background: green; padding-right: 26px; padding-left: 4px; border-radius: 4px">110000</span> |1996    |3       |2.0      |<span style="color: white; background: green; padding-right: 13px; padding-left: 4px; border-radius: 4px">1309</span> |4487.0  |1310 Kenwood Dr Greenwood IN 46143           |-86.11026 |39.59830 |
|85462889   |<span style="color: white; background: green; padding-right: 25px; padding-left: 4px; border-radius: 4px">105000</span> |2001    |3       |2.0      |<span style="color: white; background: green; padding-right: 8px; padding-left: 4px; border-radius: 4px">1105</span>  |6272.0  |1406 Osprey Way Greenwood IN 46143           |-86.08745 |39.59663 |
|85448263   |<span style="color: white; background: green; padding-right: 41px; padding-left: 4px; border-radius: 4px">185000</span> |1978    |4       |2.0      |<span style="color: white; background: green; padding-right: 60px; padding-left: 4px; border-radius: 4px">3240</span> |50529.6 |3981 Shadow Hill Ln Greenwood IN 46142       |-86.17611 |39.61510 |
|85461720   |<span style="color: white; background: green; padding-right: 39px; padding-left: 4px; border-radius: 4px">174900</span> |2001    |3       |3.0      |<span style="color: white; background: green; padding-right: 37px; padding-left: 4px; border-radius: 4px">2304</span> |9496.0  |1363 Butternut Ln Greenwood IN 46143         |-86.08107 |39.59698 |
|85453784   |<span style="color: white; background: green; padding-right: 41px; padding-left: 4px; border-radius: 4px">184900</span> |1999    |3       |3.0      |<span style="color: white; background: green; padding-right: 33px; padding-left: 4px; border-radius: 4px">2159</span> |9147.0  |1296 White Ash Dr Greenwood IN 46143         |-86.16785 |39.59782 |
|85443594   |<span style="color: white; background: green; padding-right: 37px; padding-left: 4px; border-radius: 4px">164900</span> |1994    |3       |2.0      |<span style="color: white; background: green; padding-right: 16px; padding-left: 4px; border-radius: 4px">1444</span> |5662.0  |1630 Foxmere Way Greenwood IN 46142          |-86.15547 |39.62466 |
|97329217   |<span style="color: white; background: green; padding-right: 31px; padding-left: 4px; border-radius: 4px">135000</span> |2007    |3       |3.0      |<span style="color: white; background: green; padding-right: 18px; padding-left: 4px; border-radius: 4px">1548</span> |5837.0  |1495 Pencross Ln Greenwood IN 46143          |-86.08436 |39.59594 |
|85462881   |<span style="color: white; background: green; padding-right: 27px; padding-left: 4px; border-radius: 4px">114900</span> |2002    |3       |2.0      |<span style="color: white; background: green; padding-right: 12px; padding-left: 4px; border-radius: 4px">1288</span> |5314.0  |1502 Osprey Way Greenwood IN 46143           |-86.08744 |39.59552 |
|85442691   |<span style="color: white; background: green; padding-right: 25px; padding-left: 4px; border-radius: 4px">104900</span> |1960    |3       |2.0      |<span style="color: white; background: green; padding-right: 8px; padding-left: 4px; border-radius: 4px">1120</span>  |12196.8 |705 Sunset Blvd Greenwood IN 46142           |-86.12389 |39.62543 |
|85456431   |<span style="color: white; background: green; padding-right: 34px; padding-left: 4px; border-radius: 4px">150000</span> |2005    |4       |3.0      |<span style="color: white; background: green; padding-right: 35px; padding-left: 4px; border-radius: 4px">2244</span> |10454.0 |1009 Boxwood Ln Greenwood IN 46143           |-86.08094 |39.59093 |
|85465823   |<span style="color: white; background: green; padding-right: 27px; padding-left: 4px; border-radius: 4px">113000</span> |2000    |3       |2.5      |<span style="color: white; background: green; padding-right: 17px; padding-left: 4px; border-radius: 4px">1507</span> |4486.0  |2349 Harvest Moon Dr Greenwood IN 46143      |-86.10438 |39.58217 |
|85443260   |<span style="color: white; background: green; padding-right: 23px; padding-left: 4px; border-radius: 4px">95000</span>  |1935    |2       |2.0      |<span style="color: white; background: green; padding-right: 8px; padding-left: 4px; border-radius: 4px">1144</span>  |32670.0 |1354 Fry Rd Greenwood IN 46142               |-86.14440 |39.62817 |
|85440378   |<span style="color: white; background: green; padding-right: 34px; padding-left: 4px; border-radius: 4px">149900</span> |1960    |4       |2.0      |<span style="color: white; background: green; padding-right: 45px; padding-left: 4px; border-radius: 4px">2628</span> |20473.2 |630 Averitt Rd Greenwood IN 46142            |-86.12081 |39.60645 |
|85438343   |<span style="color: white; background: green; padding-right: 22px; padding-left: 4px; border-radius: 4px">89900</span>  |1986    |3       |2.0      |<span style="color: white; background: green; padding-right: 7px; padding-left: 4px; border-radius: 4px">1068</span>  |9147.0  |1081 Laura Dr Greenwood IN 46143             |-86.10480 |39.63340 |
|2111293141 |<span style="color: white; background: green; padding-right: 34px; padding-left: 4px; border-radius: 4px">149900</span> |2015    |2       |2.0      |<span style="color: white; background: green; padding-right: 12px; padding-left: 4px; border-radius: 4px">1294</span> |9844.0  |3890 Kristi Way Greenwood IN 46142           |-86.17486 |39.63060 |
|85444318   |<span style="color: white; background: green; padding-right: 33px; padding-left: 4px; border-radius: 4px">144900</span> |2003    |2       |2.0      |<span style="color: white; background: green; padding-right: 11px; padding-left: 4px; border-radius: 4px">1252</span> |6534.0  |906 Lincoln Park West Dr Greenwood IN 46142  |-86.16545 |39.63080 |
|85464694   |<span style="color: white; background: green; padding-right: 30px; padding-left: 4px; border-radius: 4px">129900</span> |2000    |3       |2.0      |<span style="color: white; background: green; padding-right: 12px; padding-left: 4px; border-radius: 4px">1275</span> |9408.0  |2520 Longleaf Dr Greenwood IN 46143          |-86.12179 |39.58188 |
|124613582  |<span style="color: white; background: green; padding-right: 80px; padding-left: 4px; border-radius: 4px">375000</span> |2013    |4       |3.0      |<span style="color: white; background: green; padding-right: 80px; padding-left: 4px; border-radius: 4px">4074</span> |15246.0 |1325 Brentford Ln Greenwood IN 46143         |-86.14453 |39.58283 |
|97329484   |<span style="color: white; background: green; padding-right: 31px; padding-left: 4px; border-radius: 4px">132000</span> |2008    |3       |3.0      |<span style="color: white; background: green; padding-right: 30px; padding-left: 4px; border-radius: 4px">2006</span> |5662.0  |598 Greenway St Greenwood IN 46143           |-86.09505 |39.59972 |




##3. Mapping


```r
library(ggmap)

houseData$price_cat[houseData$price > 0] <- 1
houseData$price_cat[houseData$price > 1e+05] <- 2
houseData$price_cat[houseData$price > 2e+05] <- 3
houseData$price_cat[houseData$price > 3e+05] <- 4
houseData$price_cat <- as.factor(houseData$price_cat)

theme_set(theme_bw(16))
gw_in <- qmap("Greenwood IN", color = "bw", zoom = 12)

gw_in + stat_bin2d(aes(x = lon, y = lat, price_cat, fill = price_cat), size = 0.5, 
    bins = 25, alpha = 1/2, data = houseData) + scale_fill_manual(values = c(`1` = "#c7e9c0", 
    `2` = "#74c476", `3` = "#31a354", `4` = "#006d2c"), labels = c("<100,000", 
    "100,000-200,000", "200,000-300,000", ">300,000")) + 
theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), 
    axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), 
    legend.text = element_text(size = 16))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


