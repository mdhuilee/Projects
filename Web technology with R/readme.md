Summary
-------

-   Demonstrate using Zillow official API to extract information
-   Alternatively, scrape Zillow website to get richer information and
    geocoding the address
-   Visualize relationship between real estate location and value in a
    map

1. Using API
------------

<http://www.zillow.com/howto/api/GetSearchResults.htm>

    library(knitr)
    opts_chunk$set(tidy = TRUE, cache=TRUE, autodep=TRUE, message=FALSE)

    library(httr)
    library(rvest)

    ## Loading required package: xml2

    library(magrittr)
    library(ggmap)

    ## Loading required package: ggplot2
    ## 
    ## Attaching package: 'ggmap'
    ## 
    ## The following object is masked from 'package:magrittr':
    ## 
    ##     inset

    library(stringr)
    library(knitr)
    library(xml2)
     
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
    kable(data.frame(zpid,amount,low,high,valueChange30Day)[1:6,])

<table>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">zpid</th>
<th align="left">amount</th>
<th align="left">low</th>
<th align="left">high</th>
<th align="left">valueChange30Day</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">1</td>
<td align="left">48749425</td>
<td align="left">1425219</td>
<td align="left">1339706</td>
<td align="left">1539237</td>
<td align="left">144</td>
</tr>
<tr class="even">
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">NA.1</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">NA.2</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">NA.3</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">NA.4</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

2. Explore Zillow data using rvest
----------------------------------

Hui adapted from
<https://raw.githubusercontent.com/notesofdabbler/blog_notesofdabbler/master/learn_rvest/exploreZillow_w_rvest.R>

    # here the search is filtered to just homes for sale if there are less
    # filters in search, the code will need to be modified since the css might
    # be different for different types of results (eg. homes for sales vs new
    # homes vs homes for rent)
    url = "http://www.zillow.com/homes/for_sale/Greenwood-IN/fsba,fsbo,fore,cmsn_lt/house_type/52333_rid/39.638414,-86.011362,39.550714,-86.179419_rect/12_zm/0_mmm/"

    # get list of houses for sales that appears on the page
    houselist <- url %>% html() %>% html_nodes("article")

    ## Warning: 'html' is deprecated.
    ## Use 'read_html' instead.
    ## See help("Deprecated")

    # Extract zillow id for each listing
    zpid <- houselist %>% html_attr("id") %>% str_replace_all("zpid_", "")

    # get the address for each listing

    staddrlink <- houselist %>% html_node(".property-address a") %>% html_attr("href")
    straddr <- sapply(strsplit(staddrlink, "/"), function(x) x[3])
    straddr <- str_replace_all(straddr, "-", " ")

    lat_lon <- geocode(straddr, source = "google")

    lotsqft <- houselist %>% html_node(".lot-size") %>% html_text() %>% str_replace_all(",", 
        "")
    lotsqft <- sapply(lotsqft, function(x) {
        num <- as.numeric(str_extract_all(x, "(\\d*\\.)?\\d+"))
        if (!is.na(num) & str_detect(x, "(ac|acre)")) {
            num <- num * 43560
        }
        num
    })

    yrbuilt <- houselist %>% html_node(".built-year") %>% html_text()
    yrbuilt <- as.numeric(str_extract_all(yrbuilt, "\\d+"))

    price <- houselist %>% html_node(".price-large") %>% html_text() %>% gsub("[\\$a-zA-Z,]", 
        "", .) %>% as.numeric()

    # house parameters (number of beds, baths, house area)
    houseparams <- houselist %>% html_node(".property-data") %>% html_text()
    houseparamsSplit <- strsplit(houseparams, ", ")
    ## get number of beds
    numbeds <- sapply(houseparamsSplit, function(x) as.numeric(strsplit(x[1], " ")[[1]][1]))
    ## get number of baths
    numbaths <- sapply(houseparamsSplit, function(x) as.numeric(strsplit(x[1], " ")[[1]][4]))

    housesqft <- sapply(houseparamsSplit, function(x) strsplit(x[1], " ")[[1]][7]) %>% 
        str_replace_all(",", "") %>% as.numeric()

    # houseData <-
    # data.frame(zpid,price,yrbuilt,numbeds,numbaths,housesqft,lotsqft,straddr)
    houseData <- data.frame(zpid, price, yrbuilt, numbeds, numbaths, housesqft, 
        lotsqft, straddr, lat_lon)

    kable(houseData)

<table>
<thead>
<tr class="header">
<th align="left">zpid</th>
<th align="right">price</th>
<th align="right">yrbuilt</th>
<th align="right">numbeds</th>
<th align="right">numbaths</th>
<th align="right">housesqft</th>
<th align="right">lotsqft</th>
<th align="left">straddr</th>
<th align="right">lon</th>
<th align="right">lat</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">85444228</td>
<td align="right">135000</td>
<td align="right">2002</td>
<td align="right">2</td>
<td align="right">2.0</td>
<td align="right">1173</td>
<td align="right">4791.0</td>
<td align="left">1112 Lincoln Park East Dr Greenwood IN 46142</td>
<td align="right">-86.16448</td>
<td align="right">39.63262</td>
</tr>
<tr class="even">
<td align="left">2100587200</td>
<td align="right">375000</td>
<td align="right">2013</td>
<td align="right">4</td>
<td align="right">2.5</td>
<td align="right">4074</td>
<td align="right">15246.0</td>
<td align="left">Brentford Ln Greenwood IN 46143</td>
<td align="right">-86.14453</td>
<td align="right">39.58283</td>
</tr>
<tr class="odd">
<td align="left">85442093</td>
<td align="right">10</td>
<td align="right">1800</td>
<td align="right">2</td>
<td align="right">1.0</td>
<td align="right">1114</td>
<td align="right">7840.0</td>
<td align="left">627 Forest Ave Greenwood IN 46143</td>
<td align="right">-86.09644</td>
<td align="right">39.61269</td>
</tr>
<tr class="even">
<td align="left">85439013</td>
<td align="right">79900</td>
<td align="right">1960</td>
<td align="right">3</td>
<td align="right">1.0</td>
<td align="right">960</td>
<td align="right">7840.0</td>
<td align="left">611 Park Dr Greenwood IN 46143</td>
<td align="right">-86.10177</td>
<td align="right">39.62611</td>
</tr>
<tr class="odd">
<td align="left">94434235</td>
<td align="right">30</td>
<td align="right">2007</td>
<td align="right">4</td>
<td align="right">3.5</td>
<td align="right">3900</td>
<td align="right">9147.0</td>
<td align="left">597 Timeless Run Greenwood IN 46143</td>
<td align="right">-86.04300</td>
<td align="right">39.60632</td>
</tr>
<tr class="even">
<td align="left">85448555</td>
<td align="right">139900</td>
<td align="right">1977</td>
<td align="right">3</td>
<td align="right">3.0</td>
<td align="right">1819</td>
<td align="right">27442.8</td>
<td align="left">59 Idleway Ct Greenwood IN 46142</td>
<td align="right">-86.16611</td>
<td align="right">39.61024</td>
</tr>
<tr class="odd">
<td align="left">85463712</td>
<td align="right">110000</td>
<td align="right">1996</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1309</td>
<td align="right">4487.0</td>
<td align="left">1310 Kenwood Dr Greenwood IN 46143</td>
<td align="right">-86.11026</td>
<td align="right">39.59830</td>
</tr>
<tr class="even">
<td align="left">85462889</td>
<td align="right">105000</td>
<td align="right">2001</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1105</td>
<td align="right">6272.0</td>
<td align="left">1406 Osprey Way Greenwood IN 46143</td>
<td align="right">-86.08745</td>
<td align="right">39.59663</td>
</tr>
<tr class="odd">
<td align="left">85448263</td>
<td align="right">185000</td>
<td align="right">1978</td>
<td align="right">4</td>
<td align="right">2.0</td>
<td align="right">3240</td>
<td align="right">50529.6</td>
<td align="left">3981 Shadow Hill Ln Greenwood IN 46142</td>
<td align="right">-86.17611</td>
<td align="right">39.61510</td>
</tr>
<tr class="even">
<td align="left">85461720</td>
<td align="right">174900</td>
<td align="right">2001</td>
<td align="right">3</td>
<td align="right">3.0</td>
<td align="right">2304</td>
<td align="right">9496.0</td>
<td align="left">1363 Butternut Ln Greenwood IN 46143</td>
<td align="right">-86.08107</td>
<td align="right">39.59698</td>
</tr>
<tr class="odd">
<td align="left">85453784</td>
<td align="right">184900</td>
<td align="right">1999</td>
<td align="right">3</td>
<td align="right">3.0</td>
<td align="right">2159</td>
<td align="right">9147.0</td>
<td align="left">1296 White Ash Dr Greenwood IN 46143</td>
<td align="right">-86.16785</td>
<td align="right">39.59782</td>
</tr>
<tr class="even">
<td align="left">85443594</td>
<td align="right">164900</td>
<td align="right">1994</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1444</td>
<td align="right">5662.0</td>
<td align="left">1630 Foxmere Way Greenwood IN 46142</td>
<td align="right">-86.15547</td>
<td align="right">39.62466</td>
</tr>
<tr class="odd">
<td align="left">97329217</td>
<td align="right">135000</td>
<td align="right">2007</td>
<td align="right">3</td>
<td align="right">3.0</td>
<td align="right">1548</td>
<td align="right">5837.0</td>
<td align="left">1495 Pencross Ln Greenwood IN 46143</td>
<td align="right">-86.08436</td>
<td align="right">39.59594</td>
</tr>
<tr class="even">
<td align="left">85462881</td>
<td align="right">114900</td>
<td align="right">2002</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1288</td>
<td align="right">5314.0</td>
<td align="left">1502 Osprey Way Greenwood IN 46143</td>
<td align="right">-86.08744</td>
<td align="right">39.59552</td>
</tr>
<tr class="odd">
<td align="left">85442691</td>
<td align="right">104900</td>
<td align="right">1960</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1120</td>
<td align="right">12196.8</td>
<td align="left">705 Sunset Blvd Greenwood IN 46142</td>
<td align="right">-86.12389</td>
<td align="right">39.62543</td>
</tr>
<tr class="even">
<td align="left">85456431</td>
<td align="right">150000</td>
<td align="right">2005</td>
<td align="right">4</td>
<td align="right">3.0</td>
<td align="right">2244</td>
<td align="right">10454.0</td>
<td align="left">1009 Boxwood Ln Greenwood IN 46143</td>
<td align="right">-86.08094</td>
<td align="right">39.59093</td>
</tr>
<tr class="odd">
<td align="left">85465823</td>
<td align="right">113000</td>
<td align="right">2000</td>
<td align="right">3</td>
<td align="right">2.5</td>
<td align="right">1507</td>
<td align="right">4486.0</td>
<td align="left">2349 Harvest Moon Dr Greenwood IN 46143</td>
<td align="right">-86.10438</td>
<td align="right">39.58217</td>
</tr>
<tr class="even">
<td align="left">85443260</td>
<td align="right">95000</td>
<td align="right">1935</td>
<td align="right">2</td>
<td align="right">2.0</td>
<td align="right">1144</td>
<td align="right">32670.0</td>
<td align="left">1354 Fry Rd Greenwood IN 46142</td>
<td align="right">-86.14440</td>
<td align="right">39.62817</td>
</tr>
<tr class="odd">
<td align="left">85438343</td>
<td align="right">89900</td>
<td align="right">1986</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1068</td>
<td align="right">9147.0</td>
<td align="left">1081 Laura Dr Greenwood IN 46143</td>
<td align="right">-86.10480</td>
<td align="right">39.63340</td>
</tr>
<tr class="even">
<td align="left">85440378</td>
<td align="right">149900</td>
<td align="right">1960</td>
<td align="right">4</td>
<td align="right">2.0</td>
<td align="right">2628</td>
<td align="right">20473.2</td>
<td align="left">630 Averitt Rd Greenwood IN 46142</td>
<td align="right">-86.12081</td>
<td align="right">39.60645</td>
</tr>
<tr class="odd">
<td align="left">2111293141</td>
<td align="right">149900</td>
<td align="right">2015</td>
<td align="right">2</td>
<td align="right">2.0</td>
<td align="right">1294</td>
<td align="right">9844.0</td>
<td align="left">3890 Kristi Way Greenwood IN 46142</td>
<td align="right">-86.17486</td>
<td align="right">39.63060</td>
</tr>
<tr class="even">
<td align="left">85464694</td>
<td align="right">129900</td>
<td align="right">2000</td>
<td align="right">3</td>
<td align="right">2.0</td>
<td align="right">1275</td>
<td align="right">9408.0</td>
<td align="left">2520 Longleaf Dr Greenwood IN 46143</td>
<td align="right">-86.12179</td>
<td align="right">39.58188</td>
</tr>
<tr class="odd">
<td align="left">85444318</td>
<td align="right">144900</td>
<td align="right">2003</td>
<td align="right">2</td>
<td align="right">2.0</td>
<td align="right">1252</td>
<td align="right">6534.0</td>
<td align="left">906 Lincoln Park West Dr Greenwood IN 46142</td>
<td align="right">-86.16545</td>
<td align="right">39.63080</td>
</tr>
<tr class="even">
<td align="left">124613582</td>
<td align="right">375000</td>
<td align="right">2013</td>
<td align="right">4</td>
<td align="right">3.0</td>
<td align="right">4074</td>
<td align="right">15246.0</td>
<td align="left">1325 Brentford Ln Greenwood IN 46143</td>
<td align="right">-86.14453</td>
<td align="right">39.58283</td>
</tr>
<tr class="odd">
<td align="left">97329484</td>
<td align="right">132000</td>
<td align="right">2008</td>
<td align="right">3</td>
<td align="right">3.0</td>
<td align="right">2006</td>
<td align="right">5662.0</td>
<td align="left">598 Greenway St Greenwood IN 46143</td>
<td align="right">-86.09505</td>
<td align="right">39.59972</td>
</tr>
</tbody>
</table>

3. Mapping
----------

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

![](readme_files/figure-markdown_strict/unnamed-chunk-3-1.png)
