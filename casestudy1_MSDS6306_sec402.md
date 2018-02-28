---
title: "Analysis of the Breweries and their Beers in the United States"
author: "Caroll Rodriguez, Amy Paschal and Aaron Tomkins"
date: "2/16/2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
#Introduction
<p> </p>

<b>Dataset Provided</b>
<p>Two datasets were provided by the client. One file contains a list of all Breweries in the US plus District of Columbia. 
The other file contains a list of Breweries and the craft beers they produce with the ABV, IBV, and Style indication.

<b>Data Merge and Cleaning</b>
<p>The code we created imports the two .csv files, provided by the client, into a program called R. In the R program, these .csv files are merged utilizing the Brewery Id as the common field between the files. 
Merging these files provides a complete list of Breweries and the craft beers they produce with associated attributes from both files. 
A series of steps to clean the data from abnormalities is performed like the removal of whitespace, and column headings normalized. A sample of the data is shown below.</p>


```r
# load Beers.csv, setting data types
classProfile <- c("character", "integer", "numeric", "integer", "integer", "factor", "numeric")
beers <- read.csv("Beers.csv", colClasses = classProfile, na.strings = "", strip.white = TRUE)

# further specify Name column in beers
names(beers)[names(beers) == "Name"] <- "Beer_Name"

# load Breweries.csv, setting data types
classProfile <- c("integer","character", "character", "factor")
breweries <- read.csv("Breweries.csv", colClasses = classProfile, na.strings = "", strip.white = TRUE)

# further specify Name column in breweries
names(breweries)[names(breweries) == "Name"] <- "Brewery_Name"

#merge of beers and breweries files
master <- merge(beers, breweries, by.x="Brewery_id", by.y="Brew_ID", all=TRUE)
```

<b>Number of duplicates in Master file</b>

```r
# shows that the merge did not produce duplicates
sum(duplicated(master$Beer_ID)) 
```

```
## [1] 0
```

```r
# reorder columns
master <- master[c("Beer_ID", "Beer_Name", "ABV", "IBU", "Style", "Ounces", "Brewery_id", "Brewery_Name", "City", "State" )]

# normalize column names
names(master)[names(master) == "Brewery_id"] <- "Brewery_ID"

# sort rows by beer ID
master <- master[order(master$Beer_ID),]
```


# Analysis

<b>Client Request:</b> How many breweries are present in each state?
<br />
<b>Number of Breweries by State</b>
<p>The summary table below shows the number of distinct breweries currently producing craft beers in the United States. We can see there is a heavy concentration of craft beer breweries in CO.</p>

```r
# set up libraries and options
library(knitr)
```

```
## Error in value[[3L]](cond): Package 'knitr' version 1.17 cannot be unloaded:
##  Error in unloadNamespace(package) : namespace 'knitr' is imported by 'rmarkdown' so cannot be unloaded
```

```r
library(kableExtra)
library(ggplot2)
options(knitr.table.format = "html") 

unique.master <- master[!duplicated(master$Brewery_ID),]
# count the number of rows for each state
counted.master <- aggregate(c(count = Brewery_ID) ~ State, data = unique.master, FUN = function(x){NROW(x)})
# add column headers
names(counted.master) <- c("State","Count")
# sort by Count descending
counted.master$State <- factor(counted.master$State, levels = counted.master$State[order(-counted.master$Count)])
# plot
ggplot(counted.master, aes(x=State, y=Count)) +
geom_bar(stat="identity", fill="steelblue", width=.7) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Breweries by State", x ="State", y = "Number of Breweries")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

<b>Client Request:</b> Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.</b>
<br />
<b>Data Sample of merged files</b>
Sample of first and last 6 rows in Master file

```r
# head of master
kable(head(master, 6), "markdown", row.names=FALSE, align="l", padding=2)
```

```
## Error in kable(head(master, 6), "markdown", row.names = FALSE, align = "l", : could not find function "kable"
```

```r
# tail of master
kable(tail(master, 6), "markdown", row.names=FALSE, align="l", padding=2)
```

```
## Error in kable(tail(master, 6), "markdown", row.names = FALSE, align = "l", : could not find function "kable"
```

<b>Client Request:</b> Report the number of NA's in each column.
<br />
<b>Number of NAs</b>
Per below, the ABV column has 62, the IBU column has 1005, and the Style column has 5 NAs. All other columns have zero. NA represent missing data. We suggest these fields to be populated and resubmitted for analysis.

```r
na.counts <- data.frame(sapply(master, function(y) sum(length(which(is.na(y))))))
names(na.counts) <- c("NA Count")
kable(na.counts, "markdown", align="l", padding=2)
```

```
## Error in kable(na.counts, "markdown", align = "l", padding = 2): could not find function "kable"
```

<b>Client Request:</b> Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.
<br />
<b>Median ABV per State</b>
The code calculates the median IBU by State with NA's removed and sets the column names. A visual representation of this calculation is included in a bar chart.

```r
ibu.med <- aggregate(master$IBU, by = list(master$State), FUN = function(x) median(x, na.rm = TRUE))
names(ibu.med) <- c("State","IBU")

# determine if any states are missing all IBU values
ibu.na <- which(is.na(ibu.med$IBU))
states.ibu.na <- ibu.med[ibu.na,c("State")]

# remove states with no values in IBU
ibu.med <- ibu.med[-ibu.na,]

ggplot(ibu.med, aes(x=State, y=IBU)) +
geom_bar(stat="identity", fill="steelblue", width=.7) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Median IBU by State", x ="State", y = "Median IBU")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

###### The following states had no available data for IBU and were therefore left out of the graph: SD

<b>Median IBU per State</b>
The code calculates the median ABV by State with NA's removed and sets the column names. A visual representation of this calculation is included in a bar chart.

```r
abv.med <- aggregate(master$ABV, by = list(master$State), FUN = function(x) median(x, na.rm = TRUE))
names(abv.med) <- c("State","ABV")

ggplot(abv.med, aes(x=State, y=ABV)) +
geom_bar(stat="identity", fill="steelblue", width=.7) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Median ABV by State", x ="State", y = "Median ABV")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

5. Which state has the maximum alcoholic (ABV) beer? Which state as the most bitter (IBU) beer?

```r
ABV.row <- master[which.max(master$ABV),]
IBU.row <- master[which.max(master$IBU),]
```
##### CO has the maximum ABV with Lee Hill Series Vol. 5 - Belgian Style Quadrupel Ale from Upslope Brewing Company.
##### OR has the most bitter beer with Bitter Bitch Imperial IPA from Astoria Brewing Company.

6. Summary statistics for the ABV variable.

```r
my.summary <- data.frame(unclass(summary(master$ABV)))
names(my.summary) <- c("Summary Stats")
kable(my.summary, row.names=TRUE, digits=4)
```

```
## Error in kable(my.summary, row.names = TRUE, digits = 4): could not find function "kable"
```

7. Is there an apparent relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.
<br /><b>Per below, there is a positive linear relationship between ABV and IBU.</b>

```r
ggplot(master, aes(x=IBU, y=ABV)) + geom_point() + labs(title="ABV to IBU", x ="IBU", y = "ABV")
```

```
## Warning: Removed 1005 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)
