setwd("~/Documents/Projects/CF/Bankbazaar")
#load libraries
library(stringr)
library(stringi)
library(RCurl)
library(plyr)
require(RSelenium)
require(XML)

# scrape the data
banks <- c('allahabad-bank','andhra-bank','axis-bank','bajaj-finserv','bank-of-baroda',
           'bank-of-india','bank-of-maharashtra','canara-bank','canfin-homes-ltd','central-bank-of-india',
           'citibank','corporation-bank','dena-bank','dhfl','federal-bank',
           'gic-housing-finance-ltd','hdfc-limited','hsbc-bank','icici-bank','idbi-bank',
           'indiabulls','indian-bank','karur-vysya-bank','kotak-mahindra-bank','lic-housing-finance',
           'oriental-bank-of-commerce','pnb-housing-finance-limited','punjab-national-bank','saraswat-cooperative-bank','standard-chartered-bank',
           'state-bank-of-bikaner-and-jaipur','state-bank-of-hyderabad','state-bank-of-india','state-bank-of-mysore','sundaram-finance',
           'syndicate-bank','tata-capital-limited','uco-bank','union-bank-of-india','united-bank-of-india',
           'vijaya-bank')
pages <- c(1,2,50,1,3,
           3,1,4,1,2,
           2,3,1,33,3,
           1,50,2,50,9,
           17,1,1,1,30,
           1,19,3,1,15,
           1,2,34,1,2,
           1,4,2,7,1,
           1)

df <- data.frame(Bank = character(),
                 City = character(),
                 Review = character(),
                 Date = character(),
                 stringsAsFactors=FALSE)

pJS <- phantom(extras = "--ignore-ssl-errors=true")
browser = remoteDriver(browserName = "phantomJS")
browser$open(silent=TRUE)

k=1

for(i in banks){
    for (j in 1:pages[k]){
        n <- sample(15:20, 1)
        cat("\n Scraping -",i,"- Page",j,"/",pages[k],"in",n,"seconds")
        url <- paste("https://www.bankbazaar.com/reviews/",i,"/home-loan.html?reviewPageNumber=",j,sep="")
        browser$navigate(url)
        Sys.sleep(n)
        doc <- htmlParse(browser$getPageSource()[[1]])
        
        review <- xpathSApply(doc, "//div[@class='text_here']", saveXML)
        review <- stri_replace_all(review, regex = '<\\s*span.*>', replacement = '')
        review <- stri_replace_all(review, regex = '<div class=\"text_here\" threedots=\"', replacement = '')
        review <- stri_replace_all(review, regex = "^ +|(?<= ) +| +$", replacement = "")
        review <- stri_replace_all(review, regex = '\">\n \n', replacement = '')
        review <- stri_replace_all(review, regex = '&#10;', replacement = '')
        review <- stri_replace_all(review, regex = '&quot;', replacement = '')
        rating <- xpathSApply(doc, "//div[@class='rating-section review-user-score']//span[@itemprop='ratingvalue']/text()", saveXML)
        city <- xpathSApply(doc, "//div[@class='reviewer-details']//span[@class='text-capitalize']/text()", saveXML)
        date <- xpathSApply(doc, "//div[@class='reviewer-details']//span[@itemprop='datepublished']/text()", saveXML)
        date <- stri_replace_all(date, regex = "Reviewed on ", replacement = "")
        #date <- as.Date(date,"%b %d, %Y")
        bank <- xpathSApply(doc, "//div[@class='dontshow']//span[@itemprop='name']/text()",saveXML)
        bank <- stri_replace_all(bank, regex = "^ +|(?<= ) +| +$", replacement = "")
        bank <- stri_replace_all(bank, regex = "\n ", replacement = "")
        
        #combine cols
        tmp <- cbind(bank,city,review,date)
        names(tmp) <- c("Bank","City","Review","Date")
        #combine rows
        df <- rbind(df,tmp)
        #export
        write.csv(df, "./bb_reviews.csv", row.names=F)
    }
    k=k+1
}

pJS$stop()