library("XML")
library("bitops")
library("RCurl")
library("digest")
library("data.table")
search.amazon <- function(Keywords, SearchIndex = 'All', AWSAccessKeyId, AWSsecretkey, AssociateTag, ResponseGroup = 'ItemAttributes', Operation = 'ItemSearch'){
  library(digest)
  library(RCurl)
  
  base.html.string <- "http://ecs.amazonaws.com/onca/xml?"
  SearchIndex <- match.arg(SearchIndex, c('All',
                                          'Apparel',
                                          'Appliances',
                                          'ArtsAndCrafts',
                                          'Automotive',
                                          'Baby',
                                          'Beauty',
                                          'Blended',
                                          'Books',
                                          'Classical',
                                          'DigitalMusic',
                                          'DVD',
                                          'Electronics',
                                          'ForeignBooks',
                                          'Garden',
                                          'GourmetFood',
                                          'Grocery',
                                          'HealthPersonalCare',
                                          'Hobbies',
                                          'HomeGarden',
                                          'HomeImprovement',
                                          'Industrial',
                                          'Jewelry',
                                          'KindleStore',
                                          'Kitchen',
                                          'Lighting',
                                          'Magazines',
                                          'Marketplace',
                                          'Miscellaneous',
                                          'MobileApps',
                                          'MP3Downloads',
                                          'Music',
                                          'MusicalInstruments',
                                          'MusicTracks',
                                          'OfficeProducts',
                                          'OutdoorLiving',
                                          'Outlet',
                                          'PCHardware',
                                          'PetSupplies',
                                          'Photo',
                                          'Shoes',
                                          'Software',
                                          'SoftwareVideoGames',
                                          'SportingGoods',
                                          'Tools',
                                          'Toys',
                                          'UnboxVideo',
                                          'VHS',
                                          'Video',
                                          'VideoGames',
                                          'Watches',
                                          'Wireless',
                                          'WirelessAccessories'))
  Operation <- match.arg(Operation, c('ItemSearch',
                                      'ItemLookup',
                                      'BrowseNodeLookup',
                                      'CartAdd',
                                      'CartClear',
                                      'CartCreate',
                                      'CartGet',
                                      'CartModify',
                                      'SimilarityLookup'))
  ResponseGroup <- match.arg(ResponseGroup, c('Accessories',
                                              'AlternateVersions',
                                              'BrowseNodeInfo',
                                              'BrowseNodes',
                                              'Cart',
                                              'CartNewReleases',
                                              'CartTopSellers',
                                              'CartSimilarities',
                                              'Collections',
                                              'EditorialReview',
                                              'Images',
                                              'ItemAttributes',
                                              'ItemIds',
                                              'Large',
                                              'Medium',
                                              'MostGifted',
                                              'MostWishedFor',
                                              'NewReleases',
                                              'OfferFull',
                                              'OfferListings',
                                              'Offers',
                                              'OfferSummary',
                                              'PromotionSummary',
                                              'RelatedItems',
                                              'Request',
                                              'Reviews',
                                              'SalesRank',
                                              'SearchBins',
                                              'Similarities',
                                              'Small',
                                              'TopSellers',
                                              'Tracks',
                                              'Variations',
                                              'VariationImages',
                                              'VariationMatrix',
                                              'VariationOffers',
                                              'VariationSummary'),
                             several.ok = TRUE)
  version.request = '2011-08-01'
  Service = 'AWSECommerceService'
  if(!is.character(AWSsecretkey)){
    message('The AWSsecretkey should be entered as a character vect, ie be qouted')
  }
  
  pb.txt <- Sys.time()
  
  pb.date <- as.POSIXct(pb.txt, tz = Sys.timezone)
  
  Timestamp = strtrim(format(pb.date, tz = "GMT", usetz = TRUE, "%Y-%m-%dT%H:%M:%S.000Z"), 24)
  
  str = paste('GET\necs.amazonaws.com\n/onca/xml\n',
              'AWSAccessKeyId=', curlEscape(AWSAccessKeyId),
              '&AssociateTag=', AssociateTag,
              '&Keywords=', curlEscape(Keywords),
              '&Operation=', curlEscape(Operation),
              '&ResponseGroup=', curlEscape(ResponseGroup),
              '&SearchIndex=', curlEscape(SearchIndex),
              '&Service=AWSECommerceService',
              '&Timestamp=', gsub('%2E','.',gsub('%2D', '-', curlEscape(Timestamp))),
              '&Version=', version.request,
              sep = '')
  
  ## signature test
  #Signature = curlEscape(base64(hmac( enc2utf8((AWSsecretkey)), enc2utf8(str1), algo = 'sha256', serialize = FALSE,  raw = TRUE)))
  Signature = curlEscape(base64(hmac( enc2utf8((AWSsecretkey)), enc2utf8(str), algo = 'sha256', serialize = FALSE,  raw = TRUE)))
  
  AmazonURL <- paste(base.html.string,
                     'AWSAccessKeyId=', AWSAccessKeyId,
                     '&AssociateTag=', AssociateTag,
                     '&Keywords=', Keywords,
                     '&Operation=',Operation,
                     '&ResponseGroup=',ResponseGroup,
                     '&SearchIndex=', SearchIndex,
                     '&Service=AWSECommerceService',
                     '&Timestamp=', Timestamp,
                     '&Version=', version.request,
                     '&Signature=', Signature,
                     sep = '')
  AmazonResult <- getURL(AmazonURL)
  return(AmazonResult)
}

newinfo <- 1
productnew <- matrix(nrow=34134, ncol=2)

for (i in 1 :  34134){
  
  newinfo[i] <- search.amazon(Keywords=newdata$product_productid[i],AWSAccessKeyId="AKIAJCYPY2UUDPZA6W2Q",AWSsecretkey="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", AssociateTag="jzjz-20")
  print(i)
  Sys.sleep(1)
  doc<-xmlParse(newinfo[i])
  output<-function(att){
    attnode = xmlRoot(doc)[["Items"]][["Item"]][["ItemAttributes"]][[att]]
    attvalue<-as.character(sapply(xmlChildren(attnode), function(node) xmlValue(node)))
    return(attvalue)
  }
  tryCatch({
    title<- output("Title")
  },
  error =function(err){title<-NA})
  tryCatch({
    genre<-output("Genre")
  },
  error =function(err){genre<-NA})
  # Might need to bind PRODUCT ID
  productnew[i,1] <- title
  productnew[i, 2] <- genre
}



View(newinfo[1])
newdata <- amazondata[amazondata$review_helpfulness != "0/0" ]
for (i in 0:999) {
  itera <- paste0("0/", i)
  newdata <- newdata[newdata$review_helpfulness != itera ]
  
}
lessdata<- newdata
lessdata <-  lessdata[lessdata$review_helpfulness != "1/1" ]
lessdata <-  lessdata[lessdata$review_helpfulness != "1/2" ]
lessdata <-  lessdata[lessdata$review_helpfulness != "2/2" ]
lessdatakey <- lessdata$product_productid




sappley(newdata, class)
newdata$review_helpfulness <- as.numeric(newdata$review_helpfulness)
type.convert(newdata$review_helpfulness)

length(newdata$review_helpfulness)

for ( i in 1: length(newdata$review_helpfulness)){
  newdata$review_helpfulness[i] <- type.convert(newdata$review_helpfulness[i])
}

setwd("C:/Users/ouwen/Downloads/")
library("data.table")

newdata <- fread("C:/Users/ouwen/Downloads/newdata.csv")

newinfo1 <- fread("newinfo1.csv")
newinfo2 <- fread("newinfo2.csv")
newinfo3 <- fread("newinfo3.csv")
amazondata <- fread("C:/Users/ouwen/Downloads/moviescsv.csv")

setwd("C:/Users/ouwen/Downloads/")
write.csv(newdata, file = "newdata.csv")


newinfo[3411]

write.csv(newkk, file = "datawithnameand genre.csv")
write.csv(newinfo, file = "rawfromamazon.csv")

write.csv(newinfo, file = "amazonretrieved.csv")




doc<-xmlParse(newinfo)
output<-function(att){
  attnode = xmlRoot(doc)[["Items"]][["Item"]][["ItemAttributes"]][[att]]
  attvalue<-as.character(sapply(xmlChildren(attnode), function(node) xmlValue(node)))
  return(attvalue)
}
title<-output("Title")
genre<-output("Genre")
# Might need to bind PRODUCT IDprodu
product_i<-cbind(title,genre,language,rate)

# Example of getting an info
language<-output("Languages")
rate<-output("AudienceRating")
# Might need to bind PRODUCT ID
product_i<-cbind(title,genre,language,rate)

# Example of getting an info
# titlenode=xmlRoot(doc)[["Items"]][["Item"]][["ItemAttributes"]][["Title"]]
# title<-as.character(sapply(xmlChildren(titlenode), function(node) xmlValue(node)))