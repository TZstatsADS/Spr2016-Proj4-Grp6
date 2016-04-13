rm(list=ls())
## NOTE: TO RUN THE SEARCH CODE
## YOU WILL HAVE TO USE YOUR OWN API ACCESS INFO
library("XML")
library("RCurl")
search.amazon <- function(Keywords, SearchIndex = 'All', AWSAccessKeyId, AWSsecretkey, AssociateTag, ResponseGroup, Operation = 'ItemSearch'){
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

###Function for Movie Poster:
Keywords="B003AI2VGA"
AWSAccessKeyId="AKIAJCYPY2UUDPZA6W2Q"
AWSsecretkey="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"
AssociateTag="jzjz-20"

getPicture<-function(productid){
  productid<-as.character(productid)
  gg<-search.amazon(Keywords=productid,ResponseGroup = 'Images',AWSAccessKeyId=AWSAccessKeyId,AWSsecretkey=AWSsecretkey, AssociateTag=AssociateTag)
  doc<-xmlParse(gg)
  picnode = xmlRoot(doc)[["Items"]][["Item"]][["ImageSets"]][["ImageSet"]][["MediumImage"]]
  picvalue<-as.character(sapply(xmlChildren(picnode), function(node) xmlValue(node)))
  return(picvalue[1])
}

getPicture(Keywords)




###Function for Director:
getDirector<-function(productid){
  productid<-as.character(productid)
  gg<-search.amazon(Keywords=productid,ResponseGroup = 'Images',AWSAccessKeyId=AWSAccessKeyId,AWSsecretkey=AWSsecretkey, AssociateTag=AssociateTag)
  doc<-xmlParse(gg)
  picnode = xmlRoot(doc)[["Items"]][["Item"]][["ImageSets"]][["ImageSet"]][["MediumImage"]]
  picvalue<-as.character(sapply(xmlChildren(picnode), function(node) xmlValue(node)))
  return(picvalue[1])
}

getPicture(Keywords)
doc<-xmlParse(gg)
getInfo<-function(productid,att){
  productid<-as.character(productid)
  gg<-search.amazon(Keywords=productid,ResponseGroup = 'ItemAttributes',AWSAccessKeyId=AWSAccessKeyId,AWSsecretkey=AWSsecretkey, AssociateTag=AssociateTag)
  doc<-xmlParse(gg)
  attnode = xmlRoot(doc)[["Items"]][["Item"]][["ItemAttributes"]][[att]]
  attvalue<-as.character(sapply(xmlChildren(attnode), function(node) xmlValue(node)))
  return(attvalue)
}
director<-getInfo(Keywords,"Director")
actor<-getInfo(Keywords,"Actor")

#####Read as NA if no Genre/Title
# tryCatch({
#   title<- output("Title")
#   },
#   error =function(err){title<-NA})
# tryCatch({
#   genre<-output("Genre")
# },
# error =function(err){genre<-NA})


# actors<-output("Actor")
# Might need to bind PRODUCT ID
# product_i<-cbind(title,genre)

# Example of getting an info
# titlenode=xmlRoot(doc)[["Items"]][["Item"]][["ItemAttributes"]][["Title"]]
# title<-as.character(sapply(xmlChildren(titlenode), function(node) xmlValue(node)))