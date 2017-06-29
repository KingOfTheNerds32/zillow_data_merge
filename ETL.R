library(reshape2)

setwd('YOUR WD HERE')

process_csv <- function(csv, metric){
  df <- read.csv(csv)
  first_date_col <- match('SizeRank', names(df)) + 1
  melted_df <- melt(df, id = 1:(first_date_col - 1))
  colnames(melted_df)[ncol(melted_df)] <- metric
  colnames(melted_df)[ncol(melted_df) - 1] <- 'Month_Year'
  melted_df$Month_Year <- sub('X', '', melted_df$Month_Year)
  # melted_df$UID <- paste(melted_df$RegionName, melted_df$City, melted_df$State, melted_df$Metro, melted_df$CountyName, sep ='')
  
  county_index <- match('County', names(melted_df))
  if (!is.na(county_index)){
    colnames(melted_df)[county_index] <- 'CountyName'
  }
  
  return(melted_df[, setdiff(names(melted_df), c('SizeRank', 'RegionID'))])
}

merge_files <- function(l){
  df <- l[1]
  
  for (i in 2:length(l)){
    df <- merge(df, l[i], by = c('RegionName', 'City', 'State', 'CountyName', 'Metro', 'Month_Year'), all = TRUE)
  }
  return(df)
}

median <- process_csv('01. Neighborhood_MedianListingPricePerSqft_AllHomes.csv', 'Median Listing Price Per Sqft')
price_reduction <- process_csv('02. Neighborhood_PctOfListingsWithPriceReductions_AllHomes.csv', 'Pct of Listings with Price Reductions')
median_sold <- process_csv('03. Neighborhood_MedianSoldPrice_AllHomes.csv', 'Median Sold Price')
median_price_reduction <- process_csv('04. Neighborhood_MedianPctOfPriceReduction_AllHomes.csv', 'Median Pct of Price Reduction')
median_listing <- process_csv('05. Neighborhood_MedianListingPrice_AllHomes.csv', 'Median Listing Price')
median_sold_psqft <- process_csv('06. Neighborhood_MedianSoldPricePerSqft_AllHomes.csv', 'Median Sold Price per Sqft')
turnover <- process_csv('07. Neighborhood_Turnover_AllHomes.csv', 'Turnover')

files <- list(median, price_reduction, median_sold, median_price_reduction, median_listing, median_sold_psqft, turnover)
final_df <- merge_files(files)

write.csv(final_df ,'zillow.csv')