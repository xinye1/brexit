# Data source: http://www.electoralcommission.org.uk/find-information-by-subject/elections-and-referendums/past-elections-and-referendums/eu-referendum/electorate-and-count-information
# The Electoral Commssion
# CSV at: http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv
csv_url <- 'http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv'
raw_votes <- read.csv(csv_url, stringsAsFactors = F)
names_votes <- raw_votes$Area
names_diff <- setdiff(names_votes, names_shp)

names_diff1 <- setdiff(names_shp, names_votes)

# change the name in the CSV (add 'The')
raw_votes$Area[grepl('glamorgan', raw_votes$Area, ignore.case = T)] <- 'The Vale of Glamorgan'
