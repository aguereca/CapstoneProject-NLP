# Set working directory
#setwd("~/coursera/CapstoneProject-NLP/code/raw_code")

# Coursera Capstone Dataset
urlCapstoneDataset = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
localCapstoneDataset = '../../data/coursera_swiftkey.zip'

# Local paths
sourcesMedia = c('blogs', 'news', 'twitter')
dataDir = '../../data'
finalDataDir = paste(dataDir, 'final', sep='/')
filePath <- function(name, locale='en_US') {
    if (name != '') {
        name = paste(locale, name, sep='.')
    }
    return(paste(finalDataDir, locale, name, sep='/'))
}
