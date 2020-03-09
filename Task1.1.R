#reads the reddit file
reddit <- read.csv(file="C:/.../Reddit.csv")

#delete first column
reddit <- reddit[,-1]

#looks into columns that have only Na, and display them as a list
reddit[0, colSums(is.na(reddit)) == nrow(reddit)]
#remove columns with no data
reddit <- reddit[, colSums(is.na(reddit)) != nrow(reddit)]

#looks into columns that have no variance and display them as a list
reddit[0,c(TRUE, lapply(reddit[-1], var, na.rm = TRUE) == 0)]
#remove columns with only 1 factor
reddit <- reddit[c(TRUE, lapply(reddit[-1], var, na.rm = TRUE) != 0)]

#Library to manage dates 
install.packages("lubridate")
library(lubridate)

#transform seconds to standard date format using the default Unix epoch of "1970-01-01"
reddit$created_utc <- as_datetime(reddit$created_utc)
reddit$retrieved_on <- as_datetime(reddit$retrieved_on)

#transform date into weekdays
reddit$created_utc <- wday(reddit$created_utc, label = TRUE)
reddit$retrieved_on <- wday(reddit$retrieved_on, label = TRUE)

#make weekdays unordered
reddit$created_utc <- factor(reddit$created_utc, ordered = FALSE)
reddit$retrieved_on <- factor(reddit$retrieved_on, ordered = FALSE)

#Display tables
table(reddit$created_utc)
table(reddit$retrieved_on)
prop.table(table(reddit$created_utc))
prop.table(table(reddit$retrieved_on))

#create a new reddit document
write.csv(reddit, file="C:/.../Reddit2.csv")

