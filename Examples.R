
# ============================================================
# measure run time - 5 ways
# https://www.r-bloggers.com/5-ways-to-measure-running-time-of-r-code/

start_time <- Sys.time()
sleep_for_a_minute()
end_time <- Sys.time()

end_time - start_time

# ============================================================
# generate a unique id
x <- iris %>%
  mutate(Record_Id = 1:n()) %>%
  group_by(Species) %>%
  mutate(Group_Id = 1:n())

# ============================================================
# collapse a field across records
Cases.Brand <- Cases.Orig.Brand %>%
  select(ID, Brand=Text_Brand) %>%
  group_by(ID) %>%
  summarise(Brands = paste(Brand, collapse=" / "))

# ============================================================
# split text
files.matched <- files.matched %>%
  mutate(NUMBER = substring(filename,1, regexpr("_",filename) -1)) %>%
  mutate(NUMBER = gsub("-","/", NUMBER))

# ============================================================
# dcast example
library(reshape2)

# Example 1 ============================================
x <- data.frame(a=c(1,2,2,3,3,3,3,3,3),
                b=c(3,3,4,1,3,4,1,3,4),
                c=c(1,1,1,1,1,1,1,1,1))

dcast(x, a ~ b, value.var = "c", fun.aggregate = sum, margins = TRUE)
#       a 1 3 4 (all)
# 1     1 0 1 0     1
# 2     2 0 1 1     2
# 3     3 2 2 2     6
# 4 (all) 2 4 3     9

# Example 2 ============================================
y <- data.frame(a=c(1,2,2,3,3,3,3,3,3),
                b=c(3,3,4,1,3,4,1,3,4))

dcast(y, a ~ b, margins = TRUE)
#       a 1 3 4 (all)
# 1     1 0 1 0     1
# 2     2 0 1 1     2
# 3     3 2 2 2     6
# 4 (all) 2 4 3     9

# ============================================================
# use a regex expression to remove test variables
rm(list=ls(pattern='^test'))

# ============================================================
# use data.table to look forward and backward in a dataset

library(data.table)
library(dplyr)

data.DT <- data.table(A=1:5, B=1:5*10, C=1:5*100)

# D is C plus the previous row of B
data.DTX <- data.DT[ , D := C + shift(B, 1L, type="lag")]

# E is C plus the next row of B
data.DTX <- data.DT[ , E := C + shift(B, 1L, type="lead")]

glimpse(data.DT)
glimpse(data.DTX)

# ============================================================
# dplyr examples

library(dplyr)

#   data.days.year <- File.In %>% select(DAY_OF_YEAR, YEAR) %>%
#                            distinct(DAY_OF_YEAR, YEAR) %>%
#                            arrange(DAY_OF_YEAR)

# recode variables with mutate
iris.1 <- iris
iris.2 <- iris %>% mutate(Petal.Length=replace(Petal.Length, 
                                               Species=="setosa", 
                                               1))

iris.3 <- iris %>% mutate(Petal.Length=replace(Petal.Length, 
                                               Species %in% c("setosa"), 
                                               2))

iris.3 <- mutate(iris, Petal.Length = ifelse(Species=="setosa", 3, Petal.Length))

# rename a variable
# Note that the syntax is rename(data, New = Old)
# Note if you have plyr loaded then you need to fully qualify or you get an error
data.Candidates <- dplyr::rename(data.Candidates, Candidate.Number = Candidate.number)

# change all factors to character
data.Candidates <- data.Candidates %>% mutate_if(is.factor, as.character)

# change variables starting with "Section" from chr to numeric
# the dot is a placeholder for the variable name
marks <- marks %>% mutate_each(funs(as.numeric(.)), starts_with("Section"))     
marks <- marks %>% mutate_each(funs(as.numeric(.)), starts_with("Total"))     

# summaries
summary <- df %>% summarise(COUNT = n(), 
          MEAN_TOTAL_BEFORE = mean(Total), 
          COUNT_UPLIFTED = sum(Uplifted=="YES"),
          COUNT_REGRADED = sum(Regraded=="YES"))

# use grepl to filter - in this case installed libraries starting with g
libraries.g <- as.data.frame(installed.packages()) %>% filter(grepl("^g",Package))

# select everything else
dplyr::select(COLA, COLZ, everything())

# build a crosstab using dplyr and reshape2
crosstab.changes <- data.changes %>%
  dplyr::group_by(PERIOD) %>%
  dplyr::count(PERIOD, Change_Type) %>%
  dcast(PERIOD ~ Change_Type, value.var="n", fill=0) %>%
  mutate(TOTAL_CHANGES = rowSums(.[-1])) %>%
  mutate(NET_IN = rowSums(.[c("JOINED","REINSTATED")])) %>%
  mutate(NET_OUT = rowSums(.[c("DECEASED","RESIGNED")])) %>%
  mutate(NET_CHANGE = NET_IN - NET_OUT) %>%
  select(PERIOD, TOTAL_CHANGES, 
         NET_CHANGE, NET_IN, NET_OUT, 
         JOINED, REINSTATED, DECEASED, RESIGNED, everything())

crosstab.totals <- crosstab.changes %>%
  dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  dplyr::mutate(PERIOD="TOTAL") %>%
  dplyr::select(PERIOD, TOTAL_CHANGES, 
       NET_CHANGE, NET_IN, NET_OUT, 
       JOINED, REINSTATED, DECEASED, RESIGNED, everything()) %>%
  rbind(crosstab.changes)

# join by different key names, keeping all columns.NB all=TRUE doesn't seem to work consistently ??
full_join(Disk_Cases, Sql_Cases, by = c("file" = "PDF_Name"), all = TRUE) 

# select subset of variables and change to lowercase
data.boruta <- data.merged %>%
  select(HasPlan, V1:v8) %>%
  setNames(tolower(names(.)))

# ============================================================
# filter a dataframe by contents of another dataframe column

x <- data.frame(Tag=c("a","b","c"), Dat=c(1,2,3))
y <- data.frame(Tag=c("a","d","c"), Dat=c(1,2,3))

x1 <- filter(x, Tag %in% y$Tag)

# ============================================================
# RODBC example

library(RODBC)

SqlServer <- "P37\\SQLEXPRESS"
SqlDatabase <- 'TrendData_2011'

SqlConnection <- paste('driver={SQL Server};server=',
                       SqlServer,
                       ';database=',
                       SqlDatabase,
                       ';trusted_connection=true;rows_at_time=1',
                       sep="")

dbhandle <- odbcDriverConnect(SqlConnection)
sqlUpdate <- "select * from tbl_Annual_2015"
myData <- sqlQuery(dbhandle, sqlUpdate)
close(dbhandle)

# ============================================================
# set nas to 0 to make them easier to detect

data.in[is.na(data.in)] <- 0

# ============================================================
# convert a key/value type list into a dataframe

test <- list("1"="Box 1 New.xlsx",
             "2"="Box 2.xlsx",
             "3"="Box 3.xls",
             "7.5"="Box 7.5.xlsx")

test.df <- data.frame(Box.Number=names(test), SourceFile=unlist(test))

# manipulating lists ==========================================
# make 3 lists of lists
data1 <- list(dd1 = rnorm(100),dd2 = rnorm(100),dd3 = rnorm(100))
data2 <- list(dd1 = rnorm(100),dd2 = rnorm(100),dd3 = rnorm(100))
data3 <- list(dd1 = rnorm(100),dd2 = rnorm(100),dd3 = rnorm(100))

# make a list of lists of lists
data <- list(d1 = data1, d1 = data2, d3 = data3)

# get means
data.means.1 <- lapply(data, sapply, mean)
data.means.2 <- lapply(data, lapply, mean)
data.means.3 <- rapply(data, mean, how='list')

# get names
res <- vector('list', length(data))
for(i in seq_along(data)){
  #print(paste("names data[i]",names(data[i])))

  for(j in seq_along(data[[i]])){
    print(paste("names data[i][j]",names(data[i]),names(data[[i]][j])))
  }
}

names.1 <- lapply(data, sapply, names)
names.2 <- rapply(data, names, how='list')
names.3 <- lapply(data, lapply, names)

names(data)
# ============================================================
# create a dataframe with a single record
marks.FCIM9999 <- data.frame(Candidate.Number="FCIM9999",
                            Section1.Q1=0,Section1.Q2=82,Section1.Q3=0,Section1.Q4=0,
                            Total.Section.1=0.7025,
                            Total=72,
                            Grade="Pass with credit",
                            SourceFile="Box Blank New.xlsx")

# ============================================================
# dates

data.changes$DATE <- as.POSIXct(strptime(paste(data.changes$YEAR.x, 
                                               data.changes$DAY_OF_YEAR.x), format="%Y %j"))

data.changes$MONTH <- as.character(format(as.Date(data.changes$DATE), "%m"))

# ============================================================
# Excel
library(xlsx)

# read from an Excel sheet
data.Candidates <- read.xlsx(paste(WorkDir, File.Candidates, sep=""), 
                             sheetIndex = 2)

# write to Excel
write.xlsx(marks.check, 
           file = paste(WorkDir,"OutFile.xlsx",sep=""),
           sheetName = "Check", 
           row.names = FALSE)

write.xlsx(marks.stats,
           file = paste(WorkDir,"OutFile.xlsx",sep=""),
           sheetName="Stats",
           append=TRUE)

see - https://www.r-bloggers.com/write-data-frame-to-excel-file-using-r-package-xlsx/

# ============================================================
# Assertr for variable checking
https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html

# ============================================================
# convert a list to a df preserving the list item names

library(dplyr)

# make a test list
xx <- list("a"="fred", "b"="jim", "c"="joe")

# get the names as a df
x1 <- data.frame(Tag=names(xx))
# get the entries as a df by row
x2 <- data.frame(Names=matrix(xx, nrow=length(xx), byrow=T))
# bind them together
x3 <- bind_cols(x1,x2)
# or do it all in one statement
x4 <- data.frame(Tag=names(xx),
                 Names=matrix(xx, nrow=length(xx), byrow=T))

# ============================================================
# caret and tidyverse linear model 
test.x <- c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187)
test.y <- c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92) 
test.data <- data.frame(x=test.x, y=test.y)

# train linear model
test.modl <- train(y ~ x, 
                   data = test.data, 
                   method = "lm")
# intercept and slope
test.coef <- test.modl$finalModel$coefficients

# viz
ggplot(test.data, aes(x,y)) + 
  geom_point() +
  geom_abline(intercept = test.coef[1], slope = test.coef[2])
