title <- "Midterm Project"
name <- "Sutong Zhang"
date <- "11/07/2022"

library(tidyverse)
library(magrittr)
library(readxl)

#Clean Data
## Start by reading the data
strawb <- read_xlsx("~/Downloads/strawberries-2022oct30-a.xlsx", col_names = TRUE)

## Get the column names and index them
cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

## Explore data by viewing it in R.  
## Double click the strawb data frame to lauch the view() function.
## The data frame has 1008 rows, so you can't get very far by
## simply scrolling around.  But, you can generate some initial
## questions to help you explore using R functions from the
## tidyverse.  
##
## It looks like some of the columns may be blank or may contain 
## a single unique value.  These columns can be eliminated without 
## losing any information.

## Start by examining the content of the columns

## Column 1 contains two unique values.  
## Retain column 1 -- those values might be needed.
unique(strawb[1])

## Column 2 -- contains the years included in this dataset.
## Keep column 2, of course.
unique(strawb[2])

## Column 3 -- contains the time periods covered by in the dataset.
## There's only one -- years.  No info here.  Drop it
unique(strawb[3])

## you don't have to do this one column at a time.
## Note that the cells of columns that are empty contain NA, so
## the number of unique values in these columns is 1, just 
## like column_3.

## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}

## Use T to select columns to drop -- 
drop_cols <- cnames[which(T == 1)]

## Now, drop the columns with only one unique value.
strawb %<>% select(!all_of(drop_cols))

## Let's arrange the data frame by year and state.
strawb %<>% arrange(Year, State)


## Look at the strawb data frame again. You can see that the 
## columns need work. The State ANSI column contains a unique
## code for each state. If you need to access US Census data for
## the states, this code will come in handy.

colnames(strawb)

## now look at the `Data Item` column

temp1 <- strawb %>% select(`Data Item`) %>% 
         distinct()

## Look at temp1!  There's a lot going on there.
## In fact, it's at least three columns packed into one.
## Use separate() to split it up

## When you run this code you can see that there are 
## some rows where `Data Item` has 4 comma-separated 
## data items.  Look at the warning on the Console 
## after 

strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")

## try 4 columns

strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")

## That worked. Clean up the dat.

rm(strawb2, strawb3)

strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")


## now explore the new columns

## we know that "THIRAM" is a chemical in the data, so
## test for it to check out the way code
r_thiram <- grep("THIRAM", strawb$`Domain Category`)
r_thiram_1 <- grep("Thiram", 
                   strawb$`Domain Category`, 
                   ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                       strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                      strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                          strawb$`Domain Category`, 
                          ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                        strawb$`Domain Category`, 
                        ignore.case = T)

temp1 <- strawb %>% select(Strawberries) %>% 
  distinct()

pr_rec <- grep("STRAWBERRIES - PRICE RECEIVED", 
               strawb$Strawberries, 
               ignore.case = T)
#Organic and Non-organic
type_organic <- grep("organic", 
                     strawb$type, 
                     ignore.case = T)

items_organic <- grep("organic", 
                      strawb$items, 
                      ignore.case = T)

Domain_organic <- grep("organic", 
                       strawb$Domain, 
                       ignore.case = T)


Domain_Category_organic <- grep("organic", 
                                strawb$`Domain Category`, 
                                ignore.case = T)
same <- (intersect(type_organic, Domain_organic)==
           intersect(type_organic, Domain_organic))
length(same)==length(type_organic)

org_rows <- intersect(type_organic, Domain_organic)

strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)

temp1 <- strawb_non_organic %>% select(type) %>% 
  distinct()

chem_rows <- grep("BEARING - APPLICATIONS", 
                  strawb_non_organic$type, 
                  ignore.case = T)
chem_rows_1 <- grep("chemical", 
                   strawb_non_organic$Domain, 
                   ignore.case = T)
ins <- intersect(chem_rows, chem_rows_1)
chem_rows_2 <- grep("chemical", 
                    strawb_non_organic$`Domain Category`, 
                    ignore.case = T)
ins_2 <- intersect(chem_rows, chem_rows_2)
strawb_chem <- strawb_non_organic %>% slice(chem_rows, preserve = FALSE)
rm(x, T, drop_cols, temp1, r_thiram, r_thiram_1,
   df_carbendazim, df_Bifenthrin, df_methyl_bromide, 
   df_1_3_dichloropropene, df_chloropicrin, df_Telone,
   pr_rec, type_organic, items_organic, Domain_organic,
   Domain_Category_organic, same, org_rows, chem_rows,
   chem_rows_1, chem_rows_2, ins, ins_2, cnames, i)
before_cols = colnames(strawb_chem)
T = NULL
x = length(before_cols)

for(i in 1:x){
  b <- length(unlist(strawb_chem[,i] %>% unique()) )
  T <- c(T,b)
}

drop_cols <- before_cols[which(T == 1)]
strawb_chem %<>% select(!all_of(drop_cols))
after_cols = colnames(strawb_chem)


temp1 <- strawb_chem %>% select(units) %>% distinct()
strawb_chem %<>% separate(col=`Domain Category`, 
                          into = c("dc1", "chem_name"),
                          sep = ":", 
                          fill = "right")
temp1 <- strawb_chem %>% select(chem_name) %>% unique()
length(unlist(temp1))

aa  <- grep("measured in", 
            strawb_chem$items, 
            ignore.case = T)
length(aa)
sum(strawb_chem$Domain == strawb_chem$dc1) == dim(strawb_chem)[1]
strawb_chem %<>% select(Year, State, items, units, dc1, chem_name, Value)
strawb_chem %<>% rename(category = units)
strawb_chem$items <- str_remove_all(strawb_chem$items, "MEASURED IN ")
strawb_chem %<>% rename(units = items)
bb  <- grep("CHEMICAL, ", 
            strawb_chem$dc1, 
            ignore.case = T)
length(bb)
chem <- 1:2112

non_chem_rows <- setdiff(chem, bb)
length(non_chem_rows)

temp1 <- strawb_chem %>% slice(non_chem_rows)
fertilizers <- temp1

rm(temp1, temps, temp3, aa, bb)

strawb_chem$dc1 <- str_remove_all(strawb_chem$dc1, "CHEMICAL, ")

strawb_chem$dc1 %>% unique()

strawb_chem %<>% rename(chem_types = dc1)

bb  <- grep("BIFENTHRIN", 
            strawb_chem$chem_name, 
            ignore.case = T)

bifen <- strawb_chem %>% slice(bb)

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\(")

strawb_chem$chem_name <- str_remove_all(strawb_chem$chem_name, "\\)")

strawb_chem %<>% separate(col = chem_name,
                          into = c("chem_name","chem_code"),
                          sep = "=",
                          fill = "right"
) 

aa <- which(strawb_chem$units == " LB")

bb <- which(is.na(strawb_chem$category))

sum(aa==bb)==length(aa)                       


#Last
caosale <- strawb_organic %>% group_by(State) %>% filter(Year == "2016", State == "CALIFORNIA",str_detect(type,"SALES"))

mean <- as.numeric(caosale$Value[1])

mean - 1.96*mean*0.137
mean + 1.96*mean*0.137

length(unique(strawb_chem$chem_name))

ca <- filter(strawb_chem,Year == "2016", State == "CALIFORNIA")


flo <- filter(strawb_chem,Year == "2016", State == "FLORIDA")

length(unique(ca$chem_name)) - length(unique(flo$chem_name))
