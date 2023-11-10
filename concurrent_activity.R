library(tidyverse)

# data with some start and end dates
df <- data.frame(
  stringsAsFactors = FALSE,
  Ward = c("B","B","A","B","A","B",
           "A","B","A","B","A","B","A","B","A","B","B","B",
           "A","B","A","B","A","B","A","B","A","B","A"),
  Start = c("2023-02-01","2023-04-02",
            "2023-04-03","2023-04-04","2023-04-05","2023-04-06",
            "2023-04-07","2023-04-08","2023-04-09","2023-04-05",
            "2023-04-06","2023-04-07","2023-04-02","2023-04-03",
            "2023-04-04","2023-04-05","2023-04-06","2023-04-07",
            "2023-04-08","2023-04-09","2023-04-06","2023-04-07",
            "2023-04-08","2023-04-09","2023-04-05","2023-04-06",
            "2023-04-07","2023-04-02","2023-04-03"),
  End = c("2023-04-06","2023-04-03",
          "2023-04-06","2023-04-13","2023-05-11","2023-04-09",
          "2023-04-15","2023-04-18","2023-08-13","2023-04-08",
          "2023-04-12","2023-04-12","2023-04-05","2023-04-07",
          "2023-04-13","2023-04-07","2023-05-13","2023-04-15",
          "2023-04-15","2023-04-18","2023-07-13","2023-04-15",
          "2023-04-09","2023-04-09","2023-05-09","2023-04-14",
          "2023-04-11","2023-04-04","2023-04-03")
)

# just make me dates dates
df <- df |> 
  mutate(Start = as.POSIXct(Start),
         End = as.POSIXct(End))

# find start and end dates
start_min <- min(df$Start, na.rm=T)
end_max <- max(df$End, na.rm=T)

# make a sequence of dates between start end end dates
tinterval <- seq.POSIXt(start_min, end_max, by = "days")

# sapply in line function to count when date is between start and end dates
concurrent_activity <- sapply(tinterval, function(tt) sum(df$Start <= tt & tt <= df$End))

# combine the time sequence and activity back into dataframe
daily_results <- data.frame(tinterval, concurrent_activity)

# monthly checks for activity at first day of month
tinterval <- seq.POSIXt(start_min, end_max, by = "months")

# sapply in line function to count when date is between start and end dates
concurrent_activity <- sapply(tinterval, function(tt) sum(df$Start <= tt & tt <= df$End))

# join the results togther
start_month_results<- data.frame(tinterval, concurrent_activity)

# same again but with end of month
tinterval <- ceiling_date(tinterval, "month") - days(1)

# sapply in line function to count when date is between start and end dates
concurrent_activity <- sapply(tinterval, function(tt) sum(df$Start <= tt & tt <= df$End))

# join the results togther
end_month_results<- data.frame(tinterval, concurrent_activity)

# got it so far
# however didn't manage to get it to work by groups
# could do it in a horrible loopy filter, but must be a better way
# I did fudge it with a cross join, but this would get messy on a big dataset 

# add an id to the 'patients'
df <- df |>
  mutate(id = row_number())

# create a dataframe with the month ends
month_ends <- data.frame(tinterval)

# cross join joins all the dates to every line
# then check if end month between start and end dates
# then filter to end dates between start and end
df_a <- df |>
  cross_join(month_ends) |>
  mutate(month_end_flag = if_else(between(tinterval, Start, End),1,0)) |>
  filter(month_end_flag == 1)

# summary by month and ward
df_sum <- df_a |>
  group_by(tinterval, Ward) |>
  summarise(num_at_month_end = n())
