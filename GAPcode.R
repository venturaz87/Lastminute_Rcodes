library(RGA)
library(ggplot2) 
library(forecast)
library(plyr)
library(scales)

ga_token<-authorize(client.id = "633133251108-21ftopdjfaelgot5s0ftlgvesb21ang9.apps.googleusercontent.com",
                    client.secret = "T9xbQXs8zZLj1j33bzATXYSQ")
gaData <- get_ga(profileId = "ga:111928747", 
                 start.date = "2016-08-01",
                 end.date = "2016-08-31", 
                 metrics = c("ga:transactions"),
                 dimensions = c("ga:deviceCategory, ga:country"), 
                 sort = NULL, filters = NULL,
                 segment = NULL, samplingLevel = NULL, start.index = NULL,
                 max.results = NULL, include.empty.rows = NULL, fetch.by = NULL, ga_token)

test <- subset(gaData, country %in% c("France", "Germany","Italy","Spain","United Kingdom"), drop = TRUE) 
test_sorted <- arrange(test, deviceCategory, country) 
test_cumsum <- ddply(test_sorted, "transactions",
                     transform, label_ypos=cumsum(transactions))
test_sum <- aggregate(transactions ~ country, test_cumsum, sum)




ggplot(data = test_cumsum, mapping = aes(x = country, y = transactions, fill=deviceCategory)) + 
  # Only line that changes
  geom_bar(stat = "identity") + 
  geom_text(aes(y=label_ypos, label=transactions), vjust=1.6, 
            color="white", size=3.5)+
  #scale_y_continuous(labels = percent_format())+
  scale_fill_brewer(palette="Paired")+
  theme_bw() +
  ylim(0,NA) 













gaData2 <- get_ga(profileId = "ga:111928747", 
                  start.date = "2016-08-16",
                  end.date = "2016-08-26", 
                  metrics = c("ga:transactions","ga:organicSearches"),
                  #metrics = c("ga:transactions"),
                  dimensions = c("ga:date"), 
                  sort = NULL, filters = "ga:channelGrouping == DIRECT",
                  segment = NULL, samplingLevel = NULL, start.index = NULL,
                  max.results = NULL, include.empty.rows = NULL, fetch.by = NULL, ga_token)

#plot(gaData$date,gaData$sessions,type="l",ylim=c(0,max(gaData$sessions)))



# Invoke a ggplot with date vs. sessions data
ggplot(data = gaData, mapping = aes(x = date, y = sourceMedium)) + 
  # Layer in an actual line graph  
  geom_line() +
  # Layer in a theme (black and white)  
  theme_bw() +
  # Ensure the y-axis actually starts at 0 (and don't give me any grief about it!)  
  ylim(0,NA) 

# Now lets do the same thing, but with a bar chart
ggplot(data = gaData, mapping = aes(x = date, y = sessions)) + 
  # Only line that changes
  geom_bar(stat = "identity") +
  theme_bw() +
  ylim(0,NA) 

# Now let's scatter plot pageviews vs. users by changing the mapping directive
ggplot(data = gaData, mapping = aes(x = users, y = pageviews)) + 
  # Line that changes for the scatter
  geom_point() +
  theme_bw() +
  ylim(0,NA) 

# Let's hit the API again and make a new dataframe "gaDataExt" that has an additional dimension, Device Category (mobile, tablet, desktop)
gaDataExt <- get_ga(profileId = "ga:111928747", 
                    start.date = "2016-08-01",
                    end.date = "2016-08-31", 
                    metrics = c("ga:users", "ga:sessions"," ga:pageviews"), 
                    dimensions = c("ga:date","ga:deviceCategory"), 
                    sort = NULL, filters = NULL,
                    segment = NULL, samplingLevel = NULL, start.index = NULL,
                    max.results = NULL, include.empty.rows = NULL, fetch.by = NULL, ga_token)


# Invoke a ggplot with date vs. sessions data, segmented by device category
ggplot(data = gaDataExt, mapping = aes(x = date, y = sessions, color = deviceCategory) ) + 
  geom_line() +
  theme_bw() +
  ylim(0,NA) 

# Or multiple plots that are faceted by device categry.
ggplot(data = gaDataExt, mapping = aes(x = date, y = sessions) ) + 
  geom_line() +
  facet_grid(deviceCategory ~ .) +
  theme_bw() +
  ylim(0,NA) 

# Let's "Lea Pica" this plot a little more
ggplot(data = gaDataExt, mapping = aes(x = date, y = sessions)) + 
  # Only line that changes
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Sessions Peaked Early Last Week", x="Date", y="Total Site Sessions") +
  ylim(0,NA) 