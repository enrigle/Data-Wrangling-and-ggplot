#oil petrol consumption
list.files()
# oil <- read.csv("Oil Production per capita.csv", sep = ";", stringsAsFactors = FALSE)
oil <- read.csv("Oil Production per capita.csv", sep = ";", header = TRUE, 
                stringsAsFactors=FALSE)

#change column names
head(oil)
str(oil)
for (i in 2:dim(oil)[2]) {
  names(oil)[i] <- substr(names(oil)[i], 2,5)
}
names(oil)[1] <- "Country"

#change char to numeric format
dim(oil)
oil[4,2:48]
for (i in 2:dim(oil)[1]) { #column: 2:48
  for (j in 2:dim(oil)[2]) { #rows: 2:275
    oil[i, j] <- gsub(",", ".", oil[i, j])
    oil[i, j] <- as.numeric(as.character(oil[i, j]))
  }
}

for (i in 2:dim(oil)[1]) { #column: 2:48
  for (j in 2:dim(oil)[2]) { #rows: 2:275
    oil2[i, j] <- as.numeric(oil[i, j])
    as.numeric(oil2[i, j])
  }
}

for (i in 1:dim(oil)[2]) {
  as.numeric(oil[i,2:48])  
}
~ Country
names(oil)
a <- gather(oil, "year", "consumer_oil", 2:48)
oil[5, 1:48]
a$consumer_oil <- as.numeric(a$consumer_oil)
str(a)
oilC <- subset(a, !is.na(a$consumer_oil))

#consumer oil by country
ggplot(aes(consumer_oil, fill = Country), data = oilC) + geom_histogram() 

#consumer oil by country
ggplot(aes(consumer_oil), data = oilC) + geom_histogram() +
  facet_wrap(~ Country)

#consumer oil by year
ggplot(aes(consumer_oil), data = oilC) + geom_histogram() +
  facet_wrap(~ year)




######################################
###         DIAMONDS DATA          ###
######################################

# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg
data("diamonds")
names(diamonds)
ggplot(aes(price, fill = cut), data = diamonds) + geom_histogram() + 
  facet_wrap(~ color ) + scale_x_log10()

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')
ggplot(aes(table, price), data = diamonds ) + 
  geom_point(aes(colour = cut)) + xlim(50, 80) +
  scale_color_brewer(type = 'qual')

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

diamonds$volume <- diamonds$x + diamonds$y + diamonds$z

ggplot(aes(x = volume, y = price), 
       data = subset(diamonds, diamonds$volume < quantile(diamonds$volume,.99))) +
  geom_point(aes(colour = clarity)) +
  # xlim(0, 350) + 
  scale_y_log10() + scale_color_brewer(type = 'div') +
  scale_x_discrete(limits = c(0, 350), breaks = seq(0, 350, 50))

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.
list.files()
fb  <- read.delim("pseudo_facebook.tsv")
names(fb)

fb$prop_initiated <- fb$friendships_initiated/fb$friend_count
str(fb)

# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

fb$year_joined <- floor(2014 - fb$tenure/365)
fb$year_joined.bucket <- cut(fb$year_joined, breaks = c(2004,2009,2011,2012,2014))

ggplot(aes(tenure, prop_initiated), data = fb) +
  geom_line(aes(colour = year_joined.bucket), stat = 'summary', fun.y = median)

# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

# There won't be a solution image for this exercise.
# You will answer some questions about your plot in
# the next two exercises.
ggplot(aes(tenure, prop_initiated), data = fb) +
  # geom_line(aes(colour = year_joined.bucket), stat = 'summary', fun.y = median) +
  geom_smooth(aes(colour = year_joined.bucket))

tapply(fb$prop_initiated, fb$year_joined.bucket, summary)

# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

str(diamonds)
ggplot(aes(cut, price/carat), data = diamonds) +
  geom_point(aes(colour = color)) +
  facet_wrap( ~ clarity) +
  scale_color_brewer(type = 'div')
