library(ggplot2)

# SAC reformat dates - SAC sends dates as strings looking like: Jan 1, 2019 01:01:01 PM - we need
# as real R dates.

dataset_actual.xlsx$Time <- as.POSIXct(dataset_actual.xlsx$Time, tz="", format="%b %d, %Y %I:%M:%S %p")

# The Id column (from the SAC model) is delivered as a "factor" - we need it as a numeric.

# Note: use as.character first because factors have an internal identifier that as.numeric 
# returns instead of the actual value.

dataset_actual.xlsx$Id <- as.numeric(as.character(dataset_actual.xlsx$Id))

# Ensure the sort - SAC does not necessarily provide the data sorted.

dataset_actual.xlsx <- dataset_actual.xlsx[order(dataset_actual.xlsx$Id),]

# Make an easier name for the dataset - this allows me to test everything
# below regardless of data source.

dataSet <- dataset_actual.xlsx

# Build the plot area - nothing is plotted in this call - just setting up
# the canvas where we will add layers.

p <- ggplot(data=dataSet, aes(x=Id, y=Usage, label=Hour))

# Add the x and y axis layouts.

# For the y-axis, we use the actual data values, but recode the
# display to use more meaningful labels.

p <- p + scale_y_continuous(name="",
                            limits=c(0, 10),
                            breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                            labels=c("0"  = "00 kW",
                                     "1"  = "01 kW",
                                     "2"  = "02 kW",
                                    "3"  = "03 kW",
                                     "4"  = "04 kW", 
                                     "5"  = "05 kW", 
                                     "6"  = "06 kW",
                                     "7"  = "07 kW",
                                     "8"  = "08 kW", 
                                     "9"  = "09 kW",
                                     "10" = "10 kW"
                            ) 
        )

# For the x-axis, we use the Id of each 24 hour period as
# an index to recode to the date name.  In a production
# graph, this would be done programatically using the
# $Time column.

p <- p + scale_x_continuous(name = "",
                            limits=c(0, 192),
                            breaks=c(0, 25, 49, 73, 97, 121, 145, 169),
                            labels=c("0"   = "Sept 18",
                                     "25"  = "Sept 19",
                                     "49"  = "Sept 20",
                                     "73"  = "Sept 21",
                                     "97"  = "Sept 22", 
                                     "121" = "Sept 23", 
                                     "145" = "Sept 24",
                                     "169" = "Sept 25")
                            )

# WE ASSUME all 24 hours are present on each day - add shading rectangles
# for the overnight areas.

for (day in c(1, 2, 3, 4, 5, 6, 7, 8)) {
  # Again, we are using the Id column as an index to the x-axis and
  # it should be done programatically based on the actual dates.
  
  morningBegin = (day - 1) * 24 + 1
  morningEnd = morningBegin + 5
  
  nightBegin = morningBegin + 19
  nightEnd = nightBegin + 5
  
  # Create the "night" label.
  
  nightLabel <- annotate(geom="text", 
                         x=morningBegin - .2, 
                         y = 9.7, 
                         label="night", 
                         hjust=0, 
                         size=4, 
                         color="gray58", 
                         angle=-90)
  
  # Add the morning and evening rectangles - put the night label last
  # to put it on top of the rectangles.
  
  p <- p +
       annotate("rect", xmin=morningBegin, xmax=morningEnd, ymin=0, ymax=10, alpha=.08) +
       annotate("rect", xmin=nightBegin, xmax=nightEnd, ymin=0, ymax=10, alpha=.08) +
       nightLabel
}

# The hard part from the original graph was to color code what
# is considered peak demand time.  Using the Id values, split
# each day into three parts: midnight - 3:00pm (non-peak),
# 3:00pm - 8:00pm (peak) and 8:00pm - midnight (non-peak).

# The non-peak and peak sections are given different colors.  This is done
# using geom_ribbon objects where the ribbon is from the line down to
# the x-axis.  Ususally ribbons are +/- shading following a line.

# Making the max-Y value of the ribbon the actual value and the min-Y
# always 0 fills-in the area under the line.  For each region, we
# subset the data for just that section, think building blocks of
# color, that are positioned on the chart one by one.  These
# subsets have nothing to do with the actual line of the graph -
# we are NOT filling under the overall line - we are creating,
# coloring and adding additional elements that just happen to
# look like they are shading under the line.

for (day in c(1, 2, 3, 4, 5, 6, 7, 8)) {
  offBegin = (day - 1) * 24 + 1
  lastRead = offBegin + 24
  dayOfWeek = format(dataSet$Time[offBegin], "%w")
  
  if (dayOfWeek == "0" | dayOfWeek == "6") {
     # Do not highlight peak on the weekends - entire day has the same shading.
    
    p <- p + geom_ribbon(data=subset(dataSet, Id>=offBegin & Id<=lastRead),
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.8)
  } else {
    # Divide the day into the three pieces and add them to the plot.
    
    offEnd  = offBegin + 14
    peakEnd = offEnd + 5

    p <- p + geom_ribbon(data=subset(dataSet, Id>=offBegin & Id<=offEnd),  # Early non-peak
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.8) +
             geom_ribbon(data=subset(dataSet, Id>=offEnd & Id<=peakEnd),   # Peak
                         aes(ymax=Usage),
                         ymin=0,
                         fill="springgreen3",
                         colour=NA,
                         alpha=.8) +
             geom_ribbon(data=subset(dataSet, Id>=peakEnd & Id<=lastRead), # Late non-peak
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.8)
  }
}

# Add the actual line graph to the canvas last to ensure
# it is on the top of all the other layers...and render the plot.

p + geom_line(color="steelblue", size=1)

