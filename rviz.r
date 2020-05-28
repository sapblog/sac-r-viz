library(ggplot2)

# SAC reformat dates - SAC sends dates as strings looking 
# like: Jan 1, 2019 01:01:01 PM - we need real R dates.

dataSet$Time <- as.POSIXct(dataSet$Time, tz="UTC", format="%b %d, %Y %I:%M:%S %p")

# Ensure the order of the readings - SAC does not necessarily
# provide the data sorted.

dataSet <- dataSet[order(dataSet$Time),]

# Some useful constants.

secondsPerHour = 60 * 60
hours3 <- 3 * secondsPerHour
hours5 <- 5 * secondsPerHour
hours14 <- 14 * secondsPerHour
hours19 <- 19 * secondsPerHour
hours24 <- 24 * secondsPerHour

# Build the plot area - nothing is plotted in this call - just setting up
# the canvas where we will add layers.

p <- ggplot(data=dataSet, aes(x=Time, y=Usage))

# Add the x and y axis layouts.

# For the y-axis, we use the actual data values or 0 - 10 Kw.  Make
# displayed label show the unit of measure.

p <- p + scale_y_continuous(name="", 
                            breaks=1:10,
                            limits=c(0, 10),
                            labels=scales::unit_format(unit = "Kw")) 

# For the x-axis, we use the a datetime axis with easily
# identified lables.  The $Time data set when the plot
# was created supplies the values.

p <- p + scale_x_datetime(name = "",
                          date_breaks = "days",
                          date_labels = "%b %d")

# WE ASSUME all 24 hours are present on each day - add shading rectangles
# for the overnight areas.  We always do 8 days so we just need the starting
# date from the data to drive the entire axis.

startDate <- dataSet$Time[1]
endDate <- dataSet$Time[length(dataSet$Time)]
days <- as.numeric(endDate - startDate)

for (day in 1:days) {
  # Since we have a datetime based axis, we must address what we
  # want to plot based on timestamps.  Each morningBegin value
  # is the start of that day.
  
  morningBegin = startDate + ((day - 1) * hours24)
  morningEnd = morningBegin + hours5

  nightBegin = morningBegin + hours19
  nightEnd = nightBegin + hours5
  
  # Create the rotated "night" label.
  
  nightLabel <- annotate(geom="text", 
                         x=morningBegin + hours3, 
                         y = 9, 
                         label="night", 
                         size=4, 
                         color="gray58", 
                         angle=-90)

  # Add the morning sading to the axis.
  
  p <- p + annotate("rect", xmin=morningBegin, xmax=morningEnd, ymin=0, ymax=10, alpha=.1)

  # Put the night label on top of the morning rectangle.  The order
  # of the layers is important for the final result to look correct.

  p <- p + nightLabel

  # Add the evening to midnight shading.
  
  p <- p + annotate("rect", xmin=nightBegin, xmax=nightEnd, ymin=0, ymax=10, alpha=.1)
}

# The hard part from the original graph was to color code what
# is considered peak demand time.  Using the timestamps, split
# each day into three parts: midnight - 3:00pm (non-peak),
# 3:00pm - 8:00pm (peak) and 8:00pm - midnight (non-peak).

# The non-peak and peak sections are given different colors.  This is done
# using geom_ribbon objects where the ribbon is filled-in from the line down
# to the x-axis.  Ususally ribbons are +/- shading following a line.  This
# "trick" gives the illusion of shading under the line.

# Making the max-Y value of the ribbon the actual value and the min-Y
# *always* 0 fills-in the area under the line.  For each region, we
# subset the time period of data for just that section, think building
# blocks of color, that are positioned on the chart one by one.  These
# subsets have nothing to do with the actual line of the graph - we are
# NOT filling under the overall line - we are creating, coloring and adding
# additional elements that just happen to look like they are shading under
# the line.

for (day in 1:days) {
  firstRead = startDate + ((day - 1) * hours24)
  lastRead = firstRead + hours24

  dayOfWeek = format(firstRead, "%w")
  
  if (dayOfWeek == "0" | dayOfWeek == "6") {
    # Do not highlight peak on the weekends - entire day has the same shading.
    
    p <- p + geom_ribbon(data=subset(dataSet, Time>=firstRead & Time<=lastRead),
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.3)
  } else {
    # Divide the day into the three pieces and add them to the plot.
    
    offEnd  = firstRead + hours14
    peakEnd = offEnd + hours5
    
    # First section is midnight to 3 pm.
    
    p <- p + geom_ribbon(data=subset(dataSet, Time>=firstRead & Time<=offEnd),  # Early non-peak
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.3)

    # The peak section is 3 pm to 8 pm.
    
    p <- p + geom_ribbon(data=subset(dataSet, Time>=offEnd & Time<=peakEnd),   # Peak
                         aes(ymax=Usage),
                         ymin=0,
                         fill="springgreen3",
                         colour=NA,
                         alpha=.6)

    # Finally, the remainder of the day after peak.
    
    p <- p + geom_ribbon(data=subset(dataSet, Time>=peakEnd & Time<=lastRead), # Late non-peak
                         aes(ymax=Usage),
                         ymin=0, 
                         fill="skyblue3",
                         colour=NA,
                         alpha=.3)
  }
}

# Add the actual line graph to the canvas last to ensure
# it is on the top of all the other layers.

p <- p + geom_line(color="steelblue", size=.7)

# Render the plot the graph.
p
