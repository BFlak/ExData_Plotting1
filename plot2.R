plot2 <- function()
{
    ## Read date column of data into R.Convert to vector with date class.
    ## Determine which rows to include in full dataset to read in (i.e. which rows correspond to dates between 2007-02-01 and 2007-02-02).
    ## Read in all columns for selected rows.Format Date column into 'date' class with Y-m-d instead of d/m/Y.
    ## Combine Date and Time into a single column with class POSIXlt.
    ## Assumes data is in the working directory.
    
    dcols <- c("factor","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL")
    rdates <- read.table("household_power_consumption.txt",header=TRUE,sep=";",nrow=2075259,colClasses=dcols)
    rdates <- as.Date(as.vector(rdates[,1]),"%d/%m/%Y")
    selrows <- grep("2007-02-01|2007-02-02",rdates)
    data <- read.table("household_power_consumption.txt",header=FALSE,sep=";",skip=as.numeric(selrows[1]),nrows=length(selrows))
    cnamesdf <- read.table("household_power_consumption.txt",header=FALSE,sep=";",nrows=1)
    cnames <- NULL
    for (i in 1:ncol(cnamesdf))
    {
        cnames[i]<- as.character(cnamesdf[1,i])
    }
    colnames(data)<-cnames
    data$Date <- format(as.Date(data$Date,"%d/%m/%Y"),"%Y-%m-%d")
    data$Date <- as.Date(data$Date,"%Y-%m-%d")
    data$DateTime <- strptime(paste(data$Date,data$Time,sep = " "),format="%Y-%m-%d %H:%M:%S")
    
    ##Add column determining weekday of the selected date.
    data$Weekday <- weekdays(data$DateTime)
    
    ##Plot Global Active Power versus Weekday as per assignment. Copy to png file, located in working directory.
    with(data,plot(DateTime,Global_active_power,type="n",ylab="Global Active Power (kilowatts)",xlab=""))
    with(data,lines(DateTime,Global_active_power))
    dev.copy(png,file="plot2.png",width=480,height=480,units="px")
    dev.off()
}