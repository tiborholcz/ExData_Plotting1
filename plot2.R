prepare_dataset<-function(){
# Getting data from the Internet if necessary

if((!file.exists("exdata-data-household_power_consumption.zip")) & (!file.exists("household_power_consumption.txt")))
  {
  # download file
  print("Downloading data file...")
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile="exdata-data-household_power_consumption.zip")
  }

if(!file.exists("household_power_consumption.txt"))
  {
  # extract zip file
  print("Extracing zip file...")
  unzip("exdata-data-household_power_consumption.zip")
  }

print("Reading data from txt file...")
data_full<-read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")

print("Subsetting data set...")
data<-subset(data_full, as.Date(Date, format="%d/%m/%Y")==as.Date("01/02/2007", format="%d/%m/%Y") | as.Date(Date, format="%d/%m/%Y")==as.Date("02/02/2007", format="%d/%m/%Y"))
data<-cbind(data, data.frame(FullTime=with(data, strptime(paste(as.character(Date), as.character(Time)), format="%d/%m/%Y %H:%M:%S"))))

return(data)
}

# Create graph 1

create_graph_1<-function(data){
png("plot1.png", width=480, height=480)
hist(data$Global_active_power, col="Red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()
if(file.exists("plot1.png")){
  print("File plot1.png was created.")
} else{
  print("File plot1.png was not created!")
}
}

# Create graph 2

create_graph_2<-function(data){
lc<-Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
png("plot2.png", width=480, height=480)
plot(data$FullTime, data$Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab=NA)
dev.off()
Sys.setlocale("LC_TIME", lc)
if(file.exists("plot2.png")){
  print("File plot2.png was created.")
} else{
  print("File plot2.png was not created!")
}
}

# Create graph 3

create_graph_3<-function(data){
lc<-Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
png("plot3.png", width=480, height=480)
plot(data$FullTime, data$Sub_metering_1, type="l", col="black", ylab="Energy sub metering", xlab=NA)
points(data$FullTime, data$Sub_metering_2, type="l", col="red")
points(data$FullTime, data$Sub_metering_3, type="l", col="blue")
legend("topright", lty=1, col=c("black", "red", "blue"), legend=names(data)[7:9])
dev.off()
Sys.setlocale("LC_TIME", lc)
if(file.exists("plot3.png")){
  print("File plot3.png was created.")
} else{
  print("File plot3.png was not created!")
}
}

# Create graph 4

create_graph_4<-function(data){
lc<-Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))
# Top left graph
plot(data$FullTime, data$Global_active_power, type="l", ylab="Global Active Power", xlab=NA)
# Top right graph
plot(data$FullTime, data$Voltage, type="l", col="black", ylab="Voltage", xlab="datetime")
# Bottom left graph
plot(data$FullTime, data$Sub_metering_1, type="l", col="black", ylab="Energy sub metering", xlab=NA)
points(data$FullTime, data$Sub_metering_2, type="l", col="red")
points(data$FullTime, data$Sub_metering_3, type="l", col="blue")
legend("topright", lty=1, col=c("black", "red", "blue"), legend=names(data)[7:9], bty="n")
# Bottom right graph
plot(data$FullTime, data$Global_reactive_power, type="l", col="black", ylab="Global_reactive_power", xlab="datetime")
# Restore defaults
par(mfrow=c(1,1))
dev.off()
Sys.setlocale("LC_TIME", lc)
if(file.exists("plot4.png")){
  print("File plot4.png was created.")
} else{
  print("File plot4.png was not created!")
}
}

data<-prepare_dataset()
# create_graph_1(data)
create_graph_2(data)
# create_graph_3(data)
# create_graph_4(data)