# Make a plot that answers the question: 
# what is the relationship between mean covered charges (Average.Covered.Charges) 
# and mean total payments (Average.Total.Payments) in New York?

payments <- read.csv(file="payments.csv",header=TRUE, sep=",")

# Get just the ones in new york
paymentsNY <- subset(payments, Provider.State == "NY")

pdf("plot1.pdf")

plot(
    paymentsNY$Average.Covered.Charges,
    paymentsNY$Average.Total.Payments,
    xlab = "Average Covered Charges ($)", 
    ylab = "Average Total Payments ($)",
    main = "Relationship between Average Covered Charges\n and Average Total Payments in NY",
    col = rgb(0, 0, 1, 0.3),
    pch = 16
)
abline(
    lm(
        paymentsNY$Average.Total.Payments ~ paymentsNY$Average.Covered.Charges
        ), 
    col = "red",
    lwd = 2
)

dev.off()
