# Make a plot (possibly multi-panel) that answers the question: 
# how does the relationship between mean covered charges (Average.Covered.Charges) 
# and mean total payments (Average.Total.Payments) vary by medical condition (DRG.Definition) 
# and the state in which care was received (Provider.State)?

payments <- read.csv(file="payments.csv",header=TRUE, sep=",")

# Simplify the DRG definitions to just numbers
levels(payments$DRG.Definition) <- c("194", "292", "392", "641", "690", "871")

uniqueStates <- unique(payments$Provider.State)
uniqueConditions <- unique(payments$DRG.Definition)

pdf("plot2.pdf")

par(mfrow = c(6,6), oma = c(4,4,4,2), mar = rep(2,4))

# Loop through
for (state in uniqueStates) {
    for (condition in uniqueConditions){
        with(subset(payments, Provider.State == state & DRG.Definition == condition),
             plot(Average.Covered.Charges, Average.Total.Payments, 
                  mtext(condition),
                  ylim = range(payments$Average.Total.Payments),
                  xlim = range(payments$Average.Covered.Charges),
                  col = adjustcolor("blue", alpha = 0.3), pch = 16)
        )
        abline(
            lm(Average.Total.Payments~Average.Covered.Charges,
                  subset(payments, Provider.State == state & DRG.Definition == condition)), 
            col = "red")
    }
    mtext(state, side=4, line=2)
}

mtext("Relationship between Covered Charges and Total Payments\n by Medical Condition and State", outer = TRUE)
mtext("Covered Charges ($)", side = 1, outer = TRUE, line=1)
mtext("Total Payments ($)", side = 2, outer = TRUE, line=1)

dev.off()