#To add a column to end of dataframe of given function

cbind(tavg, product = (tavg$`Temperature (C)` * tavg$`Time of Observation`))