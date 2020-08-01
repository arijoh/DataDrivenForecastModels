





# 2017/29/10 02:00 X

# 2018/25/03 01:00 X 01:50
# 2018/28/10 02:00 X

# 2019/31/03 01:00 X 01:50
# 2019/27/10 02:00 X

x <- matrix(ncol = 2, nrow = 4)
colnames(x) <- c("Wintershift", "Summershift")
rownames(x) <-c(2017, 2018, 2019, 2020)

x[1,1] <- as.character("2017/10/29 02:00")
x[2,2] <- as.character("2018/03/25 01:50")
x[2,1] <- as.character("2018/10/28 02:00")
x[3,2] <- as.character("2019/03/31 01:50")
x[3,1] <- as.character("2019/10/27 02:00")
x[4,2] <- as.character("2020/03/29 01:50")
x[4,1] <- as.character("2020/10/25 02:00")
x
write.table(x, file = "../Data/Data handling/DaylightSavingDates.txt", append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE)
