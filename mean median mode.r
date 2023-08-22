mode------------------
getmode <- func􀆟on(v)
{
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- c(150,174, 138, 186, 128, 136, 171, 163, 152, 131,171,131,171)
result <- getmode(v)
print(result)
mean----------------------
height <-c(150,174, 138, 186, 128, 136, 171, 163, 152, 131)
result.mean <-mean(height)
print(result.mean)
median--------------------
height <-c(150,174, 138, 186, 128, 136, 171, 163, 152, 131)
result.median <-median(height)
print(result.median)
