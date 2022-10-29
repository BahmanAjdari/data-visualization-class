# Here is a review of r Basics

## Variables and vectors

a <- c(1,2,3,4)

b = c(5,6,7,NA)

c <- c("mina" , "sima" , "ziba" , "diba")

a + b

a * b

## calling elements
a[2]
b[c(2,3)]
c[c(2,1,5)]
c[c(1:3)]

## some basic functions

class(a)
class(b)
class(c)

length(a)

sum(a)
sum(b)
sum(b , na.rm = TRUE)

## data frame

df <- data.frame(a, b, c)
df

## calling cols
df['a']
df$hdl
df[4]
df['V4']

## adding new cols
df[,4] <- c("A","B","AB","O")
df$hdl <- c(100,87,67,90)
df

## Visulization

library(ggplot2)
# individual geoms

df <- data.frame(
  x = c(3, 1, 5), 
  y = c(2, 4, 6), 
  label = c("a","b","c")
)
p <- ggplot(df, aes(x, y, label = label)) + 
  labs(x = NULL, y = NULL) + # Hide axis label
  theme(plot.title = element_text(size = 12)) # Shrink plot title
p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = "identity") + ggtitle("bar")
p + geom_tile() + ggtitle("raster")

p + geom_line() + ggtitle("line")
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path")
p + geom_polygon() + ggtitle("polygon")

# Collective geoms
data(Oxboys, package = "nlme")
head(Oxboys)

## We are going to plot groups
## what if we map age to height
## incorrect
ggplot(Oxboys, aes(age, height)) + 
  geom_line()+
  geom_point()

##correct
ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line(color = "red")+
  geom_point(color = "darkblue")

#different groups on different layers
ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE)
#> `geom_smooth()` using formula 'y ~ x'

ggplot(Oxboys, aes(age, height)) + 
  geom_line(aes(group = Subject)) + 
  geom_smooth(method = "lm", size = 2, se = FALSE, color = "navy")
#> `geom_smooth()` using formula 'y ~ x'

## statistical transformation

y <- c(18, 11, 16)
df <- data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))

base <- ggplot(df, aes(x, y, ymin = y - se, ymax = y + se))
base + geom_crossbar()
base + geom_pointrange()
base + geom_smooth(stat = "identity")

### histogram
ggplot(diamonds, aes(depth)) + 
  geom_histogram()
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
ggplot(diamonds, aes(depth)) + 
  geom_histogram(binwidth = 0.1) + 
  xlim(55, 70)
#> Warning: Removed 45 rows containing non-finite values (stat_bin).
#> Warning: Removed 2 rows containing missing values (geom_bar).

#Display distribution
ggplot(diamonds, aes(depth)) + 
  geom_freqpoly(aes(colour = cut), binwidth = 0.1, na.rm = TRUE) +
  xlim(58, 68) + 
  theme(legend.position = "none")
ggplot(diamonds, aes(depth)) + 
  geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill",
                 na.rm = TRUE) +
  xlim(58, 68) + 
  theme(legend.position = "none")

ggplot(diamonds, aes(depth)) +
  geom_density(na.rm = TRUE) + 
  xlim(58, 68) + 
  theme(legend.position = "none")

ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
  geom_density(alpha = 0.2, na.rm = TRUE) + 
  xlim(58, 68) + 
  theme(legend.position = "none")

# introducing boxplot

## what is boxplot

#median
x <- c(3,2,3,2,1,4,1,NA,8)
x <- x[!is.na(x)]
x[order(x)]
median(x)

# quantiles
quantile(x)
quantile(x)[4] - quantile(x)[2]
IQR(x,)

#boxt plot
data_boxplot <- data.frame(factor = "blood" , pressure = x )

ggplot(data = data_boxplot , aes(factor , pressure))+
  geom_boxplot()

data = data.frame(
  name=c(rep("A",500), rep("B",500), rep("B",500), rep("C",500), rep('D', 100)  ),
  value=c(rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(500, 25, 4), rnorm(100, 12, 1) )
)

ggplot(data) +
  geom_boxplot(aes(name , value))

ggplot(diamonds) +
  geom_boxplot(aes(clarity, carat , fill = color))
  geom_jitter(aes(clarity , carat) , alpha = .02)


USArrests
df2 <- USArrests[order(-USArrests$Murder),]


ggplot(df2) +
  geom_point(aes(Murder , Rape))

ggplot(df2) +
  geom_point(aes(Murder , Assault))



