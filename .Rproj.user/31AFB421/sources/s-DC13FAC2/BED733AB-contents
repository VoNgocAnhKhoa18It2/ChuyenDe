
PATH <- "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/plm/RiceFarms.csv"
df<- read.csv(PATH, sep = ",")

#Remove data NA
data <- na.omit(df)

attach(data)

#Information of dataset 
dim(data)
nrow(data)
ncol(data)

#names of the data fields
names(data)

#Data type
str(data)

#Full statistics
summary(data)

#==============================================================

attach(data)

#filter data:
#  
filter1 <- data %>% filter(size > 3 & region == "wargabinangun")
filter1

filter2 <- data %>% filter(size > 3 | price > 180 & seed > 100)
filter2

filter3 <- data %>% filter(varieties == "high" & goutput > 10000 & wage > 100)
filter3

filter4 <- data %>% filter(price > 100 & wage > 100 & seed > 100)
filter4

filter5 <- data %>% filter(region  == "langan" & size > 3.5 | region != "langan" & wage > 170 & hiredlabor < 250)
filter5

filter6 <- data %>% filter(hiredlabor < 300 & goutput > 7000 | totlabor < 500 & noutput > 5000)
filter6

filter7 <- data %>% select(data = price) %>% filter(size > 4)
filter7

filter8 <- data %>% select(data = size, noutput, price) %>% filter(size > 3 & price > 100)
filter8

filter9 <- sqldf('SELECT id, size, noutput, MAX(price) AS Maxprice, region FROM data GROUP BY region')
filter9

filter10 <- sqldf('SELECT id, MAX(size) as Maxsize, MAX(noutput) as Maxoutput, price, region FROM data GROUP BY region')
filter10

filter11 <- sqldf('SELECT id, size, price, region FROM data WHERE (goutput - noutput) > 500 GROUP BY region')
filter11

# Graphic
filter12 = data %>% 
  group_by(region) %>%
  summarise(Number = length(x))%>%
  arrange(desc(Number))
View(filter12)
plot_ly(filter12, labels = ~region, values = ~Number, type = 'pie')

filter13 = data %>% 
  group_by(region) %>%
  summarise(Price = sum(price))%>%
  arrange(desc(Price))
View(filter13)

plot_ly(filter13, labels = ~region, values = ~Price, type = 'pie')

filter14 = data %>% 
  group_by(region) %>%
  summarise(Number = length(id),
            Price = sum(price))%>%
  arrange(desc(Price))
View(filter14)

filter15 = data %>% 
  group_by(region) %>%
  summarise(Size = sum(size),
            Price = sum(price))%>%
  arrange(desc(Size))
View(filter15)

plot_ly(data = filter15,
        x =  ~Size, y = ~Price,
        type = 'scatter', 
        mode = 'markers',
        hoverinfo = 'text',
        text = ~paste('</br> Region: ', region,
                      '</br> # Size: ', Size,
                      '</br> # Price: ', Price)) %>%
  layout(title = '', 
         xaxis = list(title = 'Size'), 
         yaxis = list(title = 'Price'))

filter16 = data %>% 
  group_by(region, varieties) %>%
  summarise(Number = length(varieties))%>%
  arrange()


filter17 = data %>% 
  group_by(region) %>%
  summarise(varieties_sum = length(varieties),
            size_sum = sum(size),
           seed_mean = round(mean(seed), 2),
           urea_mean = round(mean(urea), 2),
           phosphate_mean = round(mean(phosphate), 2),
           hiredlabor_mean = round(mean(hiredlabor), 2),
           totlabor_sum = sum(totlabor),
           wage_mean = round(mean(wage), 2),
           goutput_sum = (sum(goutput) - sum(noutput)),
           price_mean = round(mean(price), 2),
            )%>%
  arrange()

filter18 = data %>% 
  group_by(region) %>%
  summarise(varieties_sum = length(varieties),
            size_sum = sum(size),
            seed_mean = sum(seed),
            urea_mean = sum(urea),
            phosphate_mean = sum(phosphate),
            hiredlabor_mean = sum(hiredlabor),
            totlabor_sum = sum(totlabor),
            wage_mean = sum(wage),
            goutput_sum = (sum(goutput) - sum(noutput)),
            price_mean = sum(price),
  )%>%
  arrange()
filter18["Total" ,] <- colSums(filter18)

filter18 <- data.frame(t(filter18)) %>%
  rownames_to_column(var = 'title') %>%
  filter(title != "region")
filter18$title = sub("_.*", "", filter18$title)
colnames(filter18) = c('title',filter17$region)

cols <- c("mixed","high","trad")
for (i in cols) {
  summary_var <- enquo(i)
  summary_nm <- quo_name(summary_var)
  a <- data %>%
    filter(varieties == print(i)) %>%
    group_by(region) %>%
    summarise(!!summary_nm := length(varieties)) %>%
    arrange()
  filter17 <- cbind(filter17[1],a[2],filter17[,2:ncol(filter17)])
}

plot_ly(filter17, x = ~region, y = ~trad, 
        name = 'Trad', 
        marker =  list(color = 'rgb(255,215,0)'), 
        type = 'bar') %>%
  add_trace(y = ~high, name = 'High', 
            marker =  list(color = 'rgb(192,192,192)')) %>%
  add_trace(y = ~mixed, name = 'Mixed', 
            marker =  list(color = 'rgb(205,127,50)')) %>%
  layout(yaxis = list(title = 'Varieties'),
         xaxis = list(title = 'Region'),
         barmode = 'stack')
#=====================================================================
data$sum_seed <- data$seed * data$pseed

pricePerSize <- function(x)
{
  return(((x$goutput - x$noutput)/x$size)*x$price)
}

data$pps <- pricePerSize(data)

#=======================================================

#Histogram of Price 
hist(data$price, 
     main = "Histogram of Price",
     xlim = c(0,200), 
     ylim = c(0,280),xlab = "Price", ylab = "", col = "green")

#Boxplot of wage
boxplot(data$wage, main="Size Data")

#Barplot of bimas
bimas <- table(data$bimas)
barplot(bimas, main="Bimas numbers", xlab="Bimas status")

#Pieplot of varieties 
varis <- table(data$varieties)
nlabels <- paste(names(varis), ": ", varis, sep = "")
pie(varis, labels = nlabels, main = "Pie chart of varieties ")

#Corrplot of data
M <- cor(datalm)
corrplot(M, method="circle")

#=========================================================
datalm <- data[,c(3, 7:20)]

pairs(datalm)

r1 <- lm(goutput ~ noutput, data = datalm)
summary(r1)
plot(price ~ noutput, data = datalm)
abline(r1, col = 'green')

#reg <- lm(price~purea + totlabor + wage, data = datalm)
reg <- lm(price~., data = datalm)
step(reg)
summary(reg)


databma <- datalm[,c(1:14)]
pprice <- datalm[,15]
bma <- bicreg(databma, pprice, strict=FALSE, OR=20)
summary(bma)

imageplot.bma(bma)

attach(databma)
analysis <- lm(goutput ~ noutput)
anova(analysis)
summary(analysis)

attach(datalm)
twoway <- lm(price ~ totlabor + noutput + seed, data = datalm)
anova(twoway)

#=======================================================


set.seed(45)
## Phân data thành 2 ph???n: Train (80%) và test (20%)
# L???y ng???u nhiên 80% data d??? train và 20% d??? test
train.rows <- sample(rownames(datalm), dim(datalm)[1]*0.8)
train.rows
train.data <- datalm[train.rows, ]
train.data
#train.data <- datalm[train.rows, ]
test.rows <- setdiff(rownames(datalm), train.rows)
test.data <- datalm[test.rows, ]

reg <- lm(price ~ ., data = datalm, subset = train.rows)
reg
summary(reg)
tr.res <- data.frame(train.data$price, reg$fitted.values, reg$residuals)
tr.res
pred <- predict(reg, newdata = test.data)
pred
vl.res <- data.frame(test.data$price, pred, residuals = test.data$price - pred)
vl.res
head(vl.res)


#tính d??? chính xác  / lá»—i trung bÃ¬nh/ Lá»—i bÃ¬nh phÆ°Æ¡ng gá»‘c/
#Lá»—i tuyá»‡t Ä‘á»‘i trung bÃ¬nh/ Lá»—i tá»· lá»‡ pháº§n trÄƒm trung bÃ¬nh
accuracy(reg$fitted.values, train.data$price)
pred <- predict(reg, newdata = test.data)
accuracy(pred, test.data$price)
