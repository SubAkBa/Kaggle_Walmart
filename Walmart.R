library(Hmisc)
library(caret)
library(dplyr)
library(ggplot2)
library(data.table)

# 파일 불러오기
test <- read.csv("test.csv")
train <- read.csv("train.csv")
train <- train %>% filter(!is.na(Upc))

# 데이터 확인
head(train)
describe(train)
str(train)
table(train$TripType)
table(train$Upc)
table(train$DepartmentDescription)
summary(train)
boxplot(train$ScanCount)
table(train$ScanCount)
hist(train$VisitNumber)
table(train$VisitNumber)
train %>% filter(TripType == 39) %>% select(VisitNumber)
train %>% filter(ScanCount < -4)
train %>% filter(is.na(Upc))
train %>% filter(VisitNumber == 8)
train %>% filter(DepartmentDescription == 'PHARMACY RX')
train %>% filter(ScanCount > 19) %>% arrange(TripType, VisitNumber, ScanCount)
train %>% filter(FinelineNumber == 9998)
train %>% filter(TripType == 999)
train %>% filter(FinelineNumber == 1000)
train %>% filter(floor(log10(train$Upc))+1 == 12)


# DepartmentDescription과 Weekday와의 관계 확인
week_depart <- list()
wd_data <- tapply(train$DepartmentDescription, train$Weekday, table)
for(i in 1 : NROW(unique(train$Weekday))){
  week_depart <- c(week_depart, list(sort(wd_data[[i]], decreasing = T)))
}
names(week_depart) <- names(wd_data)
sort_week_depart <- c(week_depart['Monday'], week_depart['Tuesday'], 
                      week_depart['Wednesday'], week_depart['Thursday'],
                      week_depart['Friday'], week_depart['Saturday'],
                      week_depart['Sunday'])


# DepartmentDescription과 TripType과의 관계
triptype_depart <- list()
trde_data <- tapply(train$DepartmentDescription, train$TripType, table)
for(i in 1 : NROW(unique(train$TripType))){
  triptype_depart <- c(triptype_depart, list(sort(trde_data[[i]], decreasing = T)))
}
names(triptype_depart) <- names(trde_data)

trde_top10 <- list()
for(i in 1 : NROW(unique(train$TripType))){
  trde_top10 <- c(trde_top10, list(triptype_depart[[i]][1 : 10]))
}
names(trde_top10) <- names(trde_data)

trde_top10_plot_data <- train %>% 
  group_by(TripType, DepartmentDescription) %>% 
  summarise(n = n()) %>% 
  arrange(TripType, desc(n))
trde_top5_plot_result <- c()
for(i in unique(trde_top10_plot_data$TripType)){
  trde_top5_plot_result <- rbind(trde_top5_plot_result, 
                                 trde_top10_plot_data %>% filter(TripType == i) %>% head(5))
}
mosaicplot(~ DepartmentDescription + TripType, data = trde_top5_plot_result, las = 1)

# Read Data File
test <- read.csv("test.csv")
train <- read.csv("train.csv")
train <- train %>% filter(!is.na(Upc))
options(scipen = 100)

# UPC 12자리 채우기
# First : 11자리까지 채우기(0으로)
Upc12 <- matrix(0, nrow = nrow(train), ncol = 1)
ZeroFillFunc <- function(x, y){
  zeros <- c()
  zeroc <- 11 - (floor(log10(x)) + 1)
  for(j in 1 : zeroc){
    zeros <- paste0(zeros, '0')
  }
  return (paste0(zeros, x))
}
for(i in 1 : nrow(train)){
  Upc12[i] <- ifelse(floor(log10(train$Upc[i])) > 9, train$Upc[i], 
                     ZeroFillFunc(train$Upc[i]))
}
Upc12 <- as.vector(Upc12)
# Second : CheckSumDigit 만들기
MakeCheckSumDigitFunc <- function(x){
  odds_sum <- 0
  evens_sum <- 0
  digit11 <- ""
  
  split_upc <- strsplit(x, "")[[1]]
  for(i in 1 : 11){
    digit11 <- paste0(digit11, split_upc[i])
    if (i %% 2 == 1) {
      odds_sum <- odds_sum + as.numeric(split_upc[i])
    }else{
      evens_sum <- evens_sum + as.numeric(split_upc[i])
    }
  }
  total_sum <- odds_sum * 3 + evens_sum
  sum_result <- ifelse(total_sum %% 10 == 0, 0, (10 - (total_sum %% 10)))
  return (paste0(digit11, sum_result))
}
Upc12 <- sapply(Upc12, MakeCheckSumDigitFunc)
names(Upc12) <- NULL
# Final : Train데이터와 합치기
train <- data.frame(train, Upc12)
head(train, 20)
colnames(train)[8] <- "UPC"
train$Upc <- NULL


# train Copy / Divide Company Code / Divide Product Code
cptrain <- train
MakeCompanyCodeFunc <- function(x){
  ccode <- ""
  upc <- strsplit(as.character(x), "")[[1]]
  for(i in 1 : 6){
    ccode <- paste0(ccode, upc[i])
  }
  return (ccode)
}
Upc6 <- sapply(cptrain$UPC, MakeCompanyCodeFunc)
cptrain <- data.frame(cptrain, Upc6)
colnames(cptrain)[8] <- "CompanyCode"
cptrain$UPC <- NULL
cptrain$CompanyCode <- as.character(cptrain$CompanyCode)
write.csv(cptrain, "htrain.csv", row.names = F)

pUpc <- matrix(0, nrow = nrow(cptrain), ncol = 5)
for(i in 1 : 5){
  pUpc[, i] <- sapply(cptrain$UPC, function(x){
    pcode <- ""
    upc <- strsplit(as.character(x), "")[[1]]
    for(j in (7 + (i - 1)) : 11){
      pcode <- paste0(pcode, upc[j])
    }
    return (pcode)
  })
}
cptrain <- data.frame(cptrain, pUpc)
colnames(cptrain)[8 : 12] <- c("ProductCode5", "ProductCode4", "ProductCode3",
                               "ProductCode2", "ProductCode1")
for(i in 8 : 12){
  cptrain[, i] <- as.character(cptrain[, i])
}
write.csv(cptrain, "ptrain.csv", row.names = F)
write.table(cptrain, "ptrain.txt", sep = ",", col.names = T)
tt <- fread("ptrain.csv")
str(cptrain)
head(tt)


# Read Refined Data & Set Company Code len 6
library(data.table)
library(dplyr)
library(caret)
rtrain <- fread("htrain.csv", stringsAsFactors = T)
rtrain <- as.data.frame(rtrain)
rtrain <- rtrain %>% filter(TripType %in% c(3 : 20))
rtrain$TripType <- as.factor(rtrain$TripType)
rtrain <- rtrain %>% arrange(DepartmentDescription, FinelineNumber)
t <- rtrain %>% select(DepartmentDescription, FinelineNumber) %>% 
  arrange(DepartmentDescription, FinelineNumber) %>% distinct()
t$defi <- 1 : nrow(t)
rtrain <- merge(rtrain, t, by = c("DepartmentDescription", "FinelineNumber"), all.x = T)

# Divide Train / Validation
sample_idx <- rtrain %>% sample_frac(0.7) %>% rownames() %>% as.numeric()
rtrain_train <- rtrain[sample_idx, ]
rtrain_val <- rtrain[-sample_idx, ]

# Multinomial Logistic Regression
library(nnet)
mlr_model <- multinom(TripType ~ ., data = rtrain_train)
str(rtrain)
summary(mlr_model)
table(fitted(mlr_model))

# Decision Tree
library(party)
tree_control <- ctree_control(mincriterion = 0.9, minsplit = 10, maxdepth = 6)
tree_model <- ctree(TripType ~ ., data = rtrain_train)
plot(tree_model)
tree_pred <- predict(tree_model, newdata = rtrain_val)
confusionMatrix(tree_pred, rtrain_val$TripType_3)

# RandomForest
library(randomForest)
rf_model <- randomForest(TripType ~ ScanCount, data = rtrain_train)
rf_pred <- predict(rf_model, newdata = rtrain_val)
confusionMatrix(rf_pred, rtrain_val$TripType)

rf_model <- randomForest(TripType ~ VisitNumber, data = rtrain_train,
                         ntree = 200, importance = TRUE, do.trace = TRUE)
rf_pred <- predict(rf_model, newdata = rtrain_val)
rf_cfm <- confusionMatrix(rf_pred, rtrain_val$TripType)
rf_imp <- importance(rf_model)
plot(rf_model)

rf_model1 <- randomForest(TripType ~ VisitNumber + FinelineNumber, data = rtrain_train,
                          ntree = 200, importance = T, do.trace = T)
rf_pred1 <- predict(rf_model1, newdata = rtrain_val)
rf_cfm1 <- confusionMatrix(rf_pred1, rtrain_val$TripType)
rf_imp <- importance(rf_model1)
varImpPlot(rf_model1)

rf_model2 <- randomForest(TripType ~ VisitNumber + Weekday, data = rtrain_train,
                          ntree = 100, importance = T, do.trace = T)
rf_pred2 <- predict(rf_model2, newdata = rtrain_val)
rf_cfm <- confusionMatrix(rf_pred2, rtrain_val$TripType)


levels(rtrain_train$DepartmentDescription) <- c(1 : 68)
table(rtrain_train$DepartmentDescription)
rtrain_train$DepartmentDescription <- as.numeric(rtrain_train$DepartmentDescription)
levels(rtrain_val$DepartmentDescription) <- c(1 : 68)
table(rtrain_val$DepartmentDescription)
rtrain_val$DepartmentDescription <- as.numeric(rtrain_val$DepartmentDescription)
rf_model3 <- randomForest(TripType ~ VisitNumber + DepartmentDescription, data = rtrain_train,
                          ntree = 100, importance = T, do.trace = T)
rf_pred3 <- predict(rf_model3, newdata = rtrain_val)
rf_cfm3 <- confusionMatrix(rf_pred3, rtrain_val$TripType)

rf_model4 <- randomForest(TripType ~ DepartmentDescription, data = rtrain_train,
                          ntree = 100, importance = T, do.trace = T)
rf_pred4 <- predict(rf_model4, rtrain_val)
rf_cfm <- confusionMatrix(rf_pred4, rtrain_val$TripType)


rtrain_train <- rtrain_train %>% arrange(DepartmentDescription, FinelineNumber)
t <- rtrain_train %>% 
  select(DepartmentDescription, FinelineNumber) %>% 
  group_by(DepartmentDescription, FinelineNumber) %>% 
  arrange(DepartmentDescription, FinelineNumber) %>% 
  unique()
rtrain_train <- merge(rtrain_train, t, by = c("DepartmentDescription", "FinelineNumber"), all.x = T)
rtrain_val <- merge(rtrain_val, t, by = c("DepartmentDescription", "FinelineNumber"), all.x = T)

rf_model5 <- randomForest(TripType ~ VisitNumber + defi, data = rtrain_train,
                          ntree = 200, importance = T, do.trace = T)
rf_pred5 <- predict(rf_model5, newdata = rtrain_val)
rf_cfm5 <- confusionMatrix(rf_pred5, rtrain_val$TripType)
rf_imp5 <- importance(rf_model5)
varImpPlot(rf_model5)

t1 <- rtrain_train %>% 
  select(defi, CompanyCode) %>% 
  group_by(defi, CompanyCode) %>% 
  arrange(defi, CompanyCode) %>% 
  unique()
rtrain_train %>% filter(TripType %in% c(7, 8)) %>% head(20)
t1$CompanyCode <- as.character(t1$CompanyCode)
t1 <- t1 %>% arrange(CompanyCode, defi)
t1$defico <- 1 : nrow(t1)
rtrain_train <- merge(rtrain_train, t1, by = c("defi", "CompanyCode"), all.x = T)
rtrain_val <- merge(rtrain_val, t1, by = c("defi", "CompanyCode"), all.x = T)
rf_model6 <- randomForest(TripType ~ VisitNumber + defico, data = rtrain_train,
                          ntree = 200, importance = T, do.trace = T)
rf_pred6 <- predict(rf_model6, newdata = rtrain_val)
rf_cfm6 <- confusionMatrix(rf_pred6, rtrain_val$TripType)


# Make New DataSet
head(train)
sumscancount <- train %>%
  filter(ScanCount > 0) %>%
  group_by(VisitNumber) %>% 
  summarise(sumscancount = sum(ScanCount))
minuscancount <- train %>% 
  filter(ScanCount < 0) %>% 
  group_by(VisitNumber) %>% 
  summarise(n = n())
colnames(minuscancount) <- c("id", "RefundCount")
maxperdesc <- train %>% 
  group_by(VisitNumber, DepartmentDescription) %>% 
  summarise(n = n())
maxperdesc1 <- train %>%
  group_by(VisitNumber) %>% 
  summarise(n = n())
maxperdescs <- merge(maxperdesc, maxperdesc1, by = "VisitNumber", all.x = T)
maxperdesc <- maxperdescs %>%
  group_by(VisitNumber) %>% 
  summarise(maxdesc = max(n.x),
            totaldesc = mean(n.y),
            per = round(maxdesc / totaldesc, 3))
newdata <- read.csv("newdata.csv")
newdata <- merge(newdata, minuscancount, by = "id", all.x = T)
newdata$RefundCount <- ifelse(is.na(newdata$RefundCount), 0, newdata$RefundCount)
colnames(maxperdesc)[1] <- "id"
newdata <- merge(newdata, maxperdesc, by = "id", all.x = T)
head(newdata)
colnames(newdata)[5 : 7] <- c("MaxDescCount", "TotalDescCount", "max_prodrate")

write.csv(newdata, "newdata.csv", row.names = F)

library(dummies)
tt <- dummy(train$DepartmentDescription)
train <- data.frame(train, tt)
train$DepartmentDescription <- NULL
colnames(train)
head(tt)


library(dplyr)
# 데이터 읽어오기 & NA 지우기
train <- read.csv("train.csv")
train <- train %>% filter(!is.na(Upc))

# VisitNumber와 DepartmentDescription 그룹으로 묶어서 
# 각 그룹에 대한 총 갯수 데이터 셋 만들기 -- ㉠
descdummy <- train %>% 
  group_by(VisitNumber, DepartmentDescription) %>% 
  summarise(sumcategory = n())

# DepartmentDescription 더미변수를 담을 데이터프레임 공간 만들기
# column 이름 변경해주기
departdummy <- data.frame(unique(train$VisitNumber), 
                          matrix(0, nrow = length(unique(train$VisitNumber)), 
                                 ncol = length(levels(train$DepartmentDescription))))
colnames(departdummy)[1] <- "id"
colnames(departdummy)[2 : ncol(departdummy)] <- levels(train$DepartmentDescription)

# 이 전처리 방식의 핵심은 DepartmentDescription의 레벨을 1부터 69까지로 변경 후에
# 그 숫자 + 1에 해당하는 인덱스에 위의 ㉠부분에서 구한 총 갯수를 넣어주는 것
# 또한, factor형식은 사칙연산에 대해서 의미가 없기 때문에 
# 이를 numeric으로 변경 후에 연산을 진행
levels(descdummy$DepartmentDescription) <- 1 : length(levels(descdummy$DepartmentDescription))
descdummy$DepartmentDescription <- as.numeric(descdummy$DepartmentDescription)
departindex <- 1 # departdummy(데이터를 저장해야할 변수)의 행의 인덱스
descindex <- 1 # descdummy(㉠의 그룹에 대한 총 갯수가 담긴 변수)의 행의 인덱스
repeat{
  # 서로의 VisitNumber를 비교, 
  # departdummy의 id가 VisitNumber(열이름만 위에서 바꾼것)
  # why? 밑에서 데이터 셋과 조인하기위해 이름을 id로 변경 해준것.
  if(descdummy$VisitNumber[descindex] == departdummy$id[departindex]){
    # 위에서 간단히 설명한대로 VisitNumber가 같다면 총 갯수 데이터를
    # departmentdescription의 값 + 1의 인덱스에 삽입
    # 그리고 나서 총 갯수 데이터가 담긴 descindex의 값을 1증가하여 아래 행으로 이동
    departdummy[departindex, 
                descdummy$DepartmentDescription[descindex] + 1] <- descdummy$sumcategory[descindex]
    descindex <- descindex + 1
  }else{
    # 만약 VisitNumber가 다르다면 우리가 담고자 했던 departdummy의 인덱스를 1 증가
    departindex <- departindex + 1
  }
  print(paste(departindex, descindex))
  if(descindex == (nrow(descdummy) + 1)){
    break
  }
}

# 데이터 셋 불러온 후 더미변수와 조인시킨후에 저장
newdata <- read.csv("newdata.csv")
newdata <- merge(newdata, departdummy, by = "id", all.x = T)
write.csv(newdata, "newdata.csv", row.names = F)
colnames(newdata)

# TripType    - a categorical id representing the type of shopping trip the customer made. 
#               This is the ground truth that you are predicting. 
#               TripType_999 is an "other" category.
# VisitNumber - an id corresponding to a single trip by a single customer
# Weekday     - the weekday of the trip
# Upc         - the UPC number of the product purchased
# ScanCount   - the number of the given item that was purchased. 
#               A negative value indicates a product return.
# DepartmentDescription - a high-level description of the item's department
# FinelineNumber        - a more refined category for each of the products, created by Walmart

# upc 설명 : https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/discussion/18158
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/discussion/18163
# https://www.kaggle.com/c/walmart-recruiting-trip-type-classification/discussion/30345
# https://en.wikipedia.org/wiki/Check_digit