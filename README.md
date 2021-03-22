Random Forest Task
 
3/2 /2021
Major Library for Random Forest
library(randomForest)
## randomForest 4.6-14
## Type rfNews() to see new features/changes/bug fixes.
require(caTools)
## Loading required package: caTools
Load Data set
library(readr)
data <- read_csv("C:/Users/Owner/Downloads/healthcare_new_self11.csv")
## Warning: Missing column names filled in: 'X1' [1]
## Parsed with column specification:
## cols(
##   X1 = col_double(),
##   id = col_character(),
##   category_code = col_character(),
##   status = col_character(),
##   country_code = col_character(),
##   state_code = col_character(),
##   funding_rounds = col_double(),
##   funding_total_usd = col_double(),
##   funding_round_type = col_character(),
##   raised_amount_usd = col_double(),
##   participants = col_double(),
##   homepage_url_available = col_double(),
##   twitter_account_available = col_double(),
##   fundinground_duration = col_double()
## )
data<-data[,-2]
data<-data[,-4]
data<-data[,-1]
head(data)
## # A tibble: 6 x 11
##   category_code status state_code funding_rounds funding_total_u~
##   <chr>         <chr>  <chr>               <dbl>            <dbl>
## 1 health        opera~ CA                      5         68069200
## 2 health        opera~ CA                      5         68069200
## 3 health        opera~ CA                      5         68069200
## 4 health        opera~ CA                      5         68069200
## 5 health        opera~ CA                      5         68069200
## 6 medical       opera~ MA                      2         11300000
## # ... with 6 more variables: funding_round_type <chr>,
## #   raised_amount_usd <dbl>, participants <dbl>,
## #   homepage_url_available <dbl>, twitter_account_available <dbl>,
## #   fundinground_duration <dbl>
The Above is used to remove redundant variable that may affect the accuracy of the model. We removed the variable “country” because it is thesame country that all the dataset were gathered from. Also the variable Id were removed because it contributes nothing the model, hence it is less significant to the model.
data$category_code<-as.factor(data$category_code)
data$status<-as.factor(data$status)
data$state_code<-as.factor(data$state_code)
data$funding_round_type<-as.factor(data$funding_round_type)
data<-na.omit(data)
str(data)
## Classes 'tbl_df', 'tbl' and 'data.frame':    4279 obs. of  11 variables:
##  $ category_code            : Factor w/ 3 levels "biotech","health",..: 2 2 2 2 2 3 3 1 3 3 ...
##  $ status                   : Factor w/ 4 levels "acquired","closed",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ state_code               : Factor w/ 49 levels "AK","AL","AR",..: 5 5 5 5 5 20 20 23 31 31 ...
##  $ funding_rounds           : num  5 5 5 5 5 2 2 1 3 3 ...
##  $ funding_total_usd        : num  68069200 68069200 68069200 68069200 68069200 ...
##  $ funding_round_type       : Factor w/ 9 levels "angel","crowdfunding",..: 6 7 8 8 9 9 9 7 9 9 ...
##  $ raised_amount_usd        : num  2000000 9000000 12000000 43000000 2069200 ...
##  $ participants             : num  2 4 4 5 4 0 1 3 0 0 ...
##  $ homepage_url_available   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ twitter_account_available: num  1 1 1 1 1 0 0 0 1 1 ...
##  $ fundinground_duration    : num  1768 1768 1768 1768 1768 ...
##  - attr(*, "na.action")= 'omit' Named int  146 147 250 251 270 368 704 705 725 757 ...
##   ..- attr(*, "names")= chr  "146" "147" "250" "251" ...
summary(data)
##  category_code        status       state_code   funding_rounds  
##  biotech:2932   acquired : 145   CA     :1404   Min.   : 1.000  
##  health : 551   closed   :  80   MA     : 578   1st Qu.: 2.000  
##  medical: 796   ipo      : 141   TX     : 200   Median : 3.000  
##                 operating:3913   PA     : 187   Mean   : 3.331  
##                                  NY     : 154   3rd Qu.: 4.000  
##                                  WA     : 146   Max.   :14.000  
##                                  (Other):1610                   
##  funding_total_usd   funding_round_type raised_amount_usd    participants 
##  Min.   :0.000e+00   venture  :1585     Min.   :0.000e+00   Min.   : 0.0  
##  1st Qu.:2.500e+06   other    : 696     1st Qu.:7.607e+05   1st Qu.: 0.0  
##  Median :1.326e+07   series-a : 597     Median :3.252e+06   Median : 0.0  
##  Mean   :3.588e+07   series-c+: 455     Mean   :1.054e+07   Mean   : 1.2  
##  3rd Qu.:4.700e+07   series-b : 441     3rd Qu.:1.097e+07   3rd Qu.: 2.0  
##  Max.   :2.600e+09   angel    : 342     Max.   :2.600e+09   Max.   :25.0  
##                      (Other)  : 163                                       
##  homepage_url_available twitter_account_available fundinground_duration
##  Min.   :0.0000         Min.   :0.0000            Min.   :   0.0       
##  1st Qu.:1.0000         1st Qu.:0.0000            1st Qu.: 120.0       
##  Median :1.0000         Median :0.0000            Median : 697.0       
##  Mean   :0.9512         Mean   :0.3807            Mean   : 865.8       
##  3rd Qu.:1.0000         3rd Qu.:1.0000            3rd Qu.:1341.5       
##  Max.   :1.0000         Max.   :1.0000            Max.   :3987.0       
## 
For a parametric method, it is crucial to convert all the qualitative dataset to factors. Some of the variables that are charater were converted to factor using the codes given above. After the data has been converted, we used function “str” to check the class of each variables. the summary function was then use to check the distribution of each variables. The mean, meadian and other statistics was estimated.
summary(data)
##  category_code        status       state_code   funding_rounds  
##  biotech:2932   acquired : 145   CA     :1404   Min.   : 1.000  
##  health : 551   closed   :  80   MA     : 578   1st Qu.: 2.000  
##  medical: 796   ipo      : 141   TX     : 200   Median : 3.000  
##                 operating:3913   PA     : 187   Mean   : 3.331  
##                                  NY     : 154   3rd Qu.: 4.000  
##                                  WA     : 146   Max.   :14.000  
##                                  (Other):1610                   
##  funding_total_usd   funding_round_type raised_amount_usd    participants 
##  Min.   :0.000e+00   venture  :1585     Min.   :0.000e+00   Min.   : 0.0  
##  1st Qu.:2.500e+06   other    : 696     1st Qu.:7.607e+05   1st Qu.: 0.0  
##  Median :1.326e+07   series-a : 597     Median :3.252e+06   Median : 0.0  
##  Mean   :3.588e+07   series-c+: 455     Mean   :1.054e+07   Mean   : 1.2  
##  3rd Qu.:4.700e+07   series-b : 441     3rd Qu.:1.097e+07   3rd Qu.: 2.0  
##  Max.   :2.600e+09   angel    : 342     Max.   :2.600e+09   Max.   :25.0  
##                      (Other)  : 163                                       
##  homepage_url_available twitter_account_available fundinground_duration
##  Min.   :0.0000         Min.   :0.0000            Min.   :   0.0       
##  1st Qu.:1.0000         1st Qu.:0.0000            1st Qu.: 120.0       
##  Median :1.0000         Median :0.0000            Median : 697.0       
##  Mean   :0.9512         Mean   :0.3807            Mean   : 865.8       
##  3rd Qu.:1.0000         3rd Qu.:1.0000            3rd Qu.:1341.5       
##  Max.   :1.0000         Max.   :1.0000            Max.   :3987.0       
## 
Data Training and validation
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
summary(TrainSet)
##  category_code        status       state_code   funding_rounds  
##  biotech:2060   acquired : 101   CA     : 975   Min.   : 1.000  
##  health : 376   closed   :  52   MA     : 392   1st Qu.: 2.000  
##  medical: 559   ipo      :  98   TX     : 134   Median : 3.000  
##                 operating:2744   PA     : 133   Mean   : 3.333  
##                                  NY     : 109   3rd Qu.: 4.000  
##                                  WA     :  97   Max.   :14.000  
##                                  (Other):1155                   
##  funding_total_usd   funding_round_type raised_amount_usd  
##  Min.   :0.000e+00   venture  :1133     Min.   :0.000e+00  
##  1st Qu.:2.430e+06   other    : 457     1st Qu.:7.500e+05  
##  Median :1.248e+07   series-a : 431     Median :3.161e+06  
##  Mean   :3.536e+07   series-c+: 311     Mean   :1.071e+07  
##  3rd Qu.:4.599e+07   series-b : 296     3rd Qu.:1.024e+07  
##  Max.   :2.600e+09   angel    : 248     Max.   :2.600e+09  
##                      (Other)  : 119                        
##   participants   homepage_url_available twitter_account_available
##  Min.   : 0.00   Min.   :0.0000         Min.   :0.0000           
##  1st Qu.: 0.00   1st Qu.:1.0000         1st Qu.:0.0000           
##  Median : 0.00   Median :1.0000         Median :0.0000           
##  Mean   : 1.17   Mean   :0.9489         Mean   :0.3706           
##  3rd Qu.: 2.00   3rd Qu.:1.0000         3rd Qu.:1.0000           
##  Max.   :25.00   Max.   :1.0000         Max.   :1.0000           
##                                                                  
##  fundinground_duration
##  Min.   :   0.0       
##  1st Qu.:  92.0       
##  Median : 694.0       
##  Mean   : 854.5       
##  3rd Qu.:1335.0       
##  Max.   :3987.0       
## 
summary(ValidSet)
##  category_code       status       state_code  funding_rounds  
##  biotech:872   acquired :  44   CA     :429   Min.   : 1.000  
##  health :175   closed   :  28   MA     :186   1st Qu.: 2.000  
##  medical:237   ipo      :  43   TX     : 66   Median : 3.000  
##                operating:1169   PA     : 54   Mean   : 3.326  
##                                 WA     : 49   3rd Qu.: 4.000  
##                                 NY     : 45   Max.   :14.000  
##                                 (Other):455                   
##  funding_total_usd   funding_round_type raised_amount_usd  
##  Min.   :        0   venture  :452      Min.   :        0  
##  1st Qu.:  2718879   other    :239      1st Qu.:   900000  
##  Median : 15156534   series-a :166      Median :  3420000  
##  Mean   : 37071649   series-b :145      Mean   : 10158710  
##  3rd Qu.: 51000000   series-c+:144      3rd Qu.: 12000000  
##  Max.   :378000000   angel    : 94      Max.   :300000000  
##                      (Other)  : 44                         
##   participants    homepage_url_available twitter_account_available
##  Min.   : 0.000   Min.   :0.0000         Min.   :0.0000           
##  1st Qu.: 0.000   1st Qu.:1.0000         1st Qu.:0.0000           
##  Median : 0.000   Median :1.0000         Median :0.0000           
##  Mean   : 1.269   Mean   :0.9564         Mean   :0.4042           
##  3rd Qu.: 2.000   3rd Qu.:1.0000         3rd Qu.:1.0000           
##  Max.   :14.000   Max.   :1.0000         Max.   :1.0000           
##                                                                   
##  fundinground_duration
##  Min.   :   0.0       
##  1st Qu.: 163.8       
##  Median : 721.5       
##  Mean   : 892.2       
##  3rd Qu.:1360.5       
##  Max.   :3987.0       
## 
from the above, we split the training data and the test dataset in the proportion of 70:30. the test is otherwise called the valid dataset. The purpose of this is to check the accuracy of the model that would be form using the random forest methodology. The summary of the train and the valid dataset was also checked.
Now, we will create a Random Forest model with default parameters and then we will fine tune the model by changing ‘mtry’. We can tune the random forest model by changing the number of trees (ntree) and the number of variables randomly sampled at each stage (mtry). According to Random Forest package description:
Ntree: Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
Mtry: Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3) Create a random Forest Model
# Create a Random Forest model with default parameters
model1 <- randomForest(status ~ ., data = TrainSet,importance = TRUE)
model1
## 
## Call:
##  randomForest(formula = status ~ ., data = TrainSet, importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 3
## 
##         OOB estimate of  error rate: 6.74%
## Confusion matrix:
##           acquired closed ipo operating class.error
## acquired        10      1   0        90 0.900990099
## closed           1      2   0        49 0.961538462
## ipo              1      0  40        57 0.591836735
## operating        2      1   0      2741 0.001093294
By default, number of trees is 500 and number of variables tried at each split is 3 in this case.
# Fine tuning parameters of Random Forest model
model2 <- randomForest(status ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2
## 
## Call:
##  randomForest(formula = status ~ ., data = TrainSet, ntree = 500,      mtry = 6, importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 6.24%
## Confusion matrix:
##           acquired closed ipo operating  class.error
## acquired        18      1   0        82 0.8217821782
## closed           1      5   0        46 0.9038461538
## ipo              1      0  42        55 0.5714285714
## operating        1      0   0      2743 0.0003644315
When we have increased the mtry to 6 from 2, error rate has reduced from 6.74% to 6.24%. We will now predict on the train dataset first and then predict on validation
# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$status) 
##            
## predTrain   acquired closed  ipo operating
##   acquired       101      0    0         0
##   closed           0     52    0         0
##   ipo              0      0   98         0
##   operating        0      0    0      2744
# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$status)                    
## [1] 0.9314642
table(predValid,ValidSet$status)
##            
## predValid   acquired closed  ipo operating
##   acquired         7      0    1         4
##   closed           0      3    0         0
##   ipo              0      0   21         0
##   operating       37     25   21      1165
In case of prediction on train dataset, there is zero misclassification; however, in the case of validation dataset, 6 data points are misclassified and accuracy is 93.30%. We can also use function to check important variables. The below functions show the drop in mean accuracy for each of the variables.
# To check important variables
importance(model2)        
##                            acquired    closed       ipo operating
## category_code             17.060626  7.574875 11.836954 13.765540
## state_code                36.264056 24.013837 35.575955 38.865483
## funding_rounds            20.936582 16.746995 40.274609 45.046436
## funding_total_usd         22.612149 17.299825 37.161338 48.076127
## funding_round_type         3.235745 -2.903270 18.014337 23.439639
## raised_amount_usd          8.264147  1.611333  5.500591 33.865086
## participants               9.178516  6.258299  4.934376 14.797553
## homepage_url_available     3.003944  1.435201  2.635774  9.166721
## twitter_account_available 16.039665 11.205482 15.412309 18.151735
## fundinground_duration     32.057575 23.431055 29.185666 44.623805
##                           MeanDecreaseAccuracy MeanDecreaseGini
## category_code                        19.051021        10.657108
## state_code                           50.902854        54.167596
## funding_rounds                       50.769793        36.302475
## funding_total_usd                    54.034662       120.142848
## funding_round_type                   26.101847        41.001409
## raised_amount_usd                    34.662575        75.556992
## participants                         17.627461        27.936511
## homepage_url_available                9.727591         2.712415
## twitter_account_available            22.109490        11.005611
## fundinground_duration                53.539949        91.908568
varImpPlot(model2) 
 
From the above graph, we can see that the accuracy decreased when mtry was increased from 4 to 5 and then increased when mtry was changed to 6 from 5. Maximum accuracy is at mtry equal to 8.
Now, we have seen the implementation of Random Forest and understood the importance of the model. Let’s compare this model with decision tree and see how decision trees fare in comparison to random forest.
# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(status ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$status)
}
 
a
## [1] 0.9275701 0.9291277 0.9291277 0.9314642 0.9330218 0.9353583
plot(3:8,a)
 
# Compare with Decision Tree
library(rpart)
library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
## 
## Attaching package: 'ggplot2'
## The following object is masked from 'package:randomForest':
## 
##     margin
library(e1071)
# We will compare model 1 of Random Forest with Decision Tree model
 
model_dt = train(status ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$status)
##            
## model_dt_1  acquired closed  ipo operating
##   acquired         0      0    0         0
##   closed           0      0    0         0
##   ipo              0      0    8         0
##   operating      101     52   90      2744
mean(model_dt_1 == TrainSet$status)
## [1] 0.9188648
On the training dataset, the accuracy is around 91.89% and there is low misclassification. Now, look at the validation dataset.
# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$status)
##            
## model_dt_vs acquired closed  ipo operating
##   acquired         0      0    0         0
##   closed           0      0    0         0
##   ipo              0      0    1         0
##   operating       44     28   42      1169
mean(model_dt_vs == ValidSet$status)
## [1] 0.911215
The accuracy on validation dataset has decreased further to 91.12%.
The above comparison shows the true power of ensembling and the importance of using Random Forest over Decision Trees. Though Random Forest comes up with its own inherent limitations (in terms of number of factor levels a categorical variable can have), but it still is one of the best models that can be used for classification. It is easy to use and tune as compared to some of the other complex models, and still provides us good level of accuracy in the business scenario. You can also compare Random Forest with other models and see how it fares in comparison to other techniques.
Checking the impact of funding_total_usd on the independent variables
set.seed(1234)
regressor = randomForest(x = data[,-7], #returns a dataframe
y = data$raised_amount_usd, #returns a vector
ntree = 500, mtry = 5, nPerm = 4, nodesize = 2)
saveRDS(regressor, "rfreg_ntree500_mtry5_nPerm4_nodesize2.dat")
regressor2 = readRDS("rfreg_ntree500_mtry5_nPerm4_nodesize2.dat")
lm<-lm(funding_total_usd~., data=data)
summary(lm)
## 
## Call:
## lm(formula = funding_total_usd ~ ., data = data)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -88305062 -14822266  -3174260   8396319 291566520 
## 
## Coefficients:
##                                    Estimate Std. Error t value Pr(>|t|)
## (Intercept)                      -1.978e+07  1.776e+07  -1.113  0.26565
## category_codehealth              -2.456e+06  1.539e+06  -1.596  0.11053
## category_codemedical             -2.630e+05  1.268e+06  -0.207  0.83570
## statusclosed                     -3.628e+06  4.253e+06  -0.853  0.39371
## statusipo                         3.423e+07  3.750e+06   9.127  < 2e-16
## statusoperating                  -1.194e+06  2.616e+06  -0.457  0.64801
## state_codeAL                     -1.068e+07  1.890e+07  -0.565  0.57219
## state_codeAR                      9.903e+06  2.478e+07   0.400  0.68941
## state_codeAZ                      1.739e+06  1.807e+07   0.096  0.92333
## state_codeCA                      1.069e+07  1.766e+07   0.605  0.54525
## state_codeCO                      7.617e+06  1.788e+07   0.426  0.67016
## state_codeCT                      6.246e+05  1.791e+07   0.035  0.97218
## state_codeDC                      1.995e+06  2.101e+07   0.095  0.92434
## state_codeDE                      9.776e+06  2.776e+07   0.352  0.72476
## state_codeFL                      1.365e+07  1.785e+07   0.765  0.44448
## state_codeGA                     -9.492e+06  1.804e+07  -0.526  0.59879
## state_codeHI                     -4.364e+05  1.989e+07  -0.022  0.98249
## state_codeIA                      3.917e+06  2.064e+07   0.190  0.84947
## state_codeID                      1.299e+07  2.325e+07   0.559  0.57623
## state_codeIL                      2.908e+07  1.799e+07   1.617  0.10603
## state_codeIN                      1.086e+06  1.846e+07   0.059  0.95307
## state_codeKS                      1.229e+06  2.004e+07   0.061  0.95110
## state_codeKY                      1.073e+06  1.886e+07   0.057  0.95463
## state_codeLA                      7.872e+06  2.100e+07   0.375  0.70777
## state_codeMA                      1.594e+07  1.770e+07   0.900  0.36794
## state_codeMD                     -1.083e+06  1.794e+07  -0.060  0.95187
## state_codeME                     -3.408e+06  1.971e+07  -0.173  0.86274
## state_codeMI                     -1.439e+06  1.806e+07  -0.080  0.93651
## state_codeMN                      3.311e+06  1.795e+07   0.184  0.85369
## state_codeMO                     -5.124e+06  1.819e+07  -0.282  0.77819
## state_codeMS                      9.370e+06  3.507e+07   0.267  0.78936
## state_codeMT                      9.535e+06  2.487e+07   0.383  0.70147
## state_codeNC                      3.033e+06  1.788e+07   0.170  0.86529
## state_codeND                      1.774e+07  2.778e+07   0.638  0.52323
## state_codeNE                      1.615e+06  2.484e+07   0.065  0.94816
## state_codeNH                     -1.118e+07  1.887e+07  -0.592  0.55357
## state_codeNJ                      1.284e+07  1.792e+07   0.717  0.47364
## state_codeNM                     -1.346e+07  1.866e+07  -0.721  0.47078
## state_codeNV                      3.426e+06  1.989e+07   0.172  0.86327
## state_codeNY                      5.356e+06  1.779e+07   0.301  0.76342
## state_codeOH                      4.014e+06  1.797e+07   0.223  0.82327
## state_codeOK                      5.886e+06  1.972e+07   0.299  0.76532
## state_codeOR                      3.459e+06  1.872e+07   0.185  0.85340
## state_codePA                      2.488e+06  1.779e+07   0.140  0.88877
## state_codeRI                      5.746e+06  1.868e+07   0.308  0.75842
## state_codeSC                      4.532e+06  1.940e+07   0.234  0.81535
## state_codeSD                      1.381e+07  2.323e+07   0.595  0.55212
## state_codeTN                     -1.889e+06  1.797e+07  -0.105  0.91628
## state_codeTX                      2.469e+06  1.777e+07   0.139  0.88949
## state_codeUT                      1.071e+07  1.814e+07   0.590  0.55509
## state_codeVA                      5.276e+06  1.803e+07   0.293  0.76984
## state_codeWA                     -2.144e+06  1.782e+07  -0.120  0.90425
## state_codeWI                     -1.904e+06  1.820e+07  -0.105  0.91668
## state_codeWV                     -3.034e+07  1.981e+07  -1.531  0.12573
## funding_rounds                    4.506e+06  3.121e+05  14.437  < 2e-16
## funding_round_typecrowdfunding   -3.353e+06  1.250e+07  -0.268  0.78853
## funding_round_typeother           4.055e+06  2.093e+06   1.937  0.05277
## funding_round_typepost-ipo       -9.276e+06  1.062e+07  -0.873  0.38264
## funding_round_typeprivate-equity  9.829e+06  3.040e+06   3.233  0.00123
## funding_round_typeseries-a        2.024e+06  2.132e+06   0.949  0.34245
## funding_round_typeseries-b        2.160e+06  2.329e+06   0.928  0.35366
## funding_round_typeseries-c+       1.090e+07  2.407e+06   4.530 6.05e-06
## funding_round_typeventure         2.463e+06  1.874e+06   1.315  0.18871
## raised_amount_usd                 1.034e+00  8.366e-03 123.650  < 2e-16
## participants                      1.480e+06  2.796e+05   5.292 1.27e-07
## homepage_url_available            3.414e+06  2.265e+06   1.507  0.13180
## twitter_account_available        -2.420e+06  1.042e+06  -2.321  0.02032
## fundinground_duration             1.748e+04  9.073e+02  19.269  < 2e-16
##                                     
## (Intercept)                         
## category_codehealth                 
## category_codemedical                
## statusclosed                        
## statusipo                        ***
## statusoperating                     
## state_codeAL                        
## state_codeAR                        
## state_codeAZ                        
## state_codeCA                        
## state_codeCO                        
## state_codeCT                        
## state_codeDC                        
## state_codeDE                        
## state_codeFL                        
## state_codeGA                        
## state_codeHI                        
## state_codeIA                        
## state_codeID                        
## state_codeIL                        
## state_codeIN                        
## state_codeKS                        
## state_codeKY                        
## state_codeLA                        
## state_codeMA                        
## state_codeMD                        
## state_codeME                        
## state_codeMI                        
## state_codeMN                        
## state_codeMO                        
## state_codeMS                        
## state_codeMT                        
## state_codeNC                        
## state_codeND                        
## state_codeNE                        
## state_codeNH                        
## state_codeNJ                        
## state_codeNM                        
## state_codeNV                        
## state_codeNY                        
## state_codeOH                        
## state_codeOK                        
## state_codeOR                        
## state_codePA                        
## state_codeRI                        
## state_codeSC                        
## state_codeSD                        
## state_codeTN                        
## state_codeTX                        
## state_codeUT                        
## state_codeVA                        
## state_codeWA                        
## state_codeWI                        
## state_codeWV                        
## funding_rounds                   ***
## funding_round_typecrowdfunding      
## funding_round_typeother          .  
## funding_round_typepost-ipo          
## funding_round_typeprivate-equity ** 
## funding_round_typeseries-a          
## funding_round_typeseries-b          
## funding_round_typeseries-c+      ***
## funding_round_typeventure           
## raised_amount_usd                ***
## participants                     ***
## homepage_url_available              
## twitter_account_available        *  
## fundinground_duration            ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30270000 on 4211 degrees of freedom
## Multiple R-squared:  0.8298, Adjusted R-squared:  0.8271 
## F-statistic: 306.4 on 67 and 4211 DF,  p-value: < 2.2e-16
In classical regression model, the coefficient are not significant if the p-value is greater than 0.05. FRom the output given above, the estimates that significant are asteric in one "*“, two”**", etc according to their level of significance. We can then notice that state is not significant together with other factors that does not have any asterics or better put as coeficient with p-value greater than 0.05.
