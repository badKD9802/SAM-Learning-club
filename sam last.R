library(tidyverse)
setwd("C:/Users/Rprogram/Downloads/archive (2)")

train <- read.csv("train.csv",header=T)
test <- read.csv("test.csv", header = T)
Data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = T)



X <- read.csv("water_potability.csv",header=T)

glimpse(X)
summary(X)
#1. ph: pH 1. 물(0~14).
#2. 경도 : 비누 침전 용량(mg/L)
#3. 고체: 총 용존 고형분(ppm).
#4. 클로라민: 클로로아민의 양(ppm).
#5. 황산염: mg/L에 용해된 황산염의 양.
#6. 전도도: 물의 전기 전도도(μS/cm).
#7. 유기_탄소: 유기 탄소의 양(ppm).
#8. 트라이할로메탄: 트리할로메탄의 양(μg/L).
#9. 탁도: NTU 내 물의 발광 특성 측정.
#10. 휴대성: 물이 사람이 섭취하기에 안전한지 여부를 나타냅니다. 음용 -1 및 비음용 -0




library(tidyverse)
setwd("C:/Users/Rprogram/Downloads/archive (2)")

smart <- read.csv("Smart_City_index_headers.csv",header = T)
summary(smart)
smart <- smart[,-11]
smart <- smart[,-c(1,2,3)]
#Smart_Mobility : 도시 전체의 대중 교통 시스템, ICT, 접근성 기반 시설의 평가에서 계산된 지수.
#Smart_Environment : 환경 지속 가능성 영향, 모니터링 오염 및 도시 전체의 에너지 관리 시스템 이니셔티브에서 계산된 지수.
#Smart_Government : 전 세계 스마트 시티의 투명한 거버넌스 및 개방형 데이터 이니셔티브에 대한 비교 연구에서 계산된 지수입니다. 또한 의사 결정에 시민 참여가 포함되었습니다.
#Smart_Economy : 도시 전체의 생산성, 경제 활력, 기업가 정신 및 혁신에 대한 지원의 글로벌 비교를 통해 계산된 ndex.
#Smart_People : 전 세계의 사회 문화적 다양성, 교육 시스템 및 이를 지원하는 부대 시설을 비교하여 계산한 지수
#Smart_Living : 의료 서비스, 사회 보장 및 주택 품질에 대한 측정항목을 측정하여 계산한 지수입니다.
#반응변수 #SmartCity_Index : 스마트시티 슈퍼그룹을 기반으로 하는 스마트시티 모델에 대한 총점.
library(ggplot2)
library(ggExtra)
library(tidyr)
library(corrplot)

#1
plot(SmartCity_Index ~ Smart_Mobility, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_Mobility)
#2
plot(SmartCity_Index ~ Smart_Environment, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_Environment)

#3
plot(SmartCity_Index ~ Smart_Government, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_Government)

#4
plot(SmartCity_Index ~ Smart_Economy, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_Economy)

#5
plot(SmartCity_Index ~ Smart_People, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_People)

#6
plot(SmartCity_Index ~ Smart_Living, data=smart, cex=1)
cor(smart$SmartCity_Index, smart$Smart_Living)


plot(smart)
X <- cor(smart[,1:6])
corrplot(X)
smart$Smart_Living

#서로 상관관게가 Economy과 Enviromnet이 살짝있는거같고 living과 goverment가 있음으로 보임. 근데 묶을만큼
#자연에서 얻는 뭐 그런게 관련있어서 경제랑 환경이랑 관련이 있는게아닐까
#의료시설 이런 서비스 만족도가 높은거랑 정책이랑 관련이 있으니 서로상관관계가 있을 수도 있겠다.
#그렇다고 상관관계가 엄청 높지않아서 묶을필요는 없겠다고 판단.

n <- nrow(smart)
idx <- 1:n
training_idx <- sample(idx, n*.70)
test_idx <- setdiff(idx, training_idx) 
length(training_idx)
length(test_idx)

train <- smart[training_idx,]
test <- smart[test_idx,]

xx <- model.matrix(Smart_Living ~ .-1,smart)
x <- xx[training_idx,]
y <- ifelse(train$Smart_Living > 6400,1,0)
dim(x)

library(glmnet)

ad_glmnet_fit <- glmnet(x,y)
plot(ad_glmnet_fit,xvar="lambda")
ad_glmnet_fit
#디폴트값은 라쏘.

ad_cvfit <- cv.glmnet(x,y,family = 'binomial')
summary(ad_cvfit)
plot(ad_cvfit)

#예측
coef(ad_cvfit, s="lambda.min")
ad_cvfit$lambda.min
#해석
ad_cvfit$lambda.1se

length(which(coef(ad_cvfit,s="lambda.min")>0))
length(which(coef(ad_cvfit,s="lambda.1se")>0))

# 나는 x들을 넣었을때 y값인 스마트시티에 가까운지를 포기위할거기때문에
# 예측보다는 해석에 가깝다고 생각중.

# 그래서 나는 min보다는 1se를 쓸것

set.seed(3000)
cv0 <- cv.glmnet(x,y,alpha=0 , family='binomial')
length(which(coef(cv0,s="lambda.min")>0))
length(which(coef(cv0,s="lambda.1se")>0))

y_obs <- ifelse(test$SmartCity_Index > 6400 , 1, 0)
yhat_glmnet <- predict(ad_cvfit, s="lambda.1se", newx=xx[test_idx,], type="response")
yhat_glmnet <- yhat_glmnet[,1]
library(ROCR)
pred_glmnet <- prediction(yhat_glmnet,y_obs)
perf_glmnet <- performance(pred_glmnet, measure="tpr" ,x.measure="fpr")
plot(perf_glmnet, col='blue',main="ROC Curve")
abline(0,1)
performance(pred_glmnet,"auc")@y.values[[1]]

