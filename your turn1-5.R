library(nycflights13)
library(tidyverse)

a <- flights
head(a)
dim(a)
summary(a)
#year :2013년도만조사한것으로보임
#month : 1월부터 12월
#day : 1일부터 31일 
#month 와 day는 수치형이 아님 나중에 바꾸는게 좋겠음.
?flights
#dep_time, arr_time : 실제 출발 및 도착 시간(HHM 또는 HMM 형식), 현지 tz.
#음수가 나오는거는 실제로는 더빠르게 출발했다고 판단.
#sched-dep_time ,sched_arr_time: 예정된 출발 및 도착 시간(HHM 또는 HMM 형식), 현지 tz.
#dep_delay,arr_delay : 출발 및 도착 지연 시간(분) 음의 시간은 조기 출발/도착을 나타냅니다.
#carrier : 두 개의 문자 캐리어 약어. 이름을 알려면 항공사를 참고하세요.
#flight : 항공넘버 (필요없어보임.)
#tailnum : 평면 꼬리 번호. 추가 메타데이터는 평면을 참조하십시오.(필요없어보임)
#orign, dest :출발지와 목적지. 추가 메타데이터는 공항을 참조하십시오.
#air_time : 공중에서 보낸 시간(분)
#distance : 공항 간 거리(마일)입니다.
#hour, minute : 예정된 출발 시간이 시간과 분으로 나뉘었다.
#time_hour : 날짜시간 있음

library(tidyverse)
flights %>% head
flights %>%  dim
flights %>% summary

#day와 month를 수치형이 아닌것으로 바꿔야할거같음

#Q1
#1.
b <- flights %>% filter(dep_delay >= 120)  %>%  head(5)
b$carrier

#2.
c <- flights %>%  filter(dep_delay <=0 | arr_delay >0) %>%  head(5)
c$carrier
#3.
d <- flights %>%  filter(dep_delay >= 60 | arr_delay <= -30)  %>% head(5)

d$carrier

#4.
is.na(flights$dep_time)
sum(is.na(flights$sched_dep_time))
sum(is.na(flights$sched_arr_time))
#출발하지 안흥걸로보임 dep_time은 출발시간인데 출발하지않았은 NA로 된것으로 판단
#NA결측치들을 제거하는것이 올바른 판단인거 같음.
#출발한시간이없으니 지연된 출발시간이없다고 생각. 그래서 결측값갯수를 확인했는데 같다.
#물론 결측값갯수가 같다고 같은변수가 NA인지는 모르겠지만 추측할 수는 있다.
#그래서 출발을 안했으니 도착도안했募母槁底??? 도착도해봤지만 도착딜레이한것이랑 도착한것의 결측값이 다르다.
#예정된 출발시간과 예정된 도착시간은 결측값이없다.
#도착한 결측값이 더 많은것으로보아 아마 출발하고 추락하여 폭파된 것이나 실종되거나 버뮤다삼각지대로 사라진건가? ㄹㅇㅋㅋ
sum(is.na(flights$dep_time))
sum(is.na(flights$dep_delay))

sum(is.na(flights$arr_time))
sum(is.na(flights$arr_delay))

#결론 dep_time은 제거하여 결항되어 출발하지 않은것으로 판단 할 수 있다.
#그러면 dep_time에 관련된 행과 열을 다 지워야할

#your turn 2
#2-1)
flights %>% arrange(desc(dep_delay))



#dep_delay에서 음수는 조기출발이므로 지연된거서이 아니라고 판단
#그래서 음수를 제거하고 양수에서 찾음 제일빨리 출발한게 여러개가있음
#1이 제일 빠르게 출발한거라고판단 1여러개있는데 일단 맨위에걸로 채택
#2-2) 거리 = 속력 * 시간 
summary(flights)
flights$air_time
length(flights$distance)
length(flights$air_time)
sum(is.na(flights$distance))
sum(is.na(flights$air_time))

flights$speed <- flights$distance/flights$air_time
max(flights$distance/flights$air_time,na.rm=TRUE)
as.numeric(flights)
MOC <- flights %>% arrange(desc(speed))

MOC$carrier[1]
#속력을 구해서 데이터에 넣은후 데이터를 정렬하고
#솔겨이 가장 큰값의 carrier을 구했다.

#yourturn3
flights %>% select(starts_with("arr")|starts_with("dep"))

flights %>% select(year:day, ends_with("delay"))


#4번
flights %>% select(year:day, ends_with("delay"), distance, air_time) -> flights2
flights2 %>% 
  mutate(gain = dep_delay-arr_delay,
         speed = distance/air_time*60)
flights2 %>% 
  transmute(gain = dep_delay-arr_delay,
            speed = distance/air_time*60)
# Your turn 4
#1. dep_time과 sched_dep_time 저장형태를 잘 확인 후, 이를 각각 시 와 분을 구분하여 새로운 변수를 만들기.
flights %>% select(ends_with("dep_time")) -> flights2
flights2 %>% mutate(hour = dep_time %/% 60,
                    minute = dep_time %% 60)
#2. air_time과 arr_time - dep_time 간의 비교했을 때, 자신의 견해 쓰기
a <- flights %>% transmute(air_time - (arr_time-dep_time)) 
head(a)
length(which(a>0))
length(which(a<0))
#보통 비행기는 땅에서 달리다가 날라가기 때문에 air_time이 더 작은것이많고
#만약 air_time이 더크다면 아마 착륙이어려워서 공중에서 있어야할때 air_time이 더 클것이다
#그래서 대부분 air_time이 더 작고 드문경우에 착륙이 어려우므로 air_time이 더 크다

#3. dep_time, sched_dep_time 그리고 dep_delay 변수를 비교했을 때, 자신의 견해 쓰기
flights$delay <- flights %>% transmute(delay = dep_time - sched_dep_time )
flights$dep_delay
length(which(flights$delay == flights$dep_delay))
length(which(flights$delay != flights$dep_delay))
flights %>%  select(delay, dep_delay)
#흠 이거는 왜 다른지 모르겠네 잘못 작성한건가 근데 같은게 더 많긴해 같아야 정상인데
#출발시간을 잘못 체크하신걸수도.

#5번
a <- flights %>% group_by(carrier) %>% summarise(mean_1 = mean(dep_delay,na.rm=TRUE))
b <- flights %>% group_by(carrier) %>% summarise(var_1 = var(dep_delay, na.rm=TRUE))
a %>% arrange(desc(mean_1))
b %>% arrange(desc(var_1))
#F9가 가장평균이크고 HA가 분산이 가장크다.

c <- flights %>% group_by(month,day) %>% summarise(mean_2=mean(dep_delay, na.rm=T), n = n())
c %>% arrange(desc(mean_2))
#3월8일이 가장 크다.

d <- flights %>% group_by(month=3,day=8,carrier) %>% summarise(var_2 = var(dep_delay,na.rm = TRUE))

d %>% arrange((var_2))
#US가 가장작다.