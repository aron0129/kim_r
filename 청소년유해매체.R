setwd("C:\\Users\\minji\\Desktop\\청소년매체위험실태조사\\총괄_20201223_65806_데이터")

getwd()
mdis <- read.csv("총괄_2018_20201223_65806.csv",sep=",")
str(mdis)

#지역별
aa<-table(mdis[,417])
barplot(aa)

#학교급 #1초등2중학3고등
bb<-table(mdis[,413])
barplot(bb)

####1 폭력
a<-table(mdis[,112])
#11(1) 욕설이나 무시하는 말을 지속적으로 들음
#11(2) 맞거나 발로 차이거나 또는 물건으로 인해 다침
#11(3) 돈이나 물건을 빼앗김
#11(4) 때리거나 괴롭히겠다고 위협을 당함
#11(5) 왕따(따돌림)를 당함
#11(6) 강제 심부름(빵 셔틀 등)을 당함
#11(7) 사이버(인터넷)상의 따돌림이나 괴롭힘을 당함
a #응답1 1345명 응답2 14302명 응답9 10명
barplot(a)

####2 성폭력
b<-table(mdis[,134])
b #응답1 421명 응답2 15233명 응답9 3명
#15(1)의도적으로 계속 따라다니면서 괴롭히는 스토킹을 당함
#15(2)말이나 눈짓, 몸짓으로 성적 모욕감을 주거나 괴롭힘을 당함
#15(3)고의로 신체를 건드리거나 몸을 밀착시킴을 당함
#15(4)강제로 성관계 시도를 당함
#15(5)사이버(인터넷, 채팅앱)상의 스토킹이나 성희롱을 당함
#15(6)사이버(인터넷, 채팅앱)상의 조건만남(성매매) 유인을 당하거나 강요받음

c<-table(mdis[,159]) #최근 1년 동안 심각하게 가출을 고민해 본 적이 있나요?
    c #응답1 1968 #응답2 13672

d<-table(mdis[,180]) #지금까지 1잔 이상의 술을 마셔 본 적이 있나요?
d

e<-table(mdis[,214]) #지금까지 담배를 피워본 적이 있나요?
e

f<-table(mdis[,266])
f

#결론 학교폭력에 노출되어 있는 친구들은 이런친구들이다!!
table(mdis[,112])
vioyes<-mdis[mdis[,112]==1,]
nrow(vioyes)
viono<-mdis[mdis[,112]==2,]

library(corrplot)

#22 이용 여부 - 9) 인터넷 실시간 방송 및 동영상 사이트
#28 이용 여부 - 11) 인터넷 소셜네트워크서비스
#34 최근 1년 동안, ‘청소년관람불가’, ‘19세이상시청가’로 표시된 성인용 TV 프로그램, 영화, 동영상 등을 본 적 있나요?

#이 밑 3가지는 34번째 열이나 이 밑에 3가지 중에 하나만 선택하기!!
#49 1) 나 스스로 성인용 영상물을 보지 않으려 해도 일상생활에서 쉽게 보게 된다
#50 2) 나는 성인용 영상물에 대해 생각해 본 적도 없고 관심도 없다
#51 3) 나는 성인용 영상물을 보지 않으려고 스스로 노력한다

#93 최근 1달 동안, 밤 12시부터 새벽 6시 사이에 인터넷 게임을 이용한 적이 있나요?

#96 1) 다른 사람의 아이디(ID) 사용하여 게임
#97 2) 다른 사람의 주민등록번호 사용하여 게임

#99 19세 이상 이용 가능한 영화, 게임, 동영상, 잡지, 사진 등으로 인한 피해 예방 교육 여부
#101 2) 온라인 도박으로 인한 피해 예방 교육 여부
#103 3) 건전한 인터넷/스마트폰 이용 교육 여부
cora<-cor(mdis[,c(2,3,5,28,29,31,32,33,34,46,47,51,52,53,71,78,82,103,111,112,114,116,135,136,154,155,168,169,170,174)])
corrplot(cora, method="shade")
table(mdis[,341])

#159 최근 1년 동안 심각하게 가출을 고민해 본 적이 있나요?

#174 최근 1년 동안 성교육을 받은 적이 있나요?

#176 핫식스(HOT6), 레드불(RedBull), 박카스처럼 카페인이 많이 들어 있는 고카페인(또는 에너지)

#180 지금까지 1잔 이상의 술을 마셔 본 적이 있나요?
#183 최근 1개월(한 달) 동안 한 잔 이상 술을 마신 적이 있나요?

#214 지금까지 담배를 피워본 적이 있나요?
#215 최근 1개월(한 달) 동안, 담배를 피워본 적이 있나요?

#266 최근 1년 동안,  환각성 물질을 이용해 본 적이 있나요?

#287 1) 전자오락실 이용여부
#288 2) 술집(소주방, 호프집 등)
#289 3) PC방
#290 4-1) 노래방 일반노래방
#291 4-2) 노래방 코인노래방
#292 5) 찜질방
#293 6) VR체험카페
#310 4) 멀티방/룸카페

#341 현재 아르바이트를 하고 있거나, 올 해(2018년) 아르바이트를 한 적이 있나요?

##나자신
#391 1) 나는 다른 사람들에 의해 통제 받는다
#392 2) 내 일을 진행하는 방법을 내 스스로 결정할 기회가 적다
#393 3) 일상생활에서 나는 자주 남이 시키는 대로 해야만 한다
#394 4) 나는 대체로 내 생각과 의견을 자유롭게 표현할 수 있다
#395 5) 나는 내가 내 인생을 어떻게 살아갈지 스스로 결정할 수 있다
#396 6) 어떤 일을 할 때 내 생각대로 일을 처리하기 보다는 다른 사람의 생각이나 처리 방식을 따를 때가 많다

##가족
#397 1) 내가 사랑과 보살핌을 받고 있다고 느끼게 해준다
#398 2) 내가 고민되는 문제에 대해 이야기하면 기꺼이 들어줄 것이다
#399 3) 내가 마음 놓고 의지할 수 있다
#400 4) 항상 나의 일에 관심을 갖고 걱정해준다

##친구들
#402 1) 내가 사랑과 보살핌을 받고 있다고 느끼게 해준다
#403 2) 내가 고민되는 문제에 대해 이야기하면 기꺼이 들어줄 것이다
#404 3) 내가 마음 놓고 의지할 수 있다

##선생님
#407 1) 내가 사랑과 보살핌을 받고 있다고 느끼게 해준다
#408 2) 내가 고민되는 문제에 대해 이야기하면 기꺼이 들어줄 것이다

#학교급
#413 1=초 2=중 3=고

#중+고등학생만 선택
middlehigh<-mdis[mdis[,413]==2|mdis[,413]==3,]
table(middlehigh[,112])

#이상치제거
middlehigh<-middlehigh[!(middlehigh[,22]==9),]
middlehigh<-middlehigh[!(middlehigh[,34]==9),]
middlehigh<-middlehigh[!(middlehigh[,93]==9),]
middlehigh<-middlehigh[!(middlehigh[,103]==9),]
middlehigh<-middlehigh[!(middlehigh[,159]==9),]
middlehigh<-middlehigh[!(middlehigh[,176]==9),]
middlehigh<-middlehigh[!(middlehigh[,180]==9),]
middlehigh<-middlehigh[!(middlehigh[,214]==9),]
middlehigh<-middlehigh[!(middlehigh[,266]==9),]
middlehigh<-middlehigh[!(middlehigh[,289]==9),]
middlehigh<-middlehigh[!(middlehigh[,291]==9),]
middlehigh<-middlehigh[!(middlehigh[,310]==9),]
middlehigh<-middlehigh[!(middlehigh[,391]==9),]
middlehigh<-middlehigh[!(middlehigh[,397]==9),]
middlehigh<-middlehigh[!(middlehigh[,402]==9),]
middlehigh<-middlehigh[!(middlehigh[,407]==9),]


#target변수 생성
vioyes<-middlehigh[middlehigh[,112]==1,]
viono<-middlehigh[middlehigh[,112]==2,]
nrow(vioyes)
nrow(viono)
vioyes$target<-rep(1,835)
viono$target<-rep(0,9764)
middlehigh<-rbind(vioyes,viono)

#변수명지정
middlehigh$streaming<-middlehigh[,22] #1이용함 2이용안함
middlehigh$adultvideo<-middlehigh[,34] #1본적있음 2본적없음
middlehigh$midnightgame<-middlehigh[,93] #1새벽게임한적있음 2없음
middlehigh$preventedu<-middlehigh[,103] #1인터넷스마트폰교육받은적있음 2없음
middlehigh$runhome<-middlehigh[,159] #1가출고민있음 2없음
middlehigh$energydrink<-middlehigh[,176] #1거의매일마심 2. 1주1,2번 3.한달에1,2번 4.전혀안함
middlehigh$alcholdrink<-middlehigh[,180] #1술마신적있다 2없다.
middlehigh$smoke<-middlehigh[,214] #1담배핀적있다 2없다
middlehigh$drug<-middlehigh[,266] #1환각물질해봤다 2안해봄
middlehigh$pcroom<-middlehigh[,289] #1피씨방경험있음 2없음
middlehigh$coinsing<-middlehigh[,291] #1코노경험있음 2없음
middlehigh$roomcafe<-middlehigh[,310] #1룸카페경험있음 2없음

middlehigh$control<-middlehigh[,391] #나는 다른사람에의해 통제받는다
#안그렇다고 생각할수록 1번 vs  그렇다고 생각할수록 4번
middlehigh$familylove<-middlehigh[,397] #가족의 사랑과 보살핌 느낀다.
#안그렇다고 생각할수록 1번 vs  그렇다고 생각할수록 4번
middlehigh$friendlove<-middlehigh[,402] #친구의 사랑과 보살핌 느낀다.
#안그렇다고 생각할수록 1번 vs  그렇다고 생각할수록 4번
middlehigh$teacherlove<-middlehigh[,407] #선생님의 사랑과 보살핌 느낀다.
#안그렇다고 생각할수록 1번 vs  그렇다고 생각할수록 4번

#변수 reference level지정
?factor
middlehigh$streaming<-factor(middlehigh$streaming)
middlehigh$streaming<-relevel(middlehigh$streaming,ref=2)

middlehigh$adultvideo<-factor(middlehigh$adultvideo)
middlehigh$adultvideo<-relevel(middlehigh$adultvideo,ref=2)

middlehigh$midnightgame<-factor(middlehigh$midnightgame)
middlehigh$midnightgame<-relevel(middlehigh$midnightgame,ref=2)

middlehigh$preventedu<-factor(middlehigh$preventedu)
middlehigh$preventedu<-relevel(middlehigh$preventedu,ref=2)

middlehigh$runhome<-factor(middlehigh$runhome)
middlehigh$runhome<-relevel(middlehigh$runhome,ref=2)

middlehigh$alcholdrink<-factor(middlehigh$alcholdrink)
middlehigh$alcholdrink<-relevel(middlehigh$alcholdrink,ref=2)

middlehigh$smoke<-factor(middlehigh$smoke)
middlehigh$smoke<-relevel(middlehigh$smoke,ref=2)

middlehigh$drug<-factor(middlehigh$drug)
middlehigh$drug<-relevel(middlehigh$drug,ref=2)

middlehigh$pcroom<-factor(middlehigh$pcroom)
middlehigh$pcroom<-relevel(middlehigh$pcroom,ref=2)

middlehigh$coinsing<-factor(middlehigh$coinsing)
middlehigh$coinsing<-relevel(middlehigh$coinsing,ref=2)

middlehigh$roomcafe<-factor(middlehigh$roomcafe)
middlehigh$roomcafe<-relevel(middlehigh$roomcafe,ref=2)

#모형적합 #에너지드링크, 나,가족,친구,선생님만 주의
fit<-glm(target~factor(streaming)+factor(adultvideo)+factor(midnightgame)
         +factor(preventedu)+factor(runhome)
         +scale(energydrink)+factor(alcholdrink)+factor(smoke)+factor(drug)
         +factor(pcroom)+factor(coinsing)+factor(roomcafe)+scale(control)
         +scale(familylove)+scale(friendlove)+scale(teacherlove),family=binomial,data=middlehigh)

summary(fit)
anova(fit)
#결과
#실시간라방,성인물,새벽게임,예방교육한사람 가출한사람 술먹은사람 담배핀사람 
#약물한사람 피시방가본사람 룸카페가본사람 주변에서 나를제한다고느낄수록 가족이 사랑한다고 느낄수록 
#그렇지 않은 사람보다 학폭겪을확률 up

#반대의 경우
#에너지드링크 마시지 않은사람 코인노래방 이용안한사람 친구사랑못느끼는사람 선생님사랑못느끼는사람
#그런 사람보다 학폭겪을확률 up

########가설 종속변수 한번이라도 학교폭력 당한적있음 #112
#시골은 대도시와 비교해 학교폭력이 심하다?
#418 지역규모 1특별/광역시 2중소도시 3읍지역 4면지역 5도서벽지
#결론 : 유의미한 결과 없음
middlehigh$city<-middlehigh[,418]
mosaic(~city+target,data=middlehigh,main="5일수록시골",shade = TRUE, legend = TRUE)


#초중고에 따른 학교폭력 어떻게 변하는지?
#school변수 1초등학교 2중학교 3고등학교
#결론 초등학교,중학교는 학폭을 겪는 실제빈도가 기대빈도보다 크고 
#고등학교는 학폭을 겪는 실제빈도보다 기대빈도가 트다
vioyes<-mdis[mdis[,112]==1,]
viono<-mdis[mdis[,112]==2,]
nrow(vioyes)
nrow(viono)
vioyes$target<-rep(1,1345)
viono$target<-rep(0,14302)
emh<-rbind(vioyes,viono)
emh$school<-emh[,413]
mosaic(~school+target,data=emh,shade = TRUE, legend = TRUE)
#결론 초등학교,중학교는 학폭을 겪는 실제빈도가 기대빈도보다 크고 
#고등학교는 학폭을 겪는 실제빈도보다 기대빈도가 트다





#남녀공학 남학교 여학교에 따라 학폭 어떻게 달라지는지?
#1남학교 2여학교 3남녀공학
#결론 여학교의 경우 학폭 겪는 실제빈도보다 기대빈도가 크고, 
#남녀공학학교의 경우 학폭겪는 실제빈도가 기대빈도보다 크다
emh$menwomen<-emh[,412]
mosaic(~menwomen+target,data=emh,shade = TRUE, legend = TRUE)



#코노경험자 vs 무경험자 학폭 어떻게 달라지나?
#유의미한 결과 없음
mosaic(~coinsing+target,data=middlehigh,shade = TRUE, legend = TRUE)



##개별변수로 깊숙히 들어가기
summary(fit)

#adultvideo에 대한것!
a<-table(middlehigh[,36]) #티비방송 #1나이확인함 2확인안함 3확인하기도 안하기도함
barplot(a)
a<-table(middlehigh[,38]) #인터넷포털사이트 #1나이확인함 2확인안함 3확인하기도 안하기도함
barplot(a)
a<-table(middlehigh[,40]) #인터넷실시간방송및동영상사이트 #1나이확인함 2확인안함 3확인하기도 안하기도함
barplot(a)
a<-table(middlehigh[,42]) #인터넷/모바일 메신저(카톡,라인) #1나이확인함 2확인안함 3확인하기도 안하기도함
barplot(a)
a<-table(middlehigh[,44]) #인터넷 소셜네트워크서비스
barplot(a)
a<-table(middlehigh[,46]) #파일다운로드 사이트
barplot(a)
a<-table(middlehigh[,48]) #스마트폰 앱
barplot(a)


#midnightgame에 대한것 #새벽에 게임한적있냐
a<-table(middlehigh[,94]) #1거의매일 #2 1주일에 1~2회 #3 1달에 한두번 #주말,공휴일에만 
barplot(a)


#preventedu #예방교육
a<-table(middlehigh[,104]) #예방교육도움정도 1전혀안됨 4매우도움됨
barplot(a)

#runhome #가출경험
a<-table(middlehigh[,160]) #1.없다 5.5회이상
barplot(a)
runhome1<-middlehigh[!(middlehigh[,160]==1),]
a<-table(runhome1[,160]) #최근 1년 동안 가출을 경험한 적이 있나요? 있다면, 몇 회 정도인가요?
barplot(a)
a<-table(middlehigh[,161]) #가출기간 1.하루 5.한달이상
barplot(a)
a<-table(middlehigh[,163]) #청소년쉼터 1.알고있다 2.모른다
barplot(a)
a<-table(middlehigh[,165]) #청소년상담복지센터 1.알고있다 2.모른다
barplot(a)
a<-table(middlehigh[,167]) #청소년전화1388 1.알고있다 2.모른다
barplot(a)

#smoke 
#결론 담배어디서 구했는지 찾아서 조지자

#drug
#결론 약물 어디서 구했는지 찾아서 조지자

#pcroom
a<-table(middlehigh[,316]) 
#피시방청소년이용방법 인식
#1청소년이 자유롭게 언제든 출입할 수 있는 곳이다 
#2청소년이 출입할 수 있는 곳이지만, 심야 시간(밤 10시 이후)에는 이용할 수 없다
#3청소년 출입이 금지된 곳이다
#4청소년 출입에 관해 잘 모르겠다
#결론 인식에는 문제없음
barplot(a)

#coinsing
a<-table(middlehigh[,318])
#코노청소년이용방법 인식
#1청소년이 자유롭게 언제든 출입할 수 있는 곳이다 
#2청소년이 출입할 수 있는 곳이지만, 심야 시간(밤 10시 이후)에는 이용할 수 없다
#3청소년 출입이 금지된 곳이다
#4청소년 출입에 관해 잘 모르겠다
#결론 모른다고 응답한 사람이 꽤 있음 
barplot(a)


#roomcafe
a<-table(middlehigh[,311]) 
#멀티방/룸까페 나이확인
#1나이를 확인하고 들어가게 함
#2나이를 확인하지 않고 들어가게 함
barplot(a)