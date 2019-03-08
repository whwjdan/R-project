### 빅데이터 교육 프로젝트 네이버카페 크롤링 및 추출 데이터 기반 텍스트마이닝



wallet_S <- read.csv("C:/R데이터/zil/wallet_S.csv", head=F, na.strings=c(""))
userDic1 <-read.csv("C:/Users/BUK/PycharmProjects/untitled/user_dic.csv", head=F, na.strings=c(""))
s3Title<-read.csv("C:/Users/BUK/PycharmProjects/untitled/review_wallat_title.csv", head=T, na.strings=c(""))
s3Comment<-read.csv("C:/Users/BUK/PycharmProjects/untitled/review_wallat_comment.csv", head=F, na.strings=c(""))
## 1. csv import
library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY" = "")

## 아마존 S3 저장하기
s3save(s3Title, bucket = "whwjdans3/product", object = "review_wallat_title.Rdata")
s3save(s3Comment, bucket = "whwjdans3/product", object = "review_wallat_comment.Rdata")
## 아마존 S3로 로드하기
e <- new.env()
s3load(object = "wallet_T.Rdata", bucket = "whwjdans3/product", envir = e)
s3load(object = "wallet_C.Rdata", bucket = "whwjdans3/product", envir = e)
ls(e)
wallet_C <- e[["wallet_C"]]
wallet_T <- e[["s3Title"]]
rm(e)
######################################### 아마존 파일 저장 & 불러오기###############################
wallet_C <- data.frame(wallet_C[c(1:150),])

data.frame(matrix(nrow=0, ncol=2))


buildDictionary(user_dic=userDic1,replace_usr_dic = T)
head(get_dictionary('user_dic'))
extractNoun("보테가베네타 남자 반지갑  ")
as.character(userDic1[1,])

grep(as.character(userDic1[1,]),wallat_title$title)
length(grep(as.character(userDic1[1,]),wallat_title$title))


########################## 타이틀별 상품 or 브랜드 키워드 찾아오기 #####################
findLength <- function(title, dic){
  dataAA <- data.frame(matrix(nrow=0, ncol=2))
  for (i in 1:nrow(dic)){
    aa <- as.character(dic[i,])
    len <- (length(grep(aa,title))  )
    dataAA <- rbind(dataAA,data.frame(sub = aa,number = len))
  } 
  return(dataAA)
}
####################################### 댓글들 찾아오기#############################
findComment <- function(title,comment,query){
  dataBB <- data.frame(matrix(nrow=0, ncol=150))
  findNums <-(grep(query, title))
  for(i in 1:length(findNums)) {
    dataBB <- rbind(dataBB, comment[findNums[i],])
  }
  return(dataBB)
}


bb <- findComment(wallet_T$title,wallet_C,"질스튜")


################################  텍스트마이닝 - 토픽모델링 ########################################

#데이터 호출 섹션
install.packages('aws.s3')
library(aws.s3)
Sys.setenv("AWS_ACCESS_KEY_ID" = "", "AWS_SECRET_ACCESS_KEY" = "")

e <- new.env()
s3load(object = "zill_C.Rdata", bucket = "whwjdans3/brand", envir = e)
ls(e) # env 오브젝트 안에 저장된 R데이터의 리스트 확인

#데이터 선언해 리스트 요소명으로 할당
zill_C <- e[["s3Comment"]]
rm(e) # e 삭제

findComment2 <- function(comment,query){
  dataCC <- ""
  findSearch <-comment[grep(query,comment[,1]),c(2:length(comment))]
  for(i in 1:nrow(findSearch)) {
    for(j in 1:ncol(findSearch)){
      if(is.na(as.character(findSearch[i,j])) == FALSE){
        dataCC <- append(dataCC, as.character(findSearch[i,j]))  
      }
      
    }
  }
  return(dataCC)
}
ac_C<- read.csv("C:/R데이터/zil/accessory_comment.csv", head=F, na.strings=c(""))
sam_C <- read.csv("C:/R데이터/zil/sam50_comm.csv", head=F, na.strings=c(""))
zill_C <- read.csv("C:/R데이터/zil/zill800_comm.csv", head=F, na.strings=c(""))
ac_C <- findComment2(ac_C,"")
sam_C <- findComment2(sam_C,"")
zill_C <- findComment2(zill_C,"")
# 텍스트 마이닝 섹션
install.packages('tm')
install.packages('KoNLP')
install.packages('rJava')
install.packages('stringr')
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(KoNLP)
library(rJava)
library(stringr)
library(wordcloud)
library(RColorBrewer)

str(wallet_T)




#$카페 게시글 문서의 컨텐츠 영역을 하나의 벡터 문서로 통합 
mytext <- as.vector(unlist(sam_C))

mytext[1]
mytext[3]
mytext[8]
mytext <- CC
#----------------전처리(데이터 정제) 섹션----------------

#for
mytext[5]
#

# 불용어사전(스톱리스트) 호출
stopwordDic <- read.csv("C:/R데이터/zil/stopword.csv",stringsAsFactors=FALSE, header=FALSE, encoding='euc-kr')
stopwordDic <- unlist(as.list(stopwordDic$V1))
str(stopwordDic)
un <- unlist(ex1)
# 추출 명사에 대한 불용어 처리
for(i in 1:length(un)) {
  for(j in 1:length(stopwordDic)) {
    if (un[i] == stopwordDic[j]) {
      un[i] <- NA
      break
    }
  }
}
str(un) # 처리 결과 확인



mytext <- str_replace_all(mytext,"쇼핑 후기는 상세하게 작성될 공간입니다, 실사가 없거나 무성의한 게시는 패션토크를 이용해주세요- (이동 처리 될 수 있습니다.)"," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
mytext <- str_replace_all(mytext,""," ")
# 숫자 삭제, 공란(" ")으로 대체
mytext <- str_replace_all(mytext,"\\d"," ")
# 특수문자 삭제, 공란(" ")으로 대체
mytext <- str_replace_all(mytext,"\\W"," ")

# 영문표현 전부 삭제 (영문으로 브랜드, 제품명을 작성하는 경우 있으니 삭제에 주의)
mytext <- str_replace_all(mytext, "[[:lower:]]","")
mytext <- str_replace_all(mytext, "[[:upper:]]","")

# 초성활용, 이모티콘 등에 자주 사용되는 한글 자모음 제거
mytext <- str_replace_all(mytext,"ㅠ"," ")
mytext <- str_replace_all(mytext,"ㅜ"," ")
mytext <- str_replace_all(mytext,"ㅋ"," ")
mytext <- str_replace_all(mytext,"ㅎ"," ")
mytext <- str_replace_all(mytext,"ㄱ"," ")
mytext <- str_replace_all(mytext,"ㅅ"," ")
mytext <- str_replace_all(mytext,"ㅇ"," ")
mytext <- str_replace_all(mytext,"ㅈ"," ")
mytext <- str_replace_all(mytext,"ㄹ"," ")
mytext <- str_replace_all(mytext,"ㅁ"," ")

# 공란이 두 칸 이상 발생하면 한 칸으로 축소," ")
mytext <- str_replace_all(mytext,"[[:space:]]{1,}"," ")

# 전처리를 마친 mytext 벡터 데이터를 tm 패키지에서 사용할 수 있는 코퍼스 형태로 변환
corpusMT <- Corpus(VectorSource(mytext))

# 코퍼스 구조 확인을 위해 코퍼스 내 첫번째, 두번째 문서에 대한 정보를 확인하는 명령어
inspect(corpusMT[1])
inspect(corpusMT[2])
inspect(corpusMT[3])
inspect(corpusMT[4])

# 명사 추출
ex3 <- extractNoun(corpusMT)
??extractNoun
findLengthC <- function(title, dic){
  dataAA <- data.frame(matrix(nrow=0, ncol=2))
  for (i in 1:nrow(dic)){
    subDic <- as.character(dic[i,])
    len <- sum(title[grep(subDic,title$Var1),2])
    dataAA <- rbind(dataAA,data.frame(sub = subDic,number = len))
  } 
  return(dataAA)
}
s3search <- read.csv("C:/R데이터/zil/comment_search1.csv", head=F, na.strings=c(""))
s3search2 <- read.csv("C:/R데이터/zil/comment_search2.csv", head=F, na.strings=c(""))
s3search3 <- read.csv("C:/R데이터/zil/acc_C1.csv", head=F, na.strings=c(""))
#단어 빈도 테이블 생성 (wallet_T 기준 단어 14728개 나옴)
tb3 <- table(unlist(ex3))
tb <- table(un)
tt3 <- as.data.frame(tb3)
test3 <- findLengthC(tt3,s3search3)
write.csv(test3, file = "C:/R데이터/zil/samso_sub_C3.csv")
# 테이블에서 빈도 내림차순 정렬 후 상위 50개 표시
grep("가격",tt2$Var1)
tt[1,1]
sum(tt[grep("가격",tt$Var1),2])
head(sort(tb, decreasing=T), 50)
tb
sum(totalnum[,2])

#빈도테이블 전체로 워드클라우드 
palete <- brewer.pal(9, "Set1")
x11()
tb[3]
wordcloud(
  names(tb),
  freq=tb,
  scale=c(5,1),
  rot.per=0.5,
  min.freq=7,
  random.order=F,
  random.color=T,
  colors=palete
)
tb[3]
grep("279",tb)[1]
tb[grep("132",tb)[1]] <- 200
install.packages("ggplot2")
library(ggplot2)
acc_C3 <- read.csv("C:/R데이터/zil/acc_sub_C3.csv", head=T, na.strings=c(""))
zill_C2 <- read.csv("C:/R데이터/zil/zill_sub_C2.csv", head=T, na.strings=c(""))
samso_C2 <- read.csv("C:/R데이터/zil/samso_sub_C2.csv", head=T, na.strings=c(""))
acc_C3[2,]
??ggplot2
library("ggsci")
library("gridExtra")
cbPalette <- c("#066FA5", "#F70044", "#F6D600", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(acc_C3, aes(x= Noun, y= num, fill = sensitive)) + geom_bar(stat = "identity") + geom_text(aes(label = num), vjust = 1.5, color = "white") + scale_fill_manual(values=cbPalette)
ggplot(zill_C2, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity") + geom_text(aes(label = num), vjust = 0.8, color = "white", fontface = "bold") + theme_dark() + theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")) +coord_cartesian( ylim = c(0, 500)) + scale_fill_manual(values=cbPalette)+
  ggtitle("질스튜어트 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))
ggplot(acc_C3, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity") + geom_text(aes(label = num), vjust = 0.8, color = "white", fontface = "bold") + theme_dark() + theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")) +coord_cartesian( ylim = c(100, 2500)) + scale_fill_manual(values=cbPalette) +
  ggtitle("악세사리 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))
ggplot(samso_C2, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity") + geom_text(aes(label = num), vjust = 0.8, color = "white", fontface = "bold") + theme_dark() + theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")) +coord_cartesian( ylim = c(0, 40)) + scale_fill_manual(values=cbPalette) +
  ggtitle("샘소나이트 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))

ggplot(zill_C2, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity")  +coord_cartesian( ylim = c(0, 500)) + scale_fill_manual(values=cbPalette)+
  ggtitle("질스튜어트 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))
ggplot(acc_C3, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity")  + theme_dark() + theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")) +coord_cartesian( ylim = c(100, 2500)) + scale_fill_manual(values=cbPalette) +
  ggtitle("악세사리 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))
ggplot(samso_C2, aes(x= reorder(Noun, re), y= num, fill = sensitive)) + labs(x="",y="") +
  geom_bar(stat = "identity")   +coord_cartesian( ylim = c(0, 40)) + scale_fill_manual(values=cbPalette) +
  ggtitle("샘소나이트 감정단어")  + theme(plot.title=element_text(hjust = 0.5,family="NanumGothicCoding", face="bold", size=20, vjust=2, color="black"))


tophit <- tophitters2001[1:25, ]
acc_C3$pos <
  csub$pos <- csub$Anomaly10y >= 0

