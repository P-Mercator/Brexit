

library(magrittr)
library(dplyr)

parliament <- read.table("data/parliament.csv",header=F,skip=16,sep=",",nrows=401,strip.white=TRUE)[,c(1,6,7,8,9,10,11,12)]
colnames(parliament) <- c("Area","Con","Lab","LD","UKIP","Green","BNP","SNP")
parliament$SNP <- as.integer(as.character(parliament$SNP))
winning.party <- function(region){
  
  return(names(which.max(region)))

}

parliament$Won <- apply(parliament, 1, winning.party)%>%
  lapply(function(x) ifelse(is.null(x), NA, x))%>%
  unlist()

head(parliament)




age <- read.csv("data/age.csv")[-1,]%>%
  select(GEO_LABEL,GEO_CODE,F184,F185)
colnames(age) <- c("Area","Code","Mean","Median")
head(age)




qualification <-  read.csv("data/qualification.csv")[-1,]%>%
  select(GEO_LABEL,GEO_CODE,F186,F187,F192)
qualification$NoQualification <- round(100 * as.numeric(as.character(qualification$F187))/as.numeric(as.character(qualification$F186)))
qualification$UniQualification <- round(100 * as.numeric(as.character(qualification$F192))/as.numeric(as.character(qualification$F186)))
qualification <- select(qualification,GEO_LABEL,GEO_CODE,NoQualification,UniQualification)
colnames(qualification)[1:2] <- c("Area","Code")
head(qualification)




english <- read.csv("data/english.csv")[-1,]%>%
  select(GEO_LABEL,GEO_CODE,F792,F793,F86631,F322322,F322718)
english_England <- english[as.character(english$F86631)!="",]
english_England$Fluent <- round(100 * as.numeric(as.character(english_England$F793)) / as.numeric(as.character(english_England$F792)))

english_Scotland<- english[as.character(english$F322322)!="",]
english_Scotland$Fluent <- round(100 * as.numeric(as.character(english_Scotland$F322322)) / as.numeric(as.character(english_Scotland$F792)))

english_Ireland <- english[as.character(english$F322718)!="",]
english_Ireland <- data.frame(GEO_LABEL="Northern Ireland",GEO_CODE=NA,Fluent=round(100 * sum(as.numeric(as.character(english_Ireland$F322718)))/sum(as.numeric(as.character(english_Ireland$F792)))))

english <- rbind(select(english_England,GEO_LABEL,GEO_CODE,Fluent),select(english_Scotland,GEO_LABEL,GEO_CODE,Fluent))%>%
  rbind(english_Ireland)

colnames(english)[1:2] <- c("Area","Code")
head(english)




brexit <- read.csv("data/result.csv")%>%
  select(Region,Area_Code,Area,Pct_Remain,Pct_Leave)
colnames(brexit)[2] <- "Code"
head(brexit)




                                        # merge age, english, qualification
census <- inner_join(age,qualification,by = "Code")%>%
  left_join(english,by = "Code")

head(census)
tail(census)

censusBrexit <- left_join(brexit,census,by = "Code")
head(censusBrexit)
length(censusBrexit$Code)
colnames(censusBrexit)[3] <- "Area"



closest.match <- function(s,v){
  agrep(s,v,ignore.case=T,value=T,max.distance = 0.1, useBytes = FALSE)
}

match.closest <- function(toFind, charList){

  match <- vector('character')
  for (string in toFind){
    matches <- closest.match(string,charList)
    if(length(matches) == 0){
      match <- c(match,NA)
    }
    else if(length(matches) > 1){
      cat(string)
      print("Select and press Enter: ")
      print(matches)
      number <- scan(n=1)
      if (number==0) match <- c(match,NA)
      else match <- c(match,matches[number])
      cat("\n")
    }
    else match <- c(match,matches[1])
  }
  return(match)
}
                                        # If needing to rematch uncomment the next line, which takes around 5 mins to complete
matched <- match.closest(censusBrexit$Area,parliament$Area)

censusBrexit$Matched <- as.factor(matched)

tail(censusBrexit)
head(censusBrexit)

colnames(parliament)[1] <- "Matched"
brexitParliament <- left_join(censusBrexit,parliament,by = "Matched")
head(brexitParliament)
length(brexitParliament$Area)

write.csv(brexitParliament,"data/brexit.csv")
