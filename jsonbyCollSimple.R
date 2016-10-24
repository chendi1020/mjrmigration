source("S:/Institutional Research/Chen/R setup/ODBC Connection.R")


#for short name of college
COLLNM <- sqlFetch(SISInfo, 'COLLEGE')


PAG <- sqlQuery(MSUEDW, "select distinct Pid, Cohort, COLLEGE_FIRST,  COLLEGE_DEGREE, 
                MAJOR_FIRST_SEMESTER, MAJOR_NAME_FIRST, MAJOR_DEGREE, MAJOR_NAME_DEGREE
                from OPB_PERS_FALL.PERSISTENCE_V
                where student_level='UN' and level_entry_status='FRST' and (ENTRANT_SUMMER_FALL='Y' or substr(ENTRY_TERM_CODE,1,1)='F')
                and  COHORT in (2009,2008,2007,2006,2005)
                ")
library(sqldf)
PAG <- sqldf("select a.*, b.Short_Name as COLLEGE_FIRST_NAME, c.Short_Name as COLLEGE_DEGREE_NAME
             from PAG a 
             left join COLLNM b 
             on a.COLLEGE_FIRST=b.Coll_Code
             left join COLLNM c 
             on a.COLLEGE_DEGREE=c.Coll_Code")






#data <- PAG[PAG$COLLEGE_FIRST==k,]
data <- PAG

#recode those who have not graduated yet
data$COLLEGE_DEGREE <- ifelse(is.na(data$COLLEGE_DEGREE),99, data$COLLEGE_DEGREE)
data$COLLEGE_DEGREE_NAME <- as.character(data$COLLEGE_DEGREE_NAME)
data$COLLEGE_DEGREE_NAME <- ifelse(is.na(data$COLLEGE_DEGREE_NAME),'Not Graduate', data$COLLEGE_DEGREE_NAME)
data$MAJOR_DEGREE <- ifelse(is.na(data$MAJOR_DEGREE), 9999, data$MAJOR_DEGREE)

data$MAJOR_NAME_DEGREE <- as.character(data$MAJOR_NAME_DEGREE)
data$MAJOR_NAME_DEGREE <- ifelse(is.na(data$MAJOR_NAME_DEGREE), 'Not Graduate', data$MAJOR_NAME_DEGREE)

#concate to prevent same character
data$COLLEGE_FIRST_NAME <- paste(data$COLLEGE_FIRST, data$COLLEGE_FIRST_NAME, sep = "-")
data$MAJOR_NAME_FIRST <- paste(data$MAJOR_FIRST_SEMESTER, data$MAJOR_NAME_FIRST, sep = "-")

data$COLLEGE_DEGREE_NAME <- paste(data$COLLEGE_DEGREE, data$COLLEGE_DEGREE_NAME, sep = "-")
data$MAJOR_NAME_DEGREE <- paste(data$MAJOR_DEGREE, data$MAJOR_NAME_DEGREE, sep = "-")



#aggregation
library(dplyr)

#Agg1 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
Agg2 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME, MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
#Agg3 <- data %>% group_by(COHORT,  MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
#Agg4 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
#Agg5 <- data %>% group_by(COHORT, MAJOR_NAME_FIRST,  COLLEGE_DEGREE_NAME) %>% summarise(count=n())

DS1<- sqldf("select distinct COLLEGE_FIRST_NAME as coll, MAJOR_NAME_FIRST as mjr
            from Agg2
            union
            select distinct COLLEGE_DEGREE_NAME ,MAJOR_NAME_DEGREE
            from Agg2")

t<- split(DS1, DS1$coll)
listf <- function(x){ c(unique(x$coll),x$mjr)}
test <- sapply(t, listf)
listall <- as.data.frame(do.call(c,test))
#listall<-as.data.frame(listall[!duplicated(listall), ])
colnames(listall) <- "Org"
listall$merge <-1

#main structure
DS2 <- sqldf("select a.org, b.org as org1
             from listall a, listall b
             on 1=1
             ")


lvl <- unique(DS2$org1)


levels(DS2$Org) <- lvl
DS2$org1 <- as.factor(DS2$org1)
levels(DS2$org1) <- lvl





firstcoll <- unique(data$COLLEGE_FIRST_NAME)

for (k in firstcoll){
  Agg1 <- data %>% filter(COLLEGE_FIRST_NAME==k)%>%group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
  Agg3 <- data %>% filter(COLLEGE_FIRST_NAME==k)%>%group_by(COHORT,  MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
  Agg4 <- data %>% filter(COLLEGE_FIRST_NAME==k) %>% group_by(COHORT, COLLEGE_FIRST_NAME,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
  Agg5 <- data %>% filter(COLLEGE_FIRST_NAME==k)%>% group_by(COHORT, MAJOR_NAME_FIRST,  COLLEGE_DEGREE_NAME) %>% summarise(count=n())        
        
  names(Agg3)<-names(Agg1)
  names(Agg4) <- names(Agg1)
  names(Agg5) <- names(Agg1)
  mainds <- rbind(Agg1, Agg3, Agg4, Agg5)      
        
#mainds <- mainds.all[mainds.all$COLLEGE_FIRST_NAME==k,]        


cohortseq <- unique(mainds$COHORT)



#2005-2009
DS2_2005_09 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                     from DS2 a
                     left join mainds b
                     on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
xtb_2005_09 <- xtabs(count ~ Org + org1,data=DS2_2005_09)





#2005 to 2009 new

vec_2005_09<-vector()
for(i in  seq(nrow(xtb_2005_09))) {
        x<-c(xtb_2005_09[nrow(xtb_2005_09)+1-i,])
        names(x)<-NULL
        vec_2005_09<-rbind(x,vec_2005_09)
        
}

vec <- list("All"=vec_2005_09)

for (j in cohortseq){
        mds_2009 <- mainds%>% filter(COHORT==j)
        
        DS2_2009 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                          from DS2 a
                          left join mds_2009 b
                          on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
        xtb_2009 <- xtabs(count ~ Org + org1,data=DS2_2009)
        vec_2009<-vector()
        for(i in  seq(nrow(xtb_2009))) {
                x<-c(xtb_2009[nrow(xtb_2009)+1-i,])
                names(x)<-NULL
                vec_2009<-rbind(x,vec_2009)
                #names(vec_2009) <- j
                #veccur <-cbind(j,  vec_2009)
                #curlist<-as.list(vec_2009)
                curlist <- list(vec_2009)
                names(curlist) <- j
        }
       #list<- c(list,j=curlist)
        vec <- c(vec,curlist)
        #vec <- rbind(vec,veccur)
        
        
}






#region
coll <- unique(DS1$coll)


regionnum <- sapply(coll, function(x){ which(lvl== x)-1})
names(regionnum) <- NULL


names <- lvl



list <- list("names"=names, "regions"=regionnum, "matrix"=vec)

require(RJSONIO)
#test<-paste0(row1x,",",row2x,collapse = "")
#names(test)<-"test1"

jsonOut<-toJSON(list)
#cat(jsonOut)

sink(paste('data',substr(k,1, regexpr("-", k)-1), '.json', collapse ='', sep=""))
cat(jsonOut)

sink()
}


