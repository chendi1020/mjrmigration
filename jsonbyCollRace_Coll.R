source("S:/Institutional Research/Chen/R setup/ODBC Connection.R")
source("H:/GitHub/NSSC first declared to degree race fliter/jsonbyCollFunctionRev.R")
library(sqldf)
#for short name of college
COLLNM <- sqlFetch(SISInfo, 'COLLEGE')


PAG <- sqlQuery(MSUEDW, "select distinct Pid, Cohort, COLLEGE_FIRST,  COLLEGE_DEGREE, 
                MAJOR_FIRST_SEMESTER, MAJOR_NAME_FIRST, MAJOR_DEGREE, MAJOR_NAME_DEGREE, ETHNICITY
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





#recode those who have not graduated yet


PAG$COLLEGE_DEGREE <- ifelse(is.na(PAG$COLLEGE_DEGREE),99, PAG$COLLEGE_DEGREE)
PAG$COLLEGE_DEGREE_NAME <- as.character(PAG$COLLEGE_DEGREE_NAME)
PAG$COLLEGE_DEGREE_NAME <- ifelse(is.na(PAG$COLLEGE_DEGREE_NAME),'Not Graduate', PAG$COLLEGE_DEGREE_NAME)
PAG$MAJOR_DEGREE <- ifelse(is.na(PAG$MAJOR_DEGREE), 9999, PAG$MAJOR_DEGREE)

PAG$MAJOR_NAME_DEGREE <- as.character(PAG$MAJOR_NAME_DEGREE)
PAG$MAJOR_NAME_DEGREE <- ifelse(is.na(PAG$MAJOR_NAME_DEGREE), 'Not Graduate', PAG$MAJOR_NAME_DEGREE)

#concate to prevent same character
PAG$COLLEGE_FIRST_NAME <- paste(PAG$COLLEGE_FIRST, PAG$COLLEGE_FIRST_NAME, sep = "-")
PAG$MAJOR_NAME_FIRST <- paste(PAG$MAJOR_FIRST_SEMESTER, PAG$MAJOR_NAME_FIRST, sep = "-")

PAG$COLLEGE_DEGREE_NAME <- paste(PAG$COLLEGE_DEGREE, PAG$COLLEGE_DEGREE_NAME, sep = "-")
PAG$MAJOR_NAME_DEGREE <- paste(PAG$MAJOR_DEGREE, PAG$MAJOR_NAME_DEGREE, sep = "-")


#aggregation
library(dplyr)

#Agg1 <- data %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
Agg2 <- PAG %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME, MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())


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



raceselect <- c("All" ,"White", "Black", "Hispanic" , "International", "Asian_Hawaii_PI", "Others")

PAG$ETHNICITY <- as.character(PAG$ETHNICITY)

for (s in seq(raceselect)){
        if(raceselect[s]=='All'){

                
                jsonbyCollFuc(PAG)
           
        }
        else if (raceselect[s]=='Others'){
                
             
                ds <- PAG[PAG$ETHNICITY  %in% c('Amer Ind', 'BLANK', 'Multiple' ) ,]
                jsonbyCollFuc(ds)
        }
        else if (raceselect[s]=='Asian_Hawaii_PI'){
                
               
                ds <- PAG[PAG$ETHNICITY  %in% c('Asian/Hawaii/PI' ) ,]
                jsonbyCollFuc(ds)
        }
        
        else if (raceselect[s]=='International'){
                
              
                ds <- PAG[PAG$ETHNICITY  %in% c('INTERNATIONAL' ) ,]
                jsonbyCollFuc(ds)
        }
        
        else {
                
                ds <- PAG[PAG$ETHNICITY  ==raceselect[s] ,]
                jsonbyCollFuc(ds)
        }
        
}

