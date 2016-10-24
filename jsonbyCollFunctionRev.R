jsonbyCollFuc <- function(data) {

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
        
        
        jsonOut<-toJSON(list)
      
        
        sink(paste('data',substr(k,1, regexpr("-", k)-1), raceselect[s],'.json', collapse ='', sep=""))
        cat(jsonOut)
        
        sink()
}

}
