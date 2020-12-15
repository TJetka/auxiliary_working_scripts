
data.frame2<-function(...){
  data.frame(...,stringsAsFactors=FALSE)
}

tj_t_test_meta2S<-function(m1,m2,s1,s2,n1,n2){
  sd_pooled=sqrt( ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2) )
  coeff=sqrt( (1/n1)+(1/n2) )
  Tstatistic<-(m1-m2)/(sd_pooled*coeff)
  pv=2 *(1-pt(abs(Tstatistic),df=(n1+n2-2) ) )
  pv
}

tj_t_test_meta1S<-function(m1,m2,s,n){
  coeff=sqrt((1/n))
  Tstatistic<-(m1-m2)/(s*coeff)
  pv=2 *(1-pt(abs(Tstatistic),df=(n-1) ) )
  pv
}

tj_resplit<-function(df,col_name,split_char=";"){
  df_list=list()
  for (irow in 1:nrow(df)){
    temp_split=str_trim(str_split(df[[col_name]][irow],split_char)[[1]])
    df_list[[irow]]=df[rep(irow,length(temp_split)),]
    df_list[[irow]][[col_name]]=temp_split
  }
  do.call(rbind,df_list)
}

tj_mergeList=function(listDF,mergeName="SYMBOL",conflictingNames="rename",...){

  if (length(listDF)==0){
    outDF=NULL
  } else  if (length(listDF)==1){
    outDF=listDF[[1]]
  } else {
    outDF=listDF[[1]]

    for (i in 2:length(listDF)){
      temp_intersect=intersect(colnames(outDF),colnames(listDF[[i]]))
      if (length(temp_intersect)>1){
       if (conflictingNames=="rename"){  
         colnames(listDF[[i]])[!(colnames(listDF[[i]])==mergeName)]=paste0(colnames(listDF[[i]])[!(colnames(listDF[[i]])==mergeName)],"_",i)
       } else if (conflictingNames=="discard") {
         colnames_to_discard=temp_intersect[!temp_intersect==mergeName]
         for (icol in colnames_to_discard){
           listDF[[i]][[icol]]=NULL
         }
       } else {
         stop("something wrong with input argumnets")
       }
      }
      outDF=merge(outDF,listDF[[i]],by=mergeName,sort=FALSE,...)
    }
  }

  outDF
}

t_test_meta<-function(m1,m2,s1,s2,n1,n2){
  sd_pooled=sqrt( ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2) )
  coeff=sqrt( (1/n1)+(1/n2) )
  Tstatistic<-(m1-m2)/(sd_pooled*coeff)
  pv=2 *(1-pt(abs(Tstatistic),df=(n1+n2-2) ) )
  round(pv,digits=4)
}

tj_fisher_test<-function(total,group1,group2){
  intersect_g1g2=intersect(group1,group2)
  temp_matr=matrix(c(length(intersect_g1g2),
                     length(group1)-length(intersect_g1g2),
                     length(group2)-length(intersect_g1g2),
                     length(total)-length(group1)-length(group2)+length(intersect_g1g2)),2,2)
  data.frame(intersect=length(intersect_g1g2),
             pval=fisher.test(temp_matr,alternative = "greater")$p.value)
}

tj_current_time=function(){
  format(Sys.time(),"%Y%m%d_%H%M")
}

tj_dirCreate=function(path){
  dir.create(path,recursive=TRUE,showWarnings=FALSE)
}

tj_dirFiles=function(path="."){
  dirs=list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  files=list.files(path = path, full.names = FALSE, recursive = FALSE)
  files[!files%in%dirs]
}

tj_dirFolders=function(path="."){
  list.dirs(path = path, full.names = FALSE, recursive = FALSE)
}


tj_cv<-function(x,na.rm=TRUE){
  out=sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
  out
}

tj_asnumericf<-function(x){
  as.numeric(as.character(x))
}

tj_dename<-function(x){
  x<-c(unlist(x))
  names(x)<-NULL
  x
}

tj_is_finite<-function(x){
  temp=is.finite(x)
  c(sum(temp),mean(temp))
}

tj_is_na<-function(x){
  temp=is.na(x)
  c(sum(temp),mean(temp))
}

tj_sortD<-function(x){
	sort(x,decreasing=TRUE)
}

tj_orderD<-function(x){
  order(x,decreasing=TRUE)
}

tj_sort_table<-function(x){
	sort(table(x),decreasing=TRUE)
}

tj_write.table<-function(df,file){
	write.table(df,
		file=file,
		row.names=FALSE,sep="\t",
		quote=FALSE)
}

#### mathematical ####

tj_norm_1<-function(x){
  sum(abs(x))
}

tj_norm_2<-function(x){
  sqrt(sum(x^2))
}

tj_plogp<-function(x){
  out=x*log(x)
  out[x==0]=rep(0,sum(x==0))
  if(any(x<0) ){out="error"}
  out
}

tj_absmax=function(x){
	x[which.max(abs(x))]
}

#### ####

tj_summary_ext<-function(x){
  if (!is.numeric(x)){ error('not a numeric vector')}
  
  tempsummary=summary(x)
  out<-c(tempsummary,sd(x,na.rm=TRUE),quantile(x,probs = c(0.01,0.05,0.95,0.99)))
  names(out)<-c(names(tempsummary),"sd","q0.01","q0.05","q0.95","q0.99")
  
  out
}

tj_formatSc<-function(x){
  format(x,digits=2,scientific = TRUE)
}
tj_round2<-function(x){
  round(x,digits=2)
}
tj_round2Sc<-function(x){
  if ((x>0.2)|x<(-0.2)){
    round(x,digits=2)    
  } else {
    format(x,scientific=TRUE,digits=2)
  }
}
tj_round4<-function(x){
  round(x,digits=4)
}


tj_summary_numericAux<-function(x){
    x=x[!is.na(x)]
    if (length(x)>3){
    data.frame(mean=tj_round2Sc(mean(x)),sd=tj_round2Sc(sd(x)),mad=tj_round2Sc(mad(x)),
               min=tj_round2Sc(min(x)),max=tj_round2Sc(max(x)),
               q01=tj_round2Sc(quantile(x,probs = 0.01)),
               q05=tj_round2Sc(quantile(x,probs = 0.05)),
               q25=tj_round2Sc(quantile(x,probs = 0.25)),
               q50=tj_round2Sc(quantile(x,probs = 0.50)),
               q75=tj_round2Sc(quantile(x,probs = 0.75)),
               q95=tj_round2Sc(quantile(x,probs = 0.95)),
               q99=tj_round2Sc(quantile(x,probs = 0.99)),
               nsample=length(x),stringsAsFactors = FALSE
               )
    } else {
      data.frame(mean=NA,sd=NA,mad=NA,
                 min=NA,max=NA,
                 q01=NA,
                 q05=NA,
                 q25=NA,
                 q50=NA,
                 q75=NA,
                 q95=NA,
                 q99=NA,
                 nsample=length(x),stringsAsFactors = FALSE
      )      
    }
}

#### operations onf DF ####

tj_classDF<-function(df){
  sapply(df,class)
}

tj_deleteNA_DF<-function(data){
  data[apply(data,1,function(x) !any(is.na(x)) ),]
 }

 tj_asnumericDF<-function(x){
  data.frame(sapply(x,function(y) as.numeric(as.character(y))))
 }
 
 
 
 
 tj_calculate_classifier<-function(data_main,ind_variables,
  d_variable,maxit=5000,MaxNWts=30000,formula_string=NULL,tweights=NULL){
  
   if (is.null(formula_string)){
    formula_string<- (paste(d_variable,"~",
                           paste(
                             paste(c(ind_variables),collapse="+"),
                             sep="+"),
                           collapse=""))
   }
   
   formula=as.formula(formula_string)
   
   signal_levels<-levels(data_main[[d_variable]])
   
   cat("..Calculating logistic regression..")
   
  if (is.null(tweights)){
   lr_model=nnet::multinom(formula,data=data_main,
                           na.action=na.omit,maxit=maxit, MaxNWts = MaxNWts,trace=FALSE)
   } else {
   lr_model=nnet::multinom(formula,data=data_main,
                           na.action=na.omit,maxit=maxit, MaxNWts = MaxNWts,trace=FALSE,
                           weights=tweights)
  }

   prob_lr<-data.frame(fitted(lr_model))
   if (length(signal_levels)==2) {prob_lr=cbind(1-prob_lr,prob_lr)}
   
   colnames(prob_lr)<-as.character(signal_levels)
   obs<-data_main[[d_variable]]
   pred<-apply(prob_lr,1,function(x){
     idmax=which.max(x)
     as.character(signal_levels)[idmax]
   })
   
   cat("..Calculating significance..")
   temp_summary=summary(lr_model)
   temp_summary_sd=temp_summary$standard.errors
   temp_summary_sd[temp_summary_sd==0]=1
   temp_importance=data.frame((temp_summary$coefficients/temp_summary_sd)[-1])
   colnames(temp_importance)="Importance"
   temp_importance$variable=row.names(temp_importance)
   
   
   output=list()
   output$model=lr_model
   output$coeff_importance=temp_importance
   output$confusionMatrix<-try(caret::confusionMatrix(factor(pred,levels=signal_levels),obs),silent=TRUE)
   
   output
 }
 