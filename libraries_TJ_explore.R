#### Basic Exploratory analytics ####
#### T.Jetka ####

tj_logreg_acc<-function(x,y){
  
  df=data.frame(x=x,y=y)
  df$y=factor(df$y)
  y_levels<-levels(df$y)
  tweights=rep(1,nrow(df))
  for (ilevel in y_levels){
    tweights[df$y==ilevel]=(1/sum(df$y==ilevel))
  }
  
  
  lr_model=nnet::multinom(y~x,data=df,
                          na.action=na.omit,weights = tweights)
  prob_lr<-data.frame(fitted(lr_model))
  if (length(y_levels)==2) {prob_lr=cbind(1-prob_lr,prob_lr)}
  colnames(prob_lr)<-as.character(y_levels)
  obs<-df$y
  pred<-apply(prob_lr,1,function(x){
    idmax=which.max(x)
    as.character(y_levels)[idmax]
  })
  temp_confusionMatrix<-try(caret::confusionMatrix(factor(pred,levels=y_levels),obs),silent=TRUE)
  
  if (length(y_levels)==2){
    out=mean(temp_confusionMatrix$byClass[11],na.rm=TRUE)
  } else {
    out=mean(temp_confusionMatrix$byClass[,11],na.rm=TRUE)
  }
  
  if (!is.numeric(out)){
    out=0
  }
  
  out
}


tj_explore_round2<-function(x){
  round(x,digits=2)
}


tj_explore_round2Sc<-function(xx){
  xx_out=xx
  
  for (i in 1:length(xx)){
    x=xx[i]
    if ((x>1000)|x<(-10000)) {
      xx_out[i]=format(x,scientific=TRUE,digits=2)
    } else if ((x>0.2)|x<(-0.2)){
      xx_out[i]=round(x,digits=2)    
    } else {
      xx_out[i]=format(x,scientific=TRUE,digits=2)
    }
  }
  
  xx_out
}

tj_explore_summaryNum<-function(df){
  
  if (all(sapply(df,is.numeric))){
    outdf=do.call(rbind,lapply(df,function(x){
      
      x=x[!is.na(x)]
      
      temp_mean = mean(x,na.rm=TRUE)
      temp_sd = sd(x,na.rm=TRUE)
      temp_q1 = quantile(x,probs = c(0.25),na.rm = TRUE)
      temp_q3 = quantile(x,probs = c(0.75),na.rm = TRUE)
      temp_irq = temp_q3-temp_q1
      
      id_outliers1=(abs(x-temp_mean)>3*temp_sd)
      id_outliers2=(x>temp_q3+1.5*temp_irq)|(x<temp_q1-1.5*temp_irq)
      
      data.frame(NAs=sum(is.na(x)),
                 min=tj_explore_round2Sc(min(x,na.rm=TRUE)),
                 Q05=tj_explore_round2Sc(quantile(x,probs = c(0.05),na.rm = TRUE)),
                 Q25=tj_explore_round2Sc(temp_q1),
                 median=tj_explore_round2Sc(quantile(x,probs = c(0.50),na.rm = TRUE)),
                 Q75=tj_explore_round2Sc(temp_q3),
                 Q95=tj_explore_round2Sc(quantile(x,probs = c(0.95),na.rm = TRUE)),
                 max=tj_explore_round2Sc(max(x,na.rm=TRUE)),
                 mean=tj_explore_round2Sc(temp_mean),
                 sd=tj_explore_round2Sc(temp_sd),
                 irq=tj_explore_round2Sc(temp_irq),
                 zeros=sum(x[!is.na(x)]==0),
                 outliers=sum(id_outliers1|id_outliers2),
                 stringsAsFactors = FALSE)
    }))
  } else {
    stop("Non-numeric variables supplied")
  }
  
  data.frame(outdf)
}

tj_explore_introduce<-function(df){
  
  df=as.data.frame(df)
  
  temp_classes=sapply(df,class)
  temp_numeric_classes=sapply(df,is.numeric)
  temp_factor_classes=sapply(df,function(x){
    is.character(x)|is.factor(x)
  })
  temp_num_unique=sapply(df,function(x) length(unique(x)))
  temp_colnames_strange=sum(str_detect(colnames(df),"[^a-zA-Z0-9_]"))
  
  t(data.frame(rows=nrow(df),
               cols=ncol(df),
               cols_StrangeSymbols=temp_colnames_strange,
               numeric_vars=sum(temp_numeric_classes),
               short_numeric_vars=sum((temp_num_unique<7)&temp_numeric_classes),
               category_vars=sum(temp_factor_classes),
               long_category_vars=sum((temp_num_unique>6)&temp_factor_classes),
               single_val_vars=sum(temp_num_unique==1),
               unknown_class=sum(!(temp_numeric_classes|temp_factor_classes)),
               total_cases=sum(sapply(df,function(x) sum(!is.na(x)))),
               total_NAs=sum(sapply(df,function(x) sum(is.na(x)))),
               all_NA_cols=sum(sapply(df,function(x) all(is.na(x)))),
               any_NA_cols=sum(sapply(df,function(x) any(is.na(x)))),
               all_NA_rows=sum(apply(df,1,function(x) all(is.na(x)))),
               any_NA_rows=sum(apply(df,1,function(x) any(is.na(x)))),
               complete_rows=sum(apply(df,1,function(x) all(!is.na(x)))),
               stringsAsFactors = FALSE))
}

tj_explore_colsNA<-function(df){
  temp_na=tj_explore_round2(sapply(df,function(x) {
    mean(is.na(x))
  }))
  
  if (all(temp_na==0)){
    out = list("No NA in columns")
  } else {
    out = list(dfNA=data.frame(colnameX=colnames(df)[temp_na>0],
                               perc_NA=temp_na[temp_na>0],
                               stringsAsFactors = FALSE),
               cols_highNA=colnames(df)[temp_na>0.1])
    row.names(out$dfNA)<-NULL
  }
  
  out
}

tj_explore_distribCat<-function(x,nname,path_dir){
  
  tj_dirCreate(path_dir)
  num_cases=length(unique(x))
  
  plot = ggplot(data=data.frame(x=x),aes(x=x))+geom_bar()+
    theme_bw()+ggtitle(nname)
  ggsave(plot,filename = paste0(path_dir,"/",nname,".png"),height=5,width=(3+(num_cases/3)))
  
  1
}


tj_explore_distribNum<-function(x,nname,path_dir){
  
  tj_dirCreate(path_dir)
  
  num_cases=length(unique(x))
  x=x[!is.na(x)]
  
  temp_mean = mean(x,na.rm=TRUE)
  temp_sd = sd(x,na.rm=TRUE)
  temp_q1 = quantile(x,probs = c(0.25),na.rm = TRUE)
  temp_q3 = quantile(x,probs = c(0.75),na.rm = TRUE)
  temp_irq = temp_q3-temp_q1
  
  id_outliers1=(abs(x-temp_mean)>3*temp_sd)
  id_outliers2=(x>temp_q3+1.5*temp_irq)|(x<temp_q1-1.5*temp_irq)
  id_outliers=id_outliers1|id_outliers2
  
  if (sum(id_outliers)>0){
    plot1 = ggplot(data=data.frame(x=x),aes(y=x,x=1))+geom_boxplot(size=1.5)+
      theme_bw()+coord_flip()+ggtitle(nname)+
      theme(axis.title.x = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
      scale_x_continuous("",limits=c(0,2))
    plot2 = ggplot(data=data.frame(x=x),aes(x=x))+geom_density(fill="gray",size=1.5,alpha=0.6)+
      theme_bw()+theme(axis.title.x = element_blank())
    
    plot3 = ggplot(data=data.frame(x=x[!id_outliers]),aes(y=x,x=1))+geom_boxplot(size=1.5)+
      theme_bw()+coord_flip()+ggtitle("without outliers")+
      theme(axis.title.x = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
      scale_x_continuous("",limits=c(0,2))
    plot4 = ggplot(data=data.frame(x=x[!id_outliers]),aes(x=x))+geom_density(fill="gray",size=1.5,alpha=0.6)+
      theme_bw()+theme(axis.title.x = element_blank())
    
    plot5 = ggplot(data=data.frame(x=x),aes(x=x))+
      geom_histogram(bins = min(100,num_cases),colour="black",fill="gray",alpha=0.8 )+
      theme_bw()+ggtitle("Histogram")+theme(axis.title.x = element_blank())
    plot6 = ggplot(data=data.frame(x=x),aes(sample=x))+stat_qq()+stat_qq_line()+
      ggtitle("QQ plot")
    
    plot=grid.arrange(plot1,plot3,plot5,plot2,plot4,plot6,nrow=2,ncol=3)
    ggsave(plot,filename =paste0(path_dir,"/",nname,".png"),height=5,width=10)
  } else {
    plot1 = ggplot(data=data.frame(x=x),aes(y=x,x=1))+geom_boxplot(size=1.5)+
      theme_bw()+coord_flip()+ggtitle(nname)+
      theme(axis.title.x = element_blank(),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
      scale_x_continuous("",limits=c(0,2))
    plot2 = ggplot(data=data.frame(x=x),aes(x=x))+geom_density(fill="gray",size=1.5,alpha=0.6)+
      theme_bw()
    plot5 = ggplot(data=data.frame(x=x),aes(x=x))+
      geom_histogram(bins = min(100,num_cases),colour="black",fill="gray",alpha=0.8)+theme_bw()+
      ggtitle("Histogram")
    plot6 = ggplot(data=data.frame(x=x),aes(sample=x))+stat_qq()+stat_qq_line()+theme_bw()+
      ggtitle("QQ plot")
    
    plot=grid.arrange(plot1,plot5,plot2,plot6,nrow=2,ncol=2)
    ggsave(plot,filename =paste0(path_dir,"/",nname,".png"),height=5,width=7)
  }
  
  
  1
}




tj_explore_distrib<-function(df,explore_dir){
  
  tj_dirCreate(explore_dir)
  df=as.data.frame(df)
  
  temp_numeric_classes=sapply(df,is.numeric)
  temp_factor_classes=sapply(df,function(x){
    is.character(x)|is.factor(x)
  })
  temp_num_unique=sapply(df,function(x) length(unique(x)))
  
  numeric_vars=colnames(df)[((temp_num_unique>6)&temp_numeric_classes)]
  short_numeric_vars=colnames(df)[((temp_num_unique<7)&temp_numeric_classes)]
  category_vars=colnames(df)[(temp_num_unique<7)&(temp_factor_classes)]
  long_category_vars=colnames(df)[((temp_num_unique>6)&temp_factor_classes)]
  
  
  explore_dir_0=paste0(explore_dir,"/short_numeric/")
  for (icol in short_numeric_vars){
    tj_explore_distribCat(df[[icol]],icol,path_dir=explore_dir_0)
  }
  
  explore_dir_0=paste0(explore_dir,"/main_numeric/")
  num_summary=tj_explore_summaryNum(df[,numeric_vars])
  num_summary$metric=row.names(num_summary)
  tj_write.table(num_summary,paste0(explore_dir,"/numeric_summary.txt"))
  for (icol in numeric_vars){
    tj_explore_distribNum(df[[icol]],icol,path_dir=explore_dir_0)
  }
  
  explore_dir_0=paste0(explore_dir,"/main_category/")
  for (icol in category_vars){
    tj_explore_distribCat(df[[icol]],icol,path_dir=explore_dir_0)
  }
  
  explore_dir_0=paste0(explore_dir,"/long_category/")
  for (icol in long_category_vars){
    tj_explore_distribCat(df[[icol]],icol,path_dir=explore_dir_0)
  }
  
  1
}


tj_explore_correlation<-function(df,output_dir){
  
  tj_dirCreate(output_dir)
  
  temp_numeric_classes=sapply(df,is.numeric)
  temp_factor_classes=sapply(df,function(x){
    is.character(x)|is.factor(x)
  })
  temp_num_unique=sapply(df,function(x) length(unique(x)))
  
  numeric_vars=colnames(df)[((temp_num_unique>6)&temp_numeric_classes)]
  short_numeric_vars=colnames(df)[((temp_num_unique<7)&temp_numeric_classes)]
  category_vars=colnames(df)[(temp_num_unique<7)&(temp_factor_classes)]
  long_category_vars=colnames(df)[((temp_num_unique>6)&temp_factor_classes)]
  allNA_cols=sapply(df,function(x) all(is.na(x)))
  singleVal_cols=(temp_num_unique==1)
  
  

  for (icol in long_category_vars){
    temp_table=table(df[[icol]])
    temp_table=temp_table/sum(temp_table)
    temp_names_to_cut=names(temp_table)[temp_table<0.01]
    for (itemp in temp_names_to_cut){
      df[[icol]][df[[icol]]==itemp]="other" 
    }
  }
  
  cat_vars=c(short_numeric_vars,category_vars,long_category_vars)
  num_vars=numeric_vars
  
  cat_vars=cat_vars[!cat_vars%in%c(allNA_cols,singleVal_cols)]
  num_vars=num_vars[!num_vars%in%c(allNA_cols,singleVal_cols)]
  
  
  df_cor_num=cor(df[,num_vars],use = 'pairwise.complete.obs')
  
  
  df_cor_cat=matrix(0,length(cat_vars),length(cat_vars))
  for (i in 1:(length(cat_vars)-1)){
    t_namei=cat_vars[i]
    for (j in (i+1):length(cat_vars)){
      t_namej=cat_vars[j]
      df_cor_cat[i,j]=cramerV( df[[t_namei]],df[[t_namej]]  ,bias.correct = TRUE)
    }
  }
  df_cor_cat=df_cor_cat+t(df_cor_cat)
  for (i in 1:(length(cat_vars))){
    df_cor_cat[i,i]=1
  }
  
  
  
  df_cor_numcat=matrix(0,length(cat_vars),length(num_vars))
  for (i in 1:length(cat_vars)){
    t_namei=cat_vars[i]
    for (j in 1:length(num_vars)){
      t_namej=num_vars[j]
      df_cor_numcat[i,j]=tj_logreg_acc( df[[t_namej]],df[[t_namei]])
    }
  }
  colnames(df_cor_numcat)=num_vars
  row.names(df_cor_numcat)=cat_vars
  

  plot1=ggcorr(data = NULL, cor_matrix = df_cor_num, label = TRUE,hjust=1)
  plot2=ggcorr(data = NULL, cor_matrix = df_cor_cat, label = TRUE,hjust=1)
  

  ggsave(plot1,filename =paste0(output_dir,"/","num_correlations.png"))
  ggsave(plot2,filename =paste0(output_dir,"/","cat_correlations.png"))
  png(paste0(output_dir,"/","num_cat_correlations.png"))
  corrplot::corrplot(corr =  df_cor_numcat,is.corr=FALSE)
  dev.off()
  
  
  df_cor_poly=hetcor(df[,c(cat_vars,num_vars)],
                     use="pairwise.complete.obs")
  plot4=ggcorr(data = NULL, cor_matrix = df_cor_poly$correlations, label = TRUE,hjust=1)
  ggsave(plot4,filename =paste0(output_dir,"/","poly_correlations.png"))
  
  png(paste0(output_dir,"/aux_DE_corr.png"))
  plot_correlation(na.omit(df[,c(cat_vars,num_vars)]), maxcat = 5L)
  dev.off()
  
  
  tj_write.table(df_cor_num,paste0(output_dir,"/","num_corr_matrix.txt"))
  tj_write.table(df_cor_cat,paste0(output_dir,"/","cat_corr_matrix.txt"))
  tj_write.table(df_cor_numcat,paste0(output_dir,"/","numcat_corr_matrix.txt"))
  tj_write.table(df_cor_poly$correlations,paste0(output_dir,"/","poly_corr_matrix.txt"))
  
  1
}


tj_explore_pca<-function(df,output_dir){
  tj_dirCreate(output_dir)
  
  # temp_numeric_classes=sapply(df,is.numeric)
  # temp_factor_classes=sapply(df,function(x){
  #   is.character(x)|is.factor(x)
  # })
  # temp_num_unique=sapply(df,function(x) length(unique(x)))
  
  # numeric_vars=colnames(df)[((temp_num_unique>6)&temp_numeric_classes)]
  # short_numeric_vars=colnames(df)[((temp_num_unique<7)&temp_numeric_classes)]
  # category_vars=colnames(df)[(temp_num_unique<7)&(temp_factor_classes)]
  # long_category_vars=colnames(df)[((temp_num_unique>6)&temp_factor_classes)]
  # allNA_cols=sapply(df,function(x) all(is.na(x)))
  # singleVal_cols=(temp_num_unique==1)
  
  # for (icol in long_category_vars){
  #   temp_table=table(df[[icol]])
  #   temp_table=temp_table/sum(temp_table)
  #   temp_names_to_cut=names(temp_table)[temp_table<0.01]
  #   for (itemp in temp_names_to_cut){
  #     df[[icol]][df[[icol]]==itemp]="other" 
  #   }
  # }
  
  # cat_vars=c(short_numeric_vars,category_vars,long_category_vars)
  # num_vars=numeric_vars
  
  # cat_vars=cat_vars[!cat_vars%in%c(allNA_cols,singleVal_cols)]
  # num_vars=num_vars[!num_vars%in%c(allNA_cols,singleVal_cols)]
  
  
  tempFA=FAMD(base = as.data.frame(df),
              graph = FALSE,ncp = min(10,ncol(df)))
  temp_plot=data.frame(tempFA$eig)
  temp_plot$x=factor(row.names(temp_plot),levels = row.names(temp_plot))
  colnames(temp_plot)=c("eig","perc_Var","cuml_Var","component")
  temp_plot$eig=NULL
  temp_plot=melt(temp_plot,id.vars="component")
  plot1=ggplot(data=temp_plot,aes(x=component,y=value,fill=variable))+
    geom_col(position = "dodge")+theme_bw()+scale_y_continuous("Variance %")
  
  temp_plot=data.frame(tempFA$var$coord[,1:5])
  temp_plot$x=as.character(row.names(temp_plot))
  temp_vars_chosen=c()
  for (i in 1:5){
    temp_vars_chosen=c(temp_vars_chosen,
                       temp_plot$x[rank(-abs(temp_plot[[i]]))<6])
  }
  temp_plot=temp_plot[temp_plot$x%in%temp_vars_chosen,]
  temp_plot=melt(temp_plot,id.vars = "x")
  plot2=ggplot(data=temp_plot,aes(x=x,y=value))+geom_col()+coord_flip()+facet_grid(.~variable)
  
  temp_plot=data.frame(tempFA$ind$coord[,1:3])
  plot3=ggpairs(temp_plot)+theme_bw()
  
  ggsave(plot1,filename =paste0(output_dir,"/variance_decomp.png"))
  ggsave(plot2,filename =paste0(output_dir,"/variables_importance.png"))
  ggsave(plot3,filename =paste0(output_dir,"/pca_comp.png"))
  
  1
}



#### Dependance ####


tj_explore_summary_numericAux<-function(x){
  x=x[!is.na(x)]
  if (length(x)>3){
    data.frame(mean=round2Sc(mean(x)),SD=round2Sc(sd(x)),mad=round2Sc(mad(x)),
               min=tj_explore_round2Sc(min(x)),max=round2Sc(max(x)),
               q01=tj_explore_round2Sc(quantile(x,probs = 0.01)),
               q05=tj_explore_round2Sc(quantile(x,probs = 0.05)),
               q25=tj_explore_round2Sc(quantile(x,probs = 0.25)),
               q50=tj_explore_round2Sc(quantile(x,probs = 0.50)),
               q75=tj_explore_round2Sc(quantile(x,probs = 0.75)),
               q95=tj_explore_round2Sc(quantile(x,probs = 0.95)),
               q99=tj_explore_round2Sc(quantile(x,probs = 0.99)),
               nobs=length(x),stringsAsFactors = FALSE
    )
  } else {
    data.frame(media=NA,SD=NA,mad=NA,
               min=NA,max=NA,
               q01=NA,
               q05=NA,
               q25=NA,
               q50=NA,
               q75=NA,
               q95=NA,
               q99=NA,
               nmuestra=length(x),stringsAsFactors = FALSE
    )      
  }
}

tj_explore_summary_numeric<-function(df){
  do.call(rbind,lapply(colnames(df),function(xn){
    x=df[[xn]]
    data.frame(varnum=xn,tj_explore_summary_numericAux(x))
  }))
}

tj_explore_summary_numericBY<-function(df,byvar){
  do.call(rbind,lapply(colnames(df),function(xn){
    outdf=data.frame(varnum=xn,do.call(rbind,by(df,byvar,function(y){
      x=y[[xn]]
      x=as.numeric(x)
      data.frame(tj_explore_summary_numericAux(x),stringsAsFactors = FALSE)
    })),stringsAsFactors = FALSE)
    outdf$groupname=row.names(outdf)
    outdf
  }))
}


tj_explore_NumVsCategory<-function(x,y,dir_output,nname,nnameGroup){
  
  cols1=colsG1[1:length(unique(y))]
  dir.create(dir_output,recursive = TRUE)
  temp_df=data.frame(x=x,y=y)
  
  df_summary=do.call(rbind,by(temp_df,temp_df$y,function(x){
    tj_explore_summary_numericAux(x$x)
  }))
  df_summary$var=row.names(df_summary)
  
  anova_test=oneway.test(data = temp_df,x~y)
  kruskal_test=kruskal.test(data = temp_df,x~y)
  
  if (length(unique(temp_df$y))==2){
    effect_size=round2(abs(mean(temp_df$x[temp_df$y==unique(temp_df$y)[1]])-
                             mean(temp_df$x[temp_df$y==unique(temp_df$y)[2]]))/sd(temp_df$x))
  } else {
    effect_size="" 
  }
  
  df_summary$param_testPV=formatSc(anova_test$p.value)
  df_summary$nonparam_testPV=formatSc(kruskal_test$p.value)
  df_print=df_summary[,c("var","q50","media","SD","min","max","nmuestra")]
  colnames(df_print)<-c("Grupo","mediana","media","SD","min","max","nmuestra")
  
  tj_write.table(df_summary,paste0(dir_output,"/summaryTable_",nname,".txt"))
  
  textGG=textGrob(nname)
  emptyGG=textGrob("")
  plot1=ggplot(data=temp_df,aes(y=x,x=y,group=y,colour=y))+geom_boxplot()+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_y_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)
  plot2=ggplot(data=temp_df,aes(y=x,x=y,group=y,colour=y))+geom_violin(size=2)+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_y_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)
  plot3=ggplot(data=temp_df,aes(y=..density..,x=x,fill=y,colour=y))+geom_density(alpha=0.4)+theme_bw()+
    scale_fill_manual(nnameGroup,values = cols1)+scale_x_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)+
    scale_y_continuous("densidad")
  tableGG=tableGrob(df_print)
  ggpv1=textGrob("")#textGrob(paste0("Parametric Test: ", df_summary$param_testPV))
  ggpv2=textGrob(paste0("Kruskall-Wallis Test: ", df_summary$nonparam_testPV))
  gges=textGrob("")#textGrob(paste0("Tamano del efecto: ", effect_size))
  
  plotAll=grid.arrange(textGG,plot1,plot2,plot3,tableGG,ggpv1,ggpv2,emptyGG,gges,
                       layout_matrix=matrix(c(1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9),
                                            8,12))
  ggsave(plotAll,filename =paste0(dir_output,"/mainPlot_",nname,".png"),width = 12,height=8)
  
  1
}



tj_explore_CatVsCategory<-function(x,y,dir_output,nname,nnameGroup){
  
  cols1=colsG1[1:length(unique(y))]
  dir.create(dir_output,recursive = TRUE)
  temp_df=data.frame(x=x,y=y)
  temp_df$x=factor(temp_df$x)
  temp_df$y=factor(temp_df$y)
  
  temp_table=table(temp_df$x,temp_df$y)
  temp_tableDF=dcast(data.frame(temp_table),Var1~Var2,value.var="Freq")
  colnames(temp_tableDF)[1]=nname
  colnames(temp_tableDF)[-1]=paste0(nnameGroup,"_",colnames(temp_tableDF)[-1])
  tj_write.table(temp_tableDF,paste0(dir_output,"/countTable_",nname,".txt"))
  
  if (nrow(temp_table)>1&ncol(temp_table)>1){
    fisher_test=formatSc(fisher.test(temp_table,simulate.p.value = TRUE)$p.value)
    Mantel_Haenszel_test=formatSc(MHChisqTest(temp_table)$p.value)
    G_test=formatSc(GTest(temp_table)$p.value)
    chisq_test=formatSc(chisq.test(temp_table)$p.value)
    
    y_levels=unique(y)
    
    tweights=rep(1,nrow(temp_df))
    
    for (ilevel in y_levels){
      tweights[temp_df$y==ilevel]=1/sum(temp_df$y==ilevel)
    }
    
    logreg=tj_calculate_classifier(data_main = temp_df,ind_variables = "x" ,d_variable = "y",tweights=tweights)
    
    if (class(logreg$confusionMatrix)=="try-error"){
      logreg_test=""
    } else {
      
      temp_acc_sd=3*mean(c(abs(logreg$confusionMatrix$overall[4]-logreg$confusionMatrix$overall[1]),
                           abs(logreg$confusionMatrix$overall[3]-logreg$confusionMatrix$overall[1])))
      temp_pv=2*(1-pnorm(logreg$confusionMatrix$byClass[11],mean = 0.5, sd=temp_acc_sd))
      
      logreg_test=paste0("Exact.: ",round(logreg$confusionMatrix$byClass[11],digits=2),",  ",
                         "Exact_PV:",formatSc(temp_pv))
    }
    
  } else {
    fisher_test=NA
    logreg_test=NA
    Mantel_Haenszel_test=NA
    G_test=NA
    chisq_test=NA
  }
  
  temp_freq1=do.call(rbind,lapply(unique(temp_df$x),function(xx){
    x=temp_df[temp_df$x==xx,]
    data.frame(varBy=xx,table(x$y)/sum(table(x$y)))
  }))
  colnames(temp_freq1)<-c(nname,nnameGroup,"freq")
  
  temp_freq2=do.call(rbind,lapply(unique(temp_df$y),function(xx){
    x=temp_df[temp_df$y==xx,]
    data.frame(varBy=xx,table(x$x)/sum(table(x$x)))
  }))
  colnames(temp_freq2)<-c(nnameGroup,nname,"freq")
  
  
  textGG=textGrob(paste0(nname," vs ", nnameGroup))
  textGGE=textGrob("")
  textTest_2=textGrob(paste0("Fishers Test: ",fisher_test ))
  textTest_3=textGrob(paste0("Mantel-Haenszel Test: ",Mantel_Haenszel_test ))
  textTest_4=textGrob(paste0("G Test: ",G_test ))
  textTest_1=textGrob(paste0("Chi-Sq Test: ",chisq_test ))
  textTest_5=textGrob(paste0("LogReg Test: ",logreg_test ))
  plot1=ggplot(data=temp_df,aes(x=x,group=y,fill=y))+stat_count(position = "dodge")+theme_bw()+
    scale_x_discrete(nname)+scale_fill_manual(nnameGroup,values = cols1)+scale_y_continuous("Cuenta")+
    ggtitle("A1) Cuenta absoluta")
  plot2=ggplot(data=temp_df,aes(x=y,group=x,fill=x))+stat_count(position = "dodge")+theme_bw()+
    scale_fill_brewer(nname,palette="Dark2")+scale_x_discrete(nnameGroup)+scale_y_continuous("Cuenta")+
    ggtitle("A2) Cuenta absoluta")
  plot3=ggplot(data=temp_freq2,aes_string(y="freq",x=nname,group=nnameGroup,fill=nnameGroup))+
    geom_col(position = "dodge")+theme_bw()+
    scale_x_discrete(nname)+scale_fill_manual(nnameGroup,values =cols1)+ggtitle(paste0("B) Cuenta relativa a ",nnameGroup))+
    scale_y_continuous("Frec")
  plot4=ggplot(data=temp_freq1,aes_string(y="freq",x=nnameGroup,group=nname,fill=nname))+
    geom_col(position = "dodge")+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_fill_brewer(nname,palette="Dark2")+ggtitle(paste0("C) Cuenta relativa a ",nname))+
    scale_y_continuous("Frec")
  
  textGG_t1=textGrob("A) Cuenta absoluta")
  textGG_t3=textGrob(paste0("C) Cuenta relativa a ",nname))
  textGG_t4=textGrob(paste0("B) Cuenta relativa a ",nnameGroup))
  #textGG_t2=textGrob(paste0("Cuenta relativa"))
  
  tabGG1=tableGrob(temp_table)
  #tabGG2=tableGrob(round2(temp_table/sum(temp_table)))
  
  temptab=dcast(temp_freq1,as.formula(paste0(nname,"~",nnameGroup)),value.var="freq")
  temptab[,-1]=round2(temptab[,-1])
  colnames(temptab)[1]="var"
  row.names(temptab)=temptab[[1]]
  tabGG3=tableGrob(temptab[,-1])
  
  temptab=dcast(temp_freq2,as.formula(paste0(nname,"~",nnameGroup)),value.var="freq")
  temptab[,-1]=round2(temptab[,-1])
  colnames(temptab)[1]="var"
  row.names(temptab)=temptab[[1]]
  tabGG4=tableGrob(temptab[,-1])
  
  padding <- unit(5,"mm")
  
  xtabGG1 <- gtable_add_rows(
    tabGG1, 
    heights = grobHeight(textGG_t1) + padding,
    pos = 0)
  xtabGG3 <- gtable_add_rows(
    tabGG3, 
    heights = grobHeight(textGG_t3) + padding,
    pos = 0)
  xtabGG4 <- gtable_add_rows(
    tabGG4, 
    heights = grobHeight(textGG_t4) + padding,
    pos = 0)
  
  tabGG1=gtable_add_grob(xtabGG1, textGG_t1, 1, 1, 1, ncol(xtabGG1),clip="off")
  tabGG3=gtable_add_grob(xtabGG3, textGG_t3, 1, 1, 1, ncol(xtabGG3),clip="off")
  tabGG4=gtable_add_grob(xtabGG4, textGG_t4, 1, 1, 1, ncol(xtabGG4),clip="off")
  
  plotAll=grid.arrange(textGGE,#textGG_t1,textGG_t2,textGG_t3,textGG_t4,
                       plot1,plot2,plot3,plot4,
                       tabGG1,
                       #tabGG2,
                       tabGG4,
                       tabGG3,
                       textTest_1,textTest_5,
                       layout_matrix= matrix(c(3,3,3,3,5,5,5,5,7,7,7,7,10,
                                               3,3,3,3,5,5,5,5,7,7,7,7,10,
                                               3,3,3,3,5,5,5,5,8,8,8,8,2,
                                               3,3,3,3,5,5,5,5,8,8,8,8,2,
                                               4,4,4,4,6,6,6,6,8,8,8,8,11,
                                               4,4,4,4,6,6,6,6,9,9,9,9,11,
                                               4,4,4,4,6,6,6,6,9,9,9,9,11,
                                               4,4,4,4,6,6,6,6,9,9,9,9,11)-1,13,8))
  ggsave(plotAll,filename = paste0(dir_output,"/mainPlot_",nname,".png"),width = 10,height=12)
  
  1
}
