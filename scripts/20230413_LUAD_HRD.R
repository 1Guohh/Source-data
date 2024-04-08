dir.create('scripts')
dir.create('results')
rm(list = ls())
options(stringsAsFactors = F)


ggplotKMCox=function(dat,title='Groups',labs=NULL,add_text=NULL, col = mycolor){
  library(ggplot2)
  colnames(dat)=c('time','status','groups')
  #sdf<-survdiff(Surv(time,status) ~ groups,data=dat)
  #print((sdf))
  #summary(sdf)
  #p<-pchisq(sdf$chisq,length(sdf$n)-1,lower.tail=FALSE)
  sf<-survfit(Surv(time,status) ~ groups,data=dat)
  surv=survminer::ggsurvplot(sf, data = dat, palette = col, #jco palette 
                             pval = TRUE, surv.median.line='hv',
                             linetype = "dashed"
                             ,conf.int = T
                             ,conf.int.style ='step'
                             , pval.coord=c(0, 0.2), #Add p-value 
                             risk.table = TRUE, 
                             legend.title = title
                             ,legend.labs = labs
  )
  p1=surv$plot+theme_classic()+theme(axis.text.y=element_text(family="Times",face="plain")
                                ,axis.text.x=element_blank()
                                ,axis.title.x=element_blank()
                                ,plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches")
                                #,axis.title.y=element_blank()
                                ,legend.position=c(1,1), legend.justification=c(1,1)
                                ,legend.background = element_rect(fill = NA, colour = NA)
                                ,legend.title = element_text(family="Times",face="plain")
                                ,legend.text = element_text(family="Times",face="plain"))
  #p1=p1+text()
  #tms=data.frame(Group=tms.gp,value=tms.tps,Attribute=rep(data_m[1,1],length(tms.gp))
  #               ,ymax=rep(max(ylim),length(tms.gp)))
  #p4=p4+geom_text(data=tms,aes(x=Group, y=ymax, label=value),color="black")
  if(!is.null(add_text)){
    text.tb=surv$data.survplot[1,]
    text.tb[1,1]=0
    text.tb[1,5]=0
    text.tb$Text=add_text
    p1=p1+geom_text(data=text.tb,aes(x=time, y=surv, label=Text),color="black",hjust =0)
  }
  
  p2=surv$table+theme_classic()+theme(axis.text.y=element_text(family="Times",face="plain")
                                 #,axis.text.x=element_blank()
                                 #,axis.title.x=element_blank()
                                 #,axis.title.y=element_blank()
                                 ,plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches")
                                 ,plot.title=element_blank()
                                 ,legend.position=c(1,1), legend.justification=c(1,1)
                                 #,legend.background = element_rect(fill = NA, colour = NA)
                                 ,legend.title = element_text(family="Times",face="plain")
                                 ,legend.text = element_text(family="Times",face="plain"))
  
  g2=ggpubr::ggarrange(p1,p2, ncol = 1, nrow = 2,heights = c(1,0.3),align = "v")
  return(g2)
}
mg_violin <- function(data,xangle=0,ylab='value',xlab='',leg.title='Group',test_method='anova',cmp_test_method='t.test',legend.pos='r',melt=F,jitter=T,ylim=NULL,show_compare=NULL,point_size=NULL,mycolor = mycolor){
  library(ggplot2)
  if(is.null(ylim)){
    
  }
  if(melt){
    data_m=data
    colnames(data_m)=c('Group','value')
  }else{
    data_m=reshape2::melt(data)
    colnames(data_m)=c('Group','value')
  }
  if(!is.null(ylim)){
    data_m$value[data_m$value>ylim[2]]<-NA
  }
  data_m=data_m[which(!is.na(data_m[,1])),]
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }
  
  pos='right'
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else if(legend.pos=='t'){
    pos='top'
  }else if(legend.pos=='r'){
    pos='right'
  }else if(legend.pos=='b'){
    pos='bottom'
  }
  uni.group=unique(data_m[,1])
  ct=length(uni.group)
  
  p1<-ggplot(data_m,aes(x=Group,y=value))+geom_violin(alpha=1)+scale_color_manual(values = mycolor)
  if(ct<=4){
    p1=p1+ggsci::scale_fill_jco()
  }else if(ct<=10){
    p1=p1+ggsci::scale_fill_jco(name=leg.title)
  }else if(ct<=20){
    p1=p1+ggsci::scale_fill_jco(palette = "category20",name=leg.title)
  }else if(ct<=30){
    cbPalette=c(ggsci::pal_jco("nrc", alpha = 0.6)(10),ggsci::pal_jco("category20", alpha = 0.6)(20))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }else if(ct<=38){
    cbPalette=c(ggsci::pal_jco()(10)
                ,ggsci::pal_jco("nrc", alpha = 0.6)(10)
                ,ggsci::pal_jco("category20", alpha = 0.6)(20)
                ,ggsci::pal_jco("default", alpha = 0.6)(8))
    p1=p1+scale_fill_manual(values=cbPalette[1:ct])
  }
  
  if(jitter){
    if(is.null(point_size)){
      p1<-p1+geom_jitter(alpha=0.3,col='black',show.legend=FALSE,width = 0.2)
    }else{
      p1<-p1+geom_jitter(alpha=0.3,col='black',show.legend=FALSE,width = 0.2,size=point_size)
    }
  }
  
  p1=p1+theme_classic()+geom_boxplot(width=0.2,aes(fill=Group),outlier.shape = NA)
  p1=p1+theme(axis.text.x=tx, 
              axis.text.y=element_text(family="Times",face="plain"), 
              axis.title.y=element_text(family="Times",face="plain"), 
              legend.text=element_text(face="plain", family="Times", colour="black" 
              ),
              legend.title=element_text(face="plain", family="Times", colour="black"
              ),
              legend.justification=pos, legend.position=pos
              ,legend.background = element_rect(fill = NA, colour = NA)
  )+ylab(ylab)+xlab(xlab)
  til=''
  if(test_method=='anova'){
    if(length(unique(data_m[,1]))<3){
      x1=data_m[,2][which(data_m[,1]==unique(data_m[,1])[1])]
      x2=data_m[,2][which(data_m[,1]==unique(data_m[,1])[2])]
      pv=t.test(x1,x2)$p.value 
      til=paste0('t-tests p=',signif(pv,2))
    }else{
      fit <- aov(value~Group, data = data_m)
      pv=summary(fit)[[1]][5][[1]]
      fv=summary(fit)[[1]][4][[1]]
      til=paste0('ANOVA tests p=',signif(pv,2))
    }
  }else{
    if(length(unique(data_m[,1]))<3){
      x1=data_m[,2][which(data_m[,1]==unique(data_m[,1])[1])]
      x2=data_m[,2][which(data_m[,1]==unique(data_m[,1])[2])]
      pv=wilcox.test(x1,x2)$p.value 
      til=paste0('wilcox.tests p=',signif(pv,2))
    }else{
      fit=kruskal.test(value~Group, data = data_m)
      pv=fit$p.value
      til=paste0('Kruskal-Wallis test p=',signif(pv,2))
    }
  }
  p1=p1+ggtitle(til) 
  if(!is.null(ylim)){
    p1=p1+ylim(ylim)
  }
  if(is.null(show_compare)){
    if(length(uni.group)>5){
      show_compare=F
    }else{
      show_compare=T
    }
  }
  if(show_compare){
    comps=list()
    for(i in 1:(length(uni.group)-1)){
      for(j in (i+1):length(uni.group)){
        comps=c(comps,list(c(uni.group[i],uni.group[j])))
      }
    }
    p1=p1+ggpubr::stat_compare_means(comparisons = comps,method = cmp_test_method,label= "p.signif", step_increase = 0.0)
  }
  return(p1)
}
ggplotTimeROC=function(time,status,score,mks=c(1,3,5), col = mycolor){
  #time=g.os
  #status=g.ev
  #score=as.numeric(cpm.score)
  #cx=coxRun(data.frame(time,status,score))
  #if(cx[1]<=1){
  #  score=-1*score
  #}
  roc.tm=mg_surv_pROC(time,status,score,mks)
  print('roc.tm')
  print((roc.tm))
  library(survival)
  library(ggplot2)
  mks=mg_predict_time_ymd(time,mks)
  print(mks)  
  ROC.DSST=timeROC::timeROC(T=time,
                            delta=status
                            ,marker=score,
                            cause=1,weighting="marginal",
                            times=mks,
                            iid=TRUE)
  print(ROC.DSST)
  mks=mks[which(!is.na(ROC.DSST$AUC)&ROC.DSST$AUC>0)]
  print(mks)
  if(length(mks)>0){
    if(max(ROC.DSST$AUC)<0.5){
      score=-1*score
    }
    ROC.DSST=timeROC::timeROC(T=time,
                              delta=status
                              ,marker=score,
                              cause=1,weighting="marginal",
                              times=mks,
                              iid=TRUE)
    print(ROC.DSST$times)
    if(max(ROC.DSST$times)<20){
      lb=paste0(ROC.DSST$times,'-Years')
    }else if(max(ROC.DSST$times)<365){
      lb=paste0(round(ROC.DSST$times/12,0),'-Years')
    }else{
      lb=paste0(round(ROC.DSST$times/365,0),'-Years')
    }
    
    lbs=paste0(lb,',AUC=',round(ROC.DSST$AUC,2),',95%CI(',paste0(round(confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,1]/100,2),'-',
                                                                 round(confint(ROC.DSST,level = 0.95,na.rm=T)$CI_AUC[,2]/100,2)),')')
    #roc.tm=ROC.DSST$times[which(ROC.DSST$times>0)]
    
    #p.dat=rbind()
    #for(i in which(ROC.DSST$times>0)){
    #los=lowess(ROC.DSST$FP[,i], y=ROC.DSST$TP[,i], f = 1/3, iter = 100)
    #los$x=c(0,los$x,1)
    #los$y=c(0,los$y,1)
    # p.dat=rbind(p.dat,data.frame(los$x, y=los$y,rep(lbs[i],length(los$y)),stringsAsFactors = F))
    #}
    
    p.dat=rbind()
    print(length(roc.tm))
    for(i in 1:length(roc.tm)){
      #print(i)
      r1=roc.tm[[i]]
      x1=1-r1$specificities
      y1=r1$sensitivities
      #print(cbind(1-r1$specificities,r1$sensitivities))
      nx1=unique(x1)
      ny1=c()
      for(x in unique(x1)){
        x.inds=which(x1==x)
        if(length(x.inds)>0&x<0.5){
          ny1=c(ny1,min(y1[x.inds]))
        }else if(length(x.inds)>0){
          ny1=c(ny1,max(y1[x.inds]))
        }else{
          ny1=c(ny1,y1[x.inds][1])
        }
      }
      #print(cbind(nx1,ny1))
      p.dat=rbind(p.dat,data.frame(x=nx1, y=ny1,rep(lbs[i],length(nx1)),stringsAsFactors = F))
    }
    colnames(p.dat)=c('V1','V2','Type')
    p.dat=as.data.frame(p.dat)
    
    p1=ggplot(p.dat, aes(x=V1,y=V2, fill=Type))
    p1=p1+geom_line(aes(colour=Type),lwd=1.1)+
      theme_classic()+
      xlab('False positive fraction')+
      ylab('True positive fraction') +
      scale_color_manual(values = col)
    #p1=p1+stat_smooth(aes(colour=Type),se = FALSE, size = 1)+theme_classic()+xlab('False positive fraction')+ylab('True positive fraction') 
    
    p1=p1+theme(axis.text.y=element_text(family="Times",face="plain"),axis.text.x=element_text(family="Times",face="plain")
                ,axis.title.x=element_text(family="Times",face="plain"),axis.title.y=element_text(family="Times",face="plain")
                ,plot.title=element_blank()
                ,plot.margin=unit(c(0.1, 0.1, 0.1, 0.1), "inches")
                ,legend.position=c(1,0)
                ,legend.justification=c(1,0)
                ,legend.background = element_rect(fill = NA, colour = NA)
                ,legend.title = element_text(family="Times",face="plain")
                ,legend.text = element_text(family="Times",face="plain"))
    return(p1)
  }else{
    return(mg_getplot_bank('No data plot by ROC!'))
  }
}
mg_PlotMutiBoxplot=function(data,group,group_cols='jco'
                            ,test_method=c('t.test','wilcox.test','paired_t.test','paired_wilcox.test','anova','kruskal.test')[1]
                            ,order=NULL,size=1,fill=F,outlier.shape=NA,yscale=c('none','log2','log10')[1]
                            ,xangle=45,ylab='Value',xlab='',box_span=0.7
                            ,orientation = c("vertical", "horizontal", "reverse")[1]
                            ,legend.pos=NULL,melt=F,ylim=NULL,binwidth=0.05
                            ,add=c("none", "dotplot", "jitter", "boxplot", "point", "mean"
                                   , "mean_se", "mean_sd", "mean_ci", "mean_range", "median"
                                   , "median_iqr", "median_mad", "median_range")[3]){
  paired=FALSE
  if(test_method=='paired_t.test'|test_method=='paired_wilcox.test'){
    test_method=gsub('paired_','',test_method)
    paired=TRUE
  }
  print(class(data))
  if(add=='jitter'){
    fill=F
  }
  
  library(ggplot2)
  if(!melt){
    #print(class(data))
    if(class(data)=='numeric'|class(data)=='integer'){
      data=as.numeric(data)
      vd1.sbs=data.frame(group,rep('Tag',length(group)),data)
      #print(vd1.sbs)
    }else{
      data=as.data.frame(data)
      data$ID=group
      vd1.sbs <- reshape2::melt(data, id.vars=c("ID"))
    }
    colnames(vd1.sbs)=c('category','type','Score')
    Data=vd1.sbs
  }else{
    vd1.sbs=data
    colnames(vd1.sbs)=c('category','type','Score')
    Data=vd1.sbs
  }
  
  #vd1.sbs[,2]=paste0('C',as.numeric(as.character(vd1.sbs[,2])))
  if(is.null(order)){
    order=unique(vd1.sbs[,2])
  }
  
  if(xangle==0){
    tx=element_text(colour="black",family="Times")
  }else{
    tx=element_text(angle=xangle,hjust = 1,colour="black",family="Times")
  }
  
  pos=c(0,0)
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else if(legend.pos=='top'){
    pos='top'
  }else if(legend.pos=='buttom'){
    pos='buttom'
  }else{
    pos='right'
  }
  print(pos)
  if(fill){
    p <- ggpubr::ggboxplot(vd1.sbs, x="type", y="Score", fill = "category", yscale = yscale
                           ,palette = group_cols,width = box_span,size = size,order = order,outlier.shape=outlier.shape
                           ,orientation=orientation,add=add,add.params = list(binwidth=binwidth)
                           ,short.panel.labs = T)#按dose进
  }else{
    p <- ggpubr::ggboxplot(vd1.sbs, x="type", y="Score", color = "category", yscale = yscale
                           ,palette = group_cols,width = box_span,size = size,order = order,outlier.shape=outlier.shape
                           ,orientation=orientation,add=add,add.params = list(binwidth=binwidth)
                           ,short.panel.labs = T)#按dose进
  }
  
  p=p+ggpubr::stat_compare_means(aes(group=category), label = "p.signif", method = test_method,paired=paired
                                 #,label.y = max(vd1.sbs[,1])
  )
  #p=p+ylab(ylab)+xlab(xlab)
  #p=p+theme(axis.text.x = element_text(angle = xangle, hjust = 1))
  p=p+theme_classic()+theme(axis.text.x=tx, #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                       axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                       axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
                       #panel.border = element_blank(),axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                       legend.text=element_text(face="plain", family="Times", colour="black"  #设置图例的子标题的字体属性
                       ),
                       legend.title=element_text(face="plain", family="Times", colour="black" #设置图例的总标题的字体属性
                       ),
                       legend.justification=pos, legend.position=pos
                       ,legend.background = element_rect(fill = NA, colour = NA)
                       #,panel.grid.major = element_blank(),   #不显示网格线
                       #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab) #设置x轴和y轴的标题
  if(!is.null(ylim)){
    p=p+ylim(ylim)
  }
  return(p)
}

plotMutiBar=function(dat,ist=F,margin=T,xlb='',ylb='',lineCol='black',lineW=0.5,legTitle='Group',showValue=F,showLine=T,xangle=0,isAuto=T){
  library(ggplot2)
  #library(tidyverse)
  #library(reshape2)
  #library(optparse)
  if(ist){
    dat=t(dat)
  }
  lbc=colnames(dat)
  lbr=row.names(dat)
  bk_dat=dat
  if(margin){
    dat=dat%*%diag(1/c(apply(t(dat), 1, sum)))
  }
  row.names(dat)=paste0('R',1:(nrow(dat)))
  colnames(dat)=paste0('C',1:(ncol(dat)))
  row.names(bk_dat)=paste0('R',1:(nrow(bk_dat)))
  colnames(bk_dat)=paste0('C',1:(ncol(bk_dat)))
  #df=cbind(bg=paste0('R',1:nrow(dat)),dat)
  #colnames(df)=c('bg',paste0('C',1:(ncol(dat))))
  tp.dat=as.data.frame(cbind(bg=row.names(dat),dat))
  tp.dat[,1]=as.character(tp.dat[,1])
  for(i in 2:ncol(tp.dat)){
    tp.dat[,i]=as.numeric(as.character(tp.dat[,i]))
  }
  mt.df=reshape2::melt(tp.dat)
  colnames(mt.df)=c('bg','variable','value')
  
  pg=ggplot(mt.df, aes(x=variable, y=value, fill=bg))+geom_bar(stat = "identity", width=lineW, col=lineCol) + theme_classic()
  if(showLine){
    for (i in 2:(ncol(tp.dat)-1)) {
      tmp=tp.dat[order(tp.dat[,1],decreasing = T),]
      tmp[,i]=base::cumsum(tmp[,i])
      tmp[,i+1]=base::cumsum(tmp[,i+1])
      colnames(tmp)[c(i,i+1)]=c('STY','ED')
      tmp1=cbind(tmp,STX=rep(i-1+lineW/2,nrow(tmp))
                 ,EDX=rep(i-lineW/2,nrow(tmp)))
      pg=pg+geom_segment(data=tmp1,aes(x=STX, xend=EDX, y=STY, yend=ED))
    }
  }
  
  if(showValue){
    pg=pg+geom_text(data=mt.df,aes(label=sprintf("%0.2f", round(value, digits = 2))),position=position_stack(vjust=0.5))
  }
  pg=pg+scale_x_discrete(breaks = paste0('C',1:(ncol(dat))),label = lbc)
  pg=pg+labs(x=xlb, y=ylb)+ggsci::scale_fill_jco()+theme(legend.position = "bottom")
  pg=pg+ggsci::scale_fill_jco()+scale_fill_discrete(breaks = paste0('R',1:nrow(dat)),label = lbr,name=legTitle)
  if(xangle>0){
    pg=pg+theme(axis.text.x = element_text(angle = xangle, hjust = 1),legend.position = "bottom")
  }
  
  g.tb=matrix(0,nrow=ncol(dat),ncol=ncol(dat))
  for(i in 1:(ncol(dat))){
    for(j in 1:ncol(dat)){
      if(i!=j){
        g.tb[i,j]=round(-log10((chisq.test(bk_dat[,c(i,j)])$p.value)),2)
      }
    }
  }
  colnames(g.tb)=lbc
  row.names(g.tb)=lbc
  g.tb=reshape2::melt(g.tb) 
  colnames(g.tb)=c('A1','A2','A3')
  g.tb$A4=paste0(g.tb[,3],ifelse(g.tb[,3]>-log10(0.05),'(*)',''))
  stable.p=ggplot(g.tb, aes(A1, A2)) + geom_tile(aes(fill = A3),colour = "white") + theme_classic()+xlab('')+ylab('')+ scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(x=A1,y=A2,label=A4))+theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
  stable.p=stable.p+ggtitle('-log10(anova p value)')
  if(isAuto){
    g1=ggpubr::ggarrange(stable.p,pg, ncol = 1, nrow = 2,heights = c(0.5,1),align = "hv")
    return(g1)
  }else{
    return(list(Bar=pg,Table=stable.p))
  }
}

mg_plot_lasso <- function(fit,cv_fit,lambda=NULL,show_text=T,figLabels=c('A','B')){
  if(is.null(lambda)){
    lmda=cv_fit$lambda.min
  }else{
    lmda=lambda
  }
  fit.coef=fit$beta[(apply(fit$beta,1,function(x){
    return(sum(x!=0))
  })>0),]
  
  fit.coef=as.matrix(fit.coef)
  colnames(fit.coef)=fit$lambda
  #fit$lambda==cv_fit$lambda
  library(ggplot2)
  dat=data.table::melt(t(as.matrix(fit.coef)))
  dat_z=dat[which(dat$value==0),]
  dat=dat[which(dat$value!=0),]
  dat.sv=rbind()
  for (u in unique(dat_z[,2])) {
    t.z=dat_z[which(dat_z[,2]==u),1]
    t.zx=max(t.z)
    dat.sv=rbind(dat.sv,c(t.zx,u,0))
    t.zn=min(t.z)
    if(t.zx!=t.zn){
      dat.sv=rbind(dat.sv,c(t.zn,u,0))
    }
  }
  colnames(dat.sv)=colnames(dat_z)
  #dat_z=dat_z[dat_z[,2]%in%names(which(fit.coef[,which(fit$lambda==lmda)]!=0)),]
  dat=crbind2DataFrame(rbind(dat,dat.sv))
  mn=min(-log(dat$Var1))
  mx=max(-log(dat$Var1))
  if(show_text){
    mx=(mx-mn)*0.1+mx
  }
  p=ggplot(dat, aes(x=-log(Var1), y=value,colour=Var2))+geom_line()+theme_classic()+theme(legend.position = "none")
  p=p+coord_cartesian(xlim=c(mn, mx))+xlab('-ln(lambda)')+ylab('Coefficients')
  if(show_text){
    fl=fit.coef[which(fit.coef[,which(fit$lambda==lmda)]!=0),ncol(fit.coef)]
    for_label=data.frame(Var1=rep(min(dat$Var1),length(fl)),Var2=names(fl),value=fl)
    p=p+ggrepel::geom_label_repel(
      aes(label = Var2,color=Var2),
      data = for_label,hjust = 0
    )
  }
  p=p+geom_vline(aes(xintercept=-log(lmda)), colour="#BB0000", linetype="dashed")
  p=p+annotate('text',x=-log(lmda),y=min(dat[,3]),label=paste0('lambda=',round(lmda,4)))
  tgc=data.frame(lambda=cv_fit$lambda,cvm=cv_fit$cvm,cvup=cv_fit$cvup,cvlo=cv_fit$cvlo,cvsd=cv_fit$cvsd
                 ,col=ifelse(cv_fit$lambda>=cv_fit$lambda.min&cv_fit$lambda<=cv_fit$lambda.1se,ifelse(cv_fit$lambda==lmda,'A','C'),'B'))
  p1=ggplot(tgc, aes(x=log(lambda), y=cvm)) + xlab('ln(lambda)')+ ylab('Parial Likelihood Deviance')+
    geom_errorbar(aes(ymin=cvm-cvsd, ymax=cvm+cvsd)) +
    geom_point(aes(colour=col))
  p1=p1+theme_classic()+theme(legend.position = "none")
  gal=ggpubr::ggarrange(p,p1, ncol = 2, nrow = 1
                        #,align = "hv"
                        ,labels = figLabels)
  return(gal)
}

cor_point <- function(x,
                      y,
                      method='Pearson',
                      top_col='red',
                      right_col='blue',
                      ylab='y expression',
                      xlab='x expression'){
  #x=rnorm(200)
  #y=rnorm(200)
  library(ggplot2)
  data=data.frame(x,y)  
  colnames(data)=c('wt','mpg')
  til=''
  if(method=='Pearson'){
    cr=cor.test(x,y)
    p=cr$p.value
    r=cr$estimate
    til=paste0('Pearson\'s correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }else{
    cr=cor.test(x,y,method = "spearman")
    p=cr$p.value
    r=cr$estimate
    til=paste0('spearman correlation\nR=',round(r,3),'\nP=',signif(p,3))
  }
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
      ,plot.margin=unit(c(0.1, 0.1, 0, 0), "inches")
    )
  empty=empty+geom_text(aes(x=1, y=1, label=til),color="black")
  
  plot_top <- ggplot(data, aes(wt, fill=top_col)) + 
    geom_density(alpha=.5,fill=top_col) +theme_classic()+ 
    theme(legend.position = "none",                           
          #plot.background = element_blank(), 
          #panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          #panel.background = element_blank(),
          axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.ticks.x = element_blank()
          ,plot.margin=unit(c(0.1, 0, 0, 0.1), "inches")
    )
  
  plot_right <- ggplot(data, aes(mpg, fill=right_col)) + 
    geom_density(alpha=.5,fill=right_col) +coord_flip()+theme_classic()+ 
    theme(legend.position = "none",                           
          #plot.background = element_blank(), 
          #panel.grid.major = element_blank(), 
          #panel.grid.minor = element_blank(), 
          panel.border = element_blank(), 
          #panel.background = element_blank(),
          #axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          #axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          ,plot.margin=unit(c(0.01, 0.1, 0.1, 0), "inches")
    )
  #scale_fill_manual(values = c("orange", "purple")) + 
  
  p1=ggplot(data=data, aes(x=wt, y=mpg))+geom_point()+stat_smooth(method="lm")
  p1=p1+theme_classic()
  p1=p1+theme(axis.text.x=element_text(family="Times",face="plain"), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
              axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
              axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
              #panel.border = element_blank(),
              axis.line = element_line(colour = "black"), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
              legend.text=element_text(face="plain", family="Times", colour="black"),  #设置图例的子标题的字体属性
              legend.title=element_text(face="plain", family="Times", colour="black"), #设置图例的总标题的字体属性
              plot.margin=unit(c(0.01, 0.01, 0.1, 0.1), "inches")
              #,panel.grid.major = element_blank(),   #不显示网格线
              #panel.grid.minor = element_blank()
  )+ylab(ylab)+xlab(xlab)
  
  pg1=ggpubr::ggarrange(plot_top,p1, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
  pg2=ggpubr::ggarrange(empty,plot_right, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
  
  pgal=ggpubr::ggarrange(pg1,pg2, ncol = 2, nrow = 1,widths = c(1,0.3),align = "h")
  return(pgal)
}

plot_GSEA_By_nodes_wb <- function(parseGSEAResult,
                                  indexs=c(1,2,3),
                                  color = mycolor,
                                  TermNames=NULL,
                                  left=NULL,
                                  right=NULL){
  #parseGSEAResult=gsea.result.kegg.result
  library(ggplot2)
  if(is.null(parseGSEAResult$TEMPLATE)){
    if(is.null(left)){
      left='RankTop'
    }
    if(is.null(right)){
      right='RankBottom'
    }
  }
  if(is.null(left)){
    left=parseGSEAResult$TEMPLATE[1]
  }
  if(is.null(right)){
    right=parseGSEAResult$TEMPLATE[2]
  }
  if(!is.null(TermNames)){
    inds=match(TermNames,parseGSEAResult$EnrichTable[,1])
    inds=inds[!is.na(inds)]
    if(length(inds)==0){
      print(paste0(TermNames,' Not Found!'))
      return(NA)
    }
  }else{
    inds=indexs
    if(max(inds)>nrow(parseGSEAResult$EnrichTable)){
      print(paste0(inds,' out range!'))
      return(NA)
    }
  }
  #parseGSEAResult=GSE17705_GSEA
  g.rnk=parseGSEAResult$Rank
  
  all.info=rbind()
  all.dat=rbind()
  for(i in inds){
    node=parseGSEAResult$Nodes[[i]]
    es.values=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'ES_PROFILE'),' '))),0)
    hit.index=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'HIT_INDICES'),' '))),nrow(g.rnk))
    m.inds=which.max(abs(es.values))
    es=as.numeric(XML::xmlGetAttr(node,'ES'))
    np=as.numeric(XML::xmlGetAttr(node,'NP'))
    FDR=as.numeric(XML::xmlGetAttr(node,'FDR'))
    nes=as.numeric(XML::xmlGetAttr(node,'NES'))
    title=gsub('^gene_sets.gmt#','',XML::xmlGetAttr(node,'GENESET'))
    length(hit.index)
    all.dat=rbind(all.dat,data.frame(Index=hit.index,ES=es.values,Term=rep(title,length(es.values))
                                     ,Type=c('A',rep('V',length(es.values)-2),'A')))
    all.info=rbind(all.info,c(title,es.values[m.inds[1]],
                              hit.index[m.inds[1]],es,nes,np,FDR))
  }
  all.info=crbind2DataFrame(all.info)
  #all.info
  
  cbPalette=color
  
  all.dat$Colour=cbPalette[as.numeric(as.factor(all.dat$Term))]
  
  col_mp=unique(cbind(as.character(all.dat$Term),all.dat$Colour))[,2]
  names(col_mp)=unique(cbind(as.character(all.dat$Term),all.dat$Colour))[,1]
  glb=unique(unlist(lapply(strsplit(all.info[,1],'_'),function(x){return(x[1])})))
  if(length(glb)==1){
    #g.labels=paste0(gsub(paste0('^',glb,'_'),'',g.labels))
    desc=gsub(paste0('^',glb,'_'),'',all.info[,1])
  }else{
    desc=all.info[,1]
  }
  ndesc=c()
  for(de in desc){
    #de=desc[1]
    if(nchar(de)>50){
      d2=paste0(substr(de,0,47),'...')
      ndesc=c(ndesc,d2)
    }else{
      ndesc=c(ndesc,de)
    }
  }
  g.labels=paste0(ndesc,'\nES=',signif(all.info[,4],2),',NES=',signif(all.info[,5],2),',P=',signif(all.info[,6],2),',FDR=',signif(all.info[,7],2))[match(names(col_mp),all.info[,1])]
  
  #dat=data.frame(Index=hit.index,ES=es.values)
  p=ggplot(data=all.dat, aes(x=Index, y=ES)) +geom_line(aes(colour=Term))+xlim(0,nrow(g.rnk))
  if(length(glb)==1){
    p=p+labs(title=paste0('Enrichment plot ',glb,' terms'))+theme(plot.title = element_text(hjust = 0.5))
  }
  p=p+scale_colour_manual(values=col_mp
                          ,breaks = names(col_mp)
                          ,labels = g.labels
  )
  p=p+ylab('Enrichment score')+theme_classic()
  #p+guides(color = FALSE)
  p=p+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                    , color="grey"
                    ,linetype="dashed")
  
  p=p+theme(
    legend.position='none'
    ,axis.title.x=element_blank()
    ,axis.text.x = element_blank(),axis.ticks.x = element_blank()
    ,plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches"))
  #p+geom_label()
  es.min=min(all.dat$ES)
  ymin=es.min-(max(all.dat$ES)-es.min)*0.1
  
  lh=(es.min-ymin)*0.7
  
  dt1=all.dat
  dt2=all.dat
  dt1$Height=rep(ymin,nrow(all.dat))-lh*(as.numeric(as.factor(all.dat$Term))-1)
  dt2$Height=rep(ymin+(es.min-ymin)*0.7,nrow(all.dat))-lh*(as.numeric(as.factor(all.dat$Term))-1)
  dt1=dt1[which(dt1$Type!='A'),]
  dt2=dt2[which(dt2$Type!='A'),]
  #dt=rbind(dt1)
  p1=ggplot()
  #es.text=rbind()
  for(g in unique(dt1$Term)){
    cl=unique(dt1$Colour[which(dt1$Term==g)])[1]
    p1=p1+geom_line(data = rbind(dt1[which(dt1$Term==g),],dt2[which(dt1$Term==g),])
                    ,aes(x=Index,y=Height,group=Index),col=cl)
    #h1=dt1$Height[which(dt1$Term==g)][1]
    #es.text=rbind(es.text,c(g,0,h1))
  }
  #es.text=crbind2DataFrame(es.text)
  #all.info$SX=es.text[match(all.info[,1],es.text[,1]),2]
  #all.info$SY=es.text[match(all.info[,1],es.text[,1]),3]
  
  #p1=p1+geom_text(data = all.info,aes(x=SX,y=SY,label = paste0('ES=',signif(V4,2),',NES=',signif(V5,2),',P=',signif(V6,2),',FDR=',signif(V7,2)))
  #              ,vjust =-0, hjust = 0)
  
  p1=p1+theme_classic()+theme(legend.position='none',axis.title.y=element_blank(),axis.text.y = element_blank()
                         ,axis.title.x=element_blank(),axis.text.x = element_blank()
                         ,axis.line.x = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank()
                         ,axis.line.x.bottom = element_blank()
                         ,plot.margin=unit(c(0, 0.2, 0, 0.1), "inches")
                         ,axis.line = element_blank()
  )
  
  #ggpubr::ggarrange(p,p1, ncol = 1, nrow = 2,heights = c(1,0.1*length(inds)),align = "v")
  
  p2=ggplot(data=data.frame(Risk=c(0,g.rnk$V2,0),Index=c(1,1:nrow(g.rnk),nrow(g.rnk))),aes(y=Risk,x=Index))+geom_line()+theme_classic()
  p2=p2+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                      , color="grey"
                      ,linetype="dashed")
  p2=p2+theme(plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches"))+ylab('Rank')+xlab('Rank in ordered dataset')
  p2=p2+geom_text(data=data.frame(Xl=c(0),Yl=c(0)),aes(x=0,y=0,label = left),vjust =1, hjust = 0)+geom_text(data=data.frame(Xl=c(nrow(g.rnk)),Yl=c(0)),
                                                                                                            aes(x=nrow(g.rnk),y=0,label = right),vjust =0, hjust = 1)
  g.h=0.1*length(inds)
  if(g.h>0.8){
    g.h=0.8
  }
  gal=ggpubr::ggarrange(p,p1,p2, ncol = 1, nrow = 3,heights = c(1,g.h,0.6),align = "v",common.legend = TRUE,legend = "right")
  return(gal)
}

plot_GSEA_By_node_wb <- function(parseGSEAResult,
                                 index=1,
                                 col = mycolor,
                                 TermName=NULL,
                                 left=NULL,
                                 right=NULL){
  library(ggplot2)
  if(is.null(parseGSEAResult$TEMPLATE)){
    if(is.null(left)){
      left='RankTop'
    }
    if(is.null(right)){
      right='RankBottom'
    }
  }
  if(is.null(left)){
    left=parseGSEAResult$TEMPLATE[1]
  }
  if(is.null(right)){
    right=parseGSEAResult$TEMPLATE[2]
  }
  if(!is.null(TermName)){
    ind=which(parseGSEAResult$EnrichTable[,1]==TermName)
    if(length(ind)==0){
      print(paste0(TermName,' Not Found!'))
      return(NA)
    }else{
      ind=ind[1]
    }
  }else{
    ind=index
    if(ind>nrow(parseGSEAResult$EnrichTable)){
      print(paste0(ind,' out range!'))
      return(NA)
    }
  }
  node=parseGSEAResult$Nodes[[ind]]
  g.rnk=parseGSEAResult$Rank
  es.values=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'ES_PROFILE'),' '))),0)
  hit.index=c(0,as.numeric(unlist(strsplit(XML::xmlGetAttr(node,'HIT_INDICES'),' '))),nrow(g.rnk))
  es=as.numeric(XML::xmlGetAttr(node,'ES'))
  nes=as.numeric(XML::xmlGetAttr(node,'NES'))
  np=as.numeric(XML::xmlGetAttr(node,'NP'))
  FDR=as.numeric(XML::xmlGetAttr(node,'FDR'))
  
  dat=data.frame(Index=hit.index,ES=es.values)
  p=ggplot(data=dat, aes(x=Index, y=ES)) +geom_line(aes(colour='darkgreen',size=2))+xlim(0,nrow(g.rnk))+theme_classic()+ylab('Enrichment score')+labs(title=gsub('^gene_sets.gmt#','',XML::xmlGetAttr(node,'GENESET')))+theme(plot.title = element_text(hjust = 0.5))
  p=p+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                    , color="grey"
                    ,linetype="dashed")
  
  #p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=0,y=min(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2)
  #                                                              ,'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
  #            ,vjust =0, hjust = 0)
  
  if(es<0){
    p=p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=0,y=min(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2),'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
                  ,vjust =0, hjust = 0)
  }else{
    p=p+geom_text(data=data.frame(Xl=c(0),yl=c(min(es.values))),aes(x=nrow(g.rnk),y=max(es.values),label = paste0('ES=',signif(es,2),'\nNES=',signif(nes,2),'\nP=',signif(np,2),'\nFDR=',signif(FDR,2)))
                  ,vjust =1, hjust = 1)
  }
  es.min=min(dat$ES)
  if(es.min>0){
    es.min=0
  }
  
  ymin=es.min-(max(dat$ES)-es.min)*0.1
  dt1=dat
  dt2=dat
  dt1$Height=rep(ymin,nrow(dat))
  dt2$Height=rep(ymin+(es.min-ymin)*0.7,nrow(dat))
  p1=p+geom_line(data = rbind(dt1,dt2),aes(x=Index,y=Height,group=Index))
  p1=p1+ggforce::geom_link(data=data.frame(x=c(0,nrow(g.rnk)),y=c(ymin,ymin),xend=c(nrow(g.rnk)/2,nrow(g.rnk)/2),yend=c(ymin,ymin))
                           ,aes(x=x,y=y,xend=xend,yend=yend
                                ,alpha=1-..index..
                                ,colour=col
                                ,size=50
                           ))
  p1=p1+theme(legend.position='none',axis.title.x=element_blank(),axis.text.x = element_blank())
  p1=p1+geom_text(data=data.frame(Xl=c(0),yl=c(ymin)),
                  aes(x=0,y=ymin,label = left),vjust =0.5, hjust = 0)+
    geom_text(data=data.frame(Xl=c(nrow(g.rnk)),yl=c(ymin)),
              aes(x=nrow(g.rnk),y=ymin,label = right),vjust =0.5, hjust = 1)
  p1=p1+theme(plot.margin=unit(c(0.2, 0.2, 0, 0.1), "inches"))
  
  p2=ggplot(data=data.frame(Risk=c(0,g.rnk$V2,0),Index=c(1,1:nrow(g.rnk),nrow(g.rnk))),
            aes(y=Risk,x=Index))+geom_line()+theme_classic()
  p2=p2+ geom_segment(aes(x = 0, xend = nrow(g.rnk), y = 0, yend = 0)
                      , color="grey"
                      ,linetype="dashed")
  p2=p2+theme(plot.margin=unit(c(0, 0.2, 0.2, 0.1), "inches"))+ylab('Rank')+xlab('Rank in ordered dataset')
  gal=ggpubr::ggarrange(p1,p2, ncol = 1, nrow = 2,heights = c(1,0.6),align = "v")
  return(gal)
}

library(ComplexHeatmap)

library(scales)
library(ggsci)
mycolor <- pal_jco(alpha =1)(9)
show_col(mycolor)
dir.create('scripts')

library(data.table)
library(stringr)
dir.create('00_origin_datas')


gene_type <- gene_type[!duplicated(gene_type$ENSGID), ]
rownames(gene_type) <- gene_type$ENSGID
table(gene_type$TYPE)
gene_type <- crbind2DataFrame(gene_type)
genes_protein <- gene_type[gene_type$TYPE == 'protein_coding', ]$SYMBOL
genes_lncRNA <- gene_type[gene_type$TYPE == 'lncRNA', ]$SYMBOL
str(genes_protein)
str(genes_lncRNA)





# 数据预处理 #############
# TCGA


tcga_cli2 <- subset(tcga_cli2,
                    type == 'LUAD')
table(tcga_cli2$new_tumor_event_type)
tcga_cli2 <- tcga_cli2[, c("bcr_patient_barcode", "type", "OS.time", "OS", 
                           "PFI.time", "PFI", "DFI.time", "DFI", "DSS.time", "DSS")]
colnames(tcga_cli2) <- c("Samples", "type", "OS.time", "OS", 
                         "PFI.time", "PFI", "DFI.time", "DFI", "DSS.time", "DSS")
tcga_cli2$Samples <- paste0(tcga_cli2$Samples, '-01')


tcga_cli <- read.delim('00_origin_datas/TCGA/Clinical BCR XML.merge.txt',
                       header = T, stringsAsFactors = F)
tcga_cli$A0_Samples <- paste0(tcga_cli$A0_Samples, '-01')
tcga_cli <- tcga_cli[, c("A0_Samples", 
                         "age_at_initial_pathologic_diagnosis",
                         "A18_Sex",
                         "A3_T", "A4_N", "A5_M",
                         "A6_Stage")]
colnames(tcga_cli) <- c("Samples",
                        "Age",
                        "Gender",
                        "A3_T", "A4_N", "A5_M",
                        "Stage")

tcga_cli$A3_T <- gsub('[ab]', '', tcga_cli$A3_T)
tcga_cli$A3_T[tcga_cli$A3_T == ''] <- NA
tcga_cli$A3_T[tcga_cli$A3_T == 'TX'] <- NA
tcga_cli$A4_N[tcga_cli$A4_N == ''] <- NA
tcga_cli$A4_N[tcga_cli$A4_N == 'NX'] <- NA
tcga_cli$A5_M <- gsub('[ab]', '', tcga_cli$A5_M)
tcga_cli$A5_M[tcga_cli$A5_M == ''] <- NA
tcga_cli$A5_M[tcga_cli$A5_M == 'MX'] <- NA
tcga_cli$Stage <- gsub('[ABC]', '', tcga_cli$Stage)
tcga_cli$Stage <- gsub('Stage ', '', tcga_cli$Stage)
tcga_cli$Stage[tcga_cli$Stage == ''] <- NA
tcga_cli$Gender <- ifelse(tcga_cli$Gender == 'MALE', 'Male', 'Female')



tcga_cli <- merge(tcga_cli2, tcga_cli, by = 'Samples', all=T)
# tcga_cli <- merge(tcga_cli, tcga_cli1, by = 'Samples', all=T)

rownames(tcga_cli) <- tcga_cli$Samples

tcga_cli <- subset(tcga_cli,
                   !is.na(OS.time) & OS.time > 0)
table(tcga_cli$OS)
# tcga_cli$OS <- ifelse(tcga_cli$OS == 'Alive', 0, 1)
tcga_cli <- crbind2DataFrame(tcga_cli)
table(tcga_cli$Stage)
colnames(tcga_cli) <- c("Samples","type","OS.time","OS","PFI.time","PFI",
                        "DFI.time","DFI","DSS.time",
                        "DSS","Age","Gender",
                        "T.Stage","N.Stage","M.Stage","Stage")


# 1.1.2、表达谱信息 #########
tcga_exp <- read.table('00_origin_datas/TCGA/TCGA-LUAD-Symbol.txt',
                       sep = '\t', header = T, check.names = F, row.names = 1)

table(substr(colnames(tcga_exp), 14, 17))
which(substr(colnames(tcga_exp), 14, 17) == '01')

tcga_group <- data.frame(row.names = colnames(tcga_exp),
                         Samples = colnames(tcga_exp),
                         Groups  = colnames(tcga_exp),
                         stringsAsFactors = F)
tcga_group$Groups <- substr(colnames(tcga_exp), 14, 17)
tcga_group <- tcga_group[tcga_group$Groups != '02', ]
table(tcga_group$Groups)
tcga_group$Groups <- ifelse(tcga_group$Groups == '01', 'Tumor', 'Adjacent')



tcga_tpm_log2 <- log2(tcga_exp[, tcga_group$Samples] + 1)
tcga_tpm_log2 <- tcga_tpm_log2[rowSums(tcga_tpm_log2) > 0, ]

dim(tcga_tpm_log2)

tmr_samples <- intersect(tcga_cli$Samples, colnames(tcga_tpm_log2))
tcga_cli <- tcga_cli[tmr_samples, ]

#GSE30219 ########
GSE30219 <- getGEOExpData('GSE30219')
GSE30219_cli <- GSE30219$Sample
colnames(GSE30219_cli)
GSE30219_cli <- GSE30219_cli[, c("Acc", "gender", "age at surgery", "pt stage", "pn stage", "pm stage",
                                 "relapse (event=1; no event=0)","disease free survival in months","Source")]
colnames(GSE30219_cli) <- c("Samples", "Gender", "Age", "T.Stage", "N.Stage","M.Stage",
                            "OS", "OS.time","Source")

GSE30219_cli<-GSE30219_cli[which(GSE30219_cli$OS!= 'NA'),]

GSE30219_cli <- subset(GSE30219_cli,
                       !is.na(OS.time) & OS.time > 0) 

table(GSE30219_cli$Source)
GSE30219_cli$OS.time<-GSE30219_cli$OS.time*30
rownames(GSE30219_cli) <- GSE30219_cli$Samples

GSE30219_anno <- GSE30219$Anno$GPL570
GSE30219_exp <- GSE30219$Exp$GPL570_54675_Data_col1
GSE30219_exp <- exp_probe2symbol_v2(datExpr = GSE30219_exp,
                                    anno = GSE30219_anno[, c(1,11)])
GSE30219_exp <- GSE30219_exp[str_split_fixed(rownames(GSE30219_exp), ' /// ', 2)[, 2] == '', ]
GSE30219_com_samples <- intersect(GSE30219_cli$Samples, colnames(GSE30219_exp))
GSE30219_cli <- GSE30219_cli[GSE30219_com_samples, ]


GSE30219_exp <- log2(GSE30219_exp + 1)
GSE30219_exp <- GSE30219_exp[rowSums(GSE30219_exp) > 0, ]


# 1.3、GSE31210 ###########
GSE31210 <- getGEOExpData('GSE31210')
GSE31210_cli <- GSE31210$Sample
colnames(GSE31210_cli)
GSE31210_cli <- GSE31210_cli[, c("Acc", "gender", "age (years)", 
                                 "death","days before death/censor")]
colnames(GSE31210_cli) <- c("Samples", "Gender", "Age",
                            "OS", "OS.time")

GSE31210_cli<-GSE31210_cli[which(GSE31210_cli$OS!= 'NA'),]
GSE31210_cli$OS<-ifelse(GSE31210_cli$OS == "dead",0,1)
GSE31210_cli <- subset(GSE31210_cli,
                       !is.na(OS.time) & OS.time > 0)      

rownames(GSE31210_cli) <- GSE31210_cli$Samples

GSE31210_anno <- GSE31210$Anno$GPL570
GSE31210_exp <- GSE31210$Exp$GPL570_54675_Data_col1
GSE31210_exp <- exp_probe2symbol_v2(datExpr = GSE31210_exp,
                                    anno = GSE31210_anno[, c(1,11)])
GSE31210_exp <- GSE31210_exp[str_split_fixed(rownames(GSE31210_exp), ' /// ', 2)[, 2] == '', ]
GSE31210_com_samples <- intersect(GSE31210_cli$Samples, colnames(GSE31210_exp))
GSE31210_cli <- GSE31210_cli[GSE31210_com_samples, ]


GSE31210_exp <- log2(GSE31210_exp + 1)
GSE31210_exp <- GSE31210_exp[rowSums(GSE31210_exp) > 0, ]

# 1.4、GSE50081 ###########
GSE50081 <- getGEOExpData('GSE50081')
GSE50081_cli <- GSE50081$Sample
colnames(GSE50081_cli)
GSE50081_cli <- GSE50081_cli[, c("Acc", "Sex", "age", "t-stage", "n-stage",
                                 "status","disease-free survival time")]
colnames(GSE50081_cli) <- c("Samples", "Gender", "Age", "T.Stage", "N.Stage",
                            "OS", "OS.time")

GSE50081_cli$OS<-ifelse(GSE50081_cli$OS == "dead",0 ,1)
GSE50081_cli<-GSE50081_cli[which(GSE50081_cli$OS!= 'NA'),]

GSE50081_cli <- subset(GSE50081_cli,
                       !is.na(OS.time) & OS.time > 0)      
GSE50081_cli$OS.time<-GSE50081_cli$OS.time*365
rownames(GSE50081_cli) <- GSE50081_cli$Samples

GSE50081_anno <- GSE50081$Anno$GPL570
GSE50081_exp <- GSE50081$Exp$GPL570_54675_Data_col1
GSE50081_exp <- exp_probe2symbol_v2(datExpr = GSE50081_exp,
                                    anno = GSE50081_anno[, c(1,11)])
GSE50081_exp <- GSE50081_exp[str_split_fixed(rownames(GSE50081_exp), ' /// ', 2)[, 2] == '', ]
GSE50081_com_samples <- intersect(GSE50081_cli$Samples, colnames(GSE50081_exp))
GSE50081_cli <- GSE50081_cli[GSE50081_com_samples, ]


GSE50081_exp <- log2(GSE50081_exp + 1)
GSE50081_exp <- GSE50081_exp[rowSums(GSE50081_exp) > 0, ]

# 1.4、GSE19188 ###########
GSE19188 <- getGEOExpData('GSE19188')
GSE19188_cli <- GSE19188$Sample
colnames(GSE19188_cli)
GSE19188_cli <- GSE19188_cli[, c("Acc", "status","overall survival","tissue type")]
colnames(GSE19188_cli) <- c("Samples", "OS", "OS.time","tissue type")

GSE19188_cli<-GSE19188_cli[which(GSE19188_cli$OS!= 'Not available'),]
GSE19188_cli$OS<-ifelse(GSE19188_cli$OS == "alive",1 ,0)


GSE19188_cli <- subset(GSE19188_cli,
                       !is.na(OS.time) & OS.time > 0)  
table(GSE19188_cli$`tissue type`)

GSE19188_cli$OS.time<-as.numeric(GSE19188_cli$OS.time)
GSE19188_cli$OS.time<-GSE19188_cli$OS.time*30
rownames(GSE19188_cli) <- GSE19188_cli$Samples

GSE19188_anno <- GSE19188$Anno$GPL570
GSE19188_exp <- GSE19188$Exp$GPL570_54675_Data_col1
GSE19188_exp <- exp_probe2symbol_v2(datExpr = GSE19188_exp,
                                    anno = GSE19188_anno[, c(1,11)])
View(GSE19188_exp)
GSE19188_exp <- GSE19188_exp[str_split_fixed(rownames(GSE19188_exp), ' /// ', 2)[, 2] == '', ]
GSE19188_com_samples <- intersect(GSE19188_cli$Samples, colnames(GSE19188_exp))
GSE19188_cli <- GSE19188_cli[GSE19188_com_samples, ]


# hrd #############
dir.create('01_feature')
kegg.list <- clusterProfiler::read.gmt('00_origin_datas/msigdb_v7.4_GMTs/c2.cp.kegg.v7.4.symbols.gmt')
kegg.list <- split(kegg.list$gene, kegg.list$ont)
sort(names(kegg.list))
kegg.list[['KEGG_FATTY_ACID_METABOLISM']]

# write.table(kegg.list[['KEGG_FATTY_ACID_METABOLISM']],
#             file = 'results/KEGG_FATTY_ACID_METABOLISM.txt',
#             row.names = F, col.names = F, quote = F)


h.list <- clusterProfiler::read.gmt('00_origin_datas/msigdb_v7.4_GMTs/h.all.v7.4.symbols.gmt')
h.list <- split(h.list$gene, h.list$ont)
sort(names(h.list))


hrd.list <- clusterProfiler::read.gmt('00_origin_datas/hrd_PATHWAY.v2022.1.Hs.gmt')
hrd.list <- split(hrd.list$gene, hrd.list$ont)

write.table(hrd.list[[1]],
            file = 'results/hrd_PATHWAY.txt',
            row.names = F, col.names = F, sep = '\t', quote = F)


save(tcga_tpm_log2,
     GSE30219_exp,
     GSE31210_exp,
     GSE50081_exp,
     kegg.list,
     h.list,
     hrd.list,
     ssGSEAScore_by_muti_group_genes,
     file = 'ssgsea1.Rdata')
load('ssgsea1.Rdata')


tcga_kegg_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                   genelist = kegg.list)
GSE30219_kegg_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                      genelist = kegg.list)
GSE31210_kegg_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                       genelist = kegg.list)
GSE50081_kegg_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE50081_exp,
                                                       genelist = kegg.list)

tcga_hallmark_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                       genelist = h.list)
GSE30219_hallmark_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                          genelist = h.list)
GSE31210_hallmark_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                           genelist = h.list)
GSE50081_hallmark_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE50081_exp,
                                                           genelist = h.list)



tcga_hrd_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                   genelist = hrd.list)
GSE30219_hrd_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                      genelist = hrd.list)
GSE31210_hrd_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                       genelist = hrd.list)
GSE50081_hrd_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE50081_exp,
                                                       genelist = hrd.list)


save(tcga_kegg_score,
     GSE30219_kegg_score,
     GSE31210_kegg_score,
     GSE50081_kegg_score,
     tcga_hrd_score,
     GSE30219_hrd_score,
     GSE31210_hrd_score,
     GSE50081_hrd_score,
     tcga_hallmark_score,
     GSE30219_hallmark_score,
     GSE31210_hallmark_score,
     GSE50081_hallmark_score,
     file = 'ssgsea_res1.Rdata')
load('ssgsea_res1.Rdata')


# hrd 评分在肿瘤和癌旁中的差异比较 #############
dim(tcga_kegg_score)
tcga_hrd_violin <- mg_violin(data.frame(tcga_group$Groups,
                                               as.numeric(tcga_hrd_score["hrd_PATHWAY", tcga_group$Samples])),
                                    melt=TRUE, ylab='hrd_PATHWAY',
                                    leg.title='',
                                    jitter = F,
                                    xlab = 'TCGA',
                                    legend.pos='bl',
                                    show_compare = T)
tcga_hrd_violin


# FATTY_ACID 评分在肿瘤和癌旁中的 GSEA 比较 #############
dir.create('01_feature/')
# TCGA
dir.create('01_feature/TCGA')
dim(tcga_tpm_log2)
library(dplyr)
table(tcga_group$Groups)
tcga_group <- arrange(tcga_group, Groups)
head(tcga_group[, c("Samples", "Groups")])

write.table(tcga_group[, c("Samples", "Groups")], 
            file = '01_feature/TCGA/gsea_groups.txt', sep = '\t', 
            row.names = F, quote = F)
write.table(tcga_tpm_log2[intersect(genes_protein, rownames(tcga_tpm_log2)), 
                          tcga_group$Samples], 
            file = '01_feature/TCGA/tcga_gsae_data.txt', sep = '\t', quote = F)

mg_RunGSEA(mod = 'exp_group',
           exp_Path = '01_feature/TCGA/tcga_gsae_data.txt',
           sample_group_path = '01_feature/TCGA/gsea_groups.txt',
           outFolder = '01_feature/TCGA/results/',
           gmt_Path = '00_origin_datas/hrd_PATHWAY.v2022.1.Hs.gmt', 
           outLog=T)

# 读取 GSEA 的分析结果 
tcga_GSEA <- parseGSEAResult('01_feature/TCGA/results/my_analysis.Gsea.1681098823379/')
dim(tcga_GSEA$EnrichTable)
tcga_GSEA_EnrichTable <- as.data.frame(tcga_GSEA$EnrichTable)
library(dplyr)
write.table(tcga_GSEA_EnrichTable, file = '01_feature/TCGA_GSEA_results.txt', 
            sep = '\t', row.names = F, quote = F)
tcga_GSEA_Fatty_acid_plot <- plot_GSEA_By_node_wb(tcga_GSEA,
                                                  col = mycolor[1:2],
                                                  # col = c('red', 'blue'),
                                                  TermName = 'hrd_PATHWAY')
tcga_GSEA_Fatty_acid_plot


violin_GSEA_plot <- cowplot::plot_grid(tcga_hrd_violin,
                                       tcga_GSEA_Fatty_acid_plot,
                                       ncol = 2)
violin_GSEA_plot
ggsave(plot = violin_GSEA_plot,
       filename = '01_feature/violin_GSEA_plot.pdf',
       width = 20, height = 10)


# 单因素分析 ###############
hrd.genes <- read.delim('00_origin_datas/HRDgenes.txt',header = F, stringsAsFactors = F)[,1]
length(hrd.genes)    # 109
dir.create('01_feature')
pval.cutoff <- 0.05
# 3.1、TCGA 
dim(tcga_tpm_log2)

tcga_dat_m <- tcga_tpm_log2[intersect(hrd.genes, rownames(tcga_tpm_log2)), 
                            tcga_cli$Samples]
tcga.cox <- cox_batch(tcga_dat_m,
                      time = tcga_cli$OS.time,
                      event = tcga_cli$OS)
tcga.cox <- na.omit(tcga.cox)
class(tcga.cox)
tcga.cox.up <- row.names(tcga.cox[which(tcga.cox[,1] < pval.cutoff&tcga.cox[,2] > 1),])
tcga.cox.down <- row.names(tcga.cox[which(tcga.cox[,1] < pval.cutoff&tcga.cox[,2] < 1),])

tcga.cox.filtered <- tcga.cox[tcga.cox$p.value < 0.01, ]
dim(tcga.cox.filtered)     # 16
table(tcga.cox.filtered$HR > 1)  # 16
write.csv(tcga.cox,
          file = '01_feature/tcga.sigcox.csv')


head(tcga.cox.filtered)
tcga.cox.filtered$Genes <- rownames(tcga.cox.filtered)
tcga.cox.filtered <- tcga.cox.filtered[, c(5,1,2,3,4)]
head(tcga.cox.filtered)
tcga.cox.filtered$p.value <- signif(tcga.cox.filtered$p.value, 2)
tcga.cox.filtered <- dplyr::arrange(tcga.cox.filtered, desc(HR))
pdf('01_feature/tcga_sig_cox.pdf', width = 6, height = 12, onefile = F)
mg_forestplot_v2(tcga.cox.filtered,
                 lineheight =8,colgap =3,lwd.zero=2,
                 box_col=mycolor[1],
                 summary_col=mycolor[2],
                 lines_col=mycolor[3],
                 zero_col=mycolor[4])
dev.off()


hrd.sig.genes <- rownames(tcga.cox.filtered)

# 突变分析 #########
tcga.mut.dat <- getTCGAMAFByCode('LUAD')

plotmafSummary(maf = tcga.mut.dat, 
               rmOutlier = TRUE, 
               addStat = 'median', 
               dashboard = TRUE, 
               titvRaw = FALSE)

?oncoplot
dev.off()
pdf('01_feature/SNV.pdf',width = 8, height = 8)
oncoplot(maf = tcga.mut.dat, 
         genes = hrd.sig.genes,
         showTitle = F,
         removeNonMutated = F,
         draw_titv = T)
dev.off()



# CNV 分析 ################
tcga_pancan_cnv <- read.delim('00_origin_datas/broad.mit.edu_PANCAN_Genome_Wide_SNP_6_whitelisted.gene.xena.gz', row.names = 1, header = T, check.names = F)
tcga_pancan_cnv[1:5, 1:2]
intersect(hrd.sig.genes, rownames(tcga_pancan_cnv))
hrd.sig.genes
intersect('CARM1', rownames(tcga_pancan_cnv))
tcga_pancan_cnv <- tcga_pancan_cnv[intersect(hrd.sig.genes, rownames(tcga_pancan_cnv)), 
                                   intersect(colnames(tcga_pancan_cnv), tcga_cli$Samples)]
tcga_pancan_cnv <- ifelse(tcga_pancan_cnv >= 0.2, 'Gain', ifelse(tcga_pancan_cnv <= -0.2, 'Loss', 'Diploid'))
tcga_pancan_cnv[1:5, 1:3]
tcga_pancan_cnv <- as.data.frame(t(tcga_pancan_cnv))
dim(tcga_pancan_cnv)






ge_freq <- c()
for (ge in colnames(tcga_pancan_cnv)) {
  for (ty in c("Diploid", "Loss", "Gain")) {
    anum <- length(which(tcga_pancan_cnv[, ge] == ty)) / 355
    ge_freq <- c(ge_freq, anum)
  }
}

length(colnames(tcga_pancan_cnv))    # 16
tcga_pancan_cnv1 <- data.frame(Genes = rep(colnames(tcga_pancan_cnv), each = 3),
                               Type = rep(c("Diploid", "Loss", "Gain"), 16),
                               Freq = ge_freq)

tcga_pancan_cnv1$Type <- factor(tcga_pancan_cnv1$Type, levels = c("Gain", "Loss", "Diploid"))

library(ggplot2)
tcga_CNV_bar <- ggplot(tcga_pancan_cnv1, aes(x=Genes, y=Freq, fill=Type)) + 
  geom_bar(stat="identity",position = "stack", width = 0.75) +
  scale_fill_jco() + theme_classic() +
  theme(axis.text.x = element_text(angle=90))
tcga_CNV_bar
ggsave(plot = tcga_CNV_bar,
       filename = '01_feature/tcga_CNV_bar.pdf',
       width = 6, height = 4)



tcga_hrd.sig.genes_dat <- data.frame()
for (ge in hrd.sig.genes) {
  print(ge)
  tmp <- data.frame(Samples = tcga_group$Samples,
                    Genes = ge,
                    Values = as.numeric(tcga_tpm_log2[ge, tcga_group$Samples]),
                    Groups = tcga_group$Groups)
  tcga_hrd.sig.genes_dat <- rbind(tcga_hrd.sig.genes_dat, tmp)
}


head(tcga_hrd.sig.genes_dat)
library(ggpubr)
tcga_exp_plot <- ggplot(tcga_hrd.sig.genes_dat, 
                        aes(x=Genes, y=Values, fill=Groups)) +
  geom_boxplot(notch = T) +  
  stat_compare_means(method = "t.test", label = "p.signif") +
  scale_fill_jco() + theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1,vjust = 0.5)) +
  xlab('Genes') + ylab('Expression of genes')
tcga_exp_plot 
ggsave(plot = tcga_exp_plot,
       filename = '01_feature/tcga_exp_plot.pdf',
       width = 10, height = 6)








# 分子亚型 ###############
dir.create('02_subtype')

hrd.sig.genes

## 识别亚型 #################
library(ConsensusClusterPlus)
#  tcga 
tcga.sig.dat <- as.matrix(tcga_tpm_log2[hrd.sig.genes, tcga_cli$Samples])
tcga.sig.dat <- sweep(tcga.sig.dat,1, apply(tcga.sig.dat,1,median))
dim(tcga.sig.dat)
?ConsensusClusterPlus
getwd()
{
  setwd('02_subtype/')
  clust_subtype_tcga <- ConsensusClusterPlus(tcga.sig.dat
                                             , maxK = 10, reps = 500, pItem = 0.8
                                             , pFeature = 1
                                             , title = "tcga_LUAD_subtype"
                                             , clusterAlg = "pam"
                                             , distance = "euclidean"
                                             , plot = "pdf"
                                             , writeTable = T
                                             , seed = 123)
  setwd('..')
}


k=3
tcga.subtype <- data.frame(Sample = names(clust_subtype_tcga[[k]]$consensusClass),
                           Cluster = paste0('C', clust_subtype_tcga[[k]]$consensusClass),
                           stringsAsFactors = F)
rownames(tcga.subtype) <- tcga.subtype$Sample

writeMatrix(tcga.subtype,row=T,header=T
            ,outpath = 'results/tcga_LUAD_subtype.txt')
tcga.subtype.cli <- cbind(tcga_cli, tcga.subtype[tcga_cli$Samples, ])
library(survcomp)
tcga_cluster_os_km <- ggplotKMCox(data.frame(tcga.subtype.cli$OS.time/365
                                             , event = tcga.subtype.cli$OS
                                             , tcga.subtype.cli$Cluster)
                                  ,labs = c('C1','C2','C3')
                                  ,title = 'TCGA OS'
                                  , add_text = '')
tcga_cluster_os_km   # 0.0031

tcga_cluster_pfi_km <- ggplotKMCox(data.frame(tcga.subtype.cli$PFI.time/365
                                              , event = tcga.subtype.cli$PFI
                                              , tcga.subtype.cli$Cluster)
                                   ,labs = c('C1','C2','C3')
                                   ,title = 'TCGA PFI'
                                   , add_text = '')
tcga_cluster_pfi_km   # 0.036

tcga_cluster_dfi_km <- ggplotKMCox(data.frame(tcga.subtype.cli$DFI.time/365
                                              , event = tcga.subtype.cli$DFI
                                              , tcga.subtype.cli$Cluster)
                                   ,labs = c('C1','C2','C3')
                                   ,title = 'TCGA DFI'
                                   , add_text = '')
tcga_cluster_dfi_km  # 0.076

tcga_cluster_dss_km <- ggplotKMCox(data.frame(tcga.subtype.cli$DSS.time/365
                                              , event = tcga.subtype.cli$DSS
                                              , tcga.subtype.cli$Cluster)
                                   ,labs = c('C1','C2','C3')
                                   ,title = 'TCGA DSS'
                                   , add_text = '')
tcga_cluster_dss_km  # 0.0022

tcga_cluster_km <- cowplot::plot_grid(tcga_cluster_os_km,
                                      tcga_cluster_pfi_km,
                                      tcga_cluster_dfi_km,
                                      tcga_cluster_dss_km,
                                      ncol = 4)
tcga_cluster_km





ggsave(plot = tcga_cluster_os_km,
       filename = '02_subtype/tcga.KM.plot.pdf',
       width = 5, height = 5)


#  GSE30219 
GSE30219_cli$samples <- rownames(GSE30219_cli)

intersect(hrd.sig.genes, rownames(GSE30219_exp))
GSE30219.sig.dat <- as.matrix(GSE30219_exp[hrd.sig.genes, GSE30219_cli$samples])
GSE30219.sig.dat <- sweep(GSE30219.sig.dat,1, apply(GSE30219.sig.dat,1,median))
dim(GSE30219.sig.dat)
?ConsensusClusterPlus
getwd()
{
  setwd('02_subtype/')
  clust_subtype_GSE30219 <- ConsensusClusterPlus(GSE30219.sig.dat
                                                , maxK = 10, reps = 500, pItem = 0.8
                                                , pFeature = 1
                                                , title = "GSE30219_LUAD_subtype"
                                                , clusterAlg = "pam"
                                                , distance = "spearman"
                                                , plot = "pdf"
                                                , writeTable = T
                                                , seed = 123)
  setwd('..')
}


k=3
GSE30219.subtype <- data.frame(Sample = names(clust_subtype_GSE30219[[k]]$consensusClass),
                              Cluster = paste0('C', clust_subtype_GSE30219[[k]]$consensusClass),
                              stringsAsFactors = F)
rownames(GSE30219.subtype) <- GSE30219.subtype$Sample

# GSE30219.subtype$Cluster[GSE30219.subtype$Cluster == 'C1'] <- 'MC2'
# GSE30219.subtype$Cluster[GSE30219.subtype$Cluster == 'C2'] <- 'MC1'
# GSE30219.subtype$Cluster[GSE30219.subtype$Cluster == 'C3'] <- 'MC1'
# GSE30219.subtype$Cluster <- gsub('M', '', GSE30219.subtype$Cluster)


# writeMatrix(GSE30219.subtype,row=T,header=T
#             ,outpath = 'files/GSE30219_LUAD_subtype.txt')
writeMatrix(GSE30219.subtype,row=T,header=T
            ,outpath = 'results/GSE30219_LUAD_subtype.txt')
GSE30219.subtype.cli <- cbind(GSE30219_cli, GSE30219.subtype[GSE30219_cli$Samples, ])
library(survcomp)
GSE30219_cluster_os_km <- ggplotKMCox(data.frame(GSE30219.subtype.cli$OS.time/365
                                                , event = GSE30219.subtype.cli$OS
                                                , GSE30219.subtype.cli$Cluster)
                                     ,labs = c('C1','C2','C3')
                                     ,title = 'GSE30219'
                                     , add_text = '')
GSE30219_cluster_os_km  # 0.0001


#  GSE31210
library(ConsensusClusterPlus)
intersect(hrd.sig.genes, rownames(GSE31210_exp))
GSE31210.sig.dat <- as.matrix(GSE31210_exp[intersect(hrd.sig.genes, rownames(GSE31210_exp)),
                                           GSE31210_cli$Samples])
GSE31210.sig.dat <- sweep(GSE31210.sig.dat,1, apply(GSE31210.sig.dat,1,median))
dim(GSE31210.sig.dat)
?ConsensusClusterPlus
getwd()

{
  setwd('02_subtype/')
  clust_subtype_GSE31210 <- ConsensusClusterPlus(GSE31210.sig.dat
                                                 , maxK = 10, reps = 500, pItem = 0.8
                                                 , pFeature = 1
                                                 , title = "GSE31210_LUAD_subtype"
                                                 , clusterAlg = "pam"
                                                 , distance = "spearman"
                                                 , plot = "pdf"
                                                 , writeTable = T
                                                 , seed = 123)
  setwd('..')
}


k=3
GSE31210.subtype <- data.frame(Sample = names(clust_subtype_GSE31210[[k]]$consensusClass),
                               Cluster = paste0('C', clust_subtype_GSE31210[[k]]$consensusClass),
                               stringsAsFactors = F)
rownames(GSE31210.subtype) <- GSE31210.subtype$Sample

GSE31210.subtype.cli <- cbind(GSE31210_cli, GSE31210.subtype[GSE31210_cli$Samples, ])
library(survcomp)
GSE31210.KM.plot=ggplotKMCox(data.frame(GSE31210.subtype.cli$OS.time/365
                             , event = GSE31210.subtype.cli$OS
                             , GSE31210.subtype.cli$Cluster)
                   ,labs = c('C1','C2','C3')
                  ,title = 'GSE31210'
                  , add_text = '')
GSE31210.KM.plot


#  GSE50081
library(ConsensusClusterPlus)
intersect(hrd.sig.genes, rownames(GSE50081_exp))
GSE50081.sig.dat <- as.matrix(GSE50081_exp[intersect(hrd.sig.genes, rownames(GSE50081_exp)),
                                           GSE50081_cli$Samples])
GSE50081.sig.dat <- sweep(GSE50081.sig.dat,1, apply(GSE50081.sig.dat,1,median))
dim(GSE50081.sig.dat)
?ConsensusClusterPlus
getwd()

{
  setwd('02_subtype/')
  clust_subtype_GSE50081 <- ConsensusClusterPlus(GSE50081.sig.dat
                                                 , maxK = 10, reps = 500, pItem = 0.8
                                                 , pFeature = 1
                                                 , title = "GSE50081_LUAD_subtype"
                                                 , clusterAlg = "pam"
                                                 , distance = "euclidean"
                                                 , plot = "pdf"
                                                 , writeTable = T
                                                 , seed = 123)
  setwd('..')
}


k=2
GSE50081.subtype <- data.frame(Sample = names(clust_subtype_GSE50081[[k]]$consensusClass),
                               Cluster = paste0('C', clust_subtype_GSE50081[[k]]$consensusClass),
                               stringsAsFactors = F)
rownames(GSE50081.subtype) <- GSE50081.subtype$Sample

GSE50081.subtype.cli <- cbind(GSE50081_cli, GSE50081.subtype[GSE50081_cli$Samples, ])
library(survcomp)
GSE50081.KM.plot=ggplotKMCox(data.frame(GSE50081.subtype.cli$OS.time/365
                                        , event = GSE50081.subtype.cli$OS
                                        , GSE50081.subtype.cli$Cluster)
                              ,labs = c('C1','C2')
                             ,title = 'GSE50081'
                             , add_text = '')
GSE50081.KM.plot    # 0.033

#  GSE19188
library(ConsensusClusterPlus)
intersect(hrd.sig.genes, rownames(GSE19188_exp))
GSE19188.sig.dat <- as.matrix(GSE19188_exp[intersect(hrd.sig.genes, rownames(GSE19188_exp)),
                                           GSE19188_cli$Samples])
GSE19188.sig.dat <- sweep(GSE19188.sig.dat,1, apply(GSE19188.sig.dat,1,median))
dim(GSE19188.sig.dat)
?ConsensusClusterPlus
getwd()

{
  setwd('02_subtype/')
  clust_subtype_GSE19188 <- ConsensusClusterPlus(GSE19188.sig.dat
                                                 , maxK = 10, reps = 500, pItem = 0.8
                                                 , pFeature = 1
                                                 , title = "GSE19188_LUAD_subtype"
                                                 , clusterAlg = "pam"
                                                 , distance = "pearson"
                                                 , plot = "pdf"
                                                 , writeTable = T
                                                 , seed = 123)
  setwd('..')
}


k=3
GSE19188.subtype <- data.frame(Sample = names(clust_subtype_GSE19188[[k]]$consensusClass),
                               Cluster = paste0('C', clust_subtype_GSE19188[[k]]$consensusClass),
                               stringsAsFactors = F)
rownames(GSE19188.subtype) <- GSE19188.subtype$Sample

GSE19188.subtype.cli <- cbind(GSE19188_cli, GSE19188.subtype[GSE19188_cli$Samples, ])
library(survcomp)
GSE19188.KM.plot=ggplotKMCox(data.frame(GSE19188.subtype.cli$OS.time/365
                                        , event = GSE19188.subtype.cli$OS
                                        , GSE19188.subtype.cli$Cluster)
                              ,labs = c('C1','C2','C3')
                             ,title = 'GSE19188'
                             , add_text = '')
GSE19188.KM.plot   # 0.067



tcga_GSE30219_cluster_km <- cowplot::plot_grid(tcga_cluster_os_km,
                                      tcga_cluster_pfi_km,
                                      tcga_cluster_dss_km,
                                      GSE30219_cluster_os_km,
                                      ncol = 4)
tcga_GSE30219_cluster_km
ggsave(plot = tcga_GSE30219_cluster_km,
       filename = '02_subtype/tcga.GSE30219.KM.plot.pdf',
       width = 20, height = 5)


# 亚型 PCA 分析 ############
# TCGA 
library(ggbiplot)
library(ggsci)
tcga_pca <- as.data.frame(t(tcga_tpm_log2[hrd.sig.genes, 
                                          tcga.subtype.cli$Samples]),
                          stringsAsFactors = F)
tcga_pca<-prcomp(tcga_pca, scale=T)
tcga_pca_plot <- ggbiplot(tcga_pca, scale=1, groups = tcga.subtype.cli$Cluster,
                          ellipse = TRUE,ellipse.prob=0.5, circle = F,var.axes=F) +
  scale_color_jco() +
  # xlim(-2, 2) + ylim(-3, 3) +
  theme_classic() +
  theme(legend.position = 'top')  + xlab('PCA1') + ylab('PCA2')
tcga_pca_plot


# GSE30219 
library(ggbiplot)
library(ggsci)
GSE30219_exp[1:5, 1:5]
GSE30219_pca <- as.data.frame(t(GSE30219_exp[intersect(hrd.sig.genes, 
                                                     rownames(GSE30219_exp)), 
                                           GSE30219.subtype.cli$Samples]),
                             stringsAsFactors = F)
GSE30219_pca<-prcomp(GSE30219_pca, scale=T)
GSE30219_pca_plot <- ggbiplot(GSE30219_pca, scale=1, groups = GSE30219.subtype.cli$Cluster,
                             ellipse = TRUE,ellipse.prob=0.5, circle = F,var.axes=F) +
  scale_color_jco() +
  # xlim(-2, 2) + ylim(-3, 3) +
  theme_classic() +
  xlab('PCA1') + ylab('PCA2') +
  theme(legend.position = 'top')
GSE30219_pca_plot

tcga_GSE30219_pca_plot <- cowplot::plot_grid(tcga_pca_plot,
                                            GSE30219_pca_plot,
                                            ncol = 2,
                                            labels = LETTERS[5:8])
tcga_GSE30219_pca_plot
library(ggplot2)
ggsave(plot = tcga_GSE30219_pca_plot,
       filename = '02_subtype/tcga_GSE30219_pca_plot.pdf',
       width = 10, height = 5)



library(ComplexHeatmap)
library(dplyr)
class(tcga.subtype.cli)
colnames(tcga.subtype.cli)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
tcga.subtype.cli <- arrange(tcga.subtype.cli, Cluster)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
tcga_siggene_pheatmap <- t(scale(t(tcga_tpm_log2[hrd.sig.genes, 
                                                 tcga.subtype.cli$Samples])))

table(tcga.subtype.cli$Cluster)
tcga_siggene_plot <- Heatmap(tcga_siggene_pheatmap[, tcga.subtype.cli$Samples]
                             , name = "score"
                             , col = circlize::colorRamp2(c(-2, 0, 2), c(mycolor[1], 
                                                                         'white',
                                                                         mycolor[2]))
                             , border = T
                             , show_column_names = F
                             , show_row_names = F
                             , show_column_dend = F
                             , show_row_dend = F
                             , cluster_columns = T
                             , cluster_rows = T
                             , column_split = factor(tcga.subtype.cli$Cluster)
                             , clustering_distance_rows  ='pearson'
                             , clustering_method_rows = 'ward.D2'
                             , clustering_distance_columns  ='pearson'
                             , clustering_method_columns = 'ward.D2'
                             , row_names_gp = gpar(fontsize = 10)
                             , top_annotation = HeatmapAnnotation(Cluster = tcga.subtype.cli$Cluster
                                                                  , col=list(Cluster=c('C1'=mycolor[1], 
                                                                                       'C2' = mycolor[2],
                                                                                       'C3' = mycolor[3]))
                                                                  , annotation_width = unit(c(1,2), 'cm')
                                                                  , annotation_height = unit(0.2, "cm")
                                                                  , gap = unit(1, 'mm')))

tcga_siggene_plot
dev.off()
pdf('02_subtype/tcga_siggene_plot.pdf',
    width = 6, height = 4)
tcga_siggene_plot
dev.off()


# 免疫浸润分析 ###############
dir.create('03_cluster_immune')

tme1 <- openxlsx::read.xlsx('00_origin_datas/PMID.28052254.xlsx', sheet = 1)
tme1.list1 <- split(tme1$Metagene, tme1$Cell.type)
tme1.list2 <- split(tme1$Metagene, tme1$Immunity)

tme2 <-clusterProfiler::read.gmt('00_origin_datas/29_immu_signature_pmid_30594216.txt')
tme2 <- tme2[tme2$gene != '', ]
tme2.list <- split(tme2$gene, tme2$ont)

ImmuCellAI <- clusterProfiler::read.gmt('00_origin_datas/ImmuCellAI_marker_genes.txt')
ImmuCellAI <- ImmuCellAI[ImmuCellAI$gene != '', ]
table(ImmuCellAI$ont)
ImmuCellAI.list <- split(ImmuCellAI$gene, ImmuCellAI$ont)



save(tcga_tpm_log2,
     GSE30219_exp,
     GSE31210_exp,
     tme1.list1,
     tme1.list2,
     tme2.list,
     ImmuCellAI.list,
     ssGSEAScore_by_muti_group_genes,
     file = 'ssgsea2.Rdata')
load('ssgsea2.Rdata')
# TCGA
tcga_tme1.list1_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                         genelist = tme1.list1)
tcga_tme1.list2_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                         genelist = tme1.list2)
tcga_tme2.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                        genelist = tme2.list)
tcga_ImmuCellAI.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                              genelist = ImmuCellAI.list)


# GSE30219
GSE30219_tme1.list1_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                            genelist = tme1.list1)
GSE30219_tme1.list2_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                            genelist = tme1.list2)
GSE30219_tme2.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                           genelist = tme2.list)
GSE30219_ImmuCellAI.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE30219_exp,
                                                                 genelist = ImmuCellAI.list)


# GSE31210
GSE31210_tme1.list1_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                             genelist = tme1.list1)
GSE31210_tme1.list2_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                             genelist = tme1.list2)
GSE31210_tme2.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                            genelist = tme2.list)
GSE31210_ImmuCellAI.list_score <- ssGSEAScore_by_muti_group_genes(gene.exp = GSE31210_exp,
                                                                  genelist = ImmuCellAI.list)



save(tcga_tme1.list1_score,
     tcga_tme1.list2_score,
     tcga_tme2.list_score,
     tcga_ImmuCellAI.list_score,
     GSE31210_tme1.list1_score,
     GSE31210_tme1.list2_score,
     GSE31210_tme2.list_score,
     GSE31210_ImmuCellAI.list_score,
     GSE30219_tme1.list1_score,
     GSE30219_tme1.list2_score,
     GSE30219_tme2.list_score,
     GSE30219_ImmuCellAI.list_score,
     file = 'ssgsea_res2.Rdata')
load('ssgsea_res2.Rdata')






wb_boxplot <- function(dat = data,
                       groups = groups,
                       xlab = '',
                       ylab = '',
                       xangle = 90,
                       title = 'Groups',
                       col = mycolor) {
  tmp.dat <- data.frame()
  for (ge in rownames(dat)) {
    print(ge)
    tmp <- data.frame(Samples = colnames(dat),
                      Genes = ge,
                      Values = as.numeric(dat[ge, ]),
                      Groups = groups)
    tmp.dat <- rbind(tmp.dat, tmp)
  }
  
  
  print(head(tmp.dat))
  library(ggpubr)
  if (length(unique(groups)) > 2) {
    tmp_plot <- ggplot(tmp.dat, 
                       aes(x=Genes, y=Values, fill=Groups)) +
      geom_boxplot(notch = F) +  
      stat_compare_means(method = "anova", label = "p.signif") +
      scale_fill_manual(values = col) + theme_classic() +
      theme(axis.text.x = element_text(angle=xangle, 
                                       hjust = 0.95,
                                       vjust = 0.95)) +
      xlab(xlab) + ylab(ylab) + guides(fill = guide_legend(title = title))
    return(tmp_plot)
  } else {
    tmp_plot <- ggplot(tmp.dat, 
                       aes(x=Genes, y=Values, fill=Groups)) +
      geom_boxplot(notch = F) +  
      stat_compare_means(method = "t.test", label = "p.signif") +
      scale_fill_manual(values = col) + theme_classic() +
      theme(axis.text.x = element_text(angle=xangle, 
                                       hjust = 0.95,
                                       vjust = 0.95)) +
      xlab(xlab) + ylab(ylab) + guides(fill = guide_legend(title = title))
    return(tmp_plot)
  }
}

tcga_tme1.list1_score_plot <- wb_boxplot(dat = tcga_tme1.list1_score[, tcga.subtype.cli$Samples],
                                         groups = tcga.subtype.cli$Cluster,
                                         title = 'Cluster',
                                         xangle = 60,
                                         ylab = 'Score',
                                         col = mycolor)
tcga_tme1.list1_score_plot

tcga_tme1.list2_score_plot <- wb_boxplot(dat = tcga_tme1.list2_score[, tcga.subtype.cli$Samples],
                                         groups = tcga.subtype.cli$Cluster,
                                         xangle = 0,
                                         title = 'Cluster',
                                         ylab = 'Score',
                                         col = mycolor)
tcga_tme1.list2_score_plot

tcga_tme1.list2_score_list <- list()
for (fea in rownames(tcga_tme1.list2_score)) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$Cluster,
                              as.numeric(tcga_tme1.list2_score[fea, 
                                                               tcga.subtype.cli$Samples])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='tl',
                   show_compare = T)
  tcga_tme1.list2_score_list[[fea]] <- tmp
}

tcga_tme1.list2_score_plot <- cowplot::plot_grid(plotlist = tcga_tme1.list2_score_list,
                                                 ncol = 2)
tcga_tme1.list2_score_plot



tcga_estimate <- immu_estimate(exp = tcga_tpm_log2)
tcga_estimate_clister_list <- list()
for (fea in colnames(tcga_estimate)) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$Cluster,
                              as.numeric(tcga_estimate[tcga.subtype.cli$Samples, fea])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='tl',
                   show_compare = T)
  tcga_estimate_clister_list[[fea]] <- tmp
}

tcga_estimate_cluster_plot <- cowplot::plot_grid(plotlist = tcga_estimate_clister_list,
                                                 ncol = 3)
tcga_estimate_cluster_plot


tcga_mcpcounter <- immu_MCPcounter(exp = tcga_tpm_log2)
dim(tcga_mcpcounter)

tcga_mcpcounter_score_plot <- wb_boxplot(dat = t(tcga_mcpcounter[tcga.subtype.cli$Samples, ]),
                                         groups = tcga.subtype.cli$Cluster,
                                         title = 'Cluster',
                                         xangle = 60,
                                         ylab = 'Score',
                                         col = mycolor)
tcga_mcpcounter_score_plot




tcga_tme2.list_score_plot <- wb_boxplot(dat = tcga_tme2.list_score[, tcga.subtype.cli$Samples],
                                        groups = tcga.subtype.cli$Cluster,
                                        title = 'Cluster',
                                        xangle = 60,
                                        ylab = 'Score',
                                        col = mycolor)
tcga_tme2.list_score_plot

tcga_ImmuCellAI.list_score_plot <- wb_boxplot(dat = tcga_ImmuCellAI.list_score[, tcga.subtype.cli$Samples],
                                              groups = tcga.subtype.cli$Cluster,
                                              title = 'Cluster',
                                              xangle = 60,
                                              ylab = 'Score',
                                              col = mycolor)
tcga_ImmuCellAI.list_score_plot

tcga_immune_cluster_plot1 <- cowplot::plot_grid(tcga_tme1.list2_score_plot,
                                                tcga_estimate_cluster_plot,
                                                tcga_mcpcounter_score_plot,
                                                ncol = 3,
                                                rel_widths = c(2,3,2.5))
tcga_immune_cluster_plot1


tcga_immune_cluster_plot <- cowplot::plot_grid(tcga_tme1.list1_score_plot,
                                               tcga_immune_cluster_plot1,
                                               ncol = 1)
tcga_immune_cluster_plot
ggsave(plot = tcga_immune_cluster_plot,
       filename = '03_cluster_immune/tcga_immune_cluster_plot.pdf',
       width = 18, height = 10)


GSE30219_tme1.list1_score_plot <- wb_boxplot(dat = GSE30219_tme1.list1_score[, GSE30219.subtype.cli$Samples],
                                            groups = GSE30219.subtype.cli$Cluster,
                                            title = 'Cluster',
                                            xangle = 60,
                                            ylab = 'Score',
                                            col = mycolor)
GSE30219_tme1.list1_score_plot

GSE30219_tme1.list2_score_plot <- wb_boxplot(dat = GSE30219_tme1.list2_score[, GSE30219.subtype.cli$Samples],
                                            groups = GSE30219.subtype.cli$Cluster,
                                            xangle = 0,
                                            title = 'Cluster',
                                            ylab = 'Score',
                                            col = mycolor)
GSE30219_tme1.list2_score_plot

GSE30219_tme1.list2_score_list <- list()
for (fea in rownames(GSE30219_tme1.list2_score)) {
  print(fea)
  tmp <- mg_violin(data.frame(GSE30219.subtype.cli$Cluster,
                              as.numeric(GSE30219_tme1.list2_score[fea, 
                                                                  GSE30219.subtype.cli$Samples])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='tl',
                   show_compare = T)
  GSE30219_tme1.list2_score_list[[fea]] <- tmp
}

GSE30219_tme1.list2_score_plot <- cowplot::plot_grid(plotlist = GSE30219_tme1.list2_score_list,
                                                    ncol = 2)
GSE30219_tme1.list2_score_plot



GSE30219_estimate <- immu_estimate(GSE30219_exp, isTCGA = F)
GSE30219_estimate[1:5, 1:3]
GSE30219.subtype.cli$Samples <- gsub('-', '.', GSE30219.subtype.cli$Samples)
GSE30219_estimate_clister_list <- list()
for (fea in colnames(GSE30219_estimate)) {
  print(fea)
  tmp <- mg_violin(data.frame(GSE30219.subtype.cli$Cluster,
                              as.numeric(GSE30219_estimate[GSE30219.subtype.cli$Samples, fea])),
                   melt=TRUE, ylab=fea,
                   leg.title='',
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='tl',
                   show_compare = T)
  GSE30219_estimate_clister_list[[fea]] <- tmp
}

GSE30219_estimate_cluster_plot <- cowplot::plot_grid(plotlist = GSE30219_estimate_clister_list,
                                                    ncol = 3)
GSE30219_estimate_cluster_plot




GSE30219_mcpcounter <- immu_MCPcounter(exp = GSE30219_exp, isTCGA = F)
dim(GSE30219_mcpcounter)

GSE30219_mcpcounter_score_plot <- wb_boxplot(dat = t(GSE30219_mcpcounter[GSE30219.subtype.cli$Samples, ]),
                                            groups = GSE30219.subtype.cli$Cluster,
                                            title = 'Cluster',
                                            xangle = 60,
                                            ylab = 'Score',
                                            col = mycolor)
GSE30219_mcpcounter_score_plot

GSE30219.subtype.cli$samples <- GSE30219.subtype.cli$Sample
GSE30219_tme2.list_score[1:5, 1:5]
GSE30219_tme2.list_score_plot <- wb_boxplot(dat = GSE30219_tme2.list_score[, GSE30219.subtype.cli$samples],
                                           groups = GSE30219.subtype.cli$Cluster,
                                           title = 'Cluster',
                                           xangle = 60,
                                           ylab = 'Score',
                                           col = mycolor)
GSE30219_tme2.list_score_plot

GSE30219_ImmuCellAI.list_score_plot <- wb_boxplot(dat = GSE30219_ImmuCellAI.list_score[, GSE30219.subtype.cli$samples],
                                                 groups = GSE30219.subtype.cli$Cluster,
                                                 title = 'Cluster',
                                                 xangle = 60,
                                                 ylab = 'Score',
                                                 col = mycolor)
GSE30219_ImmuCellAI.list_score_plot

GSE30219_immune_cluster_plot1 <- cowplot::plot_grid(GSE30219_tme1.list2_score_plot,
                                                   GSE30219_estimate_cluster_plot,
                                                   GSE30219_mcpcounter_score_plot,
                                                   ncol = 3,
                                                   rel_widths = c(2,3,2.5))
GSE30219_immune_cluster_plot1


GSE30219_immune_cluster_plot <- cowplot::plot_grid(GSE30219_tme1.list1_score_plot,
                                                  GSE30219_immune_cluster_plot1,
                                                  ncol = 1)
GSE30219_immune_cluster_plot
ggsave(plot = GSE30219_immune_cluster_plot,
       filename = '03_cluster_immune/GSE30219_immune_cluster_plot.pdf',
       width = 18, height = 10)


# GSEA 分析 ################
dir.create('04_GSEA')
# TCGA
dir.create('04_GSEA/TCGA')
dim(tcga_tpm_log2)
library(dplyr)
table(tcga.subtype.cli$Cluster)
tcga.subtype.cli <- tcga.subtype.cli[tcga.subtype.cli$Cluster != 'C3', ]
table(tcga.subtype.cli$Cluster)
tcga.subtype.cli <- arrange(tcga.subtype.cli, Cluster)
head(tcga.subtype.cli[, c("Samples", "Cluster")])

write.table(tcga.subtype.cli[, c("Samples", "Cluster")], 
            file = '04_GSEA/TCGA/gsea_groups.txt', sep = '\t', 
            row.names = F, quote = F)
write.table(tcga_tpm_log2[intersect(genes_protein, rownames(tcga_tpm_log2)), 
                          tcga.subtype.cli$Samples], 
            file = '04_GSEA/TCGA/tcga_gsae_data.txt', sep = '\t', quote = F)

mg_RunGSEA(mod = 'exp_group',exp_Path = '04_GSEA/TCGA/tcga_gsae_data.txt'
           ,sample_group_path = '04_GSEA/TCGA/gsea_groups.txt'
           ,outFolder = '04_GSEA/TCGA/results/'
           ,gmt_Path = '00_origin_datas/msigdb_v7.4_GMTs/c2.cp.kegg.v7.4.symbols.gmt', 
           outLog=T)

# 读取 GSEA 的分析结果 
tcga_GSEA=parseGSEAResult('04_GSEA/TCGA/results/my_analysis.Gsea.1681286481749/')
dim(tcga_GSEA$EnrichTable)
tcga_GSEA_EnrichTable <- as.data.frame(tcga_GSEA$EnrichTable)
library(dplyr)
write.table(tcga_GSEA_EnrichTable, file = '04_GSEA/TCGA_KEGG_results.txt', 
            sep = '\t', row.names = F, quote = F)
tcga_GSEA_EnrichTable_filtered <- tcga_GSEA_EnrichTable[tcga_GSEA_EnrichTable$NP < 0.05, ]
dim(tcga_GSEA_EnrichTable_filtered)
write.csv(tcga_GSEA_EnrichTable_filtered,
          file = '04_GSEA/tcga_GSEA_EnrichTable_filtered.csv',
          row.names = F, quote = F)

rownames(tcga_GSEA_EnrichTable_filtered)[match(c("KEGG_MISMATCH_REPAIR",
                                                 "KEGG_CELL_CYCLE",
                                                 "KEGG_DNA_REPLICATION",         
                                                 "KEGG_HOMOLOGOUS_RECOMBINATION"),
                                               tcga_GSEA_EnrichTable_filtered$Term)]
tcga_gs2 <- plot_GSEA_By_nodes_wb(tcga_GSEA,indexs=c(11,2,10,9))
tcga_gs2
ggsave(plot = tcga_gs2,
       filename = '04_GSEA/tcga_gs2.pdf',
       width = 9, height = 6, device = cairo_pdf)




# GSE30219
dir.create('04_GSEA/GSE30219')
dim(GSE30219_exp)
library(dplyr)
GSE30219.subtype.cli <- GSE30219.subtype.cli[GSE30219.subtype.cli$Cluster != 'C3', ]
table(GSE30219.subtype.cli$Cluster)
GSE30219.subtype.cli <- arrange(GSE30219.subtype.cli, Cluster)
head(GSE30219.subtype.cli[, c("samples", "Cluster")])
GSE30219.subtype.cli$Samples<-GSE30219.subtype.cli$samples
write.table(GSE30219.subtype.cli[, c("samples", "Cluster")], 
            file = '04_GSEA/GSE30219/gsea_groups.txt', sep = '\t', 
            row.names = F, quote = F)
write.table(GSE30219_exp[intersect(genes_protein, rownames(GSE30219_exp)), 
                        GSE30219.subtype.cli$Samples], 
            file = '04_GSEA/GSE30219/GSE30219_gsae_data.txt', sep = '\t', quote = F)

mg_RunGSEA(mod = 'exp_group',exp_Path = '04_GSEA/GSE30219/GSE30219_gsae_data.txt'
           ,sample_group_path = '04_GSEA/GSE30219/gsea_groups.txt'
           ,outFolder = '04_GSEA/GSE30219/results/'
           ,gmt_Path = '00_origin_datas/msigdb_v7.4_GMTs/c2.cp.kegg.v7.4.symbols.gmt', 
           outLog=T)

# 读取 GSEA 的分析结果 
GSE30219_GSEA=parseGSEAResult('04_GSEA/GSE30219/results/my_analysis.Gsea.1681287101274/')
dim(GSE30219_GSEA$EnrichTable)
GSE30219_GSEA_EnrichTable <- as.data.frame(GSE30219_GSEA$EnrichTable)
library(dplyr)
write.table(GSE30219_GSEA_EnrichTable, file = '04_GSEA/GSE30219_KEGG_results.txt', 
            sep = '\t', row.names = F, quote = F)
GSE30219_GSEA_EnrichTable_filtered <- GSE30219_GSEA_EnrichTable[GSE30219_GSEA_EnrichTable$NP < 0.05, ]
dim(GSE30219_GSEA_EnrichTable_filtered)
write.csv(GSE30219_GSEA_EnrichTable_filtered,
          file = '04_GSEA/GSE30219_GSEA_EnrichTable_filtered.csv',
          row.names = F, quote = F)

intersect(GSE30219_GSEA_EnrichTable_filtered$Term[GSE30219_GSEA_EnrichTable_filtered$ES > 0],
          tcga_GSEA_EnrichTable_filtered$Term[tcga_GSEA_EnrichTable_filtered$ES > 0])


intersect(GSE30219_GSEA_EnrichTable_filtered$Term[GSE30219_GSEA_EnrichTable_filtered$ES < -0.7],
          tcga_GSEA_EnrichTable_filtered$Term[tcga_GSEA_EnrichTable_filtered$ES < -0.7])

rownames(GSE30219_GSEA_EnrichTable_filtered)[match(c("KEGG_MISMATCH_REPAIR",
                                                     "KEGG_CELL_CYCLE",
                                                     "KEGG_DNA_REPLICATION",         
                                                     "KEGG_HOMOLOGOUS_RECOMBINATION"),
                                                  GSE30219_GSEA_EnrichTable_filtered$Term)]

GSE30219_gs2 <- plot_GSEA_By_nodes_wb(GSE30219_GSEA,indexs=c(9,7,11,13))
GSE30219_gs2
ggsave(plot = GSE30219_gs2,
       filename = '04_GSEA/GSE30219_gs2.pdf',
       width = 9, height = 6, device = cairo_pdf)





# 差异基因分析 ##############
dir.create('05_diff_genes')
mg_volcano_wb <- function(logfc,
                          pvalue,
                          symbol=NULL,
                          cutFC=1,
                          cutPvalue=0.05
                          ,showText=NULL
                          ,colors=c(mycolor[2],'grey',mycolor[1])
                          ,xlim=NULL,ylim=NULL
                          ,legend.pos='right'
                          ,ylab='-log10(FDR)',
                          leg='State',
                          xlab='log2(FoldChange)'){
  library(ggplot2)
  pos=c(0,0)
  if(is.null(legend.pos)){
    pos='none'
  }else if(legend.pos=='tr'){
    pos=c(1,1)
  }else if(legend.pos=='br'){
    pos=c(1,0)
  }else if(legend.pos=='tl'){
    pos=c(0,1)
  }else if(legend.pos=='bl'){
    pos=c(0,0)
  }else{
    pos='right'
  }
  cange=rep('None',length(logfc))
  cange[which(logfc>cutFC&pvalue<cutPvalue)]='Up'
  cange[which(logfc< -cutFC&pvalue<cutPvalue)]='Down'
  if(is.null(symbol)){
    symbol=rep('',length(logfc))
    showText=NULL
  }
  vo.input=data.frame(logFC=logfc,FDR=pvalue,change=cange,SYMBOL=symbol)
  #print(head(vo.input))
  p1 <- ggplot(data = vo.input, 
               aes(x = logFC, 
                   y = -log10(FDR)))
  p1=p1+geom_point(alpha=0.85, size=2, aes(color=change))  
  p1=p1+scale_color_manual(values=colors,limits = c("Down",'None', "Up"),name=leg) 
  p1=p1+geom_vline(xintercept=c(-cutFC,cutFC),lty=6,col="black",lwd=0.8)  
  p1=p1+geom_hline(yintercept = -log10(cutPvalue),lty=6,col="black",lwd=0.8)  
  p1=p1+ylab(ylab)+xlab(xlab)
  p1=p1+theme_classic()
  p1=p1+theme(
    axis.text.y=element_text(family="Times",face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
    axis.title.y=element_text(family="Times",face="plain"), #设置y轴标题的字体属性
    legend.text=element_text(face="plain", family="Times", colour="black"  #设置图例的子标题的字体属性
    ),
    legend.title=element_text(face="plain", family="Times", colour="black" #设置图例的总标题的字体属性
    ),
    legend.justification=pos, legend.position=pos
    ,legend.background = element_rect(fill = NA, colour = NA)
  )
  if(is.null(showText)|is.null(symbol)){
    showText=c()
  }
  
  if(length(showText)>0){
    for_label <-vo.input[match(intersect(showText,vo.input$SYMBOL),vo.input$SYMBOL),]
    p1=p1+geom_point(size = 3, shape = 1, data = for_label)+ggrepel::geom_label_repel(
      aes(label = SYMBOL),
      data = for_label,
      color="black"
    )
  }
  if(!is.null(ylim)){
    p1=p1+ylim(ylim)
  }
  if(!is.null(xlim)){
    p1=p1+xlim(xlim)
  }
  
  return(p1)
}
# TCGA #####################
tcga_limma_dat <- tcga_tpm_log2[rownames(tcga_tpm_log2) %in% genes_protein, ]
tcga_limma_dat <- tcga_limma_dat[rowSums(tcga_limma_dat) > 0, ]

table(tcga.subtype.cli$Cluster)
# C1 vs C2 ##############
tcga_C1C2_cli <- tcga.subtype.cli[tcga.subtype.cli$Cluster != 'C3', ]

tcga_C1C2_limma <- mg_limma_DEG(exp = tcga_limma_dat[, tcga_C1C2_cli$Samples],
                                group = tcga_C1C2_cli$Cluster,
                                ulab = 'C1',
                                dlab = 'C2')
tcga_C1C2_limma_res <- tcga_C1C2_limma$DEG
tcga_C1C2_limma_res <- na.omit(tcga_C1C2_limma_res)
head(tcga_C1C2_limma_res)
tcga_C1C2_limma_filtered <- subset(tcga_C1C2_limma_res,
                                   abs(logFC) > 1 & adj.P.Val < 0.05)
dim(tcga_C1C2_limma_filtered)
table(tcga_C1C2_limma_filtered$logFC > 0)   # 920 

tcga_c1_up <- rownames(tcga_C1C2_limma_filtered)[tcga_C1C2_limma_filtered$logFC > 0]  # 358
tcga_c1_dn <- rownames(tcga_C1C2_limma_filtered)[tcga_C1C2_limma_filtered$logFC < 0]  # 562

write.csv(tcga_C1C2_limma_filtered,
          file = '05_diff_genes/tcga_C1C2_limma_filtered.csv',
          quote = F)
tcga_C1C2_limma_genes <- rownames(tcga_C1C2_limma_filtered)

tcga_C1C2_volcano <- mg_volcano_wb(logfc = tcga_C1C2_limma_res$logFC,
                                   pvalue = tcga_C1C2_limma_res$adj.P.Val,
                                   cutFC = 1,
                                   cutPvalue = 0.05,
                                   colors = c(mycolor[1], 'grey', mycolor[2]),
                                   legend.pos = 'tl',
                                   leg = 'TCGA')
tcga_C1C2_volcano

tcga_C1C2_go_kegg <- enrichmentORA(tcga_C1C2_limma_genes,
                                   mp_dbs=c('pathway_KEGG',
                                            'geneontology_Biological_Process',
                                            'geneontology_Cellular_Component',
                                            'geneontology_Molecular_Function'))
tcga_C1C2_go_kegg_filtered <- tcga_C1C2_go_kegg[tcga_C1C2_go_kegg$FDR < 0.05, ]
table(tcga_C1C2_go_kegg_filtered$DB)

# tcga_C1C2_go_plot<-dotplot_batch(tcga_C1C2_go_kegg_filtered, c('geneontology_Biological_Process'),top=10,FDR = T)
# tcga_C1C2_go_plot
# 
# taga_diff_genes_volcano_plot <- cowplot::plot_grid( tcga_C1C2_volcano,
#                                                     tcga_C1C2_go_plot,
#                                                     ncol = 2,
#                                                     labels = LETTERS[1:2])
# taga_diff_genes_volcano_plot
# ggsave(plot = taga_diff_genes_volcano_plot,
#        filename = '05_diff_genes/taga_diff_genes_volcano_plot.pdf',
#        width = 10, height = 5)

pdf('05_diff_genes/tcga_C1C2_go_kegg.pdf', width = 11, height = 8)
dotplot_batch(tcga_C1C2_go_kegg_filtered, 
              dbs =c('geneontology_Biological_Process',
                     'geneontology_Cellular_Component',
                     'geneontology_Molecular_Function',
                     'pathway_KEGG'),top=10,FDR = T,
              high_col = mycolor[1],
              low_col = mycolor[2])
dev.off()

write.table(tcga_C1C2_go_kegg_filtered, 
            file = '05_diff_genes/tcga_C1C2_go_kegg_filtered_GO_KEGG.txt', 
            sep = '\t', row.names = F, quote = F)


# GSE30219 #####################
GSE30219_limma_dat <- GSE30219_exp[rownames(GSE30219_exp) %in% genes_protein, ]
GSE30219_limma_dat <- GSE30219_limma_dat[rowSums(GSE30219_limma_dat) > 0, ]

table(GSE30219.subtype.cli$Cluster)
# C1 vs C2 ##############
GSE30219_C1C2_cli <- GSE30219.subtype.cli[GSE30219.subtype.cli$Cluster != 'C3', ]

GSE30219_C1C2_limma <- mg_limma_DEG(exp = GSE30219_limma_dat[, GSE30219_C1C2_cli$Samples],
                                   group = GSE30219_C1C2_cli$Cluster,
                                   ulab = 'C1',
                                   dlab = 'C2')
GSE30219_C1C2_limma_res <- GSE30219_C1C2_limma$DEG
GSE30219_C1C2_limma_res <- na.omit(GSE30219_C1C2_limma_res)
head(GSE30219_C1C2_limma_res)
GSE30219_C1C2_limma_filtered <- subset(GSE30219_C1C2_limma_res,
                                      abs(logFC) > log2(1.2) & adj.P.Val < 0.05)
dim(GSE30219_C1C2_limma_filtered)
table(GSE30219_C1C2_limma_filtered$logFC > 0)   # 261

GSE30219_c1_up <- rownames(GSE30219_C1C2_limma_filtered)[GSE30219_C1C2_limma_filtered$logFC > 0] # 73
GSE30219_c1_dn <- rownames(GSE30219_C1C2_limma_filtered)[GSE30219_C1C2_limma_filtered$logFC < 0] # 188

write.csv(GSE30219_C1C2_limma_filtered,
          file = '05_diff_genes/GSE30219_C1C2_limma_filtered.csv',
          quote = F)
GSE30219_C1C2_limma_genes <- rownames(GSE30219_C1C2_limma_filtered)

GSE30219_C1C2_volcano <- mg_volcano_wb(logfc = GSE30219_C1C2_limma_res$logFC,
                                      pvalue = GSE30219_C1C2_limma_res$adj.P.Val,
                                      cutFC = log2(1.2),
                                      cutPvalue = 0.05,
                                      colors = c(mycolor[1], 'grey', mycolor[2]),
                                      legend.pos = 'tl',
                                      leg = 'GSE30219')
GSE30219_C1C2_volcano

GSE30219_C1C2_go_kegg <- enrichmentORA(GSE30219_C1C2_limma_genes,
                                      mp_dbs=c('pathway_KEGG',
                                               'geneontology_Biological_Process',
                                               'geneontology_Cellular_Component',
                                               'geneontology_Molecular_Function'))
GSE30219_C1C2_go_kegg_filtered <- GSE30219_C1C2_go_kegg[GSE30219_C1C2_go_kegg$FDR < 0.05, ]
GSE30219_C1C2_go_kegg_filtered <- GSE30219_C1C2_go_kegg_filtered[!duplicated(GSE30219_C1C2_go_kegg_filtered$description), ]
table(GSE30219_C1C2_go_kegg_filtered$DB)


pdf('05_diff_genes/GSE30219_C1C2_go_kegg.pdf', width = 11, height = 8)
dotplot_batch(GSE30219_C1C2_go_kegg_filtered, 
              dbs =c('geneontology_Biological_Process',
                     'geneontology_Cellular_Component',
                     'geneontology_Molecular_Function',
                     'pathway_KEGG'),top=10,FDR = T,
              high_col = mycolor[1],
              low_col = mycolor[2])
dev.off()

write.table(GSE30219_C1C2_go_kegg_filtered, 
            file = '05_diff_genes/GSE30219_C1C2_go_kegg_filtered_GO_KEGG.txt', 
            sep = '\t', row.names = F, quote = F)


diff_genes_volcano_plot <- cowplot::plot_grid(tcga_C1C2_volcano,
                                              tcga_C1C2_go_plot,
                                              GSE30219_C1C2_volcano,
                                              GSE30219_C1C2_go_plot,
                                              ncol = 2,
                                              labels = LETTERS[1:4])
diff_genes_volcano_plot
ggsave(plot = diff_genes_volcano_plot,
       filename = '05_diff_genes/diff_genes_volcano_plot.pdf',
       width = 10, height = 10)








# 模型构建 ####################
dir.create('06_model')

rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
tcga_model_data <- cbind(tcga.subtype.cli[, c("OS.time", "OS")],
                         t(tcga_tpm_log2[hrd.sig.genes, tcga.subtype.cli$Samples]))
colnames(tcga_model_data) <- gsub('-', '_', colnames(tcga_model_data))

head(GSE30219.subtype.cli)
GSE30219_exp[1:5, 1:5]
rownames(GSE30219.subtype.cli) <- GSE30219.subtype.cli$Samples
GSE30219_model_data <- cbind(GSE30219.subtype.cli[, c("OS.time", "OS")],
                            t(GSE30219_exp[, GSE30219.subtype.cli$Samples]))
colnames(GSE30219_model_data) <- gsub('-', '_', colnames(GSE30219_model_data))

rownames(GSE31210.subtype.cli) <- GSE31210.subtype.cli$Samples
GSE31210_model_data <- cbind(GSE31210.subtype.cli[, c("OS.time", "OS")],
                             t(GSE31210_exp[, GSE31210.subtype.cli$Samples]))
colnames(GSE31210_model_data) <- gsub('-', '_', colnames(GSE31210_model_data))

rownames(GSE50081.subtype.cli) <- GSE50081.subtype.cli$Samples
GSE50081_model_data <- cbind(GSE50081.subtype.cli[, c("OS.time", "OS")],
                             t(GSE50081_exp[, GSE50081.subtype.cli$Samples]))
colnames(GSE50081_model_data) <- gsub('-', '_', colnames(GSE50081_model_data))

rownames(GSE19188.subtype.cli) <- GSE19188.subtype.cli$Samples
GSE19188_model_data <- cbind(GSE19188.subtype.cli[, c("OS.time", "OS")],
                             t(GSE19188_exp[, GSE19188.subtype.cli$Samples]))
colnames(GSE19188_model_data) <- gsub('-', '_', colnames(GSE19188_model_data))

save(tcga_model_data,
     GSE30219_model_data,
     GSE31210_model_data,
     GSE50081_model_data,
     GSE19188_model_data,
     file = 'model.Rdata')

# ####################
load('model.Rdata')

tra.data <- tcga_model_data

tra_randomForestSRC_res <- mg_randomForestSRC(dat = tcga_model_data[, 3:ncol(tcga_model_data)],
                                              time = tcga_model_data$OS.time,
                                              event = tcga_model_data$OS)

save(tra_randomForestSRC_res,file = 'tra_randomForestSRC_res.Rdata')
load('tra_randomForestSRC_res.Rdata')

tra_randomForestSRC_res$plot
tra_randomForestSRC_res$GenesList
tra_randomForestSRC_res$Model


ggsave(plot = tra_randomForestSRC_res$plot,
       filename = '06_model/tra_randomForestSRC_res.pdf',
       width = 15, height = 10)


# GenesList为三种方法（md:minimal depth，vh:variable hunting，vh.vimp:variable hunting with VIMP (variable importance)）筛选的基因
randomForest_genes <- tra_randomForestSRC_res$GenesList[[2]]
randomForest_genes
# 基因的多因素
tcga_dat1 <- cbind(time=tra.data$OS.time,
                   status=tra.data$OS,
                   tra.data[, randomForest_genes])

fmla <- as.formula(paste0("Surv(time, status) ~"
                          ,paste0(randomForest_genes, collapse = '+')))

library(survival)
cox <- coxph(fmla, data =as.data.frame(tcga_dat1))
lan <- coef(cox)
round(lan, 3)
genes <- names(cox$coefficients)
# tra.cox[genes,]
summary(cox)
pdf('06_model/multi_cox.pdf', width = 6, height = 6, onefile = F)
survminer::ggforest(cox,data=tcga_dat1)
dev.off()

paste0(round(lan, 3), '*', names(lan), collapse = '+')
#  0.079*RAD51AP1+-0.146*BRCA1+0.369*H2AFX+0.286*FANCL




# 6、验证集与测试机的KM曲线 #####
library(survcomp)
library(survminer)
# 训练集
risk.tr <- as.numeric(lan%*%as.matrix(t(tra.data[,genes])))
tra.data$RS <- risk.tr
tra.data.point <- surv_cutpoint(tra.data, time = "OS.time", event = "OS",
                                variables = 'RS')
tra.cutoff <- as.numeric(summary(tra.data.point)[1])
tra.cutoff



tr.roc <- ggplotTimeROC(tra.data$OS.time / 365,
                        tra.data$OS,
                        risk.tr,
                        mks = c(1,3,5))
tr.roc
tr.km <- ggplotKMCox(data.frame(tra.data$OS.time / 365,
                                tra.data$OS,
                                ifelse(risk.tr>=tra.cutoff,'High','Low')),
                     title = 'TCGA',
                     labs = c('High','Low'))

tr.roc.km <- cowplot::plot_grid(tr.km,
                                tr.roc,
                                ncol = 2)
tr.roc.km
ggsave(plot = tr.roc.km,
       filename = '06_model/train.roc.km.pdf',
       width = 10, height = 5)

rownames(tcga.subtype.cli)
tcga.subtype.cli <- tcga.subtype.cli[rownames(tra.data), ]

tcga.subtype.cli$RiskScore <- risk.tr
tcga.subtype.cli$RiskType <- ifelse(tcga.subtype.cli$RiskScore > tra.cutoff, 'High', 'Low')

tcga.PFI.roc <- ggplotTimeROC(tcga.subtype.cli$PFI.time / 365,
                              tcga.subtype.cli$PFI,
                              tcga.subtype.cli$RiskScore,
                              mks = c(1,3,5))
tcga.PFI.roc
tcga.PFI.km <- ggplotKMCox(data.frame(tcga.subtype.cli$PFI.time / 365,
                                      tcga.subtype.cli$PFI,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA PFI',
                           labs = c('High','Low'))
tcga.PFI.km
tcga.PFI.roc.km <- cowplot::plot_grid(tcga.PFI.roc,
                                      tcga.PFI.km)
tcga.PFI.roc.km



tcga.DFI.roc <- ggplotTimeROC(tcga.subtype.cli$DFI.time / 365,
                              tcga.subtype.cli$DFI,
                              tcga.subtype.cli$RiskScore,
                              mks = c(1,3,5))
tcga.DFI.roc
tcga.DFI.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DFI.time / 365,
                                      tcga.subtype.cli$DFI,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA DFI',
                           labs = c('High','Low'))
tcga.DFI.km
tcga.DFI.roc.km <- cowplot::plot_grid(tcga.DFI.roc,
                                      tcga.DFI.km,
                                      ncol = 2)
tcga.DFI.roc.km



tcga.DSS.roc <- ggplotTimeROC(tcga.subtype.cli$DSS.time/365,
                              tcga.subtype.cli$DSS,
                              tcga.subtype.cli$RiskScore,
                              mks = c(1,3,5))
tcga.DSS.roc
tcga.DSS.km <- ggplotKMCox(data.frame(tcga.subtype.cli$DSS.time / 365,
                                      tcga.subtype.cli$DSS,
                                      tcga.subtype.cli$RiskType),
                           title = 'TCGA',
                           labs = c('High','Low'))
tcga.DSS.km
tcga.DSS.roc.km <- cowplot::plot_grid(tcga.DSS.km,
                                      tcga.DSS.roc,
                                      ncol = 2)
tcga.DSS.roc.km


ggsave(plot = tcga.DSS.roc.km,
       filename = '06_model/tcga_km_roc.pdf',
       width = 16, height = 8)


# 全部 GSE30219
GSE30219.genes <- intersect(genes, colnames(GSE30219_model_data))
GSE30219.genes

fmla.GSE30219 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                  ,paste0(GSE30219.genes,collapse = '+')))
cox.GSE30219 <- coxph(fmla.GSE30219, data =as.data.frame(GSE30219_model_data))
GSE30219_lan <- coef(cox.GSE30219)

risk.GSE30219=as.numeric(GSE30219_lan%*%as.matrix(t(GSE30219_model_data[,names(GSE30219_lan)])))

GSE30219_model_data$RS <- risk.GSE30219

GSE30219.data.point <- surv_cutpoint(GSE30219_model_data, time = "OS.time", event = "OS",
                                    variables = 'RS')
GSE30219.cutoff <- as.numeric(summary(GSE30219.data.point)[1])
GSE30219.cutoff



GSE30219.roc <- ggplotTimeROC(GSE30219_model_data$OS.time / 365,
                             GSE30219_model_data$OS,
                             risk.GSE30219,
                             mks = c(1,3,4))
GSE30219.km <- ggplotKMCox(data.frame(GSE30219_model_data$OS.time / 365,
                                     GSE30219_model_data$OS,
                                     ifelse(risk.GSE30219>=GSE30219.cutoff,'High','Low')),
                          title = 'GSE30219',
                          labs = c('High','Low'))
GSE30219.km

GSE30219.roc.km <- cowplot::plot_grid(GSE30219.km,
                                     GSE30219.roc,
                                     ncol = 2)

GSE30219.roc.km
ggsave(plot = GSE30219.roc.km,
       filename = '06_model/GSE30219.roc.km.pdf',
       width = 10, height = 5)


# # 全部 GSE50081
# GSE50081.genes <- intersect(genes, colnames(GSE50081_model_data))
# GSE50081.genes
# 
# fmla.GSE50081 <- as.formula(paste0("Surv(OS.time, OS) ~"
#                                    ,paste0(GSE50081.genes,collapse = '+')))
# cox.GSE50081 <- coxph(fmla.GSE50081, data =as.data.frame(GSE50081_model_data))
# GSE50081_lan <- coef(cox.GSE50081)
# 
# risk.GSE50081=as.numeric(GSE50081_lan%*%as.matrix(t(GSE50081_model_data[,names(GSE50081_lan)])))
# 
# GSE50081_model_data$RS <- risk.GSE50081
# 
# GSE50081.data.point <- surv_cutpoint(GSE50081_model_data, time = "OS.time", event = "OS",
#                                      variables = 'RS')
# GSE50081.cutoff <- as.numeric(summary(GSE50081.data.point)[1])
# GSE50081.cutoff
# 
# 
# 
# GSE50081.roc <- ggplotTimeROC(GSE50081_model_data$OS.time / 365,
#                               GSE50081_model_data$OS,
#                               risk.GSE50081,
#                               mks = c(1,3,5))
# GSE50081.km <- ggplotKMCox(data.frame(GSE50081_model_data$OS.time / 365,
#                                       GSE50081_model_data$OS,
#                                       ifelse(risk.GSE50081>=GSE50081.cutoff,'High','Low')),
#                            title = 'GSE50081',
#                            labs = c('High','Low'))
# GSE50081.km
# 
# GSE50081.roc.km <- cowplot::plot_grid(GSE50081.km,
#                                       GSE50081.roc,
#                                       ncol = 2)
# 
# GSE50081.roc.km
# ggsave(plot = GSE50081.roc.km,
#        filename = '06_model/GSE50081.roc.km.pdf',
#        width = 10, height = 5)


# 全部 GSE31210
GSE31210.genes <- intersect(genes, colnames(GSE31210_model_data))
GSE31210.genes

fmla.GSE31210 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                   ,paste0(GSE31210.genes,collapse = '+')))
cox.GSE31210 <- coxph(fmla.GSE31210, data =as.data.frame(GSE31210_model_data))
GSE31210_lan <- coef(cox.GSE31210)

risk.GSE31210=as.numeric(GSE31210_lan%*%as.matrix(t(GSE31210_model_data[,names(GSE31210_lan)])))

GSE31210_model_data$RS <- risk.GSE31210

GSE31210.data.point <- surv_cutpoint(GSE31210_model_data, time = "OS.time", event = "OS",
                                     variables = 'RS')
GSE31210.cutoff <- as.numeric(summary(GSE31210.data.point)[1])
GSE31210.cutoff



GSE31210.roc <- ggplotTimeROC(GSE31210_model_data$OS.time / 365,
                              GSE31210_model_data$OS,
                              risk.GSE31210,
                              mks = c(1,3,5))
GSE31210.km <- ggplotKMCox(data.frame(GSE31210_model_data$OS.time / 365,
                                      GSE31210_model_data$OS,
                                      ifelse(risk.GSE31210>=GSE31210.cutoff,'High','Low')),
                           title = 'GSE31210',
                           labs = c('High','Low'))
GSE31210.km

GSE31210.roc.km <- cowplot::plot_grid(GSE31210.km,
                                      GSE31210.roc,
                                      ncol = 2)

GSE31210.roc.km
ggsave(plot = GSE31210.roc.km,
       filename = '06_model/GSE31210.roc.km.pdf',
       width = 10, height = 5)

# 全部 GSE19188
GSE19188.genes <- intersect(genes, colnames(GSE19188_model_data))
GSE19188.genes

fmla.GSE19188 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                   ,paste0(GSE19188.genes,collapse = '+')))
cox.GSE19188 <- coxph(fmla.GSE19188, data =as.data.frame(GSE19188_model_data))
GSE19188_lan <- coef(cox.GSE19188)

risk.GSE19188=as.numeric(GSE19188_lan%*%as.matrix(t(GSE19188_model_data[,names(GSE19188_lan)])))

GSE19188_model_data$RS <- risk.GSE19188

GSE19188.data.point <- surv_cutpoint(GSE19188_model_data, time = "OS.time", event = "OS",
                                     variables = 'RS')
GSE19188.cutoff <- as.numeric(summary(GSE19188.data.point)[1])
GSE19188.cutoff


GSE19188.roc <- ggplotTimeROC(GSE19188_model_data$OS.time / 365,
                              GSE19188_model_data$OS,
                              risk.GSE19188,
                              mks = c(1,3,5))
GSE19188.km <- ggplotKMCox(data.frame(GSE19188_model_data$OS.time / 365,
                                      GSE19188_model_data$OS,
                                      ifelse(risk.GSE19188>=GSE19188.cutoff,'High','Low')),
                           title = 'GSE19188',
                           labs = c('High','Low'))
GSE19188.km    # 0.00029

GSE19188.roc.km <- cowplot::plot_grid(GSE19188.km,
                                      GSE19188.roc,
                                      ncol = 2)

GSE19188.roc.km

ggsave(plot = GSE19188.roc.km,
       filename = '06_model/GSE19188.roc.km.pdf',
       width = 10, height = 5)

tcga.geo.roc.km <- cowplot::plot_grid(tcga.DSS.km,
                                           tcga.DSS.roc,
                                           GSE30219.km,
                                           GSE30219.roc,
                                           GSE19188.km,
                                           GSE19188.roc,
                                           ncol = 4)

tcga.geo.roc.km
ggsave(plot = tcga.geo.roc.km,
       filename = '06_model/tcga.geo.roc.km.pdf',
       width = 20, height = 10)

# head(tcga_hrd_score)
# tcga.subtype.cli$hrd <- as.numeric(tcga_hrd_score["hrd_PATHWAY", tcga.subtype.cli$Samples])
# GSE30219.subtype.cli$hrd <- as.numeric(GSE30219_hrd_score["hrd_PATHWAY", GSE30219.subtype.cli$Samples])



# 13.、RiskScore 与 hrd 在临床特征的分布的关系 ##########
# TCGA RiskScore
colnames(tcga.subtype.cli)
RiskScore_Age <- data.frame(tcga.subtype.cli[, c('Age', "RiskScore")])
RiskScore_Age <- na.omit(RiskScore_Age)
median(RiskScore_Age$Age)
RiskScore_Age$Age <- ifelse(RiskScore_Age$Age > 60, '>60', '≤60')
Age_violin_RiskScore <- mg_violin(RiskScore_Age, melt=TRUE, ylab='RiskScore',
                                  leg.title='', 
                                  xlab = 'Age',
                                  jitter = F,
                                  legend.pos='bl',
                                  show_compare = F)
Age_violin_RiskScore


colnames(tcga.subtype.cli)
RiskScore_Gender <- data.frame(tcga.subtype.cli[, c("Gender", "RiskScore")])
RiskScore_Gender <- na.omit(RiskScore_Gender)
Gender_violin_RiskScore <- mg_violin(RiskScore_Gender, melt=TRUE, ylab='RiskScore',
                                     leg.title='', 
                                     jitter = F,
                                     xlab = 'Gender',
                                     legend.pos='bl',
                                     show_compare = F)
Gender_violin_RiskScore

RiskScore_T.Stage <- data.frame(tcga.subtype.cli[, c("T.Stage", "RiskScore")])
RiskScore_T.Stage <- na.omit(RiskScore_T.Stage)
T.Stage_violin_RiskScore <- mg_violin(RiskScore_T.Stage, melt=TRUE, ylab='RiskScore',
                                      leg.title='', 
                                      jitter = F,
                                      xlab = 'T.Stage',
                                      legend.pos='tl',
                                      show_compare = T)
T.Stage_violin_RiskScore

RiskScore_N.Stage <- data.frame(tcga.subtype.cli[, c("N.Stage", "RiskScore")])
RiskScore_N.Stage <- RiskScore_N.Stage[RiskScore_N.Stage$N.Stage != 'NX', ]
RiskScore_N.Stage <- na.omit(RiskScore_N.Stage)
N.Stage_violin_RiskScore <- mg_violin(RiskScore_N.Stage, melt=TRUE, ylab='RiskScore',
                                      leg.title='', 
                                      jitter = F,
                                      xlab = 'N.Stage',
                                      legend.pos='bl',
                                      show_compare = F)
N.Stage_violin_RiskScore

RiskScore_M.Stage <- data.frame(tcga.subtype.cli[, c("M.Stage", "RiskScore")])
RiskScore_M.Stage <- RiskScore_M.Stage[RiskScore_M.Stage$M.Stage != 'MX', ]
RiskScore_M.Stage <- na.omit(RiskScore_M.Stage)
M.Stage_violin_RiskScore <- mg_violin(RiskScore_M.Stage, melt=TRUE, ylab='RiskScore',
                                      leg.title='', 
                                      jitter = F,
                                      xlab = 'M.Stage',
                                      legend.pos='bl',
                                      show_compare = F)
M.Stage_violin_RiskScore

RiskScore_Stage <- data.frame(tcga.subtype.cli[, c("Stage", "RiskScore")])
RiskScore_Stage <- na.omit(RiskScore_Stage)
Stage_violin_RiskScore <- mg_violin(RiskScore_Stage, melt=TRUE, ylab='RiskScore',
                                    leg.title='', 
                                    jitter = F,
                                    xlab = 'Stage',
                                    legend.pos='tl',
                                    show_compare = T)
Stage_violin_RiskScore

# RiskScore_Grade <- data.frame(tcga.subtype.cli[, c("Grade", "RiskScore")])
# RiskScore_Grade <- na.omit(RiskScore_Grade)
# Grade_violin_RiskScore <- mg_violin(RiskScore_Grade, melt=TRUE, ylab='RiskScore',
#                                     leg.title='', 
#                                     jitter = F,
#                                     xlab = 'Grade',
#                                     legend.pos='tl',
#                                     show_compare = T)
# Grade_violin_RiskScore

RiskScore_Cluster <- data.frame(tcga.subtype.cli[, c("Cluster", "RiskScore")])
RiskScore_Cluster <- na.omit(RiskScore_Cluster)
Cluster_violin_RiskScore <- mg_violin(RiskScore_Cluster, melt=TRUE, ylab='RiskScore',
                                      leg.title='', 
                                      jitter = F,
                                      xlab = 'Cluster',
                                      legend.pos='tl',
                                      show_compare = T)
Cluster_violin_RiskScore


tcga_RS_violin <- cowplot::plot_grid(T.Stage_violin_RiskScore,
                                     N.Stage_violin_RiskScore,
                                     M.Stage_violin_RiskScore,
                                     Stage_violin_RiskScore,
                                     ncol = 4)
tcga_RS_violin


ggsave(plot = tcga_RS_violin,
       filename = '06_model/tcga_RS_violin.pdf',
       width = 15, height = 5)

# TCGA hrd
# colnames(tcga.subtype.cli)
# hrd_Age <- data.frame(tcga.subtype.cli[, c('Age', "hrd")])
# hrd_Age <- na.omit(hrd_Age)
# median(hrd_Age$Age)
# hrd_Age$Age <- ifelse(hrd_Age$Age > 60, '>60', '≤60')
# Age_violin_hrd <- mg_violin(hrd_Age, melt=TRUE, ylab='hrd PATHWAY',
#                                    leg.title='', 
#                                    xlab = 'Age',
#                                    jitter = F,
#                                    legend.pos='bl',
#                                    show_compare = F)
# Age_violin_hrd
# 
# 
# colnames(tcga.subtype.cli)
# hrd_Gender <- data.frame(tcga.subtype.cli[, c("Gender", "hrd")])
# hrd_Gender <- na.omit(hrd_Gender)
# Gender_violin_hrd <- mg_violin(hrd_Gender, melt=TRUE, ylab='hrd PATHWAY',
#                                       leg.title='', 
#                                       jitter = F,
#                                       xlab = 'Gender',
#                                       legend.pos='bl',
#                                       show_compare = F)
# Gender_violin_hrd
# 
# hrd_T.Stage <- data.frame(tcga.subtype.cli[, c("T.Stage", "hrd")])
# hrd_T.Stage <- na.omit(hrd_T.Stage)
# T.Stage_violin_hrd <- mg_violin(hrd_T.Stage, melt=TRUE, ylab='hrd PATHWAY',
#                                        leg.title='', 
#                                        jitter = F,
#                                        xlab = 'T.Stage',
#                                        legend.pos='tl',
#                                        show_compare = T)
# T.Stage_violin_hrd
# 
# hrd_N.Stage <- data.frame(tcga.subtype.cli[, c("N.Stage", "hrd")])
# hrd_N.Stage <- hrd_N.Stage[hrd_N.Stage$N.Stage != 'NX', ]
# hrd_N.Stage <- na.omit(hrd_N.Stage)
# N.Stage_violin_hrd <- mg_violin(hrd_N.Stage, melt=TRUE, ylab='hrd PATHWAY',
#                                        leg.title='', 
#                                        jitter = F,
#                                        xlab = 'N.Stage',
#                                        legend.pos='bl',
#                                        show_compare = F)
# N.Stage_violin_hrd
# 
# hrd_M.Stage <- data.frame(tcga.subtype.cli[, c("M.Stage", "hrd")])
# hrd_M.Stage <- hrd_M.Stage[hrd_M.Stage$M.Stage != 'MX', ]
# hrd_M.Stage <- na.omit(hrd_M.Stage)
# M.Stage_violin_hrd <- mg_violin(hrd_M.Stage, melt=TRUE, ylab='hrd PATHWAY',
#                                        leg.title='', 
#                                        jitter = F,
#                                        xlab = 'M.Stage',
#                                        legend.pos='bl',
#                                        show_compare = F)
# M.Stage_violin_hrd
# 
# hrd_Stage <- data.frame(tcga.subtype.cli[, c("Stage", "hrd")])
# hrd_Stage <- na.omit(hrd_Stage)
# Stage_violin_hrd <- mg_violin(hrd_Stage, melt=TRUE, ylab='hrd PATHWAY',
#                                      leg.title='', 
#                                      jitter = F,
#                                      xlab = 'Stage',
#                                      legend.pos='tl',
#                                      show_compare = T)
# Stage_violin_hrd
# 
# hrd_Grade <- data.frame(tcga.subtype.cli[, c("Grade", "hrd")])
# hrd_Grade <- na.omit(hrd_Grade)
# Grade_violin_hrd <- mg_violin(hrd_Grade, melt=TRUE, ylab='hrd PATHWAY',
#                                      leg.title='', 
#                                      jitter = F,
#                                      xlab = 'Grade',
#                                      legend.pos='tl',
#                                      show_compare = T)
# Grade_violin_hrd
# 
# hrd_Cluster <- data.frame(tcga.subtype.cli[, c("Cluster", "hrd")])
# hrd_Cluster <- na.omit(hrd_Cluster)
# Cluster_violin_hrd <- mg_violin(hrd_Cluster, melt=TRUE, ylab='hrd PATHWAY',
#                                        leg.title='', 
#                                        jitter = F,
#                                        xlab = 'Cluster',
#                                        legend.pos='tl',
#                                        show_compare = T)
# Cluster_violin_hrd
# 
# 
# tcga_ARA_violin <- cowplot::plot_grid(T.Stage_violin_hrd,
#                                       N.Stage_violin_hrd,
#                                       M.Stage_violin_hrd,
#                                       Stage_violin_hrd,
#                                       Grade_violin_hrd,
#                                       ncol = 5)
# tcga_ARA_violin





# hrd_RiskType <- data.frame(tcga.subtype.cli[, c("RiskType", "hrd")])
# hrd_RiskType <- na.omit(hrd_RiskType)
# RiskType_violin_hrd <- mg_violin(hrd_RiskType, melt=TRUE, ylab='hrd PATHWAY',
#                                         leg.title='',
#                                         jitter = F,
#                                         xlab = 'RiskType',
#                                         legend.pos='tl',
#                                         show_compare = T)
# RiskType_violin_hrd
# 
# 
# tcga_cluster_violin <- cowplot::plot_grid(Cluster_violin_RiskScore,
#                                           Cluster_violin_hrd,
#                                           RiskType_violin_hrd,
#                                           ncol = 3,
#                                           labels = LETTERS[3:5])
# tcga_cluster_violin
# ggsave(plot = tcga_cluster_violin,
#        filename = '06_model/tcga_cluster_violin.pdf',
#        width = 9, height = 5)
# 



# 高低风险突变分析 ##################
dir.create('07_RS_mut')

tcga.mut.dat <- getTCGAMAFByCode('LUAD')
tcga.mut.dat <- as.data.frame(tcga.mut.dat@data)
tcga.mut.dat <- tcga.mut.dat[, c("Hugo_Symbol", "Tumor_Sample_Barcode", "Variant_Classification")]

tcga.mut.dat$Variant_Classification <- 1
tcga.mut.dat <- reshape2::dcast(data = tcga.mut.dat, Hugo_Symbol ~ Tumor_Sample_Barcode)
class(tcga.mut.dat)
rownames(tcga.mut.dat) <- tcga.mut.dat$Hugo_Symbol
tcga.mut.dat <- tcga.mut.dat[, -1]



colnames(tcga.mut.dat) <- paste0(colnames(tcga.mut.dat), '-01')
mut.samples <- intersect(colnames(tcga.mut.dat), tcga.subtype.cli$Samples)


tcga.mut.dat <- tcga.mut.dat[, mut.samples]
tcga_mut_cli <- tcga.subtype.cli[mut.samples, ]

tcga.mut.dat.freq <- as.data.frame(rowSums(tcga.mut.dat))
colnames(tcga.mut.dat.freq) <- 'Freq'
tcga.mut.dat.freq$Genes <- rownames(tcga.mut.dat.freq)
library(dplyr)
str(tcga.mut.dat.freq)
head(tcga.mut.dat.freq)
tcga.mut.dat.freq <- dplyr::arrange(tcga.mut.dat.freq, desc(Freq))
head(tcga.mut.dat.freq)
dim(tcga.mut.dat.freq)
write.csv(tcga.mut.dat.freq,
          file = 'results/tcga.subtype.mut.gene.csv')

mut.genes <- rownames(tcga.mut.dat.freq)[tcga.mut.dat.freq$Freq > 3]
length(mut.genes)   # 5845
tcga.mut.dat <- ifelse(tcga.mut.dat > 0, 'Mutant', 'WildType')

dim(tcga.mut.dat)



which(mut.genes == 'TSC22D1')
tcga.mut.dat.freq['TSC22D1', ]
table(tcga.mut.dat['TSC22D1', ])

mut.res <- data.frame(High = NA,
                      Low = NA)

table(tcga.mut.dat["PLA2G3", ], tcga_mut_cli$RiskType)
mut.p <- c()
for (ge in mut.genes) {
  print(ge)
  tmp <- table(tcga.mut.dat[ge, ], tcga_mut_cli$RiskType)
  pvalue <- fisher.test(tmp)
  mut.p <- c(mut.p, pvalue$p.value)
  mut.res <- rbind(mut.res, tmp[1, ])
}
mut.res <- na.omit(mut.res)
rownames(mut.res) <- mut.genes
class(mut.res)
mut.res$P.value <- mut.p

table(mut.res$P.value < 0.01)
mut.res.filtered <- mut.res[which(mut.res$P.value < 0.01), ]
mut.res.filtered
dim(mut.res.filtered)   # 140
# write.csv(mut.res.filtered,
#           file = 'results/tcga.mut.dat.csv')
write.csv(mut.res.filtered,
          file = '07_RS_mut/tcga.genes.csv')

mut.plot.dat <- tcga.mut.dat[rownames(mut.res.filtered), ]
mut.plot.dat[mut.plot.dat == 'WildType'] <- NA
# rownames(mut.plot.dat) <- paste(rownames(mut.plot.dat), signif(mut.res.filtered$P.value[1:15], 1), sep = ' ')
library(scales)
library(ggsci)
library(ComplexHeatmap)
alter_graphic <- function (graphic = c("rect", "point"), width = 1, 
                           height = 1, horiz_margin = unit(1, "pt"), vertical_margin = unit(1, 
                                                                                            "pt"), fill = "red", col = NA, pch = 16, 
                           ...) 
{
  graphic = match.arg(graphic)[1]
  if (graphic == "rect") {
    if (!is.numeric(width)) {
      stop_wrap("`width` should be nummeric.")
    }
    if (!is.numeric(height)) {
      stop_wrap("`height` should be nummeric.")
    }
    if (width != 1) {
      if (missing(horiz_margin)) {
        horiz_margin = unit(0, "pt")
      }
    }
    if (height != 1) {
      if (missing(vertical_margin)) {
        vertical_margin = unit(0, "pt")
      }
    }
    fun = function(x, y, w, h) {
      w = w * width
      h = h * height
      grid.rect(x, y, w - horiz_margin * 2, h - vertical_margin * 
                  2, gp = gpar(fill = fill, col = col, ...))
    }
  }
  else if (graphic == "point") {
    fun = function(x, y, w, h) {
      grid.points(x, y, pch = pch, gp = gpar(fill = fill, 
                                             col = col, ...))
    }
  }
  return(fun)
}

# 定义突变颜色 #########
col = c("Mutant" = mycolor[3])

alter_fun = list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(2, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
  },
  
  Mutant = function(x, y, w, h) {
    grid.rect(x, y, w-unit(3, "pt"), h-unit(2, "pt"), 
              gp = gpar(fill = col["Mutant"], col = NA))
  }
)

alter_fun = list(
  background = alter_graphic("rect", fill = "#CCCCCC"),	
  Mutant = alter_graphic("rect", fill = col["Mutant"])
)
heatmap_legend_param = list(title = "Alterations", 
                            at = c("Mutant"), 
                            labels = c("Mutant"))

color_RiskType = mycolor[1:2]
names(color_RiskType) = c('High', 'Low')

dim(mut.plot.dat)
c1.tcga_mut_cli <- tcga_mut_cli[tcga_mut_cli$RiskType == 'High', ]
c1.mut.plot.dat <- mut.plot.dat[1:20, c1.tcga_mut_cli$Samples]
# pdf('PDFs/c1.mut.plot.dat.pdf', width = 8, height = 6, onefile = F)
table(tcga_mut_cli$RiskType)
dim(c1.mut.plot.dat)
mut1 <- oncoPrint(as.matrix(c1.mut.plot.dat),
                  row_order = rownames(c1.mut.plot.dat),
                  # column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun, 
                  col = col, 
                  # column_title = "", 
                  heatmap_legend_param = heatmap_legend_param,
                  bottom_annotation = HeatmapAnnotation(RiskType=c1.tcga_mut_cli[, c("RiskType")],
                                                        col=list("RiskType"=color_RiskType),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut1
dev.off()


c2.tcga_mut_cli <- tcga_mut_cli[tcga_mut_cli$RiskType == 'Low', ]
c2.mut.plot.dat <- mut.plot.dat[1:20, c2.tcga_mut_cli$Samples]
# pdf('PDFs/c2.mut.plot.dat.pdf', width = 8, height = 6, onefile = F)
table(tcga_mut_cli$RiskType)
mut2 <- oncoPrint(as.matrix(c2.mut.plot.dat),
                  row_order = rownames(c2.mut.plot.dat),
                  # column_order = tcga_mut_cli$Samples,
                  alter_fun = alter_fun, 
                  col = col, 
                  # column_title = "", 
                  heatmap_legend_param = heatmap_legend_param,
                  bottom_annotation = HeatmapAnnotation(RiskType=c2.tcga_mut_cli[, c("RiskType")],
                                                        col=list("RiskType"=color_RiskType),
                                                        show_annotation_name = TRUE,
                                                        gap=unit(1, "mm"),
                                                        na_col="grey"),
                  pct_side = "right", row_names_side = "left")
mut2
dev.off()



pdf('07_RS_mut/RS.mut.plot.dat.pdf', width = 12, height = 5, onefile = F)
mut1 + mut2
dev.off()


tcga_tmb <- mg_getTCGATMBByCode('LUAD')
tcga_tmb <- as.data.frame(tcga_tmb)
tcga_tmb$Sample <- paste0(tcga_tmb$Sample, '-01')
head(tcga_tmb)

tcga_tmb <- merge(tcga_tmb, tcga.subtype.cli, by.x = 'Sample', by.y = 'Samples', all = T)
tcga_tmb <- tcga_tmb[!is.na(tcga_tmb$RiskType), ]

str(tcga_tmb)
tcga_tmb <- crbind2DataFrame(tcga_tmb)
fivenum(tcga_tmb$TMB)
tcga_tmb$TMB.group <- ifelse(tcga_tmb$TMB > 1.5, 'H-TMB', 'L-TMB')

tcga_tmb$new.group <- NA
tcga_tmb$new.group[tcga_tmb$RiskType == 'High' & tcga_tmb$TMB.group == 'H-TMB'] <- 'H-TMB+High'
tcga_tmb$new.group[tcga_tmb$RiskType == 'Low' & tcga_tmb$TMB.group == 'H-TMB'] <- 'H-TMB+Low'
tcga_tmb$new.group[tcga_tmb$RiskType == 'High' & tcga_tmb$TMB.group == 'L-TMB'] <- 'L-TMB+High'
tcga_tmb$new.group[tcga_tmb$RiskType == 'Low' & tcga_tmb$TMB.group == 'L-TMB'] <- 'L-TMB+Low'

tcga_RS_tmb_box <- mg_violin(tcga_tmb[, c("RiskType", "TMB")]
                             ,melt = T
                             ,ylab = 'Tumor mutation burden'
                             ,legend.pos = 'tr'
                             ,jitter=F
                             ,show_compare = T)
tcga_RS_tmb_box

library(survcomp)
ggplotKMCox(data.frame(tcga_tmb$OS.time/365
                       , event = tcga_tmb$OS
                       , tcga_tmb$TMB.group)
            # ,labs = c('C1','C2','C3')
            ,title = 'TCGA'
            , add_text = '')

tcga_Rs_tmb_km_plot <- ggplotKMCox(data.frame(tcga_tmb$OS.time/365
                                              , event = tcga_tmb$OS
                                              , tcga_tmb$new.group)
                                   ,labs = c('H-TMB+High',
                                             'H-TMB+Low',
                                             'L-TMB+High',
                                             'L-TMB+Low')
                                   ,title = 'TCGA'
                                   , add_text = '')
tcga_Rs_tmb_km_plot
ggsave(plot = tcga_Rs_tmb_km_plot,
       filename = '07_RS_mut/tcga_Rs_tmb_km_plot.pdf',
       width = 6, height = 6)


tcga_RS_tmb_cor <- cor_point(x = tcga_tmb$TMB,
                             y = tcga_tmb$RiskScore,
                             top_col = mycolor[1],
                             right_col = mycolor[2],
                             xlab = 'TMB',
                             ylab = 'RiskScore')
tcga_RS_tmb_cor
ggsave(plot = tcga_RS_tmb_cor,
       filename = '07_RS_mut/tcga_RS_tmb_cor.pdf',
       width = 7, height = 5)
table(tcga_tmb$RiskType)

pdf('07_RS_mut/plot.pdf', width = 15, height = 5)
cowplot::plot_grid(tcga_RS_tmb_cor,
                   tcga_RS_tmb_box,
                   tcga_Rs_tmb_km_plot,
                   ncol = 3,
                   labels = LETTERS[2:4]
)
dev.off()







# 风险分组的免疫评分比较 #############
dir.create('08_RS_immune')
tcga_tme1.list1_RSplot <- wb_boxplot(dat = tcga_tme1.list1_score[, tcga.subtype.cli$Samples],
                                     groups = tcga.subtype.cli$RiskType,
                                     title = 'RiskType',
                                     xangle = 60,
                                     ylab = 'Score',
                                     col = mycolor)
tcga_tme1.list1_RSplot

tcga_tme1.list2_RSplot <- wb_boxplot(dat = tcga_tme1.list2_score[, tcga.subtype.cli$Samples],
                                     groups = tcga.subtype.cli$RiskType,
                                     xangle = 0,
                                     title = 'RiskType',
                                     ylab = 'Score',
                                     col = mycolor)
tcga_tme1.list2_RSplot

tcga_tme1.list2_RSlist <- list()
for (fea in rownames(tcga_tme1.list2_score)) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$RiskType,
                              as.numeric(tcga_tme1.list2_score[fea, 
                                                               tcga.subtype.cli$Samples])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'RiskType',
                   legend.pos='tl',
                   show_compare = T)
  tcga_tme1.list2_RSlist[[fea]] <- tmp
}

tcga_tme1.list2_RSplot <- cowplot::plot_grid(plotlist = tcga_tme1.list2_RSlist,
                                             ncol = 2)
tcga_tme1.list2_RSplot


tcga_tme2.list_RSplot <- wb_boxplot(dat = tcga_tme2.list_score[, tcga.subtype.cli$Samples],
                                    groups = tcga.subtype.cli$RiskType,
                                    title = 'RiskType',
                                    xangle = 60,
                                    ylab = 'Score',
                                    col = mycolor)
tcga_tme2.list_RSplot

tcga_ImmuCellAI.list_RSplot <- wb_boxplot(dat = tcga_ImmuCellAI.list_score[, tcga.subtype.cli$Samples],
                                          groups = tcga.subtype.cli$RiskType,
                                          title = 'RiskType',
                                          xangle = 60,
                                          ylab = 'Score',
                                          col = mycolor)
tcga_ImmuCellAI.list_RSplot

tcga_immune_RiskType_plot1 <- cowplot::plot_grid(tcga_tme1.list1_RSplot,
                                                 tcga_tme1.list2_RSplot,
                                                 ncol = 2,
                                                 rel_widths = c(7,3))
tcga_immune_RiskType_plot1


tcga_immune_RiskType_plot <- cowplot::plot_grid(tcga_tme1.list1_RSplot,
                                                tcga_tme2.list_RSplot,
                                                ncol = 1)
tcga_immune_RiskType_plot
ggsave(plot = tcga_immune_RiskType_plot,
       filename = '08_RS_immune/tcga_immune_RiskType_plot.pdf',
       width = 18, height = 10)


tcga_estimate <- immu_estimate(exp = tcga_tpm_log2)


tcga_estimate_list <- list()
for (fea in colnames(tcga_estimate)) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$RiskType,
                              as.numeric(tcga_estimate[tcga.subtype.cli$Samples, fea])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'RiskType',
                   legend.pos='tl',
                   show_compare = T)
  tcga_estimate_list[[fea]] <- tmp
}

tcga_estimate_RSplot <- cowplot::plot_grid(plotlist = tcga_estimate_list,
                                           ncol = 3,
                                           labels = LETTERS[1:3])
tcga_estimate_RSplot
ggsave(plot = tcga_estimate_RSplot,
       filename = '08_RS_immune/tcga_estimate_RSplot.pdf',
       width = 9, height = 5)


load('00_origin_datas/IMvigor210/human_gene_signatures.RData')
# h.list <- clusterProfiler::read.gmt('00_origin_datas/msigdb_v7.4_GMTs/h.all.v7.4.symbols.gmt')
# h.list <- split(h.list$gene, h.list$ont)


save(human_gene_signatures,
     h.list,
     file = 'signature.Rdata')

load('signature.Rdata')

tcga_human_gene_signatures_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                                    genelist = human_gene_signatures)

tcga_h_score <- ssGSEAScore_by_muti_group_genes(gene.exp = tcga_tpm_log2,
                                                genelist = h.list)

save(tcga_human_gene_signatures_score,
     tcga_h_score,
     file = 'signature_score.Rdata')
load('signature_score.Rdata')


tcga_human_gene_signatures_score <- as.data.frame(t(tcga_human_gene_signatures_score[, tcga.subtype.cli$Samples]))
tcga_human_gene_signatures_score$RiskScore <- tcga.subtype.cli$RiskScore
library(ggcorrplot)
colnames(tcga_human_gene_signatures_score)
tcga_human_gene_signatures_score <- tcga_human_gene_signatures_score[, c(1,2,10:21)]

colnames(tcga_human_gene_signatures_score)
tcga_human_gene_signatures_boxplot <- wb_boxplot(dat = t(tcga_human_gene_signatures_score[tcga.subtype.cli$Samples, 1:13]),
                                                 groups = tcga.subtype.cli$RiskType,
                                                 title = 'RiskType',
                                                 xangle = 60,
                                                 ylab = 'Score',
                                                 col = mycolor)
tcga_human_gene_signatures_boxplot
ggsave(plot = tcga_human_gene_signatures_boxplot,
       filename = '08_RS_immune/tcga_human_gene_signatures_boxplot.pdf',
       width = 8, height = 5)



tcga_hs_RS_cor <- psych::corr.test(as.matrix(tcga_human_gene_signatures_score))
tcga_hs_RS_cor_R <- tcga_hs_RS_cor$r
tcga_hs_RS_cor_P <- tcga_hs_RS_cor$p

?ggcorrplot
tcga_hs_RS_cor_plot <- ggcorrplot(tcga_hs_RS_cor_R, 
                                  hc.order = TRUE,
                                  colors = c(mycolor[1], "white", mycolor[2]),
                                  ggtheme = ggplot2::theme_classic,
                                  # type = c("lower"),
                                  p.mat = tcga_hs_RS_cor_P)
tcga_hs_RS_cor_plot
ggsave(plot = tcga_hs_RS_cor_plot,
       filename = '08_RS_immune/tcga_hs_RS_cor_plot.pdf',
       width = 8, height = 8)


library(corrplot)
pdf('08_RS_immune/tcga_hs_RS_cor_plot.pdf', width = 8, height = 8, onefile = F)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(tcga_hs_RS_cor_R, 
         method="color", 
         col=col(200)[200:1],  
         type="lower",
         order="hclust", 
         p.mat = tcga_hs_RS_cor_P,
         sig.level = 0.05,
         addCoef.col = "black", #添加相关系数
         diag=T 
)
dev.off()




table(tcga.subtype.cli$RiskType)

tmp <- t.test(as.numeric(tcga_h_score["SPERMATOGENESIS", h.samples]),
              as.numeric(tcga_h_score["SPERMATOGENESIS", l.samples]))
tmp$p.value

dim(tcga_h_score)
rownames(tcga_h_score) <- gsub('HALLMARK_', '', rownames(tcga_h_score))

tcga_h_score_p <- c()
for (pa in rownames(tcga_h_score)) {
  print(pa)
  h.samples <- tcga.subtype.cli$Samples[tcga.subtype.cli$RiskType == 'High']
  l.samples <- tcga.subtype.cli$Samples[tcga.subtype.cli$RiskType == 'Low']
  
  tmp <- t.test(as.numeric(tcga_h_score[pa, h.samples]),
                as.numeric(tcga_h_score[pa, l.samples]))
  tcga_h_score_p <- c(tcga_h_score_p, tmp$p.value)
}

h.sing <- data.frame(Pathways = rownames(tcga_h_score),
                     pvalue  = tcga_h_score_p,
                     stringsAsFactors = F)
h.sing$sing <- ifelse(h.sing$pvalue < 0.0001, '****',
                      ifelse(h.sing$pvalue < 0.001, '***',
                             ifelse(h.sing$pvalue < 0.01, '**',
                                    ifelse(h.sing$pvalue < 0.05, '*', ''))))
h.sing <- h.sing[h.sing$sing != '', ]

h.sing$name <- paste0(h.sing$Pathways, ' ', h.sing$sing)
head(h.sing)
dim(h.sing)    # 34

tcga_h_RSplot <- wb_boxplot(dat = tcga_h_score[h.sing$Pathways, tcga.subtype.cli$Samples],
                            groups = tcga.subtype.cli$RiskType,
                            title = 'RiskType',
                            xangle = 60,
                            ylab = 'Score',
                            col = mycolor)
tcga_h_RSplot
ggsave(plot = tcga_h_RSplot,
       filename = '08_RS_immune/tcga_h_RSplot.pdf',
       width = 18, height = 5)


library(ComplexHeatmap)
library(dplyr)
class(tcga.subtype.cli)
colnames(tcga.subtype.cli)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
tcga.subtype.cli <- arrange(tcga.subtype.cli, RiskType)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
# tcga_ssgsea_immune <- t(scale(tcga_ssgsea[rownames(tcga.subtype.cli), ]))
tcga_h_score_pheatmap <- t(scale(t(tcga_h_score[h.sing$Pathways, tcga.subtype.cli$Samples])))
rownames(tcga_h_score_pheatmap) <- h.sing$name
tcga_ssgsea_h_plot <- Heatmap(tcga_h_score_pheatmap[, tcga.subtype.cli$Samples]
                              , name = "score"
                              , col = circlize::colorRamp2(c(-2, 0, 2), c(mycolor[1], 'white', mycolor[2]))
                              , border = T
                              , show_column_names = F
                              , show_column_dend = F
                              , show_row_dend = F
                              , cluster_columns = T
                              , cluster_rows = T
                              , column_split = factor(tcga.subtype.cli$RiskType)
                              , clustering_distance_rows  ='pearson'
                              , clustering_method_rows = 'ward.D2'
                              , clustering_distance_columns  ='pearson'
                              , clustering_method_columns = 'ward.D2'
                              , row_names_gp = gpar(fontsize = 10)
                              , top_annotation = HeatmapAnnotation(RiskType = tcga.subtype.cli$RiskType
                                                                   , col=list(RiskType=c('High'=mycolor[1], 
                                                                                         'Low' = mycolor[2]))
                                                                   , annotation_width = unit(c(1,2), 'cm')
                                                                   , annotation_height = unit(0.2, "cm")
                                                                   , gap = unit(1, 'mm')))

tcga_ssgsea_h_plot
dev.off()
dir.create('08_RS_immune/', showWarnings = F)
pdf('08_RS_immune/tcga_ssgsea_h_plot.pdf',
    width = 10, height = 10)
tcga_ssgsea_h_plot
dev.off()


dim(tcga_h_score)
tcga_hallmark_RS_score <- as.data.frame(t(tcga_h_score[h.sing$Pathways, tcga.subtype.cli$Samples]))
dim(tcga_hallmark_RS_score)
tcga_hallmark_RS_score$RiskScore <- tcga.subtype.cli$RiskScore
tcga_hallmark_RS_cor <- psych::corr.test(as.matrix(tcga_hallmark_RS_score))
tcga_hallmark_RS_cor_R <- tcga_hallmark_RS_cor$r
tcga_hallmark_RS_cor_P <- tcga_hallmark_RS_cor$p



library(corrplot)
pdf('08_RS_immune/tcga_hallmark_RS_cor_plot.pdf', width = 22, height = 22, onefile = F)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(tcga_hallmark_RS_cor_R,
         method="color",
         col=col(200)[200:1],
         type="upper",
         order="hclust",
         p.mat = tcga_hallmark_RS_cor_P,
         sig.level = 0.05,
         addCoef.col = "black", #添加相关系数
         diag=T
)
dev.off()






# TIDE 分析 ###################
dir.create('09_TIDE')
tcga_tide_dat <- t(scale(t(tcga_tpm_log2[, tcga.subtype.cli$Samples]),scale = F))
boxplot(tcga_tide_dat[, 1:5])
write.table(tcga_tide_dat,
            file = '09_TIDE/tcga_tide_dat.txt',
            quote = F, sep = '\t')

tcga_tide_dat<-read.csv('09_TIDE/tcga_tide_res.csv',row.names = 1,stringsAsFactors = F)
colnames(tcga_tide_dat)


colnames(tcga_tide_dat)
tcga_tide_RSplot <- wb_boxplot(dat = t(tcga_tide_dat[tcga.subtype.cli$Samples, c("TIDE",
                                                                                 "Dysfunction",
                                                                                 "Exclusion")]),
                               groups = tcga.subtype.cli$RiskType,
                               title = 'RiskType',
                               xangle = 0,
                               ylab = 'Score',
                               col = mycolor)
tcga_tide_RSplot
ggsave(plot = tcga_tide_RSplot,
       filename = '09_TIDE/tcga_tide_RSplot.pdf',
       width = 6, height = 5)


tcga_tide_list <- list()
for (fea in c("TIDE",
              "Dysfunction",
              "Exclusion")) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$RiskType,
                              as.numeric(tcga_tide_dat[tcga.subtype.cli$Samples, fea])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'RiskType',
                   legend.pos='tl',
                   show_compare = T)
  tcga_tide_list[[fea]] <- tmp
}

tcga_tide_RSplot <- cowplot::plot_grid(plotlist = tcga_tide_list,
                                       ncol = 3,
                                       labels = LETTERS[1:3])
tcga_tide_RSplot
ggsave(plot = tcga_tide_RSplot,
       filename = '09_TIDE/tcga_tide_RSplot.pdf',
       width = 9, height = 5)



tcga_tide_dat <- cbind(tcga_tide_dat,
                       tcga.subtype.cli[rownames(tcga_tide_dat), ])


tcga_tide_rs_cor <- cor_point(x = tcga_tide_dat$TIDE,
                              y = tcga_tide_dat$RiskScore,
                              xlab = 'TIDE',
                              ylab = 'RiskScore',
                              top_col = mycolor[1],
                              right_col = mycolor[2])
tcga_tide_rs_cor

tcga_Dysfunction_rs_cor <- cor_point(x = tcga_tide_dat$Dysfunction,
                                     y = tcga_tide_dat$RiskScore,
                                     xlab = 'Dysfunction',
                                     ylab = 'RiskScore',
                                     top_col = mycolor[1],
                                     right_col = mycolor[2])
tcga_Dysfunction_rs_cor

tcga_Exclusion_rs_cor <- cor_point(x = tcga_tide_dat$Exclusion,
                                   y = tcga_tide_dat$RiskScore,
                                   xlab = 'Exclusion',
                                   ylab = 'RiskScore',
                                   top_col = mycolor[1],
                                   right_col = mycolor[2])
tcga_Exclusion_rs_cor

tcga_tide_plot <- cowplot::plot_grid(tcga_tide_rs_cor,
                                     tcga_Dysfunction_rs_cor,
                                     tcga_Exclusion_rs_cor,
                                     ncol = 1)
tcga_tide_plot
ggsave(plot = tcga_tide_plot,
       filename = '09_TIDE/tcga_tide_plot.pdf',
       width = 7, height = 15)



# 分组亚型的 TIDE 分析 ###################
tcga_tide_cluster_plot <- wb_boxplot(dat = t(tcga_tide_dat[tcga.subtype.cli$Samples, c("TIDE",
                                                                                       "Dysfunction",
                                                                                       "Exclusion")]),
                                     groups = tcga.subtype.cli$Cluster,
                                     title = 'Cluster',
                                     xangle = 30,
                                     ylab = 'Score',
                                     col = mycolor)
tcga_tide_cluster_plot



tcga_tide_cluster_list <- list()
for (fea in c("TIDE",
              "Dysfunction",
              "Exclusion")) {
  print(fea)
  tmp <- mg_violin(data.frame(tcga.subtype.cli$Cluster,
                              as.numeric(tcga_tide_dat[tcga.subtype.cli$Samples, fea])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='bl',
                   show_compare = T)
  tcga_tide_cluster_list[[fea]] <- tmp
}

tcga_tide_cluster_plot <- cowplot::plot_grid(plotlist = tcga_tide_cluster_list,
                                             ncol = 3,
                                             labels = LETTERS[1:3])
tcga_tide_cluster_plot

colnames(tcga_tide_dat)
library(ggstatsplot)
tcga_bar_plot <- ggbarstats(
  data             = tcga_tide_dat,
  x                = Responder,
  y                = Cluster,
  bf.message = F,
  title            = "TCGA",
  xlab             = "Cluster",
  legend.title     = "Responder",
  package = "ggsci",
  palette = "default_jco"
)
tcga_bar_plot


# GSE30219 #########
GSE30219_exp[1:5, 1:5]
GSE30219_tide_dat <- t(scale(t(GSE30219_exp[, GSE30219.subtype.cli$samples]),scale = F))
boxplot(GSE30219_tide_dat[, 1:5])
write.table(GSE30219_tide_dat,
            file = '09_TIDE/GSE30219_tide_dat.txt',
            quote = F, sep = '\t')

GSE30219_tide_dat<-read.csv('09_TIDE/GSE30219_tide_res.csv',row.names = 1,stringsAsFactors = F)
colnames(GSE30219_tide_dat)




GSE30219.subtype.cli[1:5, 1:5]
GSE30219_tide_dat <- cbind(GSE30219_tide_dat,
                       GSE30219.subtype.cli[rownames(GSE30219_tide_dat), ])


# 分组亚型的 TIDE 分析 ###################
GSE30219_tide_cluster_plot <- wb_boxplot(dat = t(GSE30219_tide_dat[GSE30219.subtype.cli$samples, c("TIDE",
                                                                                       "Dysfunction",
                                                                                       "Exclusion")]),
                                     groups = GSE30219.subtype.cli$Cluster,
                                     title = 'Cluster',
                                     xangle = 30,
                                     ylab = 'Score',
                                     col = mycolor)
GSE30219_tide_cluster_plot



GSE30219_tide_cluster_list <- list()
for (fea in c("TIDE",
              "Dysfunction",
              "Exclusion")) {
  print(fea)
  tmp <- mg_violin(data.frame(GSE30219.subtype.cli$Cluster,
                              as.numeric(GSE30219_tide_dat[GSE30219.subtype.cli$samples, fea])), 
                   melt=TRUE, ylab=fea,
                   leg.title='', 
                   jitter = F,
                   xlab = 'Cluster',
                   legend.pos='bl',
                   show_compare = T)
  GSE30219_tide_cluster_list[[fea]] <- tmp
}

GSE30219_tide_cluster_plot <- cowplot::plot_grid(plotlist = GSE30219_tide_cluster_list,
                                             ncol = 3,
                                             labels = LETTERS[1:3])
GSE30219_tide_cluster_plot

tcga_tide_cluster_plot

library(ggstatsplot)
colnames(GSE30219_tide_dat)
GSE30219_bar_plot <- ggbarstats(
  data             = GSE30219_tide_dat,
  x                = Responder,
  y                = Cluster,
  bf.message = F,
  title            = "GSE30219",
  xlab             = "Cluster",
  legend.title     = "Responder",
  package = "ggsci",
  palette = "default_jco"
)
GSE30219_bar_plot


tcga_GSE30219_cluster_tide_plot <- cowplot::plot_grid(tcga_tide_cluster_plot,
                                                     tcga_bar_plot,
                                                     GSE30219_tide_cluster_plot,
                                                     GSE30219_bar_plot,
                                                     ncol = 2,
                                                     rel_widths = c(2, 1),
                                                     labels = LETTERS[1:4])
tcga_GSE30219_cluster_tide_plot
ggsave(plot = tcga_GSE30219_cluster_tide_plot,
       filename = '09_TIDE/tcga_GSE30219_cluster_tide_plot.pdf',
       width = 20, height = 10)








# 药物敏感性 ###########################
library(pRRophetic)
library(ggplot2)
# tcga 
## Cisplatin,顺铂
set.seed(12345)
tcga_Cisplatin <- pRRopheticPredict(as.matrix(tcga_tpm_log2[, tcga.subtype.cli$Samples])
                                    , "Cisplatin"
                                    , "all"
                                    , selection=1
                                    , dataset = "cgp2016")
tcga_Cisplatin <- data.frame(tcga_Cisplatin)

tcga_durg_ic50_res <- tcga_Cisplatin

# drugs <- c("Cisplatin","Erlotinib","Rapamycin","Sunitinib","PHA-665752","MG-132","Paclitaxel",
#            "Cyclopamine","AZ628","Sorafenib","VX-680","Imatinib","TAE684","Crizotinib","Saracatinib",
#            "S-Trityl-L-cysteine","Z-LLNle-CHO","Dasatinib","GNF-2","CGP-60474","CGP-082996","A-770041",
#            "WH-4-023","WZ-1-84","BI-2536","BMS-509744","CMK","Pyrimethamine","JW-7-52-1","A-443654",
#            "GW843682X","MS-275","Parthenolide","KIN001-135","TGX221","Bortezomib","XMD8-85",
#            "Roscovitine","Salubrinal","Lapatinib","Vinorelbine","NSC-87877","QS11","CP466722",
#            "Midostaurin","Shikonin","AKT inhibitor VIII","Embelin","Bexarotene","Bleomycin","Phenformin")
# drugs <- c("Imatinib", "Pyrimethamine", "Paclitaxel",
#            "Sunitinib", "Saracatinib", "Dasatinib")

load('../../public/drugs_names.Rdata')
library(stringr)
drugs <- drugs[!str_ends(drugs, '[0-9]')]
save(drugs,
     tcga_durg_ic50_res,
     tcga_tpm_log2,
     tcga.subtype.cli,
     file = 'drug.Rdata')
load('drug.Rdata')
dim(tcga_durg_ic50_res)
colnames(tcga_durg_ic50_res)
which(drugs == "RDEA119")
length(drugs)
for (drug in drugs) {
  print(drug)
  set.seed(12345)
  tmpic50 <- pRRopheticPredict(as.matrix(tcga_tpm_log2[, tcga.subtype.cli$Samples])
                               , drug
                               , "all"
                               , selection=1
                               , dataset = "cgp2016")
  tmpic50 <- data.frame(tmpic50)
  colnames(tmpic50) <- drug
  tcga_durg_ic50_res <- cbind(tcga_durg_ic50_res, tmpic50)
}

colnames(tcga_durg_ic50_res)
tcga_durg_ic50_res <- tcga_durg_ic50_res[, -1]
class(tcga_durg_ic50_res)

save(tcga_durg_ic50_res,
     file = 'tcga_durg_ic50_res.Rdata')
# drugs <- colnames(tcga_durg_ic50_res)
# drugs
# save(drugs,
#      file = '../../public/drugs_names.Rdata')

load('tcga_durg_ic50_res.Rdata')

dim(tcga_durg_ic50_res)
dim(tcga.subtype.cli)
table(tcga.subtype.cli$RiskType)

tcga_durg_ic50_res <- as.data.frame(t(tcga_durg_ic50_res))

tcga_durg_ic50_p <- c()
high.ic50 <- c()
low.ic50 <- c()
for (pa in rownames(tcga_durg_ic50_res)) {
  print(pa)
  h.samples <- tcga.subtype.cli$Samples[tcga.subtype.cli$RiskType == 'High']
  l.samples <- tcga.subtype.cli$Samples[tcga.subtype.cli$RiskType == 'Low']
  
  tmp <- t.test(as.numeric(tcga_durg_ic50_res[pa, h.samples]),
                as.numeric(tcga_durg_ic50_res[pa, l.samples]))
  tcga_durg_ic50_p <- c(tcga_durg_ic50_p, tmp$p.value)
  high.ic50 <- c(high.ic50, mean(as.numeric(tcga_durg_ic50_res[pa, h.samples])))
  low.ic50 <- c(low.ic50, mean(as.numeric(tcga_durg_ic50_res[pa, l.samples])))
}

drug.sing <- data.frame(Pathways = rownames(tcga_durg_ic50_res),
                        pvalue  = tcga_durg_ic50_p,
                        high.ic50 = high.ic50,
                        low.ic50 = low.ic50,
                        stringsAsFactors = F)
drug.sing$sing <- ifelse(drug.sing$pvalue < 0.0001, '****',
                         ifelse(drug.sing$pvalue < 0.001, '***',
                                ifelse(drug.sing$pvalue < 0.01, '**',
                                       ifelse(drug.sing$pvalue < 0.05, '*', ''))))
drug.sing <- drug.sing[drug.sing$sing != '', ]

drug.sing$name <- paste0(drug.sing$Pathways, ' ', drug.sing$sing)
drug.sing$sensitivity <- drug.sing$high.ic50 > drug.sing$low.ic50

drug.sing$sensitivity <- ifelse(drug.sing$sensitivity == TRUE, 'Low', 'High')
table(drug.sing$sensitivity)
library(stringr)
drug.sing <- drug.sing[!str_ends(drug.sing$Pathways, '[0-9]'), ]
dim(drug.sing)     # 57
table(drug.sing$sensitivity)   # 44 13


library(ComplexHeatmap)
library(dplyr)
class(tcga.subtype.cli)
colnames(tcga.subtype.cli)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
tcga.subtype.cli <- arrange(tcga.subtype.cli, RiskType)
rownames(tcga.subtype.cli) <- tcga.subtype.cli$Samples
# tcga_ssgsea_immune <- t(scale(tcga_ssgsea[rownames(tcga.subtype.cli), ]))
tcga_durg_ic50_pheatmap <- t(scale(t(tcga_durg_ic50_res[drug.sing$Pathways, tcga.subtype.cli$Samples])))
rownames(tcga_durg_ic50_pheatmap) <- drug.sing$name
tcga_durg_ic50_pheatmap_plot <- Heatmap(tcga_durg_ic50_pheatmap[drug.sing$name,
                                                                tcga.subtype.cli$Samples]
                                        , name = "score"
                                        , col = circlize::colorRamp2(c(-2, 0, 2), c(mycolor[2], 'white', mycolor[1]))
                                        , border = T
                                        , show_column_names = F
                                        , show_column_dend = F
                                        , show_row_dend = F
                                        , cluster_columns = T
                                        , cluster_rows = T
                                        , clustering_distance_rows  ='pearson'
                                        , clustering_method_rows = 'ward.D2'
                                        , clustering_distance_columns  ='pearson'
                                        , clustering_method_columns = 'ward.D2'
                                        , row_names_gp = gpar(fontsize = 10)
                                        , column_split = factor(tcga.subtype.cli$RiskType)
                                        , top_annotation = HeatmapAnnotation(RiskType = tcga.subtype.cli$RiskType
                                                                             , col=list(RiskType=c('High'=mycolor[1], 
                                                                                                   'Low' = mycolor[2]))
                                                                             , annotation_width = unit(c(1,2), 'cm')
                                                                             , annotation_height = unit(0.2, "cm")
                                                                             , gap = unit(1, 'mm'))
                                        , row_split = factor(drug.sing$sensitivity)
                                        , left_annotation = rowAnnotation(Sensitivity = drug.sing$sensitivity
                                                                          , col=list(Sensitivity=c('High'=mycolor[3], 
                                                                                                   'Low' = mycolor[4]))
                                                                          , annotation_width = unit(c(1,2), 'cm')
                                                                          , annotation_height = unit(0.2, "cm")
                                                                          , gap = unit(1, 'mm')))

tcga_durg_ic50_pheatmap_plot
dev.off()

pdf('09_TIDE/tcga_durg_ic50_pheatmap_plot.pdf',
    width = 10, height = 15)
tcga_durg_ic50_pheatmap_plot
dev.off()









# RS 与 临床决策树 ##############
dir.create('10_nomogram')
##survival decision tree
library(rpart)
library(rpart.plot)

all.t.cli.forModel=tcga.subtype.cli
colnames(all.t.cli.forModel)
all.t.cli.forModel$Group <- all.t.cli.forModel$RiskType
all.t.cli.forModel$Group <- factor(all.t.cli.forModel$Group,levels = c('Low','High'))

table(all.t.cli.forModel$Stage)
all.t.cli.forModel$Stage=ifelse(all.t.cli.forModel$Stage %in% c('I','II'),'I+II','III+IV')
all.t.cli.forModel$Stage=as.factor(all.t.cli.forModel$Stage)


# table(all.t.cli.forModel$Grade)
# all.t.cli.forModel$Grade=ifelse(all.t.cli.forModel$Grade %in% c('G1','G2'),'G1+G2','G3+G4')
# all.t.cli.forModel$Grade=as.factor(all.t.cli.forModel$Grade)


table(all.t.cli.forModel$T.Stage)
all.t.cli.forModel$T.Stage=ifelse(all.t.cli.forModel$T.Stage %in% c('T1','T2'),'T1+T2','T3+T4')
all.t.cli.forModel$T.Stage=as.factor(all.t.cli.forModel$T.Stage)


table(all.t.cli.forModel$N.Stage)
all.t.cli.forModel$N.Stage=ifelse(all.t.cli.forModel$N.Stage %in% c('N0','N1'),'N0+N1','N2+N3')
all.t.cli.forModel$N.Stage=as.factor(all.t.cli.forModel$N.Stage)

table(all.t.cli.forModel$M.Stage)
all.t.cli.forModel$M.Stage[all.t.cli.forModel$M.Stage == 'MX'] <- NA
all.t.cli.forModel$M.Stage=as.factor(all.t.cli.forModel$M.Stage)


table(all.t.cli.forModel$Age)
all.t.cli.forModel$Age1 <- ifelse(all.t.cli.forModel$Age > 60, '>60', '<=60')
all.t.cli.forModel$Age1=factor(all.t.cli.forModel$Age1,levels = c('<=60','>60'))

table(all.t.cli.forModel$Gender)


str(all.t.cli.forModel)
colnames(all.t.cli.forModel)
library(survival)
library(ggsci)
pfit <- rpart(Surv(OS.time, OS) ~ Age1 + Gender + T.Stage + N.Stage + M.Stage + Stage +  Group, 
              data = all.t.cli.forModel)
print(pfit)

printcp(pfit)

pfit2 <- prune(pfit, cp=0.01)
print(pfit2)

pdf('10_nomogram/Fig12A.pdf',height = 5,width = 5)
rpart.plot(pfit2)
dev.off()

all.t.cli.forModel$class=pfit2$where
table(all.t.cli.forModel$class)

all.t.cli.forModel$class[all.t.cli.forModel$class==3]='C1'
all.t.cli.forModel$class[all.t.cli.forModel$class==4]='C2'
all.t.cli.forModel$class[all.t.cli.forModel$class==6]='C3'
all.t.cli.forModel$class[all.t.cli.forModel$class==8]='C4'
all.t.cli.forModel$class[all.t.cli.forModel$class==9]='C5'





table(all.t.cli.forModel$Group,all.t.cli.forModel$class)

fit <- survfit( Surv(OS.time/365, OS) ~ class,data = all.t.cli.forModel)
library(survminer)
custom_theme <- function() {
  theme_survminer() %+replace%
    theme(
      plot.title=element_text(hjust=0.5)
    )
}
fig12b=ggsurvplot(fit,data=all.t.cli.forModel,
                  conf.int = T,
                  pval = TRUE,
                  fun = "pct",
                  risk.table = TRUE,
                  size = 1,
                  title='',
                  ggtheme=theme_classic(),
                  # linetype = "strata",
                  palette = pal_jco()(9)[1:5],
                  legend = "bottom",
                  legend.title = "Cluster",
                  legend.labs = c("C1","C2",'C3','C4','C5')
)
fig12b
fig12B=ggpubr::ggarrange(fig12b$plot,fig12b$table, ncol = 1, nrow = 2
                         ,heights = c(0.7,0.3)
                         ,align = "v",common.legend = T)
fig12B
table(all.t.cli.forModel$Group
      ,all.t.cli.forModel$class)

fig12c=plotMutiBar(table(all.t.cli.forModel$Group
                         ,all.t.cli.forModel$class))
fig12c
fig12d=plotMutiBar(table(all.t.cli.forModel$OS
                         ,all.t.cli.forModel$class))
fig12d

fig12bcd=mg_merge_plot(fig12B,fig12c,fig12d,nrow = 1,ncol = 3,labels = c('B','C','D'))
fig12bcd

savePDF('10_nomogram/Fig12BCD.pdf',fig12bcd,height = 8,width = 15)

########
mg_compare_uni_muti_cox_use=function(dat,event,os){
  # dat=crbind2DataFrame(dat)
  sig.clini=colnames(dat)
  dat$time=os
  dat$status=event
  dat=dat[which(!is.na(os)&!is.na(event)),]
  all.cox=rbind()
  rnames=c()
  for(s in sig.clini){
    fmla <- as.formula(paste0("Surv(time, status) ~",s))
    cox <- coxph(fmla, data = dat)
    #summary(cox)[[7]]
    #print(summary(cox))
    re=cbind(summary(cox)[[7]][,5],summary(cox)[[7]][,2],summary(cox)[[8]][,3],summary(cox)[[8]][,4])
    if(nrow(re)==1){
      rnames=c(rnames,s)
    }else{
      rnames=c(rnames,row.names(summary(cox)[[7]]))
    }
    all.cox=rbind(all.cox,re)
  }
  row.names(all.cox)=rnames
  colnames(all.cox)=c('p.value','HR','Low 95%CI','High 95%CI')
  
  fmla <- as.formula(paste0("Surv(time, status) ~",paste0(sig.clini,collapse = '+')))
  cox <- coxph(fmla, data = dat)
  muti.re=cbind(summary(cox)[[7]][,5],summary(cox)[[7]][,2],summary(cox)[[8]][,3],summary(cox)[[8]][,4])
  row.names(muti.re)=row.names(summary(cox)[[7]])
  colnames(muti.re)=c('p.value','HR','Low 95%CI','High 95%CI')
  return(list(muti=crbind2DataFrame(muti.re),uni=crbind2DataFrame(all.cox)))
}
getForestplotData=function(res_sig){
  res_sig<-signif(res_sig,digits=2)
  res_sig$CI_for_HR=paste0(" (",res_sig$`Low 95%CI`, "-", res_sig$`High 95%CI`, ")")
  colnames(res_sig)=c("p.value","HR","CI_lower","CI_upper", "(95%_CI_for_HR)")
  
  forest_table<-data.frame(Features=rownames(res_sig),HR=res_sig$HR,`(95%CI)`=res_sig$`(95%_CI_for_HR)`, pvalue=res_sig$p.value,check.names = F,stringsAsFactors = F)
  forest_table$sig<-mg_format_p_values(forest_table$pvalue)
  
  forest_table2<-data.frame(Features="Features",HR="HR",`(95%CI)`="(95%CI)", pvalue="p-value",sig='Significant',check.names = F,stringsAsFactors=F)
  tabletext<-rbind(forest_table2,forest_table)
  
  forest_stastic<-data.frame(mean=as.numeric(as.character(res_sig$HR)),lower=as.numeric(as.character(res_sig$CI_lower)),upper=as.numeric(as.character(res_sig$CI_upper)))
  forest_stastic1<-data.frame(mean=NA,lower=NA,upper=NA)
  cochrane_from_rmeta<-rbind(forest_stastic1,forest_stastic)
  return(list(tabletext,cochrane_from_rmeta))
}

library(forestplot)

all.t.cli.forCox=all.t.cli.forModel
colnames(all.t.cli.forCox)
colnames(all.t.cli.forCox)[c(11:16,21)]

cm.cox=mg_compare_uni_muti_cox_use(all.t.cli.forCox[,c(11:16,21)],
                                   os=all.t.cli.forCox$OS.time,
                                   event = all.t.cli.forCox$OS)

cm.cox.uni=signif(cm.cox$uni,digits=3)
cm.cox.uni$`Hazard Ratio(95%CI)`=paste0(cm.cox.uni$HR,"(",cm.cox.uni$`Low 95%CI`, "-", cm.cox.uni$`High 95%CI`, ")")

table(cm.cox.uni$p.value<0.05)
cm.cox.uni[which(cm.cox.uni$p.value<0.05),]

####### 单因素显著的
colnames(all.t.cli.forCox)[c(11:16,21)]
cm.cox2=mg_compare_uni_muti_cox_use(all.t.cli.forCox[,c(11:16,21)],
                                    os=all.t.cli.forCox$OS.time,
                                    event = all.t.cli.forCox$OS)
cm.cox.muti=signif(cm.cox2$muti,digits=3)
cm.cox.muti
cm.cox.muti$`Hazard Ratio(95%CI)`=paste0(cm.cox.muti$HR,"(",cm.cox.muti$`Low 95%CI`, "-", cm.cox.muti$`High 95%CI`, ")")

# writeMatrix(cm.cox.uni,outpath = 'files/tcga.cli.single.cox.txt')
# writeMatrix(cm.cox.muti,outpath = 'files/tcga.cli.multi.cox.txt')

cm.cox.uni=getForestplotData(cm.cox$uni)
cm.cox.muti=getForestplotData(cm.cox2$muti)

pdf('10_nomogram/Fig12E.pdf',height = 6,width = 8,onefile = F)
tabletext=cm.cox.uni[[1]]
cochrane_from_rmeta=cm.cox.uni[[2]]
forestplot(tabletext, 
           mean=cochrane_from_rmeta$mean,
           lower=cochrane_from_rmeta$lower,
           upper=cochrane_from_rmeta$upper,
           graph.pos=2,
           hrzl_lines=list('2'=gpar(lty=1,col="black"),
                           '9'=gpar(lty=1,col="black")),
           zero = 1, 
           xlog=T,
           fn.ci_norm = fpDrawDiamondCI,
           boxsize = 0.2, 
           col=fpColors(line = mycolor[1], 
                        box=mycolor[2], 
                        zero = mycolor[3], 
                        summary=mycolor[4]), 
           lty.ci = 7,  
           lwd.ci = 1,
           ci.vertices.height = 0.05, 
           lineheight = "auto", 
           xlab="Hazard ratio" )
dev.off()

pdf('10_nomogram/Fig12F.pdf',height = 6,width = 8,onefile = F)
tabletext=cm.cox.muti[[1]]
cochrane_from_rmeta=cm.cox.muti[[2]]
forestplot(tabletext, 
           mean=cochrane_from_rmeta$mean,
           lower=cochrane_from_rmeta$lower,
           upper=cochrane_from_rmeta$upper,
           graph.pos=2,
           hrzl_lines=list('2'=gpar(lty=1,col="black"),
                           '9'=gpar(lty=1,col="black")),
           zero = 1, 
           xlog=T,
           fn.ci_norm = fpDrawDiamondCI,
           boxsize = 0.2, 
           col=fpColors(line = mycolor[1], 
                        box=mycolor[2], 
                        zero = mycolor[3], 
                        summary=mycolor[4]), 
           lty.ci = 3,  
           lwd.ci = 1,
           ci.vertices.height = 0.05, 
           lineheight = "auto", 
           xlab="Hazard ratio" )

dev.off()

mg_plotDCA=function(status,fmlas,modelNames,data){
  set.seed(123)
  all.mod=list()
  for(i in 1:length(fmlas)){
    fmla <- as.formula(paste0("status~",fmlas[i]))
    model<-rmda::decision_curve(fmla,
                                data=data,
                                bootstraps=500)
    all.mod=c(all.mod,list(model))
  }
  rmda::plot_decision_curve(all.mod,
                            curve.names=modelNames,
                            # col=mg_colors[c(1,10:12,4,5,7,8)],
                            col=mg_colors[c(1,4,5,8)],
                            xlim=c(0,1),legend.position="topright",
                            lwd=1,
                            confidence.intervals=FALSE)
}


mg_nomogram=function(clinical_riskscore,os,status,title='Nomogram',quick=T){
  #clinical_riskscore=dat1[,3:5]
  #os=dat1[,1]
  #status=dat1[,2]
  #sum(is.na(norm.stat.al[,3]))
  norm.stat.al=data.frame(clinical_riskscore,time=os,status=status)
  norm.stat.al=as.data.frame(norm.stat.al)
  library(rms)
  env <- globalenv()
  env$MG_Grobal_DDSet <- rms::datadist(norm.stat.al) 
  options(datadist='MG_Grobal_DDSet')
  fmla <- as.formula(paste0("Surv(time, status) ~",paste0(colnames(clinical_riskscore),collapse = '+')))
  cox2 <- cph(fmla, data = norm.stat.al,surv = T,x = T,y = T)
  #summary(cox2)
  #surv=Survival(cox2)
  
  fp <- predict(cox2)
  cindex=getC_index(fp,norm.stat.al$time,norm.stat.al$status)
  #cindex.orig=1-rcorr.cens(fp,Surv(norm.stat.al$time,norm.stat.al$status))
  cut.time=c()
  if(quantile(os[!is.na(os)])['75%']<12){
    cut.time=c(1,3,5)
  }else if(quantile(os[!is.na(os)])['75%']<365){
    cut.time=c(12*1,12*3,12*5)
  }else{
    cut.time=c(365*1,365*3,5*365)
  }
  cut.time=cut.time[which(cut.time<quantile(os,seq(0,1,0.01))['95%'])]
  print(cut.time)
  #regplot(cox2)
  #  print(regplot(cox3#对观测2的六个指标在列线图上进行计分展示
  #,observation=pbc[2,] #也可以不展示
  #预测3年和5年的死亡风险，此处单位是day
  #              ,title=title
  #              ,failtime = cut.time
  #              ,prfail = TRUE #cox回归中需要TRUE
  #              ,showP = T #是否展示统计学差异
  #              ,droplines = F#观测2示例计分是否画线
  #,colors = mg_colors[1:3] #用前面自己定义的颜色
  #,rank="decreasing") #根据统计学差异的显著性进行变量的排序
  #,interval="confidence"
  #,rank="decreasing"
  #,clickable=T
  #              ,points=TRUE)) #展示观测的可信区间
  
  #  plot(nom)
  surv=Survival(cox2)
  survs=list()
  cal_all=list()
  for(i in 1:length(cut.time)){
    f1<-cph(formula = fmla,data=norm.stat.al,x=T,y=T,surv = T,na.action=na.delete,time.inc = cut.time[i]) 
    cal1<-calibrate(f1, cmethod="KM", method="boot",u=cut.time[i],m=floor(sum(f1$n)/3)) 
    cal_all=c(cal_all,list(cal1))
    #    surv0 <- function(x)surv(cut.time[i],lp=x) 
    #    survs=c(survs,list(surv0))
  }
  #f2<-cph(formula = fmla,data=norm.stat.al,x=T,y=T,surv = T,na.action=na.delete,time.inc = cut.time[2]) 
  #cal3<-calibrate(f2, cmethod="KM", method="boot",u=cut.time[2],m=100,B=200) 
  #f3<-cph(formula = fmla,data=norm.stat.al,x=T,y=T,surv = T,na.action=na.delete,time.inc = cut.time[3]) 
  #cal5<-calibrate(f3, cmethod="KM", method="boot",u=cut.time[3],m=100,B=200) 
  if(length(cut.time)==1){
    surv1 <- function(x)surv(cut.time[1],lp=x) 
    survs=list(surv1)
  }else if(length(cut.time)==2){
    surv1 <- function(x)surv(cut.time[1],lp=x) 
    surv2 <- function(x)surv(cut.time[2],lp=x) 
    survs=list(surv1,surv2)
  }else if(length(cut.time)==3){
    surv1 <- function(x)surv(cut.time[1],lp=x) 
    surv2 <- function(x)surv(cut.time[2],lp=x) 
    surv3 <- function(x)surv(cut.time[3],lp=x) 
    survs=list(surv1,surv2,surv3)
  }
  nom=nomogram(cox2,fun=survs,lp= F
               ,funlabel=c('1-Year Survival','3-Year survival'
                           ,'5-Year survival')[1:length(cut.time)]
               ,maxscale=100
               ,fun.at=seq(0,1,0.1)
  )
  
  if(!quick){
    cal_all=list()
    for(i in 1:length(cut.time)){
      cal1=get_best_calibrate(cox2,cut.time[i])
      cal_all=c(cal_all,list(cal1))
    }
    #cal3=get_best_calibrate(cox2,cut.time[2])
    #cal5=get_best_calibrate(cox2,cut.time[3])
  }
  lay2 <- customLayout::lay_new(matrix(1:2))
  lay1 <- customLayout::lay_new(matrix(1:1))
  cl <- customLayout::lay_bind_col(lay2, lay1, widths = c(1, 1.5)) 
  #customLayout::lay_show(cl)
  customLayout::lay_set(cl) 
  
  plot(cal_all[[1]],lwd = 2,lty = 1,errbar.col = mg_colors[1], bty = "l",xlim = c(0,1),ylim= c(0,1),xlab = "Nomogram-prediced OS (%)"
       ,ylab = "Observed OS (%)",col = mg_colors[1],cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6,subtitles = F)
  #lines(cal_all[[1]][,c('mean.predicted',"KM")],type = 'b', lwd = 1, col = mg_colors[1], pch = 16)
  mtext("")
  if(length(cal_all)>1){
    for(i in 2:length(cal_all)){
      plot(cal_all[[i]],lwd = 2,lty = 1,errbar.col = mg_colors[i],xlim = c(0,1),ylim= c(0,1),col = mg_colors[i],add = T)
      #lines(cal_all[[i]][,c('mean.predicted',"KM")],type = 'b', lwd = 1, col = mg_colors[i], pch = 16)
    }
    #plot(cal3,lwd = 2,lty = 0,errbar.col = mg_colors[3],xlim = c(0,1),ylim= c(0,1),col = mg_colors[3],add = T)
    #lines(cal3[,c('mean.predicted',"KM")],type = 'b', lwd = 1, col = mg_colors[3], pch = 16)
  }
  abline(0,1, lwd = 2, lty = 3, col = 'black')
  legend("topleft", legend = c("1-year","3-year",'5-year')[1:length(cut.time)],col =mg_colors[1:length(cut.time)],lwd = 2,cex = 1.2,bty = "n")
  
  fp <- predict(cox2)
  cindex=getC_index(fp,norm.stat.al$time,norm.stat.al$status)
  dca_dat=data.frame(Nomogram=fp,status=norm.stat.al$status)
  fp.al=cbind(fp)
  for(i in 1:ncol(clinical_riskscore)){
    fmla1 <- as.formula(paste0("Surv(time, status) ~",colnames(clinical_riskscore)[i]))
    cox1 <- cph(fmla1, data = norm.stat.al,surv = T,x = T,y = T)
    fp1 <- predict(cox1)
    fp.al=cbind(fp.al,fp1)
  }
  colnames(fp.al)=c('Nomogram',colnames(clinical_riskscore))
  fp.al=crbind2DataFrame(fp.al)
  fp.al$status=norm.stat.al$status
  mg_plotDCA(fp.al$status
             ,c('Nomogram',colnames(clinical_riskscore))
             ,c('Nomogram',colnames(clinical_riskscore)),fp.al)
  #plot(cal1,xlim=c(0,1),ylim=c(0,1))
  #plot(cal3,xlim=c(0,1),ylim=c(0,1))
  #plot(cal5,xlim=c(0,1),ylim=c(0,1))
  plot(nom)
  options(datadist=NULL)
  return(list(Mod=cox2,Cindex=cindex,CutTime=cut.time))
}




pdf('10_nomogram/nomogram.pdf', width = 12 , height = 10)
nom.plot=mg_nomogram(data.frame(T.Stage = all.t.cli.forCox$T.Stage,
                                RiskScore = all.t.cli.forCox$RiskScore),
                     os = all.t.cli.forCox$OS.time,
                     status = all.t.cli.forCox$OS)
dev.off()
table(all.t.cli.forCox$OS.time>365*3)
mg_nomogram_buti(nom.plot$Mod,cut.time = c(1*365, 3*365 ,5*365))



# 泛癌 分析 ###################
dir.create('11_pancan')
library(data.table)
library(stringr)

tcga_pancan_cli <- read.delim('00_origin_datas/PMC6066282-TCGA-CDR-clinical.txt',
                              header = T, check.names = F, stringsAsFactors = F)
tcga_pancan_cli <- tcga_pancan_cli[!duplicated(tcga_pancan_cli$bcr_patient_barcode), ]
colnames(tcga_pancan_cli)
tcga_pancan_cli <- subset(tcga_pancan_cli,
                          !is.na(OS.time) &
                            OS.time > 0 & 
                            !is.na(OS))
tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type == 'LAML'] <- paste0(tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type == 'LAML'], '-03')
tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type != 'LAML'] <- paste0(tcga_pancan_cli$bcr_patient_barcode[tcga_pancan_cli$type != 'LAML'], '-01')
rownames(tcga_pancan_cli) <- tcga_pancan_cli$bcr_patient_barcode
colnames(tcga_pancan_cli)[1] <- 'Samples'
tcga_types <- as.character(unique(tcga_pancan_cli$type))


tcga_pancan_exp <- fread(paste0('00_origin_datas/TCGA/TCGA-', "ACC", '-Symbol.txt'),
                         data.table = F)
class(tcga_pancan_exp)
dim(tcga_pancan_exp)
tcga_pancan_exp[1:5, 1:5]

tcga_groups <- data.frame(row.names = colnames(tcga_pancan_exp)[2:ncol(tcga_pancan_exp)],
                          Samples = colnames(tcga_pancan_exp)[2:ncol(tcga_pancan_exp)],
                          Type = 'ACC',
                          stringsAsFactors = F)

for (ty in tcga_types[2:33]) {
  print(ty)
  tmp <- fread(paste0('00_origin_datas/TCGA/TCGA-', ty, '-Symbol.txt'),
               data.table = F)
  colnames(tmp)[1] <- 'Tags'
  
  tmp_groups <- data.frame(row.names = colnames(tmp)[2:ncol(tmp)],
                           Samples = colnames(tmp)[2:ncol(tmp)],
                           Type = ty,
                           stringsAsFactors = F)
  tcga_pancan_exp <- merge(tcga_pancan_exp, tmp, by = 'Tags', all= T)
  tcga_groups <- rbind(tcga_groups, tmp_groups)
}

tcga_groups$Type1 <- as.numeric(substr(tcga_groups$Samples, 14, 16))
table(tcga_groups$Type1)
tcga_groups <- subset(tcga_groups,
                      Type1 == 1 | 
                        Type1 == 3 |
                        Type1 == 6 |
                        Type1 == 11)
tcga_groups$Type1 <- ifelse(tcga_groups$Type1 < 10, 'Tumor', 'Adjacent')
tcga_groups$Sample <- substr(tcga_groups$Samples, 1, 12)


tcga_pancan_exp[1:5, 1:3]
rownames(tcga_pancan_exp) <- tcga_pancan_exp$Tags
tcga_pancan_exp <- tcga_pancan_exp[, -1]
boxplot(tcga_pancan_exp[, 1:10])
tcga_pancan_exp[1:5, 1:3]
tcga_pancan_exp_log2 <- log2(tcga_pancan_exp + 1)
tcga_pancan_exp_log2 <- tcga_pancan_exp_log2[rowSums(tcga_pancan_exp_log2) > 0, ]
dim(tcga_pancan_exp_log2)
tcga_pancan_exp_log2[1:5, 1:3]
boxplot(tcga_pancan_exp_log2[, 1:5])


intersect(genes, rownames(tcga_pancan_exp_log2))


table(tcga_pancan_cli$type)

tcga_types1 <- tcga_types[which(tcga_types != 'LUAD')]
colnames(tcga_pancan_cli)

tcga_pancan_km_list <- list()
tcga_pancan_roc_list <- list()
for (ty in tcga_types1) {
  print(ty)
  if (ty == 'GBM' | ty == "SKCM") {
    mks <- c(1,2,3)
  } else {
    mks <- c(1,2,3)
  }
  tmp.cli <- tcga_pancan_cli[tcga_pancan_cli$type == ty, ]
  tmp.samples <- intersect(tmp.cli$Samples, colnames(tcga_pancan_exp_log2))
  tmp_model_dat <- cbind(tmp.cli[tmp.samples, c("OS.time", "OS", "PFI.time", "PFI",
                                                "DFI.time", "DFI", "DSS.time", "DSS")],
                         t(tcga_pancan_exp_log2[genes, tmp.samples]))
  
  fmla.tmp <- as.formula(paste0("Surv(OS.time, OS) ~"
                                ,paste0(genes,collapse = '+')))
  cox.tmp <- coxph(fmla.tmp, data =as.data.frame(tmp_model_dat))
  tmp_lan <- coef(cox.tmp)
  
  risk.tmp=as.numeric(tmp_lan%*%as.matrix(t(tmp_model_dat[,names(tmp_lan)])))
  
  library(survminer)
  # risk.tmp=as.numeric(lan%*%as.matrix(t(tmp_model_dat[,names(lan)])))
  
  tmp_model_dat$RiskScore <- risk.tmp
  tmp.data.point <- surv_cutpoint(tmp_model_dat, time = "OS.time", event = "OS",
                                  variables = 'RiskScore')
  tmp.cutoff <- as.numeric(summary(tmp.data.point)[1])
  tmp_model_dat$Group <- ifelse(risk.tmp >= tmp.cutoff, 'High', 'Low')
  write.csv(tmp_model_dat,
            file = paste0('11_pancan/', ty, '_model_dat.csv'),
            quote = F)
  
  tmp.roc <- ggplotTimeROC(tmp_model_dat$OS.time / 365,
                           tmp_model_dat$OS,
                           risk.tmp,
                           mks = mks)
  tcga_pancan_roc_list[[ty]] <- tmp.roc
  tmp.km <- ggplotKMCox(data.frame(tmp_model_dat$OS.time / 365,
                                   tmp_model_dat$OS,
                                   ifelse(risk.tmp>=tmp.cutoff,'High','Low')),
                        title = ty,
                        labs = c('High','Low'))
  tcga_pancan_km_list[[ty]] <- tmp.km
}

tcga_pamcan_km_plot <- cowplot::plot_grid(plotlist = tcga_pancan_km_list,
                                          ncol = 6)

ggsave(plot = tcga_pamcan_km_plot,
       filename = '11_pancan/tcga_pamcan_km_plot.pdf',
       width = 30, height = 30)

tcga_pamcan_roc_plot <- cowplot::plot_grid(plotlist = tcga_pancan_roc_list,
                                           ncol = 6)
ggsave(plot = tcga_pamcan_roc_plot,
       filename = '11_pancan/tcga_pamcan_roc_plot.pdf',
       width = 30, height = 30)


tmp.cli <- read.csv('11_pancan/BLCA_model_dat.csv',
                    row.names = 1, stringsAsFactors = F)

groups <- list()
groups[['OS']] <- c("OS.time", "OS")
groups[['PFI']] <- c("PFI.time", "PFI")
groups[['DFI']] <- c("DFI.time", "DFI")
groups[['DSS']] <- c("DSS.time", "DSS")
groups
names(groups)


wb_get_KM_P_HR <- function(dat, features, type) {
  groups <- c()
  HRs <- c()
  low95s <- c()
  up95s <- c()
  p.vals <- c()
  for (gr in names(features)) {
    nas <- features[[gr]]
    
    dat1 <- dat[, c(nas, 'Group')]
    colnames(dat1) <- c('time','status','groups')
    dat1 <- na.omit(dat1)
    if (nrow(dat1) > 0) {
      if (length(unique(dat1$groups)) > 1) {
        sdf <- survdiff(Surv(time, status) ~ groups, data = dat1)
        p.val = 1 - pchisq(sdf$chisq, length(sdf$n) - 1)
        HR = (sdf$obs[1] / sdf$exp[1]) / (sdf$obs[2] / sdf$exp[2]) 
        up95 = exp(log(HR) + qnorm(0.975) * sqrt(1 / sdf$exp[2] + 1 / sdf$exp[1]))
        low95 = exp(log(HR) - qnorm(0.975) * sqrt(1 / sdf$exp[2]+ 1 / sdf$exp[1]))
        
        groups <- c(groups, gr)
        HRs <- c(HRs, HR)
        low95s <- c(low95s, low95)
        up95s <- c(up95s, up95)
        p.vals <- c(p.vals, p.val)
      }
    }
  }
  return(data.frame(Type = type,
                    groups = groups,
                    HR = HRs,
                    low95 = low95s,
                    up95 = up95s,
                    p.val = p.vals))
}


# tcga_km_res <- wb_get_KM_P_HR(dat = tmp.cli,
#                               features = groups,
#                               type = 'BLCA')

tcga_km_res <- data.frame()
for (ty in tcga_types1) {
  print(ty)
  tmp.cli <- read.csv(paste0('11_pancan/', ty, '_model_dat.csv'),
                      row.names = 1, stringsAsFactors = F)
  
  tmp <- wb_get_KM_P_HR(dat = tmp.cli,
                        features = groups,
                        type = ty)
  
  tcga_km_res <- rbind(tcga_km_res, tmp)
}

head(tcga_km_res)

tcga_km_res$HR <- round(tcga_km_res$HR, 2)
tcga_km_res$Sign <- ifelse(tcga_km_res$p.val < 0.0001, '****', 
                           ifelse(tcga_km_res$p.val < 0.001, '***', 
                                  ifelse(tcga_km_res$p.val < 0.01, '**',
                                         ifelse(tcga_km_res$p.val < 0.05, '*', 'ns'))))

tcga_km_res$Sign <- paste0(tcga_km_res$HR, ' (', tcga_km_res$Sign, ')')

tcga_km_res_HR <- reshape2::dcast(data = tcga_km_res[, c("Type", "groups", "HR")], Type ~ groups)
rownames(tcga_km_res_HR) <- tcga_km_res_HR$Type
tcga_km_res_HR <- tcga_km_res_HR[, -1]

tcga_km_res_P <- reshape2::dcast(data = tcga_km_res[, c("Type", "groups", "Sign")], Type ~ groups)
rownames(tcga_km_res_P) <- tcga_km_res_P$Type
tcga_km_res_P <- tcga_km_res_P[, -1]


library(ComplexHeatmap)

tcga_types1_colors <- list()
for (i in 1:length(tcga_types1)) {
  ty <- tcga_types1[i]
  print(ty)
  tcga_types1_colors[[ty]] <- mg_colors[i]
}


tcga_types1_colors <- mg_colors[1:32]
names(tcga_types1_colors) <- tcga_types1
library(ComplexHeatmap)
?Heatmap
pdf('11_pancan/KM_P_plot.pdf', width = 6, height = 10)
KM_P_plot <- Heatmap(tcga_km_res_HR, 
                     name = "HR", 
                     col = circlize::colorRamp2(c(0, 1, 2, 5, 10),
                                                c(mycolor[3], 'white', mycolor[2], mycolor[4], mycolor[5])),
                     border = T,
                     na_col = 'grey',
                     show_column_names = F,
                     show_column_dend = F,
                     show_row_names = F,
                     show_row_dend = F,
                     cluster_columns=F,
                     cluster_rows=F,
                     column_names_side = 'bottom',
                     column_names_rot = 0,
                     row_names_side = 'right',
                     row_split = factor(rownames(tcga_km_res_HR)),
                     column_split = factor(colnames(tcga_km_res_HR)),
                     heatmap_legend_param = list(
                       at = c(0, 1, 2, 5, 10),
                       labels = c(0, 1, 2, 5, 10),
                       title = "HR"
                     ),
                     top_annotation = HeatmapAnnotation(Type = colnames(tcga_km_res_HR)
                                                        , col=list(Type=c('OS'=mycolor[1],
                                                                          'PFI' = mycolor[2],
                                                                          'DFI'=mycolor[3],
                                                                          'DSS'=mycolor[4]))
                                                        , annotation_width = unit(c(1,2), 'cm')
                                                        , annotation_height = unit(0.2, "cm")
                                                        , gap = unit(1, 'mm')),
                     row_title_rot = 0,
                     left_annotation = rowAnnotation(Cancers = rownames(tcga_km_res_HR)
                                                     , col=list(Cancers=tcga_types1_colors)
                                                     , annotation_width = unit(c(1,2), 'cm')
                                                     ,simple_anno_size = unit(1.5, "cm")
                                                     , annotation_height = unit(0.2, "cm")
                                                     , gap = unit(1, 'mm')),
                     rect_gp = gpar(col = NA, lwd = 1),
                     row_names_gp = gpar(fontsize = 12),
                     cell_fun = function(j, i, x, y, w, h, col) {
                       grid.text(tcga_km_res_P[i, j], x, y)
                     }
)
KM_P_plot
dev.off()

col_fun_prop = circlize::colorRamp2(c(0, 1, 2, 5, 10), 
                                    c(mycolor[3], 'white', mycolor[2], mycolor[4], mycolor[5]))
lgd = Legend(col_fun = col_fun_prop, title = "HR",
             at = c(0, 1, 2, 5, 10))
draw(lgd)









# 免疫治疗数据集的使用 #################
# TIDE_immune ################
dir.create('12_immune_data')

# IMvigor210
IMvigor210_cli <- read.delim('00_origin_datas/IMvigor210/IMvigor210_cli.txt', header = T)
IMvigor210_cli <- IMvigor210_cli[, c("os", "censOS",
                                     "Best.Confirmed.Overall.Response",
                                     "IC.Level", "TC.Level", "Immune.phenotype",
                                     "FMOne.mutation.burden.per.MB",
                                     "Neoantigen.burden.per.MB")]
colnames(IMvigor210_cli) <- c('OS.time', 'OS', 'Response', 
                              "IC.Level", "TC.Level", "Immune.phenotype",
                              'TMB', 'NEO')
table(IMvigor210_cli$Response)
# IMvigor210_cli <- IMvigor210_cli[IMvigor210_cli$Response != 'NE', ]


IMvigor210_anno <- read.delim('00_origin_datas/IMvigor210/IMvigor210_entrez2gene.txt', header = T)
IMvigor210_exp <- read.delim('00_origin_datas/IMvigor210/IMvigor210_Counts2TPM.txt', 
                             header = T, row.names = 1)
com_enid <- intersect(IMvigor210_anno$entrez_id, rownames(IMvigor210_exp))

IMvigor210_exp <- IMvigor210_exp[com_enid, ]
IMvigor210_anno <- IMvigor210_anno[com_enid, ]


IMvigor210_exp <- log2(IMvigor210_exp + 1)
IMvigor210_exp$genes <- IMvigor210_anno$symbol

IMvigor210_exp <- aggregate(.~genes,
                            data = IMvigor210_exp, 
                            FUN = median)
rownames(IMvigor210_exp) <- IMvigor210_exp$genes
IMvigor210_exp <- IMvigor210_exp[-1, -1]
colnames(IMvigor210_exp)
boxplot(IMvigor210_exp[, 1:5])
IMvigor210_exp <- IMvigor210_exp[, -349]
# IMvigor210_exp <- log2(IMvigor210_exp + 1)

intersect(rownames(IMvigor210_cli), colnames(IMvigor210_exp))




IMvigor210_model_data <- cbind(IMvigor210_cli[, c("OS.time", "OS")],
                               t(IMvigor210_exp[, rownames(IMvigor210_cli)]))

IMvigor210.genes <- intersect(genes, colnames(IMvigor210_model_data))
IMvigor210.genes

IMvigor210_cli1 <- IMvigor210_cli[IMvigor210_cli$Response != 'NE', ]
colnames(IMvigor210_cli1)
IMvigor210_model_data <- IMvigor210_model_data[rownames(IMvigor210_cli1), ]
fmla.IMvigor210 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                     ,paste0(IMvigor210.genes,collapse = '+')))
cox.IMvigor210 <- coxph(fmla.IMvigor210, data =as.data.frame(IMvigor210_model_data))
IMvigor210_lan <- coef(cox.IMvigor210)

risk.IMvigor210=as.numeric(IMvigor210_lan%*%as.matrix(t(IMvigor210_model_data[,names(IMvigor210_lan)])))
# risk.IMvigor210=as.numeric(lan%*%as.matrix(t(IMvigor210_model_data[,names(lan)])))

IMvigor210_model_data$RS <- risk.IMvigor210
IMvigor210.data.point <- surv_cutpoint(IMvigor210_model_data, time = "OS.time", event = "OS",
                                       variables = 'RS')
IMvigor210.cutoff <- as.numeric(summary(IMvigor210.data.point)[1])
IMvigor210.cutoff

library(survival)
IMvigor210.roc <- ggplotTimeROC(IMvigor210_model_data$OS.time / 365,
                                IMvigor210_model_data$OS,
                                risk.IMvigor210,
                                mks = c(0.5,1,1.5))
IMvigor210.km <- ggplotKMCox(data.frame(IMvigor210_model_data$OS.time / 365,
                                        IMvigor210_model_data$OS,
                                        ifelse(risk.IMvigor210>=IMvigor210.cutoff,'High','Low')),
                             title = 'IMvigor210',
                             labs = c('High','Low'))

IMvigor210.roc.km <- cowplot::plot_grid(IMvigor210.km, 
                                        IMvigor210.roc,
                                        ncol = 2,
                                        # align = 'v',
                                        labels = LETTERS[1])
IMvigor210.roc.km
# ggsave(plot = IMvigor210.roc.km,
#        filename = '12_immune_data/IMvigor210/IMvigor210.RiskScore.roc.km.pdf',
#        width = 10, height = 5)

IMvigor210_cli1 <- crbind2DataFrame(IMvigor210_cli1)
IMvigor210_cli1 <- IMvigor210_cli1[rownames(IMvigor210_model_data), ]
IMvigor210_cli1$RiskScore <- risk.IMvigor210
IMvigor210_cli1$RiskType <- ifelse(IMvigor210_cli1$RiskScore > IMvigor210.cutoff, 'High', 'Low')

IMvigor210_bar <- table(IMvigor210_cli1$RiskType, IMvigor210_cli1$Response)
IMvigor210_bar
IMvigor210_bar_plot <- plotMutiBar(IMvigor210_bar,ist = T, legTitle = 'Response', showValue = T)
IMvigor210_bar_plot

IMvigor210_bar_plot <- ggbarstats(
  data             = IMvigor210_cli1,
  x                = Response,
  y                = RiskType,
  bf.message = F,
  title            = "IMvigor210",
  xlab             = "RiskType",
  legend.title     = "Response",
  package = "ggsci",
  palette = "default_jco"
)
IMvigor210_bar_plot

colnames(IMvigor210_cli1)



IMvigor210_rs_plot <- mg_violin(data.frame(IMvigor210_cli1$Response,
                                           IMvigor210_cli1$RiskScore), 
                                melt=TRUE, 
                                leg.title='', 
                                jitter = F,
                                xlab = 'Response',
                                ylab = 'RiskScore',
                                legend.pos='tl',
                                show_compare = T)
IMvigor210_rs_plot

IMvigor210.km.bar <- cowplot::plot_grid(IMvigor210.km, 
                                        IMvigor210_bar_plot,
                                        rel_widths = c(1,1.5),
                                        ncol = 2)

IMvigor210.km.bar
IMvigor210.roc.km.bar <- cowplot::plot_grid(IMvigor210.km, 
                                            IMvigor210.roc,
                                            IMvigor210_bar_plot,
                                            # IMvigor210_rs_plot,
                                            rel_widths = c(1,1,1.5),
                                            ncol = 3)
IMvigor210.roc.km.bar

# GSE91061 
GSE91061_cli <- read.delim('00_origin_datas/GSE91061/GSE91061.cli.txt',
                           row.names = 1, check.names = F, stringsAsFactors = F)
GSE91061_cli$Title <- str_split_fixed(GSE91061_cli$Title, '_', 2)[, 1]
GSE91061_cli$Samples <- rownames(GSE91061_cli)
table(GSE91061_cli$`visit (pre or on treatment)`)
GSE91061_cli <- GSE91061_cli[GSE91061_cli$`visit (pre or on treatment)` == 'Pre', ]
rownames(GSE91061_cli) <- GSE91061_cli$Title
GSE91061_cli <- GSE91061_cli[!duplicated(GSE91061_cli$Title), ]

GSE91061_cli1 <- read.delim('00_origin_datas/GSE91061/GSE91061.cli1.txt',
                            row.names = 1, check.names = F)

GSE91061_com_samples <- intersect(GSE91061_cli$Title, rownames(GSE91061_cli1))

GSE91061_cli1 <- GSE91061_cli1[GSE91061_com_samples, ]
GSE91061_cli <- GSE91061_cli[GSE91061_com_samples, ]
rownames(GSE91061_cli1) <- GSE91061_cli$Samples
colnames(GSE91061_cli1)[3:4] <- c('OS', 'OS.time')
table(GSE91061_cli1$OS)
GSE91061_cli1$OS <- ifelse(GSE91061_cli1$OS == 'TRUE', 1, 0)
GSE91061_cli1$OS.time <- as.numeric(GSE91061_cli1$OS.time) * 7

GSE91061_exp <- read.delim('00_origin_datas/GSE91061/GSE91061.exp.txt',
                           row.names = 1, check.names = F, stringsAsFactors = F)
GSE91061_exp <- GSE91061_exp[, rownames(GSE91061_cli1)]
intersect(colnames(GSE91061_exp), rownames(GSE91061_cli1))

boxplot(GSE91061_exp[, 1:5])
GSE91061_exp <- log2(GSE91061_exp + 1)


GSE91061_model_data <- cbind(GSE91061_cli1[, c("OS.time", 'OS')],
                             t(GSE91061_exp[, rownames(GSE91061_cli1)]))
GSE91061.genes <- intersect(genes, colnames(GSE91061_model_data))
GSE91061.genes

fmla.GSE91061 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                   ,paste0(GSE91061.genes,collapse = '+')))
cox.GSE91061 <- coxph(fmla.GSE91061, data =as.data.frame(GSE91061_model_data))
GSE91061_lan <- coef(cox.GSE91061)

risk.GSE91061=as.numeric(GSE91061_lan%*%as.matrix(t(GSE91061_model_data[,names(GSE91061_lan)])))

GSE91061_model_data$RS <- risk.GSE91061
GSE91061.data.point <- surv_cutpoint(GSE91061_model_data, time = "OS.time", event = "OS",
                                     variables = 'RS')
GSE91061.cutoff <- as.numeric(summary(GSE91061.data.point)[1])
GSE91061.cutoff



GSE91061.roc <- ggplotTimeROC(GSE91061_model_data$OS.time / 365,
                              GSE91061_model_data$OS,
                              risk.GSE91061,
                              mks = c(1,2,2.5))
GSE91061.km <- ggplotKMCox(data.frame(GSE91061_model_data$OS.time / 365,
                                      GSE91061_model_data$OS,
                                      ifelse(risk.GSE91061>=GSE91061.cutoff,'High','Low')),
                           title = 'GSE91061',
                           labs = c('High','Low'))

GSE91061.roc.km <- cowplot::plot_grid(GSE91061.km, 
                                      GSE91061.roc,
                                      ncol = 2,
                                      # align = 'v',
                                      labels = LETTERS[1:2])
GSE91061.roc.km



GSE91061_cli1 <- crbind2DataFrame(GSE91061_cli1)
GSE91061_cli1 <- GSE91061_cli1[rownames(GSE91061_model_data), ]
GSE91061_cli1$RiskScore <- risk.GSE91061
GSE91061_cli1$RiskType <- ifelse(GSE91061_cli1$RiskScore > GSE91061.cutoff, 'High', 'Low')

GSE91061_bar <- table(GSE91061_cli1$RiskType, GSE91061_cli1$Response)
GSE91061_bar
GSE91061_bar_plot <- plotMutiBar(GSE91061_bar,ist = T, legTitle = 'Response', showValue = T)
GSE91061_bar_plot


GSE91061_rs_plot <- mg_violin(data.frame(GSE91061_cli1$Response,
                                         GSE91061_cli1$RiskScore), 
                              melt=TRUE, 
                              leg.title='', 
                              jitter = F,
                              xlab = 'Response',
                              ylab = 'RiskScore',
                              legend.pos='tl',
                              show_compare = T)
GSE91061_rs_plot



GSE91061_bar_plot <- ggbarstats(
  data             = GSE91061_cli1,
  x                = Response,
  y                = RiskType,
  bf.message = F,
  title            = "GSE91061",
  xlab             = "RiskType",
  legend.title     = "Response",
  package = "ggsci",
  palette = "default_jco"
)
GSE91061_bar_plot

GSE91061.km.bar <- cowplot::plot_grid(GSE91061.km, 
                                      GSE91061_bar_plot,
                                      rel_widths = c(1,1.5),
                                      ncol = 2)
GSE91061.km.bar
GSE91061.roc.km.bar <- cowplot::plot_grid(GSE91061.km, 
                                          GSE91061.roc,
                                          GSE91061_bar_plot,
                                          # GSE91061_rs_plot,
                                          rel_widths = c(1,1,1.5),
                                          ncol = 3)
GSE91061.roc.km.bar


# GSE78220
GSE78220_cli <- getGEOSampleData('GSE78220')
GSE78220_cli1 <- GSE78220_cli[, c("Acc", "Title", "anti-pd-1 response",
                                  "overall survival (days)", "vital status")]
colnames(GSE78220_cli1) <- c(c("Samples", "Title", "Rresponse",
                               "OS.time", "OS"))
GSE78220_cli1 <- na.omit(GSE78220_cli1)
table(GSE78220_cli1$OS)
GSE78220_cli1$OS <- ifelse(GSE78220_cli1$OS == 'Alive', 0, 1)
rownames(GSE78220_cli1) <- GSE78220_cli1$Title

GSE78220_exp <- openxlsx::read.xlsx('00_origin_datas/GSE78220/GSE78220_PatientFPKM.xlsx',
                                    sheet = 1)
rownames(GSE78220_exp) <- GSE78220_exp$Gene
GSE78220_exp <- GSE78220_exp[, -1]
colnames(GSE78220_exp) <- str_split_fixed(colnames(GSE78220_exp), '\\.', 3)[, 1]
boxplot(GSE78220_exp[, 1:5])

GSE78220_exp <- log2(GSE78220_exp + 1)
boxplot(GSE78220_exp[, 1:5])

GSE78220_model_data <- cbind(GSE78220_cli1[, c("OS.time", 'OS')],
                             t(GSE78220_exp[, rownames(GSE78220_cli1)]))
GSE78220.genes <- intersect(genes, colnames(GSE78220_model_data))


fmla.GSE78220 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                   ,paste0(GSE78220.genes,collapse = '+')))
cox.GSE78220 <- coxph(fmla.GSE78220, data =as.data.frame(GSE78220_model_data))
GSE78220_lan <- coef(cox.GSE78220)

risk.GSE78220=as.numeric(GSE78220_lan%*%as.matrix(t(GSE78220_model_data[,names(GSE78220_lan)])))

GSE78220_model_data$RS <- risk.GSE78220
GSE78220.data.point <- surv_cutpoint(GSE78220_model_data, time = "OS.time", event = "OS",
                                     variables = 'RS')
GSE78220.cutoff <- as.numeric(summary(GSE78220.data.point)[1])
GSE78220.cutoff


GSE78220.roc <- ggplotTimeROC(GSE78220_model_data$OS.time / 365,
                              GSE78220_model_data$OS,
                              risk.GSE78220,
                              mks = c(1,2,2.5))
GSE78220.km <- ggplotKMCox(data.frame(GSE78220_model_data$OS.time / 365,
                                      GSE78220_model_data$OS,
                                      ifelse(risk.GSE78220>=GSE78220.cutoff,'High','Low')),
                           title = 'GSE78220',
                           labs = c('High','Low'))

GSE78220.roc.km <- cowplot::plot_grid(GSE78220.km,
                                      GSE78220.roc,
                                      ncol = 2,
                                      # align = 'v',
                                      labels = LETTERS[1:2])
GSE78220.roc.km

GSE78220_cli1 <- crbind2DataFrame(GSE78220_cli1)
GSE78220_cli1 <- GSE78220_cli1[rownames(GSE78220_model_data), ]
GSE78220_cli1$RiskScore <- risk.GSE78220
GSE78220_cli1$RiskType <- ifelse(GSE78220_cli1$RiskScore > GSE78220.cutoff, 'High', 'Low')

GSE78220_cli1$Response <- GSE78220_cli1$Rresponse
table(GSE78220_cli1$Response)
GSE78220_cli1$Response[GSE78220_cli1$Response == 'Complete Response'] <- 'CR'
GSE78220_cli1$Response[GSE78220_cli1$Response == 'Partial Response'] <- 'PR'
GSE78220_cli1$Response[GSE78220_cli1$Response == 'Progressive Disease'] <- 'PD'

GSE78220_bar <- table(GSE78220_cli1$RiskType, GSE78220_cli1$Response)
GSE78220_bar
GSE78220_bar_plot <- plotMutiBar(GSE78220_bar,ist = T, legTitle = 'Response', showValue = T)
GSE78220_bar_plot

GSE78220_bar_plot <- ggbarstats(
  data             = GSE78220_cli1,
  x                = Response,
  y                = RiskType,
  bf.message = F,
  title            = "GSE78220",
  xlab             = "RiskType",
  legend.title     = "Response",
  package = "ggsci",
  palette = "default_jco"
)
GSE78220_bar_plot



GSE78220_rs_plot <- mg_violin(data.frame(GSE78220_cli1$Response,
                                         GSE78220_cli1$RiskScore), 
                              melt=TRUE, 
                              leg.title='', 
                              jitter = F,
                              xlab = 'Response',
                              ylab = 'RiskScore',
                              legend.pos='tl',
                              show_compare = T)
GSE78220_rs_plot

GSE78220.km.bar <- cowplot::plot_grid(GSE78220.km, 
                                      GSE78220_bar_plot,
                                      rel_widths = c(1,1.5),
                                      ncol = 2)
GSE78220.km.bar
GSE78220.roc.km.bar <- cowplot::plot_grid(GSE78220.km, 
                                          GSE78220.roc,
                                          GSE78220_bar_plot,
                                          # GSE78220_rs_plot,
                                          rel_widths = c(1,1,1.5),
                                          ncol = 3)
GSE78220.roc.km.bar


# GSE135222
GSE135222_cli <- getGEOSampleData('GSE135222')
GSE135222_cli1 <- GSE135222_cli[, c("Acc", "Title", "pfs.time", "progression-free survival (pfs)")]
colnames(GSE135222_cli1) <- c('Samples', "Title", 'OS.time', 'OS')

GSE135222_cli1 <- GSE135222_cli1[GSE135222_cli1$OS.time != 'NULL', ]
table(GSE135222_cli1$OS)
GSE135222_cli1$Title <- gsub(' ', '', GSE135222_cli1$Title)

GSE135222_cli2 <- openxlsx::read.xlsx('00_origin_datas/GSE135222/GSE135222_cli.xlsx', sheet = 1)
GSE135222_cli2$Patient.ID <- paste0('NSCLC', GSE135222_cli2$Patient.ID)
rownames(GSE135222_cli2) <- GSE135222_cli2$Patient.ID
intersect(GSE135222_cli2$Patient.ID, GSE135222_cli1$Title)

colnames(GSE135222_cli1)
colnames(GSE135222_cli2)

GSE135222_cli <- cbind(GSE135222_cli1,
                       GSE135222_cli2[GSE135222_cli1$Title, ])



GSE135222_exp <- read.delim('00_origin_datas/GSE135222/GSE135222_GEO_RNA-seq_omicslab_exp.tsv',
                            header = T, row.names = 1, check.names = F, stringsAsFactors = F)
rownames(GSE135222_exp) <- str_split_fixed(rownames(GSE135222_exp), '\\.', 2)[, 1]

GSE135222_exp <- exp_probe2symbol_v2(GSE135222_exp,
                                     gene_type[,c(1,2)])
GSE135222_exp <- log2(GSE135222_exp + 1)

boxplot(GSE135222_exp[, 1:2])

colnames(GSE135222_exp)
rownames(GSE135222_cli) <- GSE135222_cli$Title
GSE135222_model_data <- cbind(GSE135222_cli[, c("OS.time", "OS")],
                              t(GSE135222_exp[, GSE135222_cli$Title]))
rownames(GSE135222_model_data)

GSE135222.genes <- intersect(genes, colnames(GSE135222_model_data))
GSE135222.genes
fmla.GSE135222 <- as.formula(paste0("Surv(OS.time, OS) ~"
                                    ,paste0(GSE135222.genes,collapse = '+')))
cox.GSE135222 <- coxph(fmla.GSE135222, data =as.data.frame(GSE135222_model_data))
GSE135222_lan <- coef(cox.GSE135222)

risk.GSE135222=as.numeric(GSE135222_lan%*%as.matrix(t(GSE135222_model_data[,names(GSE135222_lan)])))

GSE135222_model_data$RS <- risk.GSE135222
GSE135222.data.point <- surv_cutpoint(GSE135222_model_data, time = "OS.time", event = "OS",
                                      variables = 'RS')
GSE135222.cutoff <- as.numeric(summary(GSE135222.data.point)[1])
GSE135222.cutoff



GSE135222.roc <- ggplotTimeROC(GSE135222_model_data$OS.time / 365,
                               GSE135222_model_data$OS,
                               risk.GSE135222,
                               mks = c(0.5, 1))
GSE135222.km <- ggplotKMCox(data.frame(GSE135222_model_data$OS.time / 365,
                                       GSE135222_model_data$OS,
                                       ifelse(risk.GSE135222>=GSE135222.cutoff,'High','Low')),
                            title = 'GSE135222',
                            labs = c('High','Low'))

GSE135222.roc.km <- cowplot::plot_grid(GSE135222.km,
                                       GSE135222.roc,
                                       ncol = 2,
                                       # align = 'v',
                                       labels = LETTERS[1:2])
GSE135222.roc.km

GSE135222_cli <- crbind2DataFrame(GSE135222_cli)
GSE135222_cli <- GSE135222_cli[rownames(GSE135222_model_data), ]
GSE135222_cli$RiskScore <- risk.GSE135222
GSE135222_cli$RiskType <- ifelse(GSE135222_cli$RiskScore > GSE135222.cutoff, 'High', 'Low')

GSE135222_cli$Response <- GSE135222_cli$benefit
table(GSE135222_cli$Response)
GSE135222_cli$Response[GSE135222_cli$Response == 'Y'] <- 'CR/PR'
GSE135222_cli$Response[GSE135222_cli$Response == 'N'] <- 'PD/SD'


GSE135222_bar <- table(GSE135222_cli$RiskType, GSE135222_cli$Response)
GSE135222_bar
GSE135222_bar_plot <- plotMutiBar(GSE135222_bar,ist = T, legTitle = 'Response', showValue = T)
GSE135222_bar_plot


GSE135222_bar_plot <- ggbarstats(
  data             = GSE135222_cli,
  x                = Response,
  y                = RiskType,
  bf.message = F,
  title            = "GSE135222",
  xlab             = "RiskType",
  legend.title     = "Response",
  package = "ggsci",
  palette = "default_jco"
)
GSE135222_bar_plot



colnames(GSE135222_cli)
GSE135222_rs_plot <- mg_violin(data.frame(GSE135222_cli$Response,
                                          GSE135222_cli$RiskScore), 
                               melt=TRUE, 
                               leg.title='', 
                               jitter = F,
                               xlab = 'Response',
                               ylab = 'RiskScore',
                               legend.pos='tl',
                               show_compare = T)
GSE135222_rs_plot

GSE135222.km.bar <- cowplot::plot_grid(GSE135222.km, 
                                       GSE135222_bar_plot,
                                       rel_widths = c(1,1.5),
                                       ncol = 2)
GSE135222.km.bar
GSE135222.roc.km.bar <- cowplot::plot_grid(GSE135222.km, 
                                           GSE135222.roc,
                                           GSE135222_bar_plot,
                                           # GSE135222_rs_plot,
                                           rel_widths = c(1,1,1.5),
                                           ncol = 3)
GSE135222.roc.km.bar


immune_data.plot <- cowplot::plot_grid(IMvigor210.km.bar,
                                       GSE78220.km.bar,
                                       GSE135222.km.bar,
                                       ncol = 1,
                                       labels = LETTERS[1:3])
immune_data.plot
ggsave(plot = immune_data.plot,
       filename = '12_immune_data/immune_data.plot.pdf',
       width = 20, height = 25)


# 保存数据 ###########
save.image('LUAD_001.Rdata')
load('LUAD_001.Rdata')


# 亚型临床信息统计 ##############
dir.create('clinic')
library(table1)
colnames(tcga.subtype.cli)
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  p <- ifelse(p < 0.001, signif(p, 2), round(p, 3))
}


tcga.subtype.cli.stat <- tcga.subtype.cli
tcga.subtype.cli.stat$OS <- as.character(tcga.subtype.cli.stat$OS)
colnames(tcga.subtype.cli.stat)
library(table1)
tcga_stat <- table1(~ OS + OS.time + + Gender + Age + T.Stage + N.Stage + M.Stage + 
                 Stage ,
               data = tcga.subtype.cli.stat,
               droplevels=F, overall=T)
tcga_stat
library(openxlsx)
write.xlsx(x = tcga_stat,
           file = 'clinic/tcga_stat.xlsx',
           overwrite = T)





GSE30219.subtype.cli.stat <- GSE30219.subtype.cli
GSE30219.subtype.cli.stat$OS <- as.character(GSE30219.subtype.cli.stat$OS)
colnames(GSE30219.subtype.cli.stat)
library(table1)
GSE30219_stat <- table1(~ OS + OS.time + + Gender + Age + T.Stage,
                    data = GSE30219.subtype.cli.stat,
                    droplevels=F, overall=T)
GSE30219_stat
library(openxlsx)
write.xlsx(x = GSE30219_stat,
           file = 'clinic/GSE30219_stat.xlsx',
           overwrite = T)

