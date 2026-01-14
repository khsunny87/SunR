library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(moonBook)
library(purrr)
library(tidyr)
library(tidycmprsk)
library(scales)
library(ggsurvfit)

toIQR<-function(quant){
  paste0(quant['50'],' (',quant['25'],'-',quant['75'],')')
}

Get_median_fu<-function(TS,digits=2){
  ifelse(TS[,2]==0,1,0)
  #ret<-survfit(Surv(TS[,1],1-TS[,2])~1)
  ret<-survfit(Surv(TS[,1],ifelse(TS[,2]==0,1,0))~1)
  #return(summary(ret)$table['median']%>%round(2))
  return(round(quantile(ret)$quantile,digits))
}

Get_median_fu2<-function(TS){
  ret<-survfit(Surv(TS[,1],ifelse(TS[,2]==0,1,0))~1)
  return(summary(ret)$table['median']%>%round(2))
}

Survival_Table<-function(TS,strata=1,time_point,unit){
  overall<-survfit(TS~1)%>%
    summary(times=time_point)%>%.[c('time','surv','std.err')]%>%as.data.frame()%>%
    mutate(strata='Overall')%>%
    mutate(value=paste(format(round(surv*100,1),nsmall=1),'±',format(round(std.err*100,1),nsmall=1),'%'))%>%
    select(time,strata,value)
  
  if(length(strata)==1) {
    if(strata==1){
      return(overall%>%
               pivot_wider(names_from='time',names_prefix=paste0(unit,' '),values_from='value'))
      
    }}
  
  return(overall%>%
           rbind(.,survfit(TS~strata)%>%
                   summary(times=time_point)%>%.[c('strata','time','surv','std.err')]%>%as.data.frame()%>%
                   mutate(value=paste(format(round(surv*100,1),nsmall=1),'±',format(round(std.err*100,1),nsmall=1),'%'))%>%
                   select(time,strata,value))%>%
           pivot_wider(names_from='time',names_prefix=paste0(unit,' '),values_from='value'))
  
}


Get_UV_Cox<-function(TS_name,var_name,data,Dx_data=T){
  
  #3개 이상 factor 제한
  
  tmp_df=data.frame(TS=data[TS_name],var=data[var_name])
  names(tmp_df)<-c(TS_name,var_name)
  
  suppressWarnings(cox_model<-coxph(formula(paste(TS_name,'~',var_name)),data=tmp_df))
  res_cox<-summary(cox_model)
  if(any(is.infinite(res_cox$conf.int))) {
    cat(var_name,' was excluded : infinite\n')
    return(list())
  }
  
  if(Dx_data){
    return(
      list(cox_df=data.frame(Var=var_name,HR=res_cox['conf.int'][[1]][1],lcl=res_cox['conf.int'][[1]][3],ucl=res_cox['conf.int'][[1]][4],pval=res_cox['coefficients'][[1]][5]),
           cox_diag=cox.zph(cox_model)%>%ggcoxzph(.)
      )
    )}
  
  return(
    list(cox_df=data.frame(Var=var_name,HR=res_cox['conf.int'][[1]][1],lcl=res_cox['conf.int'][[1]][3],ucl=res_cox['conf.int'][[1]][4],pval=res_cox['coefficients'][[1]][5]))
  )
  
}



UV_Cox<-function(data,TS_name,Dx_data=T){
  
  UV_COX_res=mycph(formula(paste(TS_name,'~ .')),data=data)
  UV_COX_list<-map(names(data)[-1],~Get_UV_Cox(TS_name,.x,data=data,Dx_data=Dx_data))
  UV_COX_df<-map_df(UV_COX_list,~.$cox_df)
  
  return(list(result=UV_COX_res,df=UV_COX_df,dx=UV_COX_list))
}


MV_Cox<-function(data,TS_name,trace=T,rename=F,Labels=NULL,keep_variable='1',plot_title='Hazard ratio',cox_method=coxph){
  
  if(keep_variable!=1){
    item_list<-names(data)
    items<-strsplit(keep_variable,'\\+')[[1]]
    keep_variable<-paste(items[items %in%item_list],collapse='+')
    if(keep_variable=="") keep_variable<-'1'
  }
  
  
  MV_trim<-data%>%
    mutate_if(is.logical,~if_else(.x,1,0))%>%
    #na.omit()%>%
    filter(complete.cases(.))%>%
    as.data.frame()
  
  if(rename){
    trim_label<-Labels%>%
      filter(`변수`%in%names(MV_trim))
    MV_trim[trim_label$변수]<-set_label(MV_trim[trim_label$변수],label=trim_label$변수설명)
  }
  
  
  MV_COX_res<-cox_method(formula(paste(TS_name,'~ .')),data=MV_trim)%>%
    step(., scope = list(lower=  formula(paste('~',keep_variable))), direction = "both")
  
  ret_list<-list(MV_data=MV_trim,MV_model=MV_COX_res)#,MV_forest=ggforest(MV_COX_res,data=MV_trim,main=plot_title))
  return(ret_list)
  
  if(!rename) return(ret_list)
  
  model_var<-MV_COX_res$terms%>%
    attr('term.labels')
  ren_MV_trim<-MV_trim%>%
    select(MV_COX_res$terms[[2]],all_of(model_var))
  names(ren_MV_trim)[-1]<-map_chr(names(ren_MV_trim)[-1],~ifelse(.x%in%Labels$`변수`,Labels$`변수설명`[Labels$`변수`==.x],.x))
  
  
  ren_MV_COX_res<-cox_method(formula(paste(TS_name,'~ .')),data=ren_MV_trim)
  
  return(append(ret_list,list(ren_MV_model=ren_MV_COX_res,ren_MV_forest=ggforest(ren_MV_COX_res,data=ren_MV_trim,main=plot_title))))
}



Get_KM2<-function(data,TS_name,Group_name="",Group_label=NULL,break.by=5,xmax=20,unit='Year',conf.int=T,palette=c(muted("blue"), muted("red")),type='survival',y_title='Survival',y_lim=c(0,1),P_x=0,P_y=Inf,main_title="",P_v="",print_p=T){
  
  
  if(Group_name==""){
    res<-data_frame(TS=data[[TS_name]])%>%
      survfit2(TS ~ 1, data = .)
    print_p=F    
    P_v=""  
    km_fig<-res%>%
      ggsurvfit(type=type,linewidth=1,color=muted('blue')) +
      if (conf.int) add_confidence_interval(fill=muted('blue')) 
    surv_tbl<-Survival_Table(data[[TS_name]],1,seq(break.by,xmax,break.by),unit)
    
    
  }else{
    
    if(is.null(Group_label)){
      if(is.factor(data[[Group_name]])){
        Group_label<-levels(droplevels(data[[Group_name]]))
        
      }
      else{Group_label<-c('Control','Treatment')}
    } else if(is.factor(data[[Group_name]])){
      levels(data[[Group_name]])<-Group_label
    }
    
    res<-data_frame(TS=data[[TS_name]],group=data[[Group_name]])%>%
      survfit2(TS ~ group, data = .)
    if(P_v=="" & print_p){
    log_rank<-data_frame(TS=data[[TS_name]],group=data[[Group_name]])%>%
      survdiff(TS ~group,data=.)
    
    real_p<-broom::glance(log_rank)$p.value
    P_v<-case_when(
      real_p<0.001~"P < 0.001",
      real_p>0.999~"P > 0.999",
      #TRUE~paste("P =",round(real_p,digits=3))
      TRUE~paste("P =",sprintf("%.3f",real_p))
    )
  }
    km_fig<-res%>%
      ggsurvfit(type=type,linewidth=1)+
      scale_color_manual(values = palette,labels=Group_label)+
      scale_fill_manual(values = palette,labels=Group_label)+
      if (conf.int) add_confidence_interval()
    
    surv_tbl<-Survival_Table(data[[TS_name]],data[[Group_name]],seq(break.by,xmax,break.by),unit)
    surv_tbl$strata=c('Overall',Group_label)
    
  }
  
  km_fig<-km_fig+
    add_censor_mark(size = 2)+
    labs(x = unit,y=y_title) + 
    {if(print_p) annotate("text", x=P_x, y=P_y, label=P_v, size=7,hjust=0,vjust=2) else NULL}+
    scale_x_continuous(labels=function(x)x,breaks = seq(0, xmax, by = break.by))+
    scale_y_continuous(labels = scales::percent,limits=y_lim)+ 
    theme_classic()+
    theme(axis.title.x = element_text(face='bold',size=18),
          axis.title.y = element_text(face='bold',size=18),
          axis.text= element_text(size=20),
          legend.text = element_text(size = 17, color = "black", face = "bold"),
          legend.position="bottom")+
    coord_cartesian(xlim =c(0, xmax))
  
  if(main_title!=""){
    km_fig<-km_fig+ggtitle(main_title)+
      theme(plot.title = element_text(size=20, face="bold"))
  }
  
  km_fig_no_risk<-km_fig
  
  km_fig<-km_fig+add_risktable(risktable_height = 0.2,risktable_stats = "n.risk",size=6,
                               theme=list(theme_void(),
                                          theme(title=element_text(size=15),
                                                axis.text.y=element_text(size=15),
                                                axis.title.y=element_blank(),
                                                axis.title.x=element_blank())))
  
  
  
  
  return(list(tbl=surv_tbl,fig=km_fig,norisk=km_fig_no_risk))
}


Get_CMP<-function(data,TS_name,Group_name="",Group_label=NULL,break.by=5,xmax=20,unit='Year',conf.int=T,outcome='outcome',palette=c(muted("blue"), muted("red")),y_lim=c(0,1),main_title="",print_p=T,P_v=""){
  
  if(is.null(Group_label)){
    if(is.factor(data[[Group_name]])){
      Group_label<-levels(data[[Group_name]])
      
    }
    else{Group_label<-c('Control','Treatment')}
  } else if(is.factor(data[[Group_name]])){
    levels(data[[Group_name]])<-Group_label
  }
  
  if(Group_name==""){
    res<-data_frame(TS=data[[TS_name]])%>%
      tidycmprsk::cuminc(TS ~ 1, data = .)
    print_p=F
    P_v=""  
    cmp_fig<-res%>%
      ggcuminc(color=muted('blue'),outcome=outcome,linewidth=1) +
      if (conf.int) add_confidence_interval(fill=muted('blue')) 
    
    
  }else{
    res<-data_frame(TS=data[[TS_name]],group=data[[Group_name]])%>%
      tidycmprsk::cuminc(TS ~ group, data = .)
     if(P_v=="" & print_p){
   
    real_p<-broom::glance(res)$p.value_1
    P_v<-case_when(
      real_p<0.001~"P < 0.001",
      real_p>0.999~"P > 0.999",
      TRUE~paste("P =",round(real_p,digits=3))
    )
  }
    
    
    
    cmp_fig<-res%>%
      ggcuminc(outcome=outcome,linewidth=1) + 
      scale_color_manual(values = palette,labels=Group_label)+
      scale_fill_manual(values = palette,labels=Group_label)+
      if (conf.int) add_confidence_interval()
    
  }
  
  cmp_fig<-cmp_fig+
    labs(x = unit) + 
    {if (print_p) annotate("text", x=0, y=Inf, label=P_v, size=7,hjust=0,vjust=2) else NULL}+
    scale_x_continuous(labels=function(x) x,breaks = seq(0, xmax, by = break.by))+
    scale_y_continuous(labels = scales::percent,limits=y_lim)+ 
    
    theme_classic()+
    theme(axis.title.x = element_text(face='bold',size=18),
          axis.title.y = element_text(face='bold',size=18),
          axis.text= element_text(size=20),
          legend.text = element_text(size = 17, color = "black", face = "bold"),
          legend.position="bottom")+
    coord_cartesian(xlim =c(0, xmax))
  
  if(!is.null(y_lim)){
    cmp_fig<-cmp_fig+scale_y_continuous(labels = scales::percent,limits=y_lim)
  }
  
  if(length(outcome)==1){
    cmp_fig<-cmp_fig+add_censor_mark(size = 2)}
  
  if(main_title!=""){
    cmp_fig<-cmp_fig+ggtitle(main_title)+
      theme(plot.title = element_text(size=20, face="bold"))
  } 
  
  cmp_fig_no_risk<-cmp_fig
  
  cmp_fig<-cmp_fig+add_risktable(risktable_height = 0.2,risktable_stats = "n.risk",size=6,
                                 theme=list(theme_void(),
                                            theme(title=element_text(size=15),
                                                  axis.text.y=element_text(size=15),
                                                  axis.title.y=element_blank(),
                                                  axis.title.x=element_blank())))
  
  if(Group_name!=""){
    cmp_tbl<-res%>%
      tbl_cuminc(times=seq(break.by,xmax,break.by),
                 label_header = paste0("**{time} ",unit,"**"),
                 estimate_fun=function(x) style_percent(x, symbol = F, digits = 1))%>%
      add_p(pvalue_fun = function(x) style_number(x, digits = 3))%>%
      add_n()
    
    
    res_total<-data_frame(TS=data[[TS_name]],group=data[[Group_name]])%>%
      tidycmprsk::cuminc(TS ~ 1, data = .)
    
    cmp_tbl_overall<-res_total%>%
      tbl_cuminc(times=seq(break.by,xmax,break.by),
                 label_header = paste0("**{time} ",unit,"**"),
                 estimate_fun=function(x) style_percent(x, symbol = F, digits = 1))%>%
      add_n()
    
    
    
    return(list(tbl=tbl_stack(list(cmp_tbl_overall, cmp_tbl)),fig=cmp_fig,norisk=cmp_fig_no_risk))
    
  }
  
  
  
  cmp_tbl_overall<-res%>%
    tbl_cuminc(times=seq(break.by,xmax,break.by),
               label_header = paste0("**{time} ",unit,"**"),
               estimate_fun=function(x) style_percent(x, symbol = F, digits = 1))%>%
    add_n()
  
  
  
  return(list(tbl=cmp_tbl_overall,fig=cmp_fig,norisk=cmp_fig_no_risk))
}



