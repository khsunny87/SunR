library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(moonBook)
library(purrr)
library(tidyr)



Get_median_fu<-function(TS){
  ret<-survfit(Surv(TS[,1],1-TS[,2])~1)
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


MV_Cox<-function(data,TS_name,trace=T,rename=F,Labels=NULL,keep_variable='1',plot_title='Hazard ratio'){
  
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
  
  
  MV_COX_res<-coxph(formula(paste(TS_name,'~ .')),data=MV_trim)%>%
    step(., scope = list(lower=  formula(paste('~',keep_variable))), direction = "both")
    
ret_list<-list(MV_data=MV_trim,MV_model=MV_COX_res)#,MV_forest=ggforest(MV_COX_res,data=MV_trim,main=plot_title))
return(ret_list)

if(!rename) return(ret_list)

model_var<-MV_COX_res$terms%>%
  attr('term.labels')
ren_MV_trim<-MV_trim%>%
  select(MV_COX_res$terms[[2]],all_of(model_var))
names(ren_MV_trim)[-1]<-map_chr(names(ren_MV_trim)[-1],~ifelse(.x%in%Labels$`변수`,Labels$`변수설명`[Labels$`변수`==.x],.x))


ren_MV_COX_res<-coxph(formula(paste(TS_name,'~ .')),data=ren_MV_trim)

return(append(ret_list,list(ren_MV_model=ren_MV_COX_res,ren_MV_forest=ggforest(ren_MV_COX_res,data=ren_MV_trim,main=plot_title))))
}
