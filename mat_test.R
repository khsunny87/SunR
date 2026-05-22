library(readr)
library(tidyr)
library(MatchIt)
library(gtsummary)
library(officer)
library(flextable)
library(purrr)
library(flexlsx)

library(openxlsx2)

big_border = fp_border(color="black", width = 2)

p_sig<-function(p_v,sig_cut=0.05){
  return(map_lgl(p_v,p_sig_atom))
}


p_sig_atom<-function(p_v,sig_cut=0.05){
  
  #print(p_v)
  if(is.na(p_v)) {return(F)}
  if(is.numeric(p_v)){
    if(p_v<sig_cut) {return(T)}
    return(F)
  }
  
  if(p_v=="") {return(F)}
  if(substr(p_v,1,1)=='<') {return(T)}
  if(substr(p_v,1,1)=='>') {return(F)}
  if(as.numeric(p_v)<0.05) {return(T)}
  return (F)
}



Get_Table<-function(data,include,by_key,type_list,del_var=c(),norm_var=c(),echo=T,...){
  
  if(echo){
  cat(paste("Get tables: ",paste(include=setdiff(include,del_var),collapse = ", "),"\n"))
    }
  
  inter_norm=intersect(include,norm_var)
  
  if(is.null(inter_norm)|length(inter_norm)==0){

    stat_list=list(all_continuous()~"{median} ({p25}–{p75})",all_categorical() ~ "{n} ({p})")
    p_test=NULL
  }else{
    
    norm_stat<-as.formula(paste0('c(',paste(inter_norm,collapse=','),')~"{mean} ± {sd}"'))
    norm_test<-as.formula(paste0('c(',paste(inter_norm,collapse=','),')~"t.test"'))
    
    stat_list = list(all_continuous()~"{median} ({p25}–{p75})",
                     all_categorical() ~ "{n} ({p})",
                     norm_stat)
    p_test=norm_test
    
  }
  
  type<-c(map(intersect(include,type_list$continuous),~as.formula(paste0(.x," ~ 'continuous'"))),
          map(intersect(include,type_list$categorical),~as.formula(paste0(.x," ~ 'categorical'"))))
  
  ret_tbl<-data%>%
    tbl_summary(include=setdiff(include,del_var),by=by_key,digits = list(all_categorical() ~ c(0, 1),
                                                                         all_continuous()~c(1,1)),
                statistic = stat_list,
                type=type,...)%>%
    add_n()%>%
    add_p(test=p_test,pvalue_fun=~style_pvalue(., digits = 3))%>%
    add_overall()%>%
    #add_difference(everything()~"smd")%>%
    modify_header(
      label = '**Variable**',
      stat_0 = '**Total**\n(N = {N})',
      stat_1 = '**{level}**\n(N = {n})',
      stat_2 = '**{level}**\n(N = {n})',
      p.value = '**P**')%>%
    modify_footnote(c(all_stat_cols(), p.value) ~ NA)%>%
    modify_column_indent(columns=label, rows= ((row_type=="level"| row_type=='missing')),double_indent=F)%>%
    add_stat(fns=everything()~SMD_labelled)%>%
    modify_header(SMD="**SMD**")
  
  
  if(echo){
  cat('\n')}
  return(ret_tbl)
}

Get_Table_SMD<-function(data,include,by_key,type_list,del_var=c(),norm_var=c(),echo=T,...){
  
  if(echo){
  cat(paste("Get tables: ",paste(include=setdiff(include,del_var),collapse = ", "),"\n"))
    }
  
  inter_norm=intersect(include,norm_var)
  
  if(is.null(inter_norm)|length(inter_norm)==0){

    stat_list=list(all_continuous()~"{median} ({p25}–{p75})",all_categorical() ~ "{n} ({p})")
    p_test=NULL
  }else{
    
    norm_stat<-as.formula(paste0('c(',paste(inter_norm,collapse=','),')~"{mean} ± {sd}"'))
    norm_test<-as.formula(paste0('c(',paste(inter_norm,collapse=','),')~"t.test"'))
    
    stat_list = list(all_continuous()~"{median} ({p25}–{p75})",
                     all_categorical() ~ "{n} ({p})",
                     norm_stat)
    p_test=norm_test
    
  }
  
  type<-c(map(intersect(include,type_list$continuous),~as.formula(paste0(.x," ~ 'continuous'"))),
          map(intersect(include,type_list$categorical),~as.formula(paste0(.x," ~ 'categorical'"))))
  
  ret_tbl<-data%>%
    tbl_summary(include=setdiff(include,del_var),by=by_key,digits = list(all_categorical() ~ c(0, 1),
                                                                         all_continuous()~c(1,1)),
                statistic = stat_list,
                type=type,...)%>%
    add_n()%>%
    add_p(test=p_test,pvalue_fun=~style_pvalue(., digits = 3))%>%
    add_overall()%>%
    #add_difference(everything()~"smd")%>%
    modify_header(
      label = '**Variable**',
      stat_0 = '**Total**\n(N = {N})',
      stat_1 = '**{level}**\n(N = {n})',
      stat_2 = '**{level}**\n(N = {n})',
      p.value = '**P**')%>%
    modify_footnote(c(all_stat_cols(), p.value) ~ NA)%>%
    modify_column_indent(columns=label, rows= ((row_type=="level"| row_type=='missing')),double_indent=F)%>%
    add_stat(fns=everything()~SMD_labelled)%>%
    modify_header(SMD="**SMD**")
  
  
  if(echo){
  cat('\n')}
  return(ret_tbl)
}

#Get_instrument3<-function(data,include,by_key,type_list,del_var=c()){
#  type<-c(map(intersect(include,type_list$continuous),~as.formula(paste0(.x," ~ 'continuous'"))),
#          map(intersect(include,type_list$categorical),~as.formula(paste0(.x," ~ 'categorical'"))))
#print(type)
#  return(Get_Table(data,setdiff(include,del_var),by_key,type=type))
#}


print_table_sig<-function(table_gt){
  table_gt%>%
    as_flex_table()%>%
    #theme_vanilla()%>%
    hline_top(part="all",border=big_border) %>%
    hline_bottom(part="body",border=big_border)%>%
    bg( ~p_sig(`p.value`), bg = "yellow")%>%
    color(~p_sig(`p.value`),color='red')%>%
    autofit()%>%
    return()
}

#Get_instrument<-function(inst_name,add_var=c(),del_var=c(),...){
#  var_name<-setdiff(data_dic%>%filter(Form.Name==inst_name)%>%.$Variable...Field.Name,radio_var)
#  
#  return(Get_Table(tbl_data,setdiff(c(var_name,add_var),del_var),"root_by_me",...))
#}
#
#Get_instrument2<-function(include,del_var=c(),...){
#  return(Get_Table(tbl_data,setdiff(include,del_var),"root_by_me",...))
#}



#int_num<-function(var_name){
#  ori_label<-label(tbl_data[[var_name]])
#  tbl_data[[var_name]]<<-as.numeric(tbl_data[[var_name]])
#  label(tbl_data[[var_name]])<<-ori_label
#}


Add_tbl_list<-function(t_list,tbl,inst_name){
  t_list[[inst_name]]<-tbl
  return(t_list)
}

Table_Excel<-function(tbl_list,file_name){
  wb<-wb_workbook()
  map(names(tbl_list),function(x) wb<<-wb%>%wb_add_worksheet(x)%>%wb_add_flextable(x,tbl_list[[x]]%>%print_table_sig()))
  wb$save(file_name)
  
}


shapiro_error<-function(x){
  tryCatch({
    ret=shapiro.test(x)$p.value>0.05
    return(ret)
  },error=function(e){
    print(e)
  })
  return(F)
}

#tbl_instrument<-unique(data_dic["Form.Name"])%>%.[[1]]



#age,calc_echo_pasys,calc_echo_ef,calc_echo_av_gradient,calc_echo_mv_gradient,calc_op_blood_plat_u,calc_op_blood_cryo_u,calc_op_incision_m,calc_op_xclamp_m,calc_op_perfus_m,calc_po_blood_rbc_u,calc_po_blood_plat_u,calc_po_blood_cryo_u
#calc_losas,calc_lossd,calc_losad
#int_num("calc_age")
#int_num("calc_echo_pasys")
#int_num("calc_echo_ef")
#int_num("calc_echo_av_gradient")
#int_num("calc_echo_mv_gradient")
#int_num("calc_op_blood_rbc_u")
#int_num("calc_op_blood_plat_u")
#int_num("calc_op_blood_cryo_u")
#int_num("calc_op_incision_m")
#int_num("calc_op_circarrest_m")
#int_num("calc_op_xclamp_m")
#int_num("calc_op_perfus_m")
#int_num("calc_op_blood_ffp_u")
#int_num("calc_po_blood_rbc_u")
#int_num("calc_po_blood_plat_u")
#int_num("calc_po_blood_cryo_u")
#int_num("calc_po_blood_ffp_u")
#int_num("calc_losas")
#int_num("calc_lossd")
#int_num("calc_losad")


SMD_labelled <- function(data, variable, by, ...) {
  # weights 컬럼이 있으면 사용
  w <- if ("weights" %in% names(data)) data$weights else NULL
  tmp_x <- data[[variable]]
  class(tmp_x)[1] <- ifelse(class(tmp_x)[1]=="labelled", class(tmp_x)[2], class(tmp_x)[1])
  ret <- smd::smd(x=tmp_x, g=data[[by]], w=w, na.rm=TRUE)[2]
  names(ret) <- "SMD"
  return(ret)
}







Calc_SMD<-function(f,data){
  
  myt<-terms(f,data=data)
  group<-as.character(myt[[2]])
  

  
  if(length(unique(data[[group]]))!=2)
    cat(group,"is NOT binomial factor")
  
  myvar<-attr(myt,'term.labels')
  
  return(map_df(myvar,Calc_each_SMD,group=group,data=data))
  
}


Calc_each_SMD<-function(item,group,data,digits=4){
  cate=unique(data[[group]])
  G1<-data%>%filter(.[[group]]==cate[1])%>%.[[item]]
  G2<-data%>%filter(.[[group]]==cate[2])%>%.[[item]]
  
  if(is.numeric(na.omit(data[[item]])) & length(unique(na.omit(data[[item]])))>2){
    cat(item,'is a continuous variable\n')
    smd=(mean(G1,na.rm=T)-mean(G2,na.rm=T))/sqrt(((sd(G1,na.rm=T)^2+sd(G2,na.rm=T)^2))/2)
  }
  else if(length(unique(na.omit(data[[item]])))==2){
    cat(item,'is a binomial variable\n')
    X=unique(G1)[1]
    p1<-sum(G1==X,na.rm=T)/sum(!is.na(G1),na.rm=T)
    p2<-sum(G2==X,na.rm=T)/sum(!is.na(G2),na.rm=T)
    smd=(p1-p2)/sqrt((p1*(1-p1)+p2*(1-p2))/2)
  }
  else{
    cat(item,'is NOT proper variable\n')
    smd=NA
  }
  return(data.frame(variable=item,smd=round(smd,digits=digits)))
}

Get_norm_var<-function(data,total_var){
  
  num_lgl<-map_lgl(total_var,~is.numeric(data[[.x]]))
  num_var<-total_var[num_lgl]
  
  norm_lgl<-map_lgl(num_var,~shapiro_error(data[[.x]]))
  return(num_var[norm_lgl])
}

my_smd2 <- function(data, variable, by, ...) {

  tmp_x=data$variables[[variable]]
  class(tmp_x)[1]<-ifelse(class(tmp_x)[1]=="labelled",class(tmp_x)[2],class(tmp_x)[1])
  
  ret<-smd::smd(x=tmp_x,g=data$variables[[by]],w=weights(data),na.rm=T)[2]
  names(ret)<-"SMD"
  #ret=0
  return(ret)
}

Get_svTable <- function(data, include, by_key, type_list, del_var=c(), norm_var=c(), echo=T, ...) {
  
  if(echo) {
    cat(paste("Get tables: ", paste(include=setdiff(include,del_var), collapse=", "), "\n"))
  }
  
  inter_norm <- intersect(include, norm_var)
  
  if(is.null(inter_norm) | length(inter_norm)==0) {
    stat_list <- list(all_continuous()~"{median} ({p25}–{p75})", all_categorical()~"{n} ({p})")
    p_test <- NULL
  } else {
    norm_stat <- as.formula(paste0('c(', paste(inter_norm, collapse=','), ')~"{mean} ± {sd}"'))
    norm_test <- as.formula(paste0('c(', paste(inter_norm, collapse=','), ')~"t.test"'))
    stat_list <- list(all_continuous()~"{median} ({p25}–{p75})",
                      all_categorical()~"{n} ({p})",
                      norm_stat)
    p_test <- norm_test
  }
  
  type <- c(map(intersect(include, type_list$continuous), ~as.formula(paste0(.x, "~'continuous'"))),
            map(intersect(include, type_list$categorical), ~as.formula(paste0(.x, "~'categorical'"))))
  
  ret_tbl <- data %>%
    tbl_svysummary(
      include = setdiff(include, del_var),
      by = by_key,
      digits = list(all_categorical()~c(1,1), all_continuous()~c(1,1)),
      statistic = stat_list,
      type = type,
      ...) %>%
    add_p(test=p_test, pvalue_fun=~style_pvalue(., digits=3)) %>%
    modify_header(
      label   = '**Variable**',
      p.value = '**P**',
      all_stat_cols() ~ "**{level}**\nN = {style_number(n, digits=1)}") %>%
    modify_footnote(c(all_stat_cols(), p.value) ~ NA) %>%
    modify_column_indent(columns=label, rows=(row_type=="level" | row_type=='missing'), double_indent=F) %>%
    add_stat(fns=everything() ~ my_smd2) %>%
    modify_header(SMD="**SMD**")
  
  if(echo) cat('\n')
  return(ret_tbl)
}

matching_cov<-c("Age", "Male", "single_lung", "multi_organ", "Race_White", "Height", "Weight", "Diabetes", "Smoking", "Dialysis", "Creatinine", "mPAP", "MV", "ECMO", "waitlist_days", "Dx_OB", "DON_Age", "DON_Male", "DON_Race_White", "DON_smoking20", "DON_Diabetes", "DON_HTN", "DON_Height", "DON_Weight", "DON_PFR", "DON_Distance")

prematch<-read_csv("PS_AI/pre_match.csv")%>%
  drop_na(any_of(matching_cov))


  match_ratio = floor(sum(!prematch$DCD) / sum(prematch$DCD))  # 1:N 전체매칭,
  matched<-prematch%>%
    #matchit(as.formula(paste0('DCD~',paste0(matching_cov,collapse='+'))),data=.,method="nearest",caliper=0.2,ratio=3,distance = "probit") 
    #matchit(as.formula(paste0('DCD~',paste0(matching_cov,collapse='+'))),data=.,method="nearest",caliper=1,ratio=match_ratio,distance = "logit") 
    matchit(DCD ~ Age + Male + single_lung + multi_organ + Height + Race_White + Weight + Diabetes + Smoking + Dialysis + Creatinine + mPAP + MV + ECMO + waitlist_days + Dx_OB + DON_Age + DON_Male + DON_Race_White + DON_smoking20 + DON_Diabetes + DON_HTN + DON_Height + DON_Weight + DON_PFR + DON_Distance,
        data = .,
        method = "nearest",
        distance = "logit",
        ratio = match_ratio, # 1:N 전체매칭,
        caliper = 1,
        estimand = "ATT")

mat_data<-match.data(matched)%>%
  mutate(DCD=factor(ifelse(DCD,"DCD","DBD"),
  levels=c("DBD","DCD")))

  
var_inst=list(all=names(mat_data))
type_list=list(continuous=c(''),categorical=c(''))



#inst_index=c("Recipient","Donor","PostOp")
inst_index=names(var_inst)
del_var=c()
  
  total_var<-unlist(map(inst_index,~var_inst[[.x]]))
  norm_var=Get_norm_var(mat_data,total_var)


tbl_list<-map(inst_index,function(x) mat_data%>%Get_Table(var_inst[[x]],"DCD",type_list,del_var=del_var,norm_var=norm_var))
names(tbl_list)<-inst_index


tbl_list$all
