library(MatchIt)
library(cobalt)
library(survey)
library(WeightIt)

mat_cov<-c(
  "calc_age","calc_rf_cva","Dx_ICMP","Dx_RCMP_HCMP", #Treatment & overall/after
  "calc_rf_weight", "calc_rf_bmi", #Treatment & before/after
  "calc_rf_htn",   #Treatment & after
  "wait_time",
  "wt_diff30",
  "race_white"
  )


my_smd2 <- function(data, variable, by, ...) {
  tmp_x=data$variables[[variable]]
  class(tmp_x)[1]<-ifelse(class(tmp_x)[1]=="labelled",class(tmp_x)[2],class(tmp_x)[1])
  
  ret<-smd::smd(x=tmp_x,g=data$variables[[by]],w=weights(data),na.rm=T)[2]
  names(ret)<-"SMD"
  #ret=0
  return(ret)
}


pre_match<-anal_data%>%
           mutate(wt_diff30=wt_diff<(-30)|wt_diff>30)%>%
  drop_na(any_of(mat_cov))


#PSM
#matched<-pre_match%>%
#  matchit(as.formula(paste0('DM_factor~',paste0(mat_cov,collapse='+'))),data=.,method="nearest",caliper=0.28,ratio=2) #0.28 works

#IPTW

#ps_model <- glm(as.formula(paste0('DM_factor~',paste0(mat_cov,collapse='+'))),
 #               data = pre_match, family = binomial)

iptw_model <- weightit(as.formula(paste0('DM_factor~',paste0(mat_cov,collapse='+'))),
                data = pre_match, method = "ps")


#pre_match$ps<-ps_model$fitted.values

#pre_match$weights <- ifelse(pre_match$DM_factor == "DM",
#                            1 / pre_match$ps,       # Weight for treated group
#                            1 / (1 - pre_match$ps))

pre_match$weights <-iptw_model$weights


max_weight<-quantile(iptw_model$weights,0.99)
pre_match<-pre_match%>%
  filter(weights<max_weight)

#weight_trimmed<-pmin(iptw_model$weights,max_weight)


summary(pre_match$weights)

pre_match%>%
  ggplot()+
  geom_density(aes(x=weights,color=DM_factor))
  #geom_boxplot(aes(weights,fill=DM_factor))

?geom_boxplot()

boxplot(pre_match$weights,col="lightblue")

mat_design <- svydesign(ids = ~1, data = pre_match, weights = ~weights)
var_inst$demographic



mat_design%>%
  
  tbl_svysummary(include=var_inst$demographic,by="DM_factor")%>%
  
  add_p()%>%
  add_stat(fns=everything() ~ my_smd2)



p_val<-svylogrank(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor, design = mat_design)[[2]]["p"]
p_val
str_p_val<-paste0("P=",format(round(p_val,3),nsmall=3))


palette=c(muted("blue"), muted("red"))

survfit2(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weights, data = pre_match)%>%
  #survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  
  ggsurvfit(linewidth=1)+
  scale_color_manual(values = palette,labels=c("Non DM","DM"))+
  scale_fill_manual(values = palette,labels=c("Non DM","DM"))+
  
  add_confidence_interval() +
  geom_point(aes(x = time, y = estimate,color = strata),size = 2,shape=3)+
  
  scale_ggsurvfit() +
  #scale_x_continuous(limits = c(0, 15))+
  #add_pvalue("annotation", size = 5,caption=str_p_val)+
  annotate("text", x=10, y=1.0, label=str_p_val, size=7,hjust=0,vjust=2)+
  
  theme_classic()+
  theme(axis.title.x = element_text(face='bold',size=18),
        axis.title.y = element_text(face='bold',size=18),
        axis.text= element_text(size=20),
        legend.text = element_text(size = 17, color = "black", face = "bold"),
        legend.position="bottom")+
  coord_cartesian(xlim =c(0, 15))+
  add_risktable(
    risktable_height = 0.2,size=6,
    risktable_stats = "{format(round(n.risk), nsmall = 0)}",
    stats_label = list("At Risk"),
    
    theme=list(theme_void(),
               theme(title=element_text(size=15),
                     axis.text.y=element_text(size=15),
                     axis.title.y=element_blank(),
                     axis.title.x=element_blank())))



cutoff<-5


after_db<-pre_match%>%filter(TS_mortality[,1]>=cutoff)
after_db$TS_mortality[,1]=after_db$TS_mortality[,1]-cutoff

after_design<-svydesign(ids = ~1, data = after_db, weights = ~weights)
p_val<-svylogrank(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor, design = after_design)[[2]]["p"]
str_p_val<-paste0("P=",format(round(p_val,3),nsmall=3))

summary(coxph(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weights, data = after_db) )



survfit2(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weights, data = after_db)%>%
  #survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  
  ggsurvfit(linewidth=1)+
  scale_color_manual(values = palette,labels=c("Non DM","DM"))+
  scale_fill_manual(values = palette,labels=c("Non DM","DM"))+
  
  add_confidence_interval() +
  geom_point(aes(x = time, y = estimate,color = strata),size = 2,shape=3)+
  
  scale_ggsurvfit() +
  #scale_x_continuous(limits = c(0, 15))+
  #add_pvalue("annotation", size = 5,caption=str_p_val)+
  annotate("text", x=10, y=1.0, label=str_p_val, size=7,hjust=0,vjust=2)+
  
  theme_classic()+
  theme(axis.title.x = element_text(face='bold',size=18),
        axis.title.y = element_text(face='bold',size=18),
        axis.text= element_text(size=20),
        legend.text = element_text(size = 17, color = "black", face = "bold"),
        legend.position="bottom")+
  coord_cartesian(xlim =c(0, 15))+
  add_risktable(
    risktable_height = 0.2,size=6,
    risktable_stats = "{format(round(n.risk), nsmall = 0)}",
    stats_label = list("At Risk"),
    
    theme=list(theme_void(),
               theme(title=element_text(size=15),
                     axis.text.y=element_text(size=15),
                     axis.title.y=element_blank(),
                     axis.title.x=element_blank())))
#######################################

ps_weights <- 1 / ps_model$fitted.values

pre_match$weights <- ps_weights
design <- svydesign(ids = ~1, data = pre_match, weights = ~weights)

design %>%
  tbl_svysummary(
    by = "DM_factor",  # 치료 여부에 따른 그룹 나누기
    #design = design,  # 설계 객체 (가중치 포함)
    include=c("calc_age","sex_male"),
    
    statistic = list(all_continuous() ~ "{mean} ({sd})", 
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"  # 결측값을 처리하지 않도록 설정
  ) %>%
  add_n() %>%  # 각 그룹별 n 수 추가
  add_p()


summary_table <- pre_match %>%
  tbl_summary(
    by = "DM_factor", # 치료 여부에 따른 그룹 나누기
    weight = weights, # IPTW 가중치 적용
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    missing = "no" # 결측값을 처리하지 않도록 설정
  ) %>%
  add_p()


?tbl_summary()

matched<-pre_match%>%
  matchit(as.formula(paste0('DM_factor~',paste0(mat_cov,collapse='+'))),data=.,
          method="nearest",
          distance="logit",
          estimand="ATT") #ATE?

pre_match%>%
  matchit(as.formula(paste0('DM_factor~',paste0(mat_cov,collapse='+'))),data=.,
          estimand="ATT") 

pre_match$ps <- matched$distance  # Extract propensity scores
pre_match$weights <- ifelse(pre_match$DM_factor == "DM",
                      1 / pre_match$ps,       # Weight for treated group
                      1 / (1 - pre_match$ps)) # Weight for control group




summary(matched, un = FALSE, weights = "weight")


mat_design <- svydesign(ids = ~1, data = pre_match, weights = ~weights)

View(mat_design)

var_inst$demographic



mat_design%>%
  
  tbl_svysummary(include=var_inst$demographic,by="DM_factor")%>%
  
  add_p()%>%
  add_stat(fns=everything() ~ my_smd2)



p_val<-svylogrank(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor, design = mat_design)[[2]]["p"]

str_p_val<-paste0("P=",format(round(p_val,3),nsmall=3))


palette=c(muted("blue"), muted("red"))

survfit2(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weight, data = pre_match)%>%
  #survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  
  ggsurvfit(linewidth=1)+
  scale_color_manual(values = palette,labels=c("Non DM","DM"))+
  scale_fill_manual(values = palette,labels=c("Non DM","DM"))+
  
  add_confidence_interval() +
  geom_point(aes(x = time, y = estimate,color = strata),size = 2,shape=3)+
  
  scale_ggsurvfit() +
  #scale_x_continuous(limits = c(0, 15))+
  #add_pvalue("annotation", size = 5,caption=str_p_val)+
  annotate("text", x=10, y=1.0, label=str_p_val, size=7,hjust=0,vjust=2)+
  
  theme_classic()+
  theme(axis.title.x = element_text(face='bold',size=18),
        axis.title.y = element_text(face='bold',size=18),
        axis.text= element_text(size=20),
        legend.text = element_text(size = 17, color = "black", face = "bold"),
        legend.position="bottom")+
  coord_cartesian(xlim =c(0, 15))+
  add_risktable(
    risktable_height = 0.2,size=6,
    risktable_stats = "{format(round(n.risk), nsmall = 0)}",
    stats_label = list("At Risk"),
    
    theme=list(theme_void(),
               theme(title=element_text(size=15),
                     axis.text.y=element_text(size=15),
                     axis.title.y=element_blank(),
                     axis.title.x=element_blank())))

  
  
cutoff<-5


after_db<-pre_match%>%filter(TS_mortality[,1]>=cutoff)
after_db$TS_mortality[,1]=after_db$TS_mortality[,1]-cutoff

after_design<-svydesign(ids = ~1, data = after_db, weights = ~weight)
p_val<-svylogrank(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor, design = after_design)[[2]]["p"]
str_p_val<-paste0("P=",format(round(p_val,3),nsmall=3))

summary(coxph(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weight, data = after_db) )



survfit2(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor,weights=weight, data = after_db)%>%
  #survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  
  ggsurvfit(linewidth=1)+
  scale_color_manual(values = palette,labels=c("Non DM","DM"))+
  scale_fill_manual(values = palette,labels=c("Non DM","DM"))+
  
  add_confidence_interval() +
  geom_point(aes(x = time, y = estimate,color = strata),size = 2,shape=3)+
  
  scale_ggsurvfit() +
  #scale_x_continuous(limits = c(0, 15))+
  #add_pvalue("annotation", size = 5,caption=str_p_val)+
  annotate("text", x=10, y=1.0, label=str_p_val, size=7,hjust=0,vjust=2)+
  
  theme_classic()+
  theme(axis.title.x = element_text(face='bold',size=18),
        axis.title.y = element_text(face='bold',size=18),
        axis.text= element_text(size=20),
        legend.text = element_text(size = 17, color = "black", face = "bold"),
        legend.position="bottom")+
  coord_cartesian(xlim =c(0, 15))+
  add_risktable(
    risktable_height = 0.2,size=6,
    risktable_stats = "{format(round(n.risk), nsmall = 0)}",
    stats_label = list("At Risk"),
    
    theme=list(theme_void(),
               theme(title=element_text(size=15),
                     axis.text.y=element_text(size=15),
                     axis.title.y=element_blank(),
                     axis.title.x=element_blank())))






pre_match$TS_weight <- survfit2(Surv(TS_mortality[,1], TS_mortality[,2]) ~ DM_factor, 
                       data = pre_match,
                       weights = weight)

pre_match%>%
  Get_KM2("TS_mortality",Group_name = "DM_factor",break.by =5,xmax=15,P_x=4)

pre_match%>%
Get_KM2("TS_mortality",Group_name = "DM_factor",break.by =5,xmax=15,P_x=4,weights=pre_match$weight)
tmp$norisk

is.null(pre_match$weight)


  
  add_risktable(
                theme=list(theme_void(),
                           theme(title=element_text(size=15),
                                 axis.text.y=element_text(size=15),
                                 axis.title.y=element_blank(),
                                 axis.title.x=element_blank())))

  Get_KM2<-function(data,TS_name,Group_name="",Group_label=NULL,break.by=5,xmax=20,
                    unit='Year',conf.int=T,palette=c(muted("blue"), muted("red")),type='survival',y_title='Survival',y_lim=c(0,1),P_x=0,P_y=Inf,main_title=""){
    
?survfit
design%>%
tbl_svysummary(include="calc_age",by="DM_factor")%>%
  add_p()

?svydesign

tbl_svysummary(by = Result, percent = "cell")



res_psm<-summary(matched)

mat_bal<-bal.tab(matched,continuous="std",s.d.denom = "weighted",m.threshold=0.2,v.threshold=2)


#bal.tab(matched,continuous="std",s.d.denom = "weighted",m.threshold=0.2,v.threshold=2)


mat_fig1<-bal.plot(matched, var.name = "distance", which = "both",type = "histogram", mirror = T)

new.names<-if_else(label(pre_match[mat_cov])=="",mat_cov,label(pre_match[mat_cov]))
names(new.names)<-mat_cov
mat_fig2<-love.plot(matched,
                    var.names = new.names,
                    binary = "std",s.d.denom = "pooled",threshold=0.2,sample.names=c('Unmatched','Matched'))+scale_x_continuous(limits=c(-0.5,0.5),breaks = seq(from = -0.5, to = 0.5, by = 0.1))



#love.plot(matched,binary = "std",stars='std',.s.d.denom = "pooled",threshold=0.2,sample.names=c('Unmatched','Matched'))+scale_x_continuous(limits=c(-0.5,0.5),breaks = seq(from = -0.5, to = 0.5, by = 0.1))



mat_data<-match.data(matched)

mat_data%>%
  tbl_summary(include="calc_age",by="DM_factor")
