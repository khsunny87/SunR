library(survey)
library(WeightIt)

IPTW_cov<-c("single_lung", "Age", "Male", "Height", "Weight", "Diabetes", "Smoking", "Dialysis", "Creatinine", "mPAP", "MV", "ECMO", "waitlist_days", "DON_Age", "DON_HTN", "DON_Height", "DON_Weight", "DON_PFR", "DON_Distance", "Dx_OB", "INIT_single_lung", "Tx_interval", "Race_White", "DON_Race_White", "DON_Diabetes")

  prematch<-surv_data%>%
    mutate(DON_Race_White=DON_Race == "White",Race_White=Race == "White")%>%
    drop_na(any_of(IPTW_cov))

prematch%>%select(-TS)%>%
  write_excel_csv("prematch_test.csv")


IPTW_data<-prematch%>%
  weightit(as.formula(paste0('DCD~',paste0(IPTW_cov,collapse='+'))),data = ., method = "cbps",estimand = "ATE")
    
prematch$weights <-IPTW_data$weights

mat_design <- svydesign(ids = ~1, data = prematch, weights = ~weights)

my_smd2 <- function(data, variable, by, ...) {
  tmp_x=data$variables[[variable]]
  class(tmp_x)[1]<-ifelse(class(tmp_x)[1]=="labelled",class(tmp_x)[2],class(tmp_x)[1])
  
  ret<-smd::smd(x=tmp_x,g=data$variables[[by]],w=weights(data),na.rm=T)[2]
  names(ret)<-"SMD"
  #ret=0
  return(ret)
}


mat_design%>%
  
  tbl_svysummary(include=PSM_var_inst$Recipient,by="DCD")%>%
  
  add_p()%>%
  add_stat(fns=everything() ~ my_smd2)

