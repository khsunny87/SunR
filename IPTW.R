library(ggsurvfit)
library(MatchIt)
library(survey)
library(purrr)
library(gtsummary)
View(df_colon)

str(df_colon)

map_int(df_colon,~sum(is.na(.)))
sum(is.na(df_colon$differ))
?smd
name

my_smd <- function(data, variable, by, ...) {

  ret<-smd::smd(x=data[[variable]],g=data[[by]],na.rm=T)[2]
  names(ret)<-"SMD"
  return(ret)
}

#View(design)


my_smd2 <- function(data, variable, by, ...) {
  ret<-smd::smd(x=data$variables[[variable]],g=data$variables[[by]],w=weights(data),na.rm=T)[2]
  names(ret)<-"SMD"
  #ret=0
  return(ret)
}



smd::smd(x=pre_mat$age,g=pre_mat$surg)
?smd::smd

smd::smd(x=design$variables$id,g=design$variables$surg,w=weights(design))

design$variables$sex
?smd::smd
tmp<-smd::smd(x=df_colon$age,g=df_colon$surg)[2]

names(tmp)<-"SMD"
tmp

pre_mat<-df_colon

iptw_model <- matchit(surg ~ age + sex + status,  # Replace with your covariates
                      data = pre_mat,
                      method = "nearest",  # For IPTW, no matching is performed, so method doesn't matter
                      distance = "logit",  # Logistic regression for propensity scores
                      estimand = "ATT")    # Change to "ATE" if needed

# Extract Propensity Scores and Calculate IPTW
pre_mat$ps <- iptw_model$distance  # Extract propensity scores
pre_mat$weight <- ifelse(pre_mat$surg =="Extended Time Since Surgery",
                      1 / pre_mat$ps,       # Weight for treated group
                      1 / (1 - pre_mat$ps)) # Weight for control group

pre_mat%>%
  select(-id)%>%
  tbl_summary(by="surg")%>%
  add_p()%>%
  add_stat(fns=everything() ~ my_smd)




# Step 2: Check Balance with Weights
# Summarize balance using standardized mean differences (SMD)
summary(iptw_model, un = FALSE, weights = "weight")  # Unweighted (un) vs weighted comparisons

# Step 3: Perform Weighted Analysis
# Use the 'survey' package for weighted regression

View(design)
design <- svydesign(ids = ~1, data = pre_mat, weights = ~weight)

design%>%

  tbl_svysummary(by="surg")%>%
  
  add_p()%>%
  #add_difference(everything()~"smd")%>%
  add_stat(fns=everything() ~ my_smd2)
  

?survfit2()
summary(pre_mat$weight)
str(pre_mat$weight)
?add_censor_mark()



#p_val<-survdiff(Surv(time, status) ~ surg, data = df_colon)%>%broom::glance()%>%.$p.value
p_val<-svylogrank(Surv(time, status) ~ surg, design = design)[[2]]["p"]

str_p_val<-paste0("P=",format(round(p_val,3),nsmall=3))

survfit2(Surv(time, status) ~ surg,weights=weight, data = pre_mat) %>%
  #survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  
  ggsurvfit() +
  add_confidence_interval() +
#add_censor_mark(size=2)+
  scale_ggsurvfit() +
  add_risktable(
    #risktable_stats = "{format(round(n.risk, nsmall = 0)}",
    risktable_stats = "{format(round(n.risk), nsmall = 0)}",
    
    stats_label = list("Number at Risk"),
  )+
#  labs(title = "Default") +
  scale_x_continuous(limits = c(0, 8))+
  add_pvalue("annotation", size = 5,caption=str_p_val)+
  #coord_cartesian(xlim = c(0, 8)) +
  scale_color_manual(values = c('#54738E', '#82AC7C')) +
  scale_fill_manual(values = c('#54738E', '#82AC7C')) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  labs(
    title = "Modified",
    y = "Percentage Survival"
  )

gg_styled

survdiff(Surv(time, status) ~ surg, data = df_colon)%>%broom::glance()%>%.$p.value
svylogrank(Surv(time, status) ~ surg, design = design)[[2]]["p"]

logrank_test <- svyranktest(
  Surv(time, status) ~ surg,  # 생존 분석 공식
  design = design       # IPTW 적용된 설계
)
logrank_test
?add_pvalue()
?survival::survdiff()


