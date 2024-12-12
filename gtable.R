source("../SunR/Sun_Survival.R")

library(forcats)
library(flextable)

tmp_tbl1<-trial |>
  dplyr::mutate(
    age60 = ifelse(age < 60, "<60", "60+")
  )%>%

#trial%>%
  tbl_summary(by = trt, missing = "no", include = c(trt, age, age60))
  
tmp_tbl1
tmp_tbl1%>%
  remove_row_type(age60, type = "header")


  
  View(tmp_tbl1)
  View(tbl_total)

fct_recode(df_colon$surg, fruit = "apple", fruit = "banana")

ts_colon<-df_colon%>%
  mutate(TS_mortality=Surv(time, status))

levels(ts_colon$surg)
levels(ts_colon$surg)[1]<-"Limited"
levels(ts_colon$surg)[2]<-"Extended"
levels(ts_colon$surg)


Get_median_fu2(ts_colon$TS_mortality)

fig_list<-ts_colon%>%
Get_KM2(TS_name="TS_mortality",Group_name="surg",break.by = 1,xmax=10,P_x=3)


fig_list$fig
?tbl_survfit

surv_tbl<-list(survfit(TS_mortality ~ 1, ts_colon),survfit(TS_mortality ~ surg, ts_colon))%>%
  tbl_survfit(times = c(1, 5,8),
              reverse=F,
              statistic = "{estimate}",
              label_header = "**{time} Year**")%>%
  as_tibble()

surv_tbl<-surv_tbl[-2,]
names(surv_tbl)<-str_remove_all(names(surv_tbl),"\\*")
names(surv_tbl)[1]<-""
surv_tbl

?ttheme_default
ncol(surv_tbl)
#hmatrix<-matrix(c(0,0.5,0.5,0.5),ncol=4,byrow=T,nrow=3)

hmatrix<-matrix(c(0,rep(0.5,ncol(surv_tbl)-1)),byrow=T,nrow=nrow(surv_tbl),ncol=ncol(surv_tbl))
hmatrix
xmatrix<-matrix(c(c(0.1,rep(0.5,ncol(surv_tbl)-1)),
                rep(c(0.2,rep(0.5,ncol(surv_tbl)-1)),2)),byrow=T,nrow=nrow(surv_tbl),ncol=ncol(surv_tbl))
xmatrix


rep(c(0.2,0.5,0.5,0.5),2)
surv_g<-tableGrob(surv_tbl, rows = NULL,
              theme = ttheme_default(base_size=14,
                                     colhead=list(bg_params=list(fill=brewer.pal(9,'PuBu')[5]),
                                                  fg_params=list(fontface='bold')),
                                     core = list(bg_params=list(fill=brewer.pal(9,'PuBu')[3]),
                                                 fg_params=list(hjust=hmatrix, x=xmatrix))
                                                                       
                                                 
                                                 ))
#colhead=list(bg_params=list(fill="red"))))
w_ratio=2
w_sum=w_ratio+3
surv_g$widths<-unit(c(w_ratio/w_sum,rep(1/w_sum,3)),'npc')
surv_g$widths
grid.draw(surv_g)
fig_list$fig+annotation_custom(surv_g,    xmin  = 4.5, xmax = 10, ymin = 0.5, ymax = 1)



fig_list$fig


tbl_total


summary_tmp
View(summary_tmp)
  remove_row_type(
    variables = surg,
    type = "header")


f_tbl<-tbl_total%>%
  as_flex_table()%>%
  delete_rows(i=2,part="body")%>%
  width(width=c(1.5,rep(1,3)))%>%
  height_all(height=10)

f_tbl

g_table<-f_tbl%>%
  gen_grob(fit = "fixed",scaling ="full")
  #gen_grob(fit = "fixed", just = "center")

fig_list$fig+
  annotation_custom(
    g_table,
    xmin  = 4.5, xmax = 10, ymin = 0, ymax = 1)

?annotation_custom


View(g_table)

w_ratio=1.5
w_sum=w_ratio+3
g_table$widths

g_table$widths<-unit(c(w_ratio/w_sum,rep(1/w_sum,3)),'npc')
g_table$widths
sum(g_table$widths)



class(g_table)
class(g1)


tb_tbl<-tbl_total%>%
  as_tibble()%>%.[-2,]
library(stringr)
names(tb_tbl)<-str_remove_all(names(tb_tbl),"\\*")



tb_tbl[-2,]


library(gridExtra)
library(RColorBrewer)
g1<-tableGrob(tb_tbl, rows = NULL,
              theme = ttheme_default(base_size=14,
                                     colhead=list(bg_params=list(fill=brewer.pal(9,'PuBu')[5]),
                                       fg_params=list(col="navyblue", fontface=4L)),
                                     core = list(fg_params=list(fontface=c('bold','plain')),
                                                 bg_params=list(fill=brewer.pal(9,'PuBu')[3]))))
#colhead=list(bg_params=list(fill="red"))))
brewer.pal(9,'PuBu')[c(5,3)]
w_ratio=2
w_sum=w_ratio+3
g1$widths<-unit(c(w_ratio/w_sum,rep(1/w_sum,3)),'npc')
fig_list$fig+annotation_custom(g1,    xmin  = 4.5, xmax = 10, ymin = 0.5, ymax = 1)




theme = ttheme_default(base_size=14,
                       core = list(fg_params=list(fontface=c('bold','plain')),
                                   bg_params=list(fill=brewer.pal(9,'PuBu')[c(5,3)]))))



?gen_grob
tbl_overall<-tbl_survfit(survfit(TS_mortality ~ 1, ts_colon),
                       times = c(1, 5,8),
                       reverse=F,
                       label_header = "**{time} Year**")

tbl_group<-tbl_survfit(survfit(TS_mortality ~ surg, ts_colon),
                  times = c(1, 5,8),
                  reverse=F,
                  label_header = "**{time} Year**")


tbl_group
  remove_row_type(surg, type = "header")
?remove_row_type

?remove_row_type
tbl_stack(tbls = list(tbl_overall, tbl_group))
tbl_overall+tbl_group


?tbl_survfit
res<-anal_data%>%
  Get_KM2('TS_Death','',break.by=12,xmax=120,unit='Year',y_lim=c(0,1))


row1=paste0(map_chr(str_split(res$tbl[1,c(1,2,5,10)+1,1],' ± '),~.[[1]]),"%")


res_tbl<-rbind(
  c('Years','1','2','5','10'),
  c('Survival',row1))

res_tbl

?tableGrob

g1<-tableGrob(res_tbl, rows = NULL,
              theme = ttheme_default(base_size=14,
                                     core = list(fg_params=list(fontface=c('bold','plain')),
                                                 bg_params=list(fill=brewer.pal(9,'PuBu')[c(5,3)]))))

#colhead=list(bg_params=list(fill="red"))))

w_ratio=1.75
w_sum=w_ratio+4
g1$widths<-unit(c(w_ratio/w_sum,rep(1/w_sum,4)),'npc')
fig_surv_all<-res$fig+scale_x_continuous(labels=function(x)x/12,breaks = seq(0, 120, by = 24))+annotation_custom(g1,xmin=12,ymin=0.5,xmax=120,ymax=0.8)

fig_surv_all

ggsave(paste0('Result/fig_surv_all.',fig_format),plot=fig_surv_all,device=cairo_pdf,width=KM_save$width,height=KM_save$height,dpi=fig_dpi) 

