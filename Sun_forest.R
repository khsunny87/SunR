library(purrr)

MV_COX_res
ggforest(MV_COX_res)
lung$sex
tmp_lung<-lung%>%
  mutate(TS=Surv(time,status))%>%
  mutate(karno_group=case_when(
    ph.karno<70~'A',
    ph.karno<90~'B',
    TRUE~'C'
  ))%>%
  mutate(male=(sex==1))

tmp_model<-coxph(TS~age+male+ph.ecog+karno_group,data=tmp_lung)

tmp_model

ggforest(tmp_model)

coef(tmp_model)

hist(tmp_lung$ph.karno)


ggforest(coxph(Surv(lung$time,lung$status)~age+sex+ph.ecog+ph.karno,data=lung))


Render_P<-function(pval,p_digit=3,
                   starring=T,star_mark='*',mark_int=c(0.05,0.01,0.001)){
  
  min_lim<-10^(-p_digit)
  p_char<-case_when(
    is.na(pval)~"",
    pval<min_lim~paste0('<',min_lim),
    TRUE~format(round(pval,p_digit))
  )
  if(!starring) return(p_char)
  
  p_mark<-map_chr(pval,~paste(if_else(!is.na(.x)&.x<mark_int,star_mark,""),collapse=""))
  return(paste0(p_char,p_mark))
}

Get_Num<-function(num,digits=2){
  return(format(round(num,digits=digits)))
}

Render_P(plot_df$pval)

plot_df<-tribble(
  ~var,~level,~N,~ref,~estimate,~conf.low,~conf.high,~p.value,~color,
  'BSI',"",252,F,0.600202799,0.094973050,1.1054325470,0.02034,muted("red"),
  'ECPR',"",252,F,0.718367093,0.237792381,1.1989418063,0.00265,"blue",
  'VV_ECMO',"",252,F,0.592377,0.011803854,1.1729497939,0.04552,"",
  'Segmented',"",252,F,-0.017576,-0.033777174,-0.0013742078,0.03349,"orange"
)

Plot_forest(plot_df)

Plot_forest<-function(plot_df,main="Hazard ratio",fontsize=0.7){
  
#plot_df <- input_df[nrow(input_df):1, ]
plot_df$N<-paste0('(N=',plot_df$N,')')
plot_df$color<-if_else(plot_df$color=="","black",plot_df$color)
plot_df$exp_estimate<-if_else(plot_df$ref,'reference',Get_Num(exp(plot_df$estimate)))
plot_df$ci<-if_else(plot_df$ref,"",
          paste0('(',Get_Num(exp(plot_df$conf.low)),' - ',Get_Num(exp(plot_df$conf.high)),')'))
plot_df <- plot_df[nrow(plot_df):1, ]


cpositions=c(0.02, 0.22, 0.4)



rangeb <- range(plot_df$conf.low, plot_df$conf.high, na.rm = TRUE)
breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
rangeplot <- rangeb
# make plot twice as wide as needed to create space for annotations
rangeplot[1] <- rangeplot[1] - diff(rangeb)
# increase white space on right for p-vals:
rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)

width <- diff(rangeplot)
# y-coordinates for labels:
y_variable <- rangeplot[1] +  cpositions[1] * width
y_nlevel <- rangeplot[1]  +  cpositions[2] * width
y_cistring <- rangeplot[1]  +  cpositions[3] * width
y_stars <- rangeb[2]
x_annotate <- seq_len(nrow(plot_df))

# geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
annot_size_mm <- fontsize *
  as.numeric(convertX(unit(theme_get()$text$size, "pt"), "mm"))


ret<-plot_df%>%
  ggplot(aes(seq_along(var), exp(estimate)))+
  geom_rect(aes(xmin = seq_along(var) - .5, xmax = seq_along(var) + .5,
                ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                fill = ordered(seq_along(var) %% 2 + 1))) +
  scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none")+
  geom_point(pch = 15, size = 4,color=plot_df$color)+
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15,color=plot_df$color)+
  geom_hline(yintercept = 1, linetype = 3) +
  coord_flip(ylim = exp(rangeplot)) +
  ggtitle(main)+
  scale_y_log10(
    name = "",
    labels = sprintf("%g", breaks),
    expand = c(0.02, 0.02),
    breaks = breaks) +
  theme_light() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        panel.border=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  xlab("")+
  annotate(geom = "text", x = x_annotate, y = exp(y_variable),
           label = plot_df$var, fontface = "bold", hjust = 0,
           size = annot_size_mm)+
  annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
           label = plot_df$level, vjust = -0.1, size = annot_size_mm)+
  annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
           label = plot_df$N, fontface = "italic", hjust = 0,
           vjust = ifelse(plot_df$level == "", .5, 1.1),
           size = annot_size_mm)+
  annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
           label = plot_df$exp_estimate, size = annot_size_mm,
           vjust = ifelse(plot_df$exp_estimate== "reference", .5, -0.1))+
  annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
           label = plot_df$ci, size = annot_size_mm,
           vjust = 1.1,  fontface = "italic")+
  annotate(geom = "text", x = x_annotate, y = exp(y_stars),
           label = Render_P(plot_df$p.value), size = annot_size_mm,
           hjust = -0.2,  fontface = "italic")
  

return(ret)
}



model<-tmp_model
model<-MV_COX_res
model<-MV_LR_res

data = NULL
main = "Hazard ratio"
cpositions=c(0.02, 0.22, 0.4)
fontsize = 0.7
refLabel = "reference"
noDigits=2



conf.high <- conf.low <- estimate <- NULL
if(is.null(data)) data<-eval(model$call$data)

#labels<-map_chr(data,~ifelse(is.null(attr(.x,'label')),"",attr(.x,'label')))


terms <- attr(model$terms, "dataClasses")[-1]
coef <- as.data.frame(tidy(model, conf.int = TRUE))
gmodel <- glance(model)




allTermsDF<-map_df(names(terms),function(.x){
  if (terms[.x] %in% c("factor", "character")) {
    adf <- as.data.frame(table(data[, .x]))
    cbind(var = .x, adf, ref=c(T,rep(F,nrow(adf)-1)),pos = 1:nrow(adf))
  }
  else if (terms[.x] == "numeric") {
    data.frame(var = .x, Var1 = "", Freq = nrow(data),ref=F,pos = 1)
  }
  else {
    vars = grep(paste0("^", .x, "*."), coef$term, value=TRUE)
    data.frame(var = vars, Var1 = "", Freq = nrow(data),ref=F,pos = seq_along(vars))
    #data.frame(var = .x, Var1 = "", Freq = nrow(data),pos = seq_along(vars))
  }
})

  colnames(allTermsDF) <- c("var", "level", "N", "ref","pos")
  inds<-str_trim(paste0(allTermsDF$var,allTermsDF$level))
  rownames(coef) <- gsub(coef$term, pattern = "`", replacement = "")
  toShow <- cbind(allTermsDF, coef[inds,])[,c("var", "level", "N", "ref","p.value", "estimate", "conf.low", "conf.high", "pos")]
  toShow$estimate[is.na(toShow$estimate)]<-0
  toShow$var[duplicated(toShow$var)] = ""

  tmp<-toShow%>%
    mutate(color=if_else(estimate<0,muted('blue'),muted('red')))%>%
  transmute(var,level,N,ref,estimate,conf.low,conf.high,p.value,color)
  tmp%>%
  Plot_forest()
  
  
  toShowExp <- toShow%>%
    select(estimate,conf.low,conf.high)
  toShowExp[is.na(toShowExp)] <- 0
  toShowExp <- format(exp(toShowExp), digits=noDigits)
  toShowExpClean <- data.frame(toShow%>%select(-ref),
                               pvalue = signif(toShow$p.value,noDigits+1),
                               toShowExp)
  toShowExpClean$stars <- paste0(round(toShowExpClean$p.value, noDigits+1), " ",
                                 ifelse(toShowExpClean$p.value < 0.05, "*",""),
                                 ifelse(toShowExpClean$p.value < 0.01, "*",""),
                                 ifelse(toShowExpClean$p.value < 0.001, "*",""))
  toShowExpClean$ci <- paste0("(",toShowExpClean[,"conf.low.1"]," - ",toShowExpClean[,"conf.high.1"],")")
  toShowExpClean$estimate.1[is.na(toShowExpClean$estimate)] = refLabel
  toShowExpClean$stars[which(toShowExpClean$p.value < 0.001)] = "<0.001 ***"
  toShowExpClean$stars[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$ci[is.na(toShowExpClean$estimate)] = ""
  toShowExpClean$estimate[is.na(toShowExpClean$estimate)] = 0
  toShowExpClean$var = as.character(toShowExpClean$var)
  toShowExpClean$var[duplicated(toShowExpClean$var)] = ""
  # make label strings:
  toShowExpClean$N <- paste0("(N=",toShowExpClean$N,")")
  
  #flip order
  toShowExpClean <- toShowExpClean[nrow(toShowExpClean):1, ]
  
  tmp<-toShowExpClean$var
  
  str_remove(tmp,'TRUE$')
  

  
  rangeb <- range(toShowExpClean$conf.low, toShowExpClean$conf.high, na.rm = TRUE)
  breaks <- axisTicks(rangeb/2, log = TRUE, nint = 7)
  rangeplot <- rangeb
  # make plot twice as wide as needed to create space for annotations
  rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # increase white space on right for p-vals:
  rangeplot[2] <- rangeplot[2] + .15 * diff(rangeb)
  
  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_nlevel <- rangeplot[1]  +  cpositions[2] * width
  y_cistring <- rangeplot[1]  +  cpositions[3] * width
  y_stars <- rangeb[2]
  x_annotate <- seq_len(nrow(toShowExpClean))
  
  # geom_text fontsize is in mm (https://github.com/tidyverse/ggplot2/issues/1828)
  annot_size_mm <- fontsize *
    as.numeric(convertX(unit(theme_get()$text$size, "pt"), "mm"))
  
  #p <- 
    ggplot(toShowExpClean, aes(seq_along(var), exp(estimate))) +
    geom_rect(aes(xmin = seq_along(var) - .5, xmax = seq_along(var) + .5,
                  ymin = exp(rangeplot[1]), ymax = exp(rangeplot[2]),
                  fill = ordered(seq_along(var) %% 2 + 1))) +
    scale_fill_manual(values = c("#FFFFFF33", "#00000033"), guide = "none") +
    geom_point(pch = 15, size = 4) +
    geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.15) +
    geom_hline(yintercept = 1, linetype = 3) +
    coord_flip(ylim = exp(rangeplot)) +
    ggtitle(main) +
    scale_y_log10(
      name = "",
      labels = sprintf("%g", breaks),
      expand = c(0.02, 0.02),
      breaks = breaks) +
    theme_light() +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          panel.border=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    annotate(geom = "text", x = x_annotate, y = exp(y_variable),
             label = toShowExpClean$var, fontface = "bold", hjust = 0,
             size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_nlevel), hjust = 0,
             label = toShowExpClean$level, vjust = -0.1, size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_nlevel),
             label = toShowExpClean$N, fontface = "italic", hjust = 0,
             vjust = ifelse(toShowExpClean$level == "", .5, 1.1),
             size = annot_size_mm) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
             label = toShowExpClean$estimate.1, size = annot_size_mm,
             vjust = ifelse(toShowExpClean$estimate.1 == "reference", .5, -0.1)) +
    annotate(geom = "text", x = x_annotate, y = exp(y_cistring),
             label = toShowExpClean$ci, size = annot_size_mm,
             vjust = 1.1,  fontface = "italic") +
    annotate(geom = "text", x = x_annotate, y = exp(y_stars),
             label = toShowExpClean$stars, size = annot_size_mm,
             hjust = -0.2,  fontface = "italic") +
    annotate(geom = "text", x = 0.5, y = exp(y_variable),
             label = paste0("# Events: ", gmodel$nevent, "; Global p-value (Log-Rank): ",
                            format.pval(gmodel$p.value.log, eps = ".001"), " \nAIC: ", round(gmodel$AIC,2),
                            "; Concordance Index: ", round(gmodel$concordance,2)),
             size = annot_size_mm, hjust = 0, vjust = 1.2,  fontface = "italic")
  # switch off clipping for p-vals, bottom annotation:
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  # invisible(p)
  ggpubr::as_ggplot(gt)
}
