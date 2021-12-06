library(purrr)
library(broom)
library(tidyr)
library(grid)
library(scales)
library(stringr)



OnlyOne<-function(v_list){
  not_unique<-unique(v_list[duplicated(v_list)])
  return(!v_list%in%not_unique)
}


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



Plot_forest<-function(plot_df,main="Hazard ratio",fontsize=0.7,cpositions=c(0.02, 0.22, 0.4)){
    plot_df$N<-paste0('(N=',plot_df$N,')')
    plot_df$color<-if_else(plot_df$color=="","black",plot_df$color)
    plot_df$exp_estimate<-if_else(plot_df$ref,'reference',Get_Num(exp(plot_df$estimate)))
    plot_df$ci<-if_else(plot_df$ref,"",
              paste0('(',Get_Num(exp(plot_df$conf.low)),' - ',Get_Num(exp(plot_df$conf.high)),')'))
    plot_df <- plot_df[nrow(plot_df):1, ]
    
    
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
    


Forest_df<-function(model,data=NULL,refLabel='reference',noDigits=2,
                    use_label=F,group_arrange=1){
  conf.high <- conf.low <- estimate <- NULL
  if(is.null(data)) data<-eval(model$call$data)
  if(use_label) labels<-map_chr(data,~ifelse(is.null(attr(.x,'label')),"",attr(.x,'label')))
  
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
  
  if(use_label) {
    toShow$var<-if_else(labels[str_remove(toShow$var,'TRUE$')]=="",str_remove(toShow$var,'TRUE$'),labels[str_remove(toShow$var,'TRUE$')])
  } else {
    toShow$var<-str_remove(toShow$var,'TRUE$')
  }
  
if(group_arrange==1){ 
  ret_df<-toShow%>%
    filter(!ref)%>%
    arrange(desc(estimate))%>%
    mutate(color=if_else(estimate<0,muted('blue'),muted('red')))%>%
    transmute(var,level,N,ref,estimate,conf.low,conf.high,p.value,color)
    #Plot_forest()%>%
}else{
 ret_df<-toShow%>%
    filter(OnlyOne(var))%>%
    arrange(desc(estimate))%>%
    rbind(.,toShow%>%filter(!OnlyOne(var)))%>%
    mutate(var=if_else(duplicated(var),"",var))%>%
    mutate(color=if_else(estimate<0,muted('blue'),muted('red')))%>%
    transmute(var,level,N,ref,estimate,conf.low,conf.high,p.value,color)
 }
    #Plot_forest()%>%
    return(ret_df)
    
}  
