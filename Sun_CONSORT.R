library(purrr)
library(stringr)
library(dplyr)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)




# 0 null
# 1 none
# 2 forward
# 3 back
# 4 both

#node [shape = point, width = 0]


GetID<-function(r,c,n_C){return((r-1)*n_C+c)}

Init_nodes<-function(n_R,n_C){
  n_total=n_R*n_C
  data.frame(ID=c(1:n_total),name="",attr_label="''",attr_style='invis',attr_shape='point',attr_color='black',attr_width=1,attr_fontcolor='black')
}

Init_edges<-function(n_R,n_C){
  rbind(
    map_df(c(1:n_R),~data.frame(from=seq(from=((.x-1)*n_C+1),by=1,length.out=n_C-1),
                                to=seq(from=((.x-1)*n_C+2),by=1,length.out=n_C-1),type='H',ind=.x,attr_constraint='false')),
    map_df(c(1:n_C),~data.frame(from=seq(from=.x,by=n_C,length.out=n_R-1),
                                to=seq(from=.x+n_C,by=n_C,length.out=n_R-1),type='V',ind=.x,attr_constraint='true')))%>%
    mutate(ID=c(1:nrow(.)),attr_dir='none',attr_style='invis')%>%
    select(ID,everything())%>%
    return()
}


paste_comma<-function(...){
  paste(...,sep=',')
}

Get_attr<-function(g_df){
  df_attr<-names(g_df)[startsWith(names(g_df),'attr')]
  return(map(df_attr,~paste0(str_remove(.x,'attr_'),'=',g_df[[.x]]))%>%do.call(paste_comma,.))
}


Set_text_node<-function(ndf,text_df){
  #text_df<-text_df%>%mutate(ID=(r-1)*n_C+c)
  ndf$name[text_df$ID]<-text_df$name
  ndf$attr_label[text_df$ID]<-paste0("'",text_df$label,"'")
  ndf$attr_style[text_df$ID]='solid'
  ndf$attr_shape[text_df$ID]=text_df$attr_shape
  ndf$attr_color[text_df$ID]=text_df$attr_color
  ndf$attr_width[text_df$ID]=2
  return(ndf)
}

Set_plain_node<-function(ndf,plain_df){
  #text_df<-text_df%>%mutate(ID=(r-1)*n_C+c)
  ndf$name[plain_df$ID]<-plain_df$name
  ndf$attr_label[plain_df$ID]<-paste0("'",plain_df$label,"'")
  ndf$attr_style[plain_df$ID]='solid'
  ndf$attr_shape[plain_df$ID]='plaintext'
  ndf$attr_width[plain_df$ID]=2
  return(ndf)
}
Set_null_node<-function(ndf,null_df){
  #null_df<-null_df%>%mutate(ID=(r-1)*n_C+c)
  ndf$name[null_df$ID]=null_df$name
  ndf$attr_style[null_df$ID]='solid'
  ndf$attr_shape[null_df$ID]='point'
  ndf$attr_width[null_df$ID]=0
  return(ndf)
}

Set_edge<-function(edf,edge_data){
  edf$attr_style[edge_data$eID]="''"
  edf$attr_dir[edge_data$eID]=edge_data$attr_dir
  return(edf)
}



#Decode<-function(etype,eind,code){
#  max_dig<-ifelse(etype=='V',n_R-1,n_C-1)
#  #print(max_dig)
#  decode<-map_dbl(c(1:max_dig),~floor((code%%(5^.x))/(5^(.x-1))))%>%
#    rev()
#  
#  eID<-edge_df%>%
#    filter(type==etype,ind==eind)%>%.$ID
  
#  data.frame(eID,decode)%>%return()
#}

Get_CONSORT<-function(fname){
fdata<-read.delim(fname,header=F,sep=" ")

n_R<-as.numeric(fdata[1,1])
n_C<-as.numeric(fdata[1,2])


text_node=null_node=plain_node=data.frame()

n_text<-as.numeric(fdata[2,1])
f_ind=3

if (n_text>0) text_node<-data.frame(name=fdata[seq(f_ind,f_ind+n_text-1),1],
           r=as.numeric(fdata[seq(f_ind,f_ind+n_text-1),2]),
           c=as.numeric(fdata[seq(f_ind,f_ind+n_text-1),3]),
           attr_shape=fdata[seq(f_ind,f_ind+n_text-1),4],
           attr_color=fdata[seq(f_ind,f_ind+n_text-1),5],
           label=fdata[seq(f_ind,f_ind+n_text-1),6],
           type='T')%>%mutate(ID=GetID(r,c,n_C))
n_null<-as.numeric(fdata[f_ind+n_text,1])
f_ind=f_ind+n_text+1
if (n_null>0) null_node<-data.frame(name=fdata[seq(f_ind,f_ind+n_null-1),1],
                      r=as.numeric(fdata[seq(f_ind,f_ind+n_null-1),2]),
                      c=as.numeric(fdata[seq(f_ind,f_ind+n_null-1),3]),
                      attr_shape="",
                      attr_color="",
                      label="",
                      type='N')%>%mutate(ID=GetID(r,c,n_C))


n_plain<-as.numeric(fdata[f_ind+n_null,1])
f_ind=f_ind+n_null+1

if (n_plain>0) plain_node<-data.frame(name=fdata[seq(f_ind,f_ind+n_plain-1),1],
                      r=as.numeric(fdata[seq(f_ind,f_ind+n_plain-1),2]),
                      c=as.numeric(fdata[seq(f_ind,f_ind+n_plain-1),3]),
                      attr_shape="",
                      attr_color="",
                      label=fdata[seq(f_ind,f_ind+n_plain-1),4],
                      type='P')%>%mutate(ID=GetID(r,c,n_C))

total_nodes<-rbind(text_node,null_node,plain_node)

n_edge<-as.numeric(fdata[f_ind+n_plain,1])
f_ind=f_ind+n_plain+1


if(n_edge>0) raw_edge<-data.frame(from=fdata[seq(f_ind,f_ind+n_edge-1),1],
           to=fdata[seq(f_ind,f_ind+n_edge-1),2],
           dir=as.numeric(fdata[seq(f_ind,f_ind+n_edge-1),3]))%>%
            mutate(from_ID=total_nodes$ID[map_dbl(from,~which(total_nodes$name==.x))],
                   to_ID=total_nodes$ID[map_dbl(to,~which(total_nodes$name==.x))])

node_df<-Init_nodes(n_R,n_C)%>%
  Set_text_node(text_node)%>%
  Set_plain_node(plain_node)%>%
  Set_null_node(null_node)


i_edge_df<-Init_edges(n_R,n_C)

edge_df<-raw_edge%>%
  mutate(eID=map2_dbl(from_ID,to_ID,~i_edge_df%>%filter(from==.x,to==.y)%>%.$ID),
        attr_dir=case_when(
           dir==2~'forward',
           dir==3~'back',
           dir==4~'both',
           TRUE~'none'))%>%
  Set_edge(i_edge_df,.)
  


consort_graph<-list(n_row=n_R,n_col=n_C,node=node_df,edge=edge_df)
return(consort_graph)
}


Consort2Graphviz<-function(consort_list){
  node_graphviz<-with(consort_list$node,
                      #paste0("N",ID," [label='",label,"',style=",style,",shape=",shape,"]")
                      paste0("N",ID," [",Get_attr(consort_list$node),"]")
  )%>%paste(collapse="\n")
  edge_graphviz<-with(consort_list$edge,
                      #paste0("N",from,"->N",to," [constraint=",constraint,"]")#
                      paste0("N",from,"->N",to," [",Get_attr(consort_list$edge),"]")#
  )%>%paste(collapse="\n")
  return(paste(c("digraph CONSORT {\ngraph [fontname = Helvetica]",node_graphviz,edge_graphviz,"}"),collapse ="\n"))
}




consort<-Get_CONSORT("graph.txt")

consort$node$attr_label[consort$node$name=='A']="'Sibal'"
consort$node$attr_label
FC<-Consort2Graphviz(consort)
grViz(FC)

grViz(FC)%>%
  #grViz(flowchart)%>%
  export_svg() %>%
  charToRaw() %>%
  #rsvg_png('Result/study_flow_chart.png')
  rsvg_eps('test.eps')

consort
edge_data<-map_df(c(1:nrow(edge_code)),~Decode(edge_code$type[.x],edge_code$ind[.x],edge_code$code[.x]))%>%
  filter(decode!=0)%>%
  mutate(attr_dir=case_when(
    decode==2~'forward',
    decode==3~'back',
    decode==4~'both',
    TRUE~'none'
  ))
edge_df<-write_edge(edge_df,edge_data)








#node_rank<-map_chr(c(1:n_R),~paste(paste0('N',seq(from=(.x-1)*n_C+1,length.out=n_C)),collapse=" "))%>%
#  paste0('{rank=same;',.,'}')%>%paste(collapse="\n")

#flowchart<-paste(c("digraph diag {  graph [fontname = Helvetica]",node_graphvis,node_rank,edges,"}"),collapse ="\n")
flowchart<-paste(c("digraph diag {  graph [fontname = Helvetica]",node_graphvis,edge_graphvis,"}"),collapse ="\n")


flowchart%>%cat()
grViz(flowchart)
grViz(flowchart)%>%
  #grViz(flowchart)%>%
  export_svg() %>%
  charToRaw() %>%
  #rsvg_png('Result/study_flow_chart.png')
  rsvg_eps('test.eps')
#rsvg()%>%
#rsvg(width = 20 *(300/2.54), height = 16 *(300/2.54))%>%
#tiff::writeTIFF("test.tiff", bits.per.sample = 8L)
