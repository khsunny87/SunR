

euro_coef<-c(
  Age=0.0285181,
  NYHA_II=0.1070545,
  NYHA_III=0.2958358,
  NYHA_IV=0.5597929,
  CCS4=0.2226147,
  IDDM=0.3542749,
  Female=0.2196434,
  ECA=0.5360268,
  CPD=0.1886564,
  pMobility=0.2407181,
  Redo=1.1185990,
  
  Renal_dialysis=0.6421508,
  Renal_severe=0.8592256,
  Renal_moderate=0.3035530,
  
  Endocarditis=0.6194522,
  Critical=1.0865170,
  EF_moderate=0.3150652,
  EF_poor=0.8084096,
  EF_very=0.9346919,
  rMI=0.1528943,
  PH_moderate=0.1788899,
  PH_severe=0.3491475,
  
  Urgency=0.3174673,
  Emergency=0.7039121,
  Salvage=1.3629470,
  
  non_CABG=0.0062118,
  Double=0.5521478,
  Triple=0.9724533,
  
  Aorta=0.6527205)




pt_risk<-list(Age=84)
risks=pt_risk



Euro_score_II<-function(risks){
  sum_coef<--5.324537
  
  
  sum_coef=sum_coef+ifelse(risks$Age<61,euro_coef['Age'],(risks$Age-59)*euro_coef['Age'])

  sum_coef=sum_coef+case_when(
    risks$NYHA=='II'~euro_coef['NYHA_II'],
    risks$NYHA=='III'~euro_coef['NYHA_III'],
    risks$NYHA=='IV'~euro_coef['NYHA_IV'],
    TRUE~0)
  if(risks$CCS4==T)  sum_coef=sum_coef+euro_coef['CCS4']
  if(risks$IDDM==T)  sum_coef=sum_coef+euro_coef['IDDM']
  if(risks$Female==T)  sum_coef=sum_coef+euro_coef['Female']
  if(risks$ECA==T)  sum_coef=sum_coef+euro_coef['ECA']
  if(risks$CPD==T)  sum_coef=sum_coef+euro_coef['CPD']
  if(risks$pMobility==T)  sum_coef=sum_coef+euro_coef['pMobility']
  if(risks$Redo==T)  sum_coef=sum_coef+euro_coef['Redo']
  
  
  sum_coef=sum_coef+case_when(
    risks$Renal_CC==-1~euro_coef['Renal_dialysis'],
    risks$Renal_CC<50~euro_coef['Renal_severe'],
    risks$Renal_CC<=85~euro_coef['Renal_moderate'],
    TRUE~0)
  
  if(risks$Endocarditis==T)  sum_coef=sum_coef+euro_coef['Endocarditis']
  if(risks$Critical==T)  sum_coef=sum_coef+euro_coef['Critical']
  
  sum_coef=sum_coef+case_when(
    risks$EF<=20~euro_coef['EF_very'],
    risks$EF<=30~euro_coef['EF_severe'],
    risks$EF<=50~euro_coef['EF_moderate'],
    TRUE~0)
  
  if(risks$rMI==T)  sum_coef=sum_coef+euro_coef['rMI']
  
  sum_coef=sum_coef+case_when(
    risks$PH>55~euro_coef['PH_severe'],
    risks$PH>=31~euro_coef['PH_moderate'],
    TRUE~0)

  sum_coef=sum_coef+case_when(
    risks$Urgency=='Urgency'~euro_coef['Urgency'],
    risks$Urgency=='Emergency'~euro_coef['Emergency'],
    risks$Urgency=='Salvage'~euro_coef['Salvage'],
    TRUE~0)
  
  sum_coef=sum_coef+case_when(
    risks$Weight==1~euro_coef['non_CABG'],
    risks$Weight==2~euro_coef['Double'],
    risks$Weight==3~euro_coef['Triple'],
    TRUE~0)

  if(risks$Aorta==T)  sum_coef=sum_coef+euro_coef['Aorta']

    
  return(exp(sum_coef)/(1+exp(sum_coef)))
}

Euro_score_II(list(Age=43,Female=F,CPD=F,ECA=T,pMobility=F,Redo=F,Endocarditis=T,Critical=T,Renal_CC=100,IDDM=T,
                   CCS4=T,EF=35,rMI=F,PH=20,NYHA='II',
                   Aorta=T,Urgency='Elective',Weight=2))*100

library(purrr)

?split(
  
)
split(mtcars[1:5,],c(1:5))

