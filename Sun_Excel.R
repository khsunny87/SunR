library(Microsoft365R)
library(stringr)

Get_Excel_from_Onedrive<-function(proj_name,input_path='Input/Raw'){

#od <- get_personal_onedrive(auth_type="device_code") #Ubunbu
od <- get_personal_onedrive() #Mac

fpath<-paste0('My journal/OA/',proj_name,'/Data/')
fname<-paste0(proj_name,'_data.xlsx')
#od$list_items(fpath)


destfile <- fname

od$download_file(paste0(fpath,fname),paste0(input_path,'/',destfile),overwrite=T)

}
