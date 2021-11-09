#   plotname <- "301 Infektionsweg"

# Speichert Plot in Drei versionen
mkr.saveplotCORONA <- function(plotname,datum,pwidth = 10,pheight = 7,format = "jpg"){
  
  for (picfmt in format) {
    
    pname<-paste0("Plots/CORONA in Jena - ",plotname,".",picfmt)
    ggsave(pname,width = pwidth,height = pheight)
    pname<-paste0("Plots/Archiv/",datum," CORONA in Jena - ",plotname,".",picfmt)
    ggsave(pname,width = pwidth,height = pheight)
    pnameJena<-paste0("PlotsJena/CORONA in Jena - ",plotname)
    mkr.jenaplot(pname, pnameJena, img_scale = "1500", img_kind = picfmt)
    pnameJena<-paste0("PlotsJena/Archiv/",datum," CORONA in Jena - ",plotname)
    mkr.jenaplot(pname, pnameJena, img_scale = "1500", img_kind = picfmt)
  }
  
}




mkr.formatnumber<- function(number, decimals= 1){
  formatC(number,digits=decimals,format="f")
}

# Speichert Plot in Drei versionen
mkr.saveplotCORONASB <- function(plotname,datum,pwidth = 10,pheight = 7){
  
  pname<-paste0("Steuerungsbericht/CORONA in Jena - ",plotname,".png")
  ggsave(pname,width = pwidth,height = pheight)
  pname<-paste0("Steuerungsbericht/Plots/Archiv/",datum," CORONA in Jena - ",plotname,".png")
  ggsave(pname,width = pwidth,height = pheight)
  pnameJena<-paste0("Steuerungsbericht/Plots/PlotsJena/",datum," CORONA in Jena - ",plotname)
  mkr.jenaplot(pname, pnameJena, img_scale = "1500", img_kind = "png")
  
}
