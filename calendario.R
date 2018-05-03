if(require(pacman)){
  p_load(bizdays,data.table,dplyr,tibble)
}else{
  install.packages(pacman)
  pacman::p_load(bizdays,data.table,dplyr,tibble)
}


#feriados = fread("feriados_nacionais.csv",sep = ";",encoding = "Latin-1")
#datas = stringr::str_split(feriados$Data,pattern = "/") 

# for(i in 1:length(datas)){
#   aux = paste(datas[[i]][3],datas[[i]][2],datas[[i]][1],sep = "-")
#   aux = aux[[1]]       
#   datas[i] = aux
# }
# feriados$Data = datas
# rm(i,aux)  
#   
# feriados_sabados = feriados %>% filter(Dia.da.Semana == "sábado")


dias_uteis <- function(from = "2001-01-01",to ='2050-12-31' ,fds = c("saturday","sunday")){
  
  if(as.Date(from) < "2001-01-01"){
    stop("Dias uteis abaixo de janeiro de 2001 indisponiveis!")
  }
  
  cal  <- create.calendar("Brazil/ANBIMA", holidays=holidaysANBIMA, weekdays=fds)
  dias  = bizseq(from, to, cal)
  datas = seq.Date(as.Date(from),as.Date(to),by = 'month')
  
  dias_d = tibble(
    dias = dias,
    x = NA
  )
  
  final = tibble(
    data = datas,
    count = NA
  )
  
  for(i in 1:nrow(final)){
    count = dias_d %>% filter(format(dias,"%Y-%m") == format(final$data[i],"%Y-%m")) %>% select(dias) %>% count()
    print(paste(i,"-",final$data[i],"-","dias uteis:",count,sep=" "))
    final$count[i] = count
  }
  
  ano = format(from,"%Y") %>% as.numeric()
  mes  = format(from,"%m") %>% as.numeric()
  serie_dias_uteis = ts(final$count,start = c(ano,mes),frequency = 12)
  return(serie_dias_uteis)
}


