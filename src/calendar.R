if(require(pacman)){
  p_load(bizdays,data.table,dplyr,tibble,glue,BETS)
}else{
  install.packages(pacman)
  pacman::p_load(bizdays,data.table,dplyr,tibble,glue,BETS)
}




feriadosFunctionDummy <- function(feriadotipo){
  feriados = read.csv2("data/feriados_nacionais.csv",encoding = "Latin-1",stringsAsFactors = F)
  for (i in 1:nrow(feriados)) {
    if (feriados$Dia.da.Semana[i] == "segunda-feira" && feriados$Feriado[i] =="Carnaval"){
      feriados[i,1] = NA
    }
  }
  feriados$Data  = feriados$Data %>% as.character()
  '%out%' <- function(x,y){!(x %in% y)}
  feriadoLista = unique(feriados$Feriado)
  if(any(feriadotipo %out% feriadoLista)){ 
    print(glue('Tipo de feriado inserido nao esta disponivel'))
    glue("|>{feriadoLista}")
    stop()
  }else{
    datas = feriados %>% 
        filter(Feriado %in% feriadotipo) %>% 
        pull(Data)
    datas = as.Date(datas,'%d/%m/%Y')
    anos  = as.numeric(str_extract(datas,'\\d{4}'))
    meses  = as.numeric(month(datas))
    lista = list()
    for(i in 1:length(anos)){
      lista[[i]] = c(anos[i],meses[i])
    }
    dummy = BETS::dummy(start = c(2001,01) ,end = c(2078,12),date = lista,frequency = 12)
    return(dummy) 
  }
  
}






trading_days <- function(from = "2001-01-01",to ='2050-12-31',feriados = NULL ,fds = c("saturday","sunday"),ts = F){
  
  if(as.Date(from) < "2001-01-01"){
    stop("Dias uteis abaixo de janeiro de 2001 indisponiveis!")
  }
  
  if(!is.null(feriados)){
    cal  <- create.calendar("Brazil/ANBIMA", holidays=feriados, weekdays=fds,financial =F)
  }else{
    cal  <- create.calendar("Brazil/ANBIMA", holidays=feriados_datas, weekdays=fds,financial =F)
  }
  
  
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
    message(glue({i} - {final$data[i]} - dias uteis: {count})
    final$count[i] = count
  }
  
  
  if(ts){
    ano = format(from,"%Y") %>% as.numeric()
    mes  = format(from,"%m") %>% as.numeric()
    serie_dias_uteis = ts(final$count,start = c(ano,mes),frequency = 12)  
    return(serie_dias_uteis)
  }
  
  return(final)
}



dummy = feriadosFunctionDummy(feriadotipo = c('Carnaval','Corpus Christi'))


calendario = trading_days(feriados = feriados_datas,fds = c("sunday"))
calendario_ts = ts(calendario$count,start = c(2001,01),frequency = 12)
