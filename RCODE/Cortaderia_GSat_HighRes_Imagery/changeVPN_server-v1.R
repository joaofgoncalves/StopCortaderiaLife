
suspendTimeSeconds <- 60*30

countriesList <- c("Portugal","Spain","France","Switzerland",
"Belgium","Germany","Italy","Ireland","Luxembourg","Netherlands")
#probs <- c(0.5, 0.2, 0.1, 0.05, 0.05, 0.05, 0.05)

#length(countriesList)
#length(probs)
#sum(probs)

for(i in 1:1000){
  ti <- format(Sys.time())
  #rnd <- sample(1:length(countriesList),1, prob = probs)
  rnd <- sample(1:length(countriesList),1)
  
  cmd_disconnect <- paste("C:/Progra~1/NordVPN/NordVPN -d", sep="")
  cmd_connect <- paste("C:/Progra~1/NordVPN/NordVPN -c -g \"",countriesList[rnd],"\"",sep="")
  
  cat("-> [",i,"]",ti,"Disconnecting VPN..... \n")
  suppressWarnings(try(shell(cmd_disconnect)))
  Sys.sleep(5)

  ti <- format(Sys.time())
  cat("-> [",i,"]",ti,"Connecting VPN to server in country:", countriesList[rnd],"..... \n\n")  
  suppressWarnings(shell(cmd_connect))
  
  Sys.sleep(suspendTimeSeconds)
  
}
