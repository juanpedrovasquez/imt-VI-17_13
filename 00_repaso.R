# LIBRERIAS Y VALORES INICIALES -------------------------------------------
library(RMySQL) #para conectarse con la BD
library(doMC) #para cómputo paralelo
library(AMORE) #para RNAf
library(stats)

# OPCIONES PARA MODIFICAR -------------------------------------------------
dummy=1 #1 a 7 rap maxima
for (arranque in c(0,50,100,150,200)){
  # arranque=0 #puntos anteriores
  numPuntos <- 50 #reduce complejidad del problema
  registerDoMC(cores = 7) #4 bueno para Core-i5
  tope <- 21 #número de casos de elevación inicial
  colObj <- 6 #6 rapMax o 7 para rapMedia
  n <- 4 #divisiones de parámetros propios de la RNAf
  archiDatos <- "normP.txt"
  set.seed(12) #fija la semilla de aleatorios
  setwd("/home/spock/bfrCrsh/Documents/2013-06-28-proyVI17-13/zonas")
  
  # FUNCION PARA NORMALIZAR CONTINUOS ---------------------------------------
  norma <- function(original) {
    normalizado <- (original - mean(original))/sd(original)
    return(normalizado)
  }
  
  # ARCHIVOS DE CORRIENTES Y ELEVACIONES INICIALES --------------------------
  archisV <- list.files(path = "../datos/", pattern = "corr_") #CARGA ARCHIVOS
  ETHA0 <- matrix(nrow = length(archisV), ncol = 1)
  for (i in 1:length(archisV)) {
    ETHA0[i, 1] <- as.numeric(substr(archisV[i], start = 6, stop = 9))/100
  }
  # MATRICES VACÍAS 
  rMax <- matrix(nrow = numPuntos, ncol = dim(ETHA0)[1])
  rMedia <- matrix(nrow = numPuntos, ncol = dim(ETHA0)[1])
  e0 <- matrix(nrow = numPuntos, ncol = dim(ETHA0)[1])
  # LEE LAS COORDENADAS DE LOS PUNTOS DE MODELACION
  cc <- read.table("../datos/TranspuestasXYZ.dat")
  cc <- cc[(arranque+1):(arranque+numPuntos), ] #######sólo los puntos efectivos
  
  # PROCESO DE CADA ARCHIVO DE CORRIENTES -----------------------------------
  for (i in 1:dim(ETHA0)[1]) {
    a <- paste("../datos/", archisV[i], sep = "")
    d <- read.table(a)  
    d <- d[, (arranque+1):(arranque+numPuntos)] #######sólo los puntos efectivos
    vx <- d[1:59, ] #componente x
    vy <- d[60:118, ] #componente y
    v <- sqrt(vx * vx + vy * vy) #rapidez
    # RAPIDEZ EN CADA PUNTO (RENGLON) POR CADA EXPERIMENTO (COLUMNA)
    rMedia[, i] <- matrix(sapply(v, mean), ncol = 1)
    rMax[, i] <- matrix(sapply(v, max), ncol = 1)
    e0[, i] <- matrix(rep(ETHA0[i], numPuntos), ncol = 1)
    if (i <= (dim(ETHA0)[1] - 1)) {
      cc0 <- read.table("../datos/TranspuestasXYZ.dat") #repite lectura de coordenadas
      cc0 <- cc0[(arranque+1):(arranque+numPuntos), ] #######sólo los puntos efectivos
      cc <- rbind(cc, cc0)
    }
  }
  
  # TRANSFORMACION A VECTORES -----------------------------------------------
  rapMax <- matrix(as.vector(rMax), ncol = 1)
  rapMedia <- matrix(as.vector(rMedia), ncol = 1)
  names(cc) <- c("lon", "lat", "z")
  puntos <- matrix(rep((arranque+1):(arranque+numPuntos), dim(ETHA0)[1]), ncol = 1)
  elevac0 <- matrix(as.vector(e0), ncol = 1)
  P <- cbind(puntos, elevac0, cc, rapMax, rapMedia) #junta vectores
  P <- P[order(P[, 2], P[, 1]), ] #ordena por elevac. inic. y núm. pto. modelac.
  
  # NORMALIZACION DE CONTINUOS ----------------------------------------------
  normP <- P #copia la matriz P
  for (j in 1:dim(P)[2]) {
    # normaliza cada columna
    normP[, j] <- matrix(norma(P[, j]), ncol = 1)
  }
  
  # SALIDA A NORMP.TXT ------------------------------------------------------
  write.table(P, "P.txt", row.names = FALSE) #sin normalizar
  write.table(normP, "normP.txt", row.names = FALSE) #NORMALIZADO
  
  # CONJUNTOS DOMINIO -------------------------------------------------------
  numCasosP <- rev(c(2,7,20)) #casos de aprendizaje
  colsP <- rev(list(c(1,2),c(1,2,5),c(1, 2, 3, 4, 5))) #columnas de entrada
  colsQ <- rev(list(c(colObj),c(colObj,1),c(colObj,1,2),c(colObj,2,5))) #columnas de salida
  LR <- c(0.02, 0.002, 2e-04, 2e-05) #tasas de aprendizaje
  MG <- c(0.05, 0.005, 5e-04, 5e-05) #momenta
  SS <- c(100, 500, 1000, 9000) #pasos de entrenamiento
  FF <- c(2, 2.25, 2.5, 2.75, 3, 3.25) #factores p/núm. neuronas capa oculta
  picked <- 0 #bandera: combinación en proceso
  done <- 0 #bandera: combinación ya procesada
  
  # PREPARACIÓN DE BASE DE DATOS --------------------------------------------
  base <- "corridas"
  usuario <- "userRNAf"
  passw <- "imt123"
  bd <- dbConnect(MySQL(), user = usuario, password = passw, dbname = base,host = "localhost")
  dbSendQuery(bd, "TRUNCATE combinaciones") #borra registros pasados
  
  # CICLOS PARALELIZADOS PARA LLENAR COMBINACIONES EN BD --------------------
  for (nCP in numCasosP) {
    for (cP in colsP) {
      for (cQ in colsQ) {
        for (factor in FF) {
          foreach(k = 1:n) %dopar% {
            for (j in 1:n) {
              foreach(i = 1:n) %dopar% {
                bd <- dbConnect(MySQL(), user = usuario,password = passw, dbname = base, host = "localhost")
                lr = LR[k]
                mg = MG[j]
                ss = SS[i]
                orden <- paste("INSERT INTO combinaciones  (nCP,colsP,colsQ,hidden,lr,mg,ss,picked,done) VALUES (",
                               nCP, ",'", paste(as.character(cP), collapse = "-"),
                               "',", "'", paste(as.character(cQ), collapse = "-"),
                               "',", (floor(length(cP) * factor + 1)),
                               ",", lr, ",", mg, ",", ss, ",", picked,
                               ",", done, ")", sep = "")
                dbSendQuery(bd, orden)
              }
            }
          }
        }
      }
    }
  }
  dbDisconnect(bd) #desconecta la base de datos
  
  # CICLO PARALELIZADO QUE EJECUTA TODAS LAS COMBINACIONES REGISTRAD --------
  # ARREGLA Y CUENTA CICLOS RESTANTES 
  t0 <- proc.time() #inicia cronómetro
  bd <- dbConnect(MySQL(), user = usuario, password = passw, dbname = base, host = "localhost")
  Q0 <- "UPDATE combinaciones SET picked=0 WHERE picked=1 AND done=0"
  dbGetQuery(bd, Q0) #repara interrupción
  Q1 <- "SELECT * FROM combinaciones WHERE picked=0 AND done=0"
  combinacs <- dbGetQuery(bd, Q1) #faltan por procesar
  dbDisconnect(bd)
  #EJECUTA LOS RESTANTES CICLOS
  foreach(k = 1:dim(combinacs)[1], .inorder = FALSE, .combine = rbind) %dopar% {
    set.seed(12) #homologa semilla aleatoria
    caso <- combinacs[k, ] #cada una de las combinaciones
    bd <- dbConnect(MySQL(), user = usuario, password = passw,dbname = base, host = "localhost")
    marcaPicked <- paste("UPDATE combinaciones SET picked = 1 WHERE nCP=",
                         caso$nCP, " AND colsP='", caso$colsP, "' AND colsQ='",
                         caso$colsQ, "' AND hidden=", caso$hidden, " AND lr=",
                         caso$lr, " AND mg=", caso$mg, " AND ss=", caso$ss,
                         sep = "")
    dbGetQuery(bd, marcaPicked) #asegura para procesar
    t1 <- proc.time() #inicia cronómetro local del caso
    P1 <- read.table(archiDatos, header = TRUE) #lee los datos
    nCP <- caso$nCP #fracción de casos para entrenamiento
    # LEE LAS COLUMNAS DE ENTRADA
    A <- as.character(caso$colsP)
    B <- strsplit(A, "-")[[1]]
    D <- paste(B, collapse = ",")
    E <- paste("c(", D, ")")
    cP <- eval(parse(text = as.character(E)))
    # LEE LAS COLUMNAS DE SALIDA 
    A <- as.character(caso$colsQ)
    B <- strsplit(A, "-")[[1]]
    D <- paste(B, collapse = ",")
    E <- paste("c(", D, ")")
    cQ <- eval(parse(text = as.character(E)))
    hidden = caso$hidden #núm. neuronas capa oculta
    lr = caso$lr #tasa de aprendizaje
    mg = caso$mg #momentum
    ss = caso$ss #pasos de entrenamiento
    P <- as.matrix(P1[1:(nCP * numPuntos), cP]) #matriz de entrada a la RNAf
    Q <- as.matrix(P1[1:(nCP * numPuntos), cQ]) #patrón de entrenamiento
#es excesiva    set.seed(12)
    # ARMADO DE LA RNAf 
    net.start <- newff(n.neurons = c(dim(P)[2], hidden,length(cQ)), learning.rate.global = lr, momentum.global = mg, error.criterium = "LMLS", Stao = NA, hidden.layer = "tansig", output.layer = "purelin", method = "ADAPTgdwm")
    # entrenamiento con fracción de datos
    result <- train(net.start, P, Q, error.criterium = "LMLS", report = TRUE, show.step = ss, n.shows = 3)
    # simulación con el resto de datos
    sim <- sim.MLPnet(result$net, as.matrix(P1[(numPuntos *nCP + 1):(numPuntos * tope), cP]))
    # valores observados (resto de datos) 
    obs <- as.matrix(P1[(numPuntos * nCP + 1):(numPuntos *tope), cQ])
    # modelo lineal simulados vs observados 
    fit <- lm(sim[, 1] ~ obs[, 1])
    # cálculo del MAPE 
    mape <- mean(abs((sim[, 1] - obs[, 1])/obs[, 1]))
    tf <- round((proc.time() - t1)[[3]], 4) #tiempo de cómputo RNAf
    tff <- round((proc.time() - t0)[[3]], 4) #tiempo total del programa
    #SALIDA DE RESULTADO A BD
    marcaDone = paste("UPDATE combinaciones SET done = 1, mape=", mape, 
                      ", sigma=",round(summary(fit)$sigma,4), 
                      ",lmls=",round(result$Merror[3], 4),
                      ", tf=" , tf, 
                      ", tff=",tff,
                      ", colObj=",colObj,
                      "  WHERE nCP=",caso$nCP,
                      " AND colsP='", caso$colsP, 
                      "' AND colsQ='", caso$colsQ,
                      "' AND hidden=", caso$hidden, 
                      " AND lr=",caso$lr, 
                      " AND mg=", caso$mg, 
                      " AND ss=", caso$ss,
                      " AND picked=", 1, sep = "")
    dbGetQuery(bd, marcaDone) #marca la combinación de 'procesada'
    # DESCONECTA LA BD
    dbDisconnect(bd)
  }
  
  # RECUPERA LA RNAf OPTIMA -------------------------------------------------
  bd <- dbConnect(MySQL(), user = usuario, password = passw, dbname = base,host = "localhost")
  q0 <- "SELECT * FROM combinaciones WHERE mape IN (SELECT MIN(mape) FROM combinaciones)"
  pmin <- dbGetQuery(bd, q0) #recupera parámetros de la RNAf optima
  dbDisconnect(bd)
  lr <- pmin$lr #tasa de aprendizaje
  mg <- pmin$mg #momentum global
  ss <- pmin$ss #núm. de pasos de entrenam.
  nCP <- pmin$nCP #fracc. de datos de entrenam.
  # LEE LAS COLUMNAS DE ENTRADA
  A <- as.character(pmin$colsP)
  B <- strsplit(A, "-")[[1]]
  D <- paste(B, collapse = ",")
  E <- paste("c(", D, ")")
  cP <- eval(parse(text = as.character(E)))
  # LEE LAS COLUMNAS DE SALIDA 
  A <- as.character(pmin$colsQ)
  B <- strsplit(A, "-")[[1]]
  D <- paste(B, collapse = ",")
  E <- paste("c(", D, ")")
  cQ <- eval(parse(text = as.character(E)))
  P1 <- read.table(archiDatos, header = TRUE) #archivo de datos
  P <- as.matrix(P1[1:(nCP * numPuntos), cP]) #matriz de entrada
  Q <- as.matrix(P1[1:(nCP * numPuntos), cQ]) #patrón de entenam.
  # armado de la RNAf 
  net.start <- newff(n.neurons = c(dim(P)[2], pmin$hidden, length(cQ)),learning.rate.global = lr, momentum.global = mg, error.criterium = "LMLS",Stao = NA, hidden.layer = "tansig", output.layer = "purelin",method = "ADAPTgdwm")
  # entrenamiento 
  set.seed(12)
  result <- train(net.start, P, Q, error.criterium = "LMLS", report = TRUE,show.step = ss, n.shows = 3)
  # simulación 
  sim <- sim.MLPnet(result$net, as.matrix(P1[(nCP * numPuntos +1):(numPuntos * tope), cP]))
  # fracción de datos para validación
  obs <- as.matrix(P1[(nCP * numPuntos + 1):(numPuntos * tope),cQ])
  # modelo lineal simulados vs observados
  fit <- lm(sim[, 1] ~ obs[, 1])
  mape <- mean(abs((sim[, 1] - obs[, 1])/obs[, 1])) #MAPE
  rmse <- round(sqrt(mean(resid(fit)^2)), 4) #RMSE
  write.table(matrix(pmin,nrow=1),file='status.txt',append=TRUE,row.names=FALSE,col.names=FALSE,sep='|',quote=F)



  # GRAFICAS DE LA RNAf OPTIMA Y CALIDAD DE PREDICC. ------------------------
  graphviz.MLPnet(net=net.start,filename=paste(dummy,'_',arranque,'_RNAf-rapMedia.dot',sep=''))
  png(filename=paste(dummy,'_',arranque,'_lmRapMedia.png',sep=''),width=8,height=4.944,units='in',res=600)
  layout(matrix(c(1:3),1,3,byrow=TRUE))
  plot(obs[,1],sim[,1],main=paste('MAPE=',round(mape,4),sep=""),ylab="RNAf",xlab="Observados") 
  deltaOrdenada<-summary(fit)$coefficients[1]+summary(fit)$coefficients[3] 
  abline(deltaOrdenada,1) 
  abline(-deltaOrdenada,1) 
  abline(0,1) 
  hist(fit$residuals,probability=T,main="",xlab="residuales") 
  lines(density(fit$residuals),lwd=2) 
  plot(fit,which=2)
  dev.off()
  png(filename=paste(dummy,'_',arranque,'_RapMedia.png',sep=''),width=8,height=4.944,units='in',res=600)
  layout(1)
  plot(obs[,1],type='n',ylab='rapidez media normalizada [u.a.]',xlab='Puntos de modelación',ylim=c(min(min(obs[,1],min(sim[,1]))),max(max(obs[,1],max(sim[,1]))))) 
  low <- sim[,1]*(1-mape) 
  up <- sim[,1]*(1+mape) 
  polygon(c(1:dim(obs)[1],rev(1:dim(obs)[1])),c(low,rev(up)),col = "lightblue", border = FALSE) 
  points(obs[,1],lwd=1,col='black',type='b',pch='+') 
  lines(sim[,1],lwd=2,col='blue')
  dev.off()
  dummy=dummy+1
}
