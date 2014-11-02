###---------------------------------------------------------
###   GENERATE PROTOTYPES
###   Author: Stephen O'Connell
###   Date: 05/03/2001
###---------------------------------------------------------
# THE PATTERN GENERATION ALGORITHMS WAS ADAPTED FROM A 
#    THE FOLLOWING PAPER ON TIME SERIES
#  "Time-Series Similarity Queries Employing a Feature-Based Approach",
#      R.J. Alcock and Y. Manolopoulos
# THE ALGORITHMS WERE HEAVILY MODIFIED TO INCREASE THE RANGE OF 
#    VARIATION IN THE PATTERNS, from 0 to 100, AND INCREASE THE RANDOMNESS
#    THE STARTING POINT AND DEGREE OF FLUCTUATION.
#
#  DURING THE GENERATION PROCESS A CHART IS PRODUCED FOR EACH 
#    INDIVIDUAL PATTERN, COMMENT OUT THE PLOT AND LINES COMMANDS IF YOU
#    WANT TO SKIP THE INDIVUDAL PLOT (WHICH ARE 5X5 AND CAN BE HARD READ UNLESS
#    YOU HAVE A LARGE MONITOR)
#
#  TWO PLOTS AT THE END SHOW ONE EACH FOR THE PATTERNS, AND ALL GENERATED PATTERNS
#    PER TYPE.

### CHANGE TO YOUR HOME DIRECTORY THAT CONTRAINS THE FILES AND LOCATION OF FILES CREATED
setwd("/Users/oconste/Documents/workspace/sao_projects/CMG_Presentation")

### SETUP
m <- 50
s <- 2
N <- 130

Min <- runif(100, min=-20, max=0)
Max <- runif(100, min=0, max=20)
StartM <- runif(100, min=25, max=75)

Y <- matrix(0, nrow=100, ncol=N)

par(mfrow=c(5,5))
###---------------------------
###     NORMAL
###---------------------------

for (x in 1:100) {
  y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s)
	}
	Y[x,] <- t(y)
	plot(Y[x,], type='l', ylim=c(-15,115))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}
min(Y)
max(Y)

Y[Y<0] <- 1 
Y[Y>100] <- 99
###---------------------------
###     CYCLIC
###---------------------------

a <- round(runif(100, min=5, max=20),0)
T <- round(runif(100, min=5, max=20),0)
f <- round(runif(100, min=.1, max=.9),1)

Yhat <- matrix(0, nrow=100, ncol=N)
for (x in 1:100) {
	y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s) + (a[x] * sin((2*pi*(i*f[x]))/T[x]))
	}
	Yhat[x,] <- t(y)
	plot(Yhat[x,], type='l', ylim=c(-15,115), main=paste("a = ", a[x], "T = ", T[x], " f = ", f[x], sep=""))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}

min(Yhat)
max(Yhat)

Yhat[Yhat<0] <- 1 
Yhat[Yhat>100] <- 99

Y <- rbind(Y, Yhat)

###---------------------------
###     INCREASING SHIFT
###---------------------------

g <- round(runif(100, min=.15, max=.4),1)

Yhat <- matrix(0, nrow=100, ncol=N)
for (x in 1:100) {
	y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s) + (g[x] * i)
	}
	if (max(y) > 100) {
		delta <- abs(max(y) - 100)
		y <- y - delta
	}
	Yhat[x,] <- t(y)
	plot(Yhat[x,], type='l', ylim=c(-15,115), main=paste("a = ", a[x], "T = ", T[x], " f = ", f[x], sep=""))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}

min(Yhat)
max(Yhat)

Yhat[Yhat<0] <- 1 
Yhat[Yhat>100] <- 99

Y <- rbind(Y, Yhat)

###---------------------------
###     DECREASING SHIFT
###---------------------------

g <- round(runif(100, min=.15, max=.4),1)
M <- 50
Yhat <- matrix(0, nrow=100, ncol=N)
for (x in 1:100) {
	y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s) - (g[x] * i)
	}
	if (min(y) < 0) {
		delta <- abs(min(y))
		y <- y + delta
	}
	Yhat[x,] <- t(y)
	
	plot(Yhat[x,], type='l', ylim=c(-15,115), main=paste("a = ", a[x], "T = ", T[x], " f = ", f[x], sep=""))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}

min(Yhat)
max(Yhat)

Yhat[Yhat<0] <- 1 
Yhat[Yhat>100] <- 99

Y <- rbind(Y, Yhat)


###---------------------------
###     UPWARD SHIFT
###---------------------------

j <- round(runif(100, min=15, max=45),1)
t3 <- round(runif(100, min=N/10, max=(2*N)/3),0)


M <- 50
Yhat <- matrix(0, nrow=100, ncol=N)

for (x in 1:100) {
	y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	K <- rep(0,N)
	K[t3[x]:N] <- 1    	
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s) + (K[i] * j[x]) 
	}
	if (max(y) > 100) {
		delta <- abs(max(y) - 100)
		y <- y - delta
	}
	Yhat[x,] <- t(y)
	plot(Yhat[x,], type='l', ylim=c(-15,115), main=paste("j = ", j[x], " k = ", t3[x], sep=""))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}

min(Yhat)
max(Yhat)

Yhat[Yhat<0] <- 1 
Yhat[Yhat>100] <- 99

Y <- rbind(Y, Yhat)


###---------------------------
###     DOWNWARD SHIFT
###---------------------------

j <- round(runif(100, min=15, max=45),1)
#t3 <- round(runif(100, min=N/5, max=(2*N)/3),0)
t3 <- round(runif(100, min=N/10, max=(2*N)/3),0)

M <- 50
Yhat <- matrix(0, nrow=100, ncol=N)

for (x in 1:100) {
	y <- rep(0,N)
	r <- runif(N, min=Min[x], max=Max[x])
	K <- rep(0,N)
	K[t3[x]:N] <- 1    	
	for (i in 1:N) {
		y[i] <- StartM[x] + (r[i] * s) - (K[i] * j[x]) 
	}
	if (min(y) < 0) {
		delta <- abs(min(y))
		y <- y + delta
	}
	Yhat[x,] <- t(y)
	plot(Yhat[x,], type='l', ylim=c(0,110), main=paste("j = ", j[x], " k = ", t3[x], sep=""))
  lines(c(1:N), rep(0,N), col='red')
  lines(c(1:N), rep(100,N), col='red')
}

min(Yhat)
max(Yhat)

Yhat[Yhat<0] <- 1 
Yhat[Yhat>100] <- 99

Y <- rbind(Y, Yhat)

####-----------------------------------------------
###   PLOTS
####-----------------------------------------------

## LOAD A SPECIFIC DATASET PREVIOUSLY CREATED
##Y<- dget("Y_7")
par(mfrow=c(3,2))

### PLOT ONE OF EACH TYPE
plot(Y[1,], type='l', ylim=c(0,100), col='black', main='Normal', ylab="%Util")

plot(Y[101,], type='l', ylim=c(0,100), col='black', main='Cyclic', ylab="%Util")

plot(Y[201,], type='l', ylim=c(0,100), col='black', main='Trend Up', ylab="%Util")

plot(Y[301,], type='l', ylim=c(0,100), col='black', main='Trend Down', ylab="%Util")

plot(Y[451,], type='l', ylim=c(0,100), col='black', main='Shift Up', ylab="%Util")

plot(Y[501,], type='l', ylim=c(0,100), col='black', main='Shift Down', ylab="%Util")

####-----------------------------------------------
###   PLOTS
####-----------------------------------------------

### PLOT ALL THE OBSERVATIONS BY TYPE, FIRST BLACK, ALL THE REST RED.
par(mfrow=c(3,2))
plot(Y[1,], type='l', ylim=c(0,100), col='black', main='Normal', ylab="%Util")
for (i in seq(from=2, to=100, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}

plot(Y[101,], type='l', ylim=c(0,100), col='black', main='Cyclic', ylab="%Util")
for (i in seq(from=102, to=200, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}

plot(Y[201,], type='l', ylim=c(0,100), col='black', main='Trend Up', ylab="%Util")
for (i in seq(from=202, to=300, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}

plot(Y[301,], type='l', ylim=c(0,100), col='black', main='Trend Down', ylab="%Util")
for (i in seq(from=302, to=400, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}

plot(Y[451,], type='l', ylim=c(0,100), col='black', main='Shift Up', ylab="%Util")
for (i in seq(from=402, to=500, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}

plot(Y[501,], type='l', ylim=c(0,100), col='black', main='Shift Down', ylab="%Util")
for (i in seq(from=502, to=600, by=1)) {
	lines(Y[i,], col='#E41A1C07')
}


## WRITE OUT THE DATA TO A FILE FOR LATER USE:
#dput(Y, "Y_7")
