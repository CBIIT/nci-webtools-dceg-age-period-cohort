findN_scorep1p2 <- function(R0, p1_t, p2_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
	R0 <- as.vector(R0)
	p1_t <- as.vector(p1_t)
	p2_t <- as.vector(p2_t)
	alpha <- as.vector(alpha)
	power_score <- as.vector(power_score)
	k <- as.vector(k)
  N <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- p1_t[i]/p2_t[i]
    VE[i] <- 1 - R[i]
    N1 <- k[i]*checkN
    N2 <- checkN - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t[i]*N2
    A <- checkN*R0[i]
    B <- -((x2[i] + N1)*R0[i] + x1[i] + N2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N")
  table <- as.table(data)
  table
}

findN_scoreVEp2 <- function(R0, VE, p2_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  k <- as.vector(k)
  N <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  p1_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- 1 - VE[i]
    p1_t[i] <- p2_t[i]*R[i]
    N1 <- k[i]*checkN
    N2 <- checkN - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t[i]*N2
    A <- checkN*R0[i]
    B <- -((x2[i] + N1)*R0[i] + x1[i] + N2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N")
  table <- as.table(data)
  table
}

findN_scoreVEp1 <- function(R0, VE, p1_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  k <- as.vector(k)
  N <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  p2_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- 1 - VE[i]
    p2_t[i] <- p1_t[i]/R[i]
    N1 <- k[i]*checkN
    N2 <- checkN - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t[i]*N2
    A <- checkN*R0[i]
    B <- -((x2[i] + N1)*R0[i] + x1[i] + N2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N")
  table <- as.table(data)
  table
}

findN_scoreRp1 <- function(R0, R, p1_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  k <- as.vector(k)
  N <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  p2_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    VE[i] <- 1 - R[i]
    p2_t[i] <- p1_t[i]/R[i]
    N1 <- k[i]*checkN
    N2 <- checkN - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t[i]*N2
    A <- checkN*R0[i]
    B <- -((x2[i] + N1)*R0[i] + x1[i] + N2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N")
  table <- as.table(data)
  table
}

findN_scoreRp2 <- function(R0, R, p2_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  k <- as.vector(k)
  N <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  p1_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    VE[i] <- 1 - R[i]
    p1_t[i] <- p2_t[i]*R[i]
    N1 <- k[i]*checkN
    N2 <- checkN - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t[i]*N2
    A <- checkN*R0[i]
    B <- -((x2[i] + N1)*R0[i] + x1[i] + N2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N")
  table <- as.table(data)
  table
}

findPower_scorep1p2 <- function(R0, p1_t, p2_t,alpha,N,k) {
  R0 <- as.vector(R0)
  p1_t <- as.vector(p1_t)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  N <- as.vector(N)
  k <- as.vector(k)
  N1 <- k*N
  N2 <- N - N1
  x1 <- p1_t*N1
  x2 <- p2_t*N2
  R <- p1_t/p2_t
  VE <- 1 - R
  A <- N*R0
  B <- -((x2 + N1)*R0 + x1 + N2)
  C <- x1 + x2
  p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
  p1_bar <- R0*p2_bar
  power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t*(1-p2_t)/(1-k)))
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N, k, power_score), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","k","power")
  table <- as.table(data)
  table
}

findPower_scoreVEp2 <- function(R0, VE, p2_t,alpha,N,k) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  N <- as.vector(N)
  k <- as.vector(k)
  R <- 1 - VE
  p1_t <- R*p2_t
  N1 <- k*N
  N2 <- N - N1
  x1 <- p1_t*N1
  x2 <- p2_t*N2
  A <- N*R0
  B <- -((x2 + N1)*R0 + x1 + N2)
  C <- x1 + x2
  p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
  p1_bar <- R0*p2_bar
  power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t*(1-p2_t)/(1-k)))
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N, k, power_score), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","k","power")
  table <- as.table(data)
  table
}

findPower_scoreVEp1 <- function(R0, VE, p1_t,alpha,N,k) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  N <- as.vector(N)
  k <- as.vector(k)
  R <- 1 - VE
  p2_t <- p1_t/R
  N1 <- k*N
  N2 <- N - N1
  x1 <- p1_t*N1
  x2 <- p2_t*N2
  A <- N*R0
  B <- -((x2 + N1)*R0 + x1 + N2)
  C <- x1 + x2
  p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
  p1_bar <- R0*p2_bar
  power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t*(1-p2_t)/(1-k)))
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N, k, power_score), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","k","power")
  table <- as.table(data)
  table
}

findPower_scoreRp1 <- function(R0, R, p1_t,alpha,N,k) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  N <- as.vector(N)
  k <- as.vector(k)
  VE <- 1 - R
  p2_t <- p1_t/R
  N1 <- k*N
  N2 <- N - N1
  x1 <- p1_t*N1
  x2 <- p2_t*N2
  A <- N*R0
  B <- -((x2 + N1)*R0 + x1 + N2)
  C <- x1 + x2
  p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
  p1_bar <- R0*p2_bar
  power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t*(1-p2_t)/(1-k)))
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N, k, power_score), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","k","power")
  table <- as.table(data)
  table
}

findPower_scoreRp2 <- function(R0, R, p2_t,alpha,N,k) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  N <- as.vector(N)
  k <- as.vector(k)
  VE <- 1 - R
  p1_t <- p2_t*R
  N1 <- k*N
  N2 <- N - N1
  x1 <- p1_t*N1
  x2 <- p2_t*N2
  A <- N*R0
  B <- -((x2 + N1)*R0 + x1 + N2)
  C <- x1 + x2
  p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
  p1_bar <- R0*p2_bar
  power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t*(1-p2_t)/(1-k)))
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N, k, power_score), ncol=9, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","k","power")
  table <- as.table(data)
  table
}