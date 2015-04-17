findN_scorep1p2 <- function(R0, p1_t, p2_t,alpha,power_score,k) {
  checkN <- seq(from=1,to=10000,by=0.1)
	R0 <- as.vector(R0)
	p1_t <- as.vector(p1_t)
	p2_t <- as.vector(p2_t)
	alpha <- as.vector(alpha)
	power_score <- as.vector(power_score)
	k <- as.vector(k)
  N <- rep(NA,times=length(k))
  N1 <- rep(NA,times=length(k))
  N2 <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- p1_t[i]/p2_t[i]
    VE[i] <- 1 - R[i]
    checkN1 <- k[i]*checkN
    checkN2 <- checkN - checkN1
    x1 <- p1_t[i]*checkN1
    x2 <- p2_t[i]*checkN2
    A <- checkN*R0[i]
    B <- -((x2[i] + checkN1)*R0[i] + x1[i] + checkN2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
        N1[i] <- N[i]*k[i]
        N2[i] <- N[i]*(1-k[i])
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
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
  N1 <- rep(NA,times=length(k))
  N2 <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  p1_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- 1 - VE[i]
    p1_t[i] <- p2_t[i]*R[i]
    checkN1 <- k[i]*checkN
    checkN2 <- checkN - checkN1
    x1 <- p1_t[i]*checkN1
    x2 <- p2_t[i]*checkN2
    A <- checkN*R0[i]
    B <- -((x2[i] + checkN1)*R0[i] + x1[i] + checkN2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
        N1[i] <- N[i]*k[i]
        N2[i] <- N[i]*(1-k[i])
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
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
  N1 <- rep(NA,times=length(k))
  N2 <- rep(NA,times=length(k))
  R <- rep(NA,times=length(k))
  p2_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    R[i] <- 1 - VE[i]
    p2_t[i] <- p1_t[i]/R[i]
    checkN1 <- k[i]*checkN
    checkN2 <- checkN - checkN1
    x1 <- p1_t[i]*checkN1
    x2 <- p2_t[i]*checkN2
    A <- checkN*R0[i]
    B <- -((x2[i] + checkN1)*R0[i] + x1[i] + checkN2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
        N1[i] <- N[i]*k[i]
        N2[i] <- N[i]*(1-k[i])
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
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
  N1 <- rep(NA,times=length(k))
  N2 <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  p2_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    VE[i] <- 1 - R[i]
    p2_t[i] <- p1_t[i]/R[i]
    checkN1 <- k[i]*checkN
    checkN2 <- checkN - checkN1
    x1 <- p1_t[i]*checkN1
    x2 <- p2_t[i]*checkN2
    A <- checkN*R0[i]
    B <- -((x2[i] + checkN1)*R0[i] + x1[i] + checkN2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
        N1[i] <- N[i]*k[i]
        N2[i] <- N[i]*(1-k[i])
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
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
  N1 <- rep(NA,times=length(k))
  N2 <- rep(NA,times=length(k))
  VE <- rep(NA,times=length(k))
  p1_t <- rep(NA,times=length(k))
  for(i in 1:length(k)) {
    VE[i] <- 1 - R[i]
    p1_t[i] <- p2_t[i]*R[i]
    checkN1 <- k[i]*checkN
    checkN2 <- checkN - checkN1
    x1 <- p1_t[i]*checkN1
    x2 <- p2_t[i]*checkN2
    A <- checkN*R0[i]
    B <- -((x2[i] + checkN1)*R0[i] + x1[i] + checkN2)
    C <- x1[i] + x2[i]
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0[i]*p2_bar
    checkpower_score1 <- pnorm((qnorm(alpha[i])*sqrt(p1_bar[i]*(1-p1_bar[i])/k[i] + R0[i]^2*p2_bar[i]*(1-p2_bar[i])/(1-k[i])) + sqrt(checkN)*(R0[i]*p2_t[i] - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k[i] + R0[i]^2*p2_t[i]*(1-p2_t[i])/(1-k[i])))
    checkpower_score1 <- round(checkpower_score1,5)
    for(n in 1:length(checkN)) {
      if(checkpower_score1[n] == power_score[i]) {
        N[i] <- checkN[n]
        N1[i] <- N[i]*k[i]
        N2[i] <- N[i]*(1-k[i])
      }
    }
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN2_scorep1p2 <- function(R0, p1_t, p2_t,alpha,power_score,N1) {
  R0 <- as.vector(R0)
  p1_t <- as.vector(p1_t)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N1 <- as.vector(N1)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N2 <- rep(NA,times=length(R0))
  R <- p1_t/p2_t
  VE <- 1 - R
  for(i in 1:length(R0)) {
    checkN <- seq(N1[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,N1[i]/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- N1[i]/N[i]
    N2[i] <- N[i] - N1[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN2_scoreVEp2 <- function(R0, VE, p2_t,alpha,power_score,N1) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N1 <- as.vector(N1)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N2 <- rep(NA,times=length(R0))
  R <- 1 - VE
  p1_t <- R*p2_t
  for(i in 1:length(R0)) {
    checkN <- seq(N1[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,N1[i]/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- N1[i]/N[i]
    N2[i] <- N[i] - N1[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN2_scoreVEp1 <- function(R0, VE, p1_t,alpha,power_score,N1) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N1 <- as.vector(N1)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N2 <- rep(NA,times=length(R0))
  R <- 1 - VE
  p2_t <- p1_t/R
  for(i in 1:length(R0)) {
    checkN <- seq(N1[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,N1[i]/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- N1[i]/N[i]
    N2[i] <- N[i] - N1[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN2_scoreRp1 <- function(R0, R, p1_t,alpha,power_score,N1) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N1 <- as.vector(N1)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N2 <- rep(NA,times=length(R0))
  VE <- 1 - R
  p2_t <- p1_t/R
  for(i in 1:length(R0)) {
    checkN <- seq(N1[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,N1[i]/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- N1[i]/N[i]
    N2[i] <- N[i] - N1[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN2_scoreRp2 <- function(R0, R, p2_t,alpha,power_score,N1) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N1 <- as.vector(N1)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N2 <- rep(NA,times=length(R0))
  VE <- 1 - R
  p1_t <- p2_t*R
  for(i in 1:length(R0)) {
    checkN <- seq(N1[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,N1[i]/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- N1[i]/N[i]
    N2[i] <- N[i] - N1[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN1_scorep1p2 <- function(R0, p1_t, p2_t,alpha,power_score,N2) {
  R0 <- as.vector(R0)
  p1_t <- as.vector(p1_t)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N2 <- as.vector(N2)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N1 <- rep(NA,times=length(R0))
  R <- p1_t/p2_t
  VE <- 1 - R
  for(i in 1:length(R0)) {
    checkN <- seq(N2[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,(checkN - N2[i])/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- (N[i] - N2[i])/N[i]
    N1[i] <- N[i] - N2[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN1_scoreVEp2 <- function(R0, VE, p2_t,alpha,power_score,N2) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N2 <- as.vector(N2)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N1 <- rep(NA,times=length(R0))
  R <- 1 - VE
  p1_t <- p2_t*R
  for(i in 1:length(R0)) {
    checkN <- seq(N2[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,(checkN - N2[i])/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- (N[i] - N2[i])/N[i]
    N1[i] <- N[i] - N2[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN1_scoreVEp1 <- function(R0, VE, p1_t,alpha,power_score,N2) {
  R0 <- as.vector(R0)
  VE <- as.vector(VE)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N2 <- as.vector(N2)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N1 <- rep(NA,times=length(R0))
  R <- 1 - VE
  p2_t <- p1_t/R
  for(i in 1:length(R0)) {
    checkN <- seq(N2[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,(checkN - N2[i])/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- (N[i] - N2[i])/N[i]
    N1[i] <- N[i] - N2[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN1_scoreRp1 <- function(R0, R, p1_t,alpha,power_score,N2) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p1_t <- as.vector(p1_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N2 <- as.vector(N2)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N1 <- rep(NA,times=length(R0))
  VE <- 1 - R
  p2_t <- p1_t/R
  for(i in 1:length(R0)) {
    checkN <- seq(N2[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,(checkN - N2[i])/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- (N[i] - N2[i])/N[i]
    N1[i] <- N[i] - N2[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
  table <- as.table(data)
  table
}

findN1_scoreRp2 <- function(R0, R, p2_t,alpha,power_score,N2) {
  R0 <- as.vector(R0)
  R <- as.vector(R)
  p2_t <- as.vector(p2_t)
  alpha <- as.vector(alpha)
  power_score <- as.vector(power_score)
  N2 <- as.vector(N2)
  N <- rep(NA,times=length(R0))
  k <- rep(NA,times=length(R0))
  N1 <- rep(NA,times=length(R0))
  VE <- 1 - R
  p1_t <- p2_t*R
  for(i in 1:length(R0)) {
    checkN <- seq(N2[i]+0.1,100000,by=0.1)
    y <- round(findPower_scorep1p2(rep(R0[i],times=length(checkN)),rep(p1_t[i],times=length(checkN)),rep(p2_t[i],times=length(checkN)),rep(alpha[i],times=length(checkN)),checkN,(checkN - N2[i])/checkN),5)
    n <- match(power_score[i],y)
    N[i] <- checkN[n]
    k[i] <- (N[i] - N2[i])/N[i]
    N1[i] <- N[i] - N2[i]
  }
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, power_score, k, N,N1,N2), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","power (score method)","k","N","N1","N2")
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
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N,N1,N2, k, power_score), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","N1","N2","k","power")
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
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N,N1,N2, k, power_score), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","N1","N2","k","power")
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
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N,N1,N2, k, power_score), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","N1","N2","k","power")
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
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N,N1,N2, k, power_score), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","N1","N2","k","power")
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
  data <- matrix(c(R0, p1_t, p2_t, R, VE, alpha, N,N1,N2, k, power_score), ncol=11, nrow=length(R0))
  colnames(data) <- c("R0","p1","p2","R = p1/p2","vaccine efficacy", "alpha","N","N1","N2","k","power")
  table <- as.table(data)
  table
}

DrawPowerRp2 <- function(R, p2_t,R0,alpha,N,k) {
  R <- seq(from=min(R),to=max(R),by=0.000001)
  
  Iterations <- 1:length(p2_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    p1_t <- p2_t[i]*R
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t*N1
    x2 <- p2_t[i]*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t[i] - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t[i]*(1-p2_t[i])/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~R,type="l",main=c("Power vs. R",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="R = p1/p2",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=R,lty=LTY[i], col = LTY[i],lwd=3)
    }
  legend(lty=LTY,legend=p2_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p2")
  }
}

DrawPowerp1p2 <- function(p1_t, p2_t,R0,alpha,N,k) {
  p1_t <- seq(from=min(p1_t),to=max(p1_t),by=0.000001)
  
  Iterations <- 1:length(p2_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- p1_t/p2_t[i]
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t*N1
    x2 <- p2_t[i]*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t[i] - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t[i]*(1-p2_t[i])/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p1_t,type="l",main=c("Power vs. Attack Rate in 1-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 1-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p1_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=p2_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p2")
  }
}

DrawPowerVEp2 <- function(VE, p2_t,R0,alpha,N,k) {
  VE <- seq(from=min(VE),to=max(VE),by=0.000001)
  
  Iterations <- 1:length(p2_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- 1 - VE
    p1_t <- p2_t[i]/R
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t*N1
    x2 <- p2_t[i]*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t[i] - p1_t))/sqrt(p1_t*(1-p1_t)/k + R0^2*p2_t[i]*(1-p2_t[i])/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~VE,type="l",main=c("Power vs. Vaccine Efficacy",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Vaccine Efficacy",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=VE,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=p2_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p2")
  }
}

DrawPowerRp1 <- function(R, p1_t,R0,alpha,N,k) {
  R <- seq(from=min(R),to=max(R),by=0.000001)
  
  Iterations <- 1:length(p1_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    p2_t <- p1_t[i]/R
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k + R0^2*p2_t*(1-p2_t)/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~R,type="l",main=c("Power vs. R",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="R = p1/p2",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=R,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=p1_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p1")
  }
}

DrawPowerp2p1 <- function(p2_t, p1_t,R0,alpha,N,k) {
  p2_t <- seq(from=min(p2_t),to=max(p2_t),by=0.000001)
  
  Iterations <- 1:length(p1_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- p1_t[i]/p2_t
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k + R0^2*p2_t*(1-p2_t)/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p2_t,type="l",main=c("Power vs. Attack Rate in 2-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 2-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p2_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=p1_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p1")
  }
}

DrawPowerVEp1 <- function(VE, p1_t,R0,alpha,N,k) {
  VE <- seq(from=min(VE),to=max(VE),by=0.000001)
  
  Iterations <- 1:length(p1_t)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- 1 - VE
    p2_t <- p1_t[i]/R
    N1 <- k*N
    N2 <- N - N1
    x1 <- p1_t[i]*N1
    x2 <- p2_t*N2
    A <- N*R0
    B <- -((x2 + N1)*R0 + x1 + N2)
    C <- x1 + x2
    p2_bar <- (-B - sqrt(B^2 - 4*A*C))/(2*A)
    p1_bar <- R0*p2_bar
    power_score <- pnorm((qnorm(alpha)*sqrt(p1_bar*(1-p1_bar)/k + R0^2*p2_bar*(1-p2_bar)/(1-k)) + sqrt(N)*(R0*p2_t - p1_t[i]))/sqrt(p1_t[i]*(1-p1_t[i])/k + R0^2*p2_t*(1-p2_t)/(1-k)))
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~VE,type="l",main=c("Power vs. Vaccine Efficacy",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Vaccine Efficacy",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=VE,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=p1_t,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="p1")
  }
}

DrawPowerp2R <- function(p2_t,R,R0,alpha,N,k) {
  p2_t <- seq(from=min(p2_t),to=max(p2_t),by=0.000001)
  
  Iterations <- 1:length(R)
  LTY <- Iterations
  
  for(i in Iterations) {
    p1_t <- p2_t*R[i]
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
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p2_t,type="l",main=c("Power vs. Attack Rate in 2-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 2-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p2_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=R,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="R = p1/p2")
  }
}

DrawPowerp1R <- function(p1_t,R,R0,alpha,N,k) {
  p1_t <- seq(from=min(p1_t),to=max(p1_t),by=0.000001)
  
  Iterations <- 1:length(R)
  LTY <- Iterations
  
  for(i in Iterations) {
    p2_t <- p1_t/R[i]
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
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p1_t,type="l",main=c("Power vs. Attack Rate in 1-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 1-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p1_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=R,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="R = p1/p2")
  }
}

DrawPowerp1VE <- function(p1_t,VE,R0,alpha,N,k) {
  p1_t <- seq(from=min(p1_t),to=max(p1_t),by=0.000001)
  
  Iterations <- 1:length(VE)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- 1 - VE[i]
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
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p1_t,type="l",main=c("Power vs. Attack Rate in 1-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 1-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p1_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=VE,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Vaccine Efficacy")
  }
}

DrawPowerp2VE <- function(p2_t,VE,R0,alpha,N,k) {
  p2_t <- seq(from=min(p2_t),to=max(p2_t),by=0.000001)
  
  Iterations <- 1:length(VE)
  LTY <- Iterations
  
  for(i in Iterations) {
    R <- 1 - VE[i]
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
    if(i==1) {
      par(mar=c(5.1, 4.1, 5.1, 9.5))
      plot(power_score~p2_t,type="l",main=c("Power vs. Attack Rate in 2-dose arm",paste("R0 =",R0,", alpha =",alpha,", N =",N,", k =",k)),xlab="Attack Rate in 2-dose arm",ylab="Power",ylim=c(0,1),lwd=3,lty=LTY[i], col = LTY[i],font.lab=2,font=2,cex.axis=1.15,cex.lab=1.15)
    }
    else {
      lines(y=power_score, x=p2_t,lty=LTY[i], col = LTY[i],lwd=3)
    }
    legend(lty=LTY,legend=VE,"bottomright",cex=1.15, lwd=3,col= LTY,text.font=2,bty="o",inset=c(-0.5,0),xpd = TRUE,title="Vaccine Efficacy")
  }
}