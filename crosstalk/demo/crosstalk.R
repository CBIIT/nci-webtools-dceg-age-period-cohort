apcwaldtests2 <- function(M, N)
{
 
  
  W = matrix(NaN, 17, 3)
  rl = c(
        "Equal Net Drifts",  # 1
        "Equal Age Deviations", # 2
        "Equal Period Deviations",  # 3
        "Equal Cohort Deviations",  # 4
        "Equal Period RR",  # 5
        "Equal Cohort RR", # 6
        "Equal Local Drifts",  # 7
        "Equal Long Age Trends",  # 8
        "Parallel Longitudinal Age Curves",  # 9
        "Equal Cross-Sectional Age Trends",  # 10
        "Parallel Cross-Sectional Age Curves",  # 11
        "Parallel Fitted Temporal Trends",  # 12 same as 5        
        "Parallel Fitted Cohort Pattern",  # 13 same as 6
        "PH-L",  # 14
        "PH-T",  # 15
        "PH-X",  # 16
        "PH-A"  # 17
  )  
  
  
  ###
  # contrast net drifts
  ###
  dmn <- M$APCModel$B[3] - N$APCModel$B[3]
  vm <- M$APCModel$s2VAR[3,3]
  vn <- N$APCModel$s2VAR[3,3]
  smn <- sqrt(vm + vn)
  zmn <- dmn/smn  
  x21 <- zmn^2
  df1 <- 1
  PVAL1 <- pchisq(x21, df1,lower.tail = FALSE)
  W[1,] <- c(x21, df1, PVAL1)
  
  ###
  # contrast age deviations
  ###
  dmn <- matrix(M$AgeDeviations[,2]-N$AgeDeviations[,2])
  vm <- M$V$va
  vn <- N$V$va
  p <- length(dmn)
  INC <- seq(from = 2, to = p - 1)
  x22 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df2 <- p - 2
  PVAL2 <- pchisq(x22, df2, lower.tail = FALSE)
  W[2,] <- c(x22, df2, PVAL2)
  
  ###
  # contrast per deviations
  ###
  dmn <- matrix(M$PerDeviations[,2]-N$PerDeviations[,2])
  vm <- M$V$vp
  vn <- N$V$vp
  p <- length(dmn)
  INC <- seq(from = 2, to = p - 1)
  x23 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df3 <- p - 2
  PVAL3 <- pchisq(x23, df3, lower.tail = FALSE)
  W[3,] <- c(x23, df3, PVAL3)
  
  
  ###
  # contrast coh deviations
  ###
  dmn <- matrix(M$CohDeviations[,2]-N$CohDeviations[,2])
  vm <- M$V$vc
  vn <- N$V$vc
  p <- length(dmn)
  INC <- seq(from = 2, to = p - 1)
  x24 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df4 <- p - 2
  PVAL4 <- pchisq(x24, df4, lower.tail = FALSE)
  W[4,] <- c(x24, df4, PVAL4)
  
  ###
  # contrast PeriodRR
  ###
  dmn <- matrix(log(M$PeriodRR[,2])-log(N$PeriodRR[,2]))
  vm <- M$V$vpr
  vn <- N$V$vpr
  p <- length(dmn)
  per <- matrix(M$Inputs$D$p)
  p0 <- M$Inputs$RVals[2]
  p0LOC <- match(p0, per)
  INC <- c(seq(from = 1, to = p0LOC-1), seq(from = p0LOC+1, to = p))
  x25 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df5 <- p - 1
  PVAL5 <- pchisq(x25, df5, lower.tail = FALSE)
  W[5,] <- c(x25, df5, PVAL5)
  
  ###
  # contrast CohortRR
  ###
  dmn <- matrix(log(M$CohortRR[,2])-log(N$CohortRR[,2]))
  vm <- M$V$vcr
  vn <- N$V$vcr
  p <- length(dmn)
  coh <- matrix(M$Inputs$D$c)
  c0 <- M$Inputs$RVals[3]
  c0LOC <- match(c0, coh)
  INC <- c(seq(from = 1, to = c0LOC-1), seq(from = c0LOC+1, to = p))
  x26 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df6 <- p - 1
  PVAL6 <- pchisq(x26, df6, lower.tail = FALSE)
  W[6,] <- c(x26, df6, PVAL6)
  
  ###
  # contrast local drifts
  ###
  lldm <- log(1 + 0.01*M$LocalDrifts[,2])
  lldn <- log(1 + 0.01*N$LocalDrifts[,2])
  dmn <- matrix(lldm-lldn)
  vm <- M$V$vld
  vn <- N$V$vld
  p <- length(dmn)
  INC <- seq(from = 1, to = p)
  x27 <- t(matrix(dmn[INC])) %*% solve((vm[INC, INC]+vn[INC, INC]), dmn[INC])
  df7 <- p
  PVAL7 <- pchisq(x27, df7, lower.tail = FALSE)
  W[7,] <- c(x27, df7, PVAL7)
  
  
  ###
  # contrast Longitudinal Age Trend
  ###
  dmn <- M$APCModel$B[2] - N$APCModel$B[2]
  vm <- M$APCModel$s2VAR[2,2]
  vn <- N$APCModel$s2VAR[2,2]
  smn <- sqrt(vm + vn)
  zmn <- dmn/smn  
  x28 <- zmn^2
  df8 <- 1
  PVAL8 <- pchisq(x28, df8,lower.tail = FALSE)
  W[8,] <- c(x28, df8, PVAL8)
  
  
  
  ###
  # contrast Longitudinal Age Curves
  ###
  dmn <- matrix(log(M$LongAge[,2])-log(N$LongAge[,2]))
  vm <- M$V$lav
  vn <- N$V$lav
  A <- length(dmn)
  # Sweep out the mean
  XA <- diag(1, nrow = A) - (1/A)*matrix(1, nrow = A, ncol = A)
  d <- XA %*% dmn
  v <- XA %*% (vm + vn) %*% t(XA)
  # Extract reference value
  age <- matrix(M$Inputs$D$a)
  a0 <- M$Inputs$RVals[1]
  a0LOC <- match(a0, age)
  INC <- c(seq(from = 1, to = a0LOC-1), seq(from = a0LOC+1, to = A))
  x29 <- t(matrix(d[INC])) %*% solve(v[INC, INC], d[INC])
  df9 <- p - 1
  PVAL9 <- pchisq(x29, df9, lower.tail = FALSE)
  W[9,] <- c(x29, df9, PVAL9)
  
  ###
  # contrast Cross-Sectional Age Trend
  ###
  dmn <- M$APCModel$B[c(2,3)] - N$APCModel$B[c(2,3)]
  vm <- M$APCModel$s2VAR[c(2,3),c(2,3)]
  vn <- N$APCModel$s2VAR[c(2,3),c(2,3)]
  X <- cbind(1,-1)
  d <- X %*% dmn
  v <- X %*% (vm + vn) %*% t(X)
  x210 <- t(d) %*% solve(v, d)
  df10 <- 1
  PVAL10 <- pchisq(x210, df10, lower.tail = FALSE)
  W[10,] <- c(x210, df10, PVAL10)
  
  ###
  # contrast Cross-Sectional Age Curves
  ###
  dmn <- matrix(log(M$CrossAge[,2])-log(N$CrossAge[,2]))
  vm <- M$V$xav
  vn <- N$V$xav
  d <- XA %*% dmn
  v <- XA %*% (vm + vn) %*% t(XA)
  x211 <- t(matrix(d[INC])) %*% solve(v[INC, INC], d[INC])
  df11 <- A - 1
  PVAL11 <- pchisq(x211, df11, lower.tail = FALSE)
  W[11,] <- c(x211, df11, PVAL11)
  
  
  ###
  # contrast Fitted Temporal Trends
  ###
  dmn <- matrix(log(M$FittedTemporalTrends[,2])-log(N$FittedTemporalTrends[,2]))
  vm <- M$V$ftv
  vn <- N$V$ftv
  P <- length(dmn)
  # Sweep out the mean
  XA <- diag(1, nrow = P) - (1/P)*matrix(1, nrow = P, ncol = P)
  d <- XA %*% dmn
  v <- XA %*% (vm + vn) %*% t(XA)
  # Extract reference value  
  INC <- c(seq(from = 1, to = p0LOC-1), seq(from = p0LOC+1, to = P))
  x212 <- t(matrix(d[INC])) %*% solve(v[INC, INC], d[INC])
  df12 <- P - 1
  PVAL12 <- pchisq(x212, df12, lower.tail = FALSE)
  W[12,] <- c(x212, df12, PVAL12)
  
  ###
  # Parallel fitted Temporal Trends same as equal PeriodRR
  # and here: Parallel Fitted Cohort Pattern same as Equal CohortRR
  ###
  W[13,] = W[6,];
  
  
  ####
  # PH-L: 
  # * cohort-specific age-specific rates are parallel
  # * RR depends only on cohort
  # * LAT, age deviations, and period deviations are all equal
  # * df = 1 + (A-2) + (P-2) = A + P - 3
  ####
  INC <- c(M$Pt[[2]], M$Pt[[4]], M$Pt[[5]])
  dmn <- matrix(M$APCModel$B[INC] - N$APCModel$B[INC])
  vm <- M$APCModel$s2VAR[INC, INC]
  vn <- N$APCModel$s2VAR[INC, INC]
  v <- (vm+vn)
  x214 <- t(dmn) %*% solve(v, dmn)
  df14 <- A + P - 3
  PVAL14 <- pchisq(x214, df14, lower.tail = FALSE)
  W[14,] <- c(x214, df14, PVAL14)
  
 
  ####
  # PH-T: 
  # * age-specific rates over time are parallel
  # * RR depends only on age
  # * net drift, period, and cohort deviations are all equal
  # * df = 1 + (P-2) + (A+P-3) = A + 2*P - 4
  ####
  INC <- c(M$Pt[[3]], M$Pt[[5]], M$Pt[[6]])
  dmn <- matrix(M$APCModel$B[INC] - N$APCModel$B[INC])
  vm <- M$APCModel$s2VAR[INC, INC]
  vn <- N$APCModel$s2VAR[INC, INC]
  v <- (vm+vn)
  x215 <- t(dmn) %*% solve(v, dmn)
  df15 <- A + 2*P - 4
  PVAL15 <- pchisq(x215, df15, lower.tail = FALSE)
  W[15,] <- c(x215, df15, PVAL15)
  
  ####
  # PH-X: 
  # * cross-sectional age curves for each period are proportional
  # * RR depends only on period
  # * CAT, age deviations, and cohort deviations are all equal
  # * df = 1 + (A-2) + (A+P-3) = 2*A + P - 4
  ####
  INC <- c(M$Pt[[2]], M$Pt[[3]], M$Pt[[4]], M$Pt[[6]])
  dmn <- matrix(M$APCModel$B[INC] - N$APCModel$B[INC])
  
  # construcct block diagonal matrix
  X1 <- c(1, -1)
  # X2 <- diag(1, nrow = A-2)
  # X3 <- diag(1, nrow = A+P-3)
  X23 <- diag(1, nrow = 2*A + P - 5)
  X <- matrix(0, nrow = 1 + 2*A + P - 5, ncol = 2 + 2*A + P - 5)
  X[1,c(1,2)] <- X1
  X[seq(2, 1 + 2*A + P - 5),seq(3, 2 + 2*A + P - 5)] <- X23
  vm <- M$APCModel$s2VAR[INC, INC]
  vn <- N$APCModel$s2VAR[INC, INC]
  v <- X %*% (vm+vn) %*% t(X)
  d <- X %*% dmn
  x216 <- t(d) %*% solve(v, d)
  df16 <- 2*A + P - 4
  PVAL16 <- pchisq(x216, df16, lower.tail = FALSE)
  W[16,] <- c(x216, df16, PVAL16)
  
  
  ####
  # PH-A: 
  # * all the parameters are equal except for maybe the intercepts
  # * RR is a constant
  # * df = 2*(A+P)-5
  ####
  INC <- c(M$Pt[[2]], M$Pt[[3]], M$Pt[[4]], M$Pt[[5]], M$Pt[[6]])
  dmn <- matrix(M$APCModel$B[INC] - N$APCModel$B[INC])
  vm <- M$APCModel$s2VAR[INC, INC]
  vn <- N$APCModel$s2VAR[INC, INC]
  v <- (vm+vn)
  x217 <- t(dmn) %*% solve(v, dmn)
  df17 <- 2*(A + P) - 5
  PVAL17 <- pchisq(x217, df17, lower.tail = FALSE)
  W[17,] <- c(x217, df17, PVAL17)
  
  dimnames(W) <- list(rl,  c("X2", "df", "P-Value"))
  
  W <- list(W = W)
            
  
  
}

####
# rrcomp1
####


rrcomp1 <- function(M1, M0)
{
  
  Pt <- M1$Pt
  a <- matrix(M1$AgeDeviations[,1])
  p <- matrix(M1$PerDeviations[,1])
  c <- matrix(M1$CohDeviations[,1])
  
  B1 <- M1$APCModel$B
  V1 <- M1$APCModel$s2VAR
  B0 <- M0$APCModel$B
  V0 <- M0$APCModel$s2VAR
  
  
  
  ###
  # FV(a|c) = LongAge * CRR *EPD
  ###
  
  
  # Includes a contrast of the cohort deviations in order to center the curve
  # on the referent cohort
  XXa <- M1$Matrices$XLA
  INCa <- c(1, 2, M1$Matrices$Pt[[4]], M1$Matrices$Pt[[6]])
  
  la1 <- XXa%*%matrix(B1[INCa,1])
  va1 <- XXa%*%V1[INCa, INCa]%*%t(XXa)
 
  la0 <- XXa%*%matrix(B0[INCa,1])
  va0 <- XXa%*%V0[INCa, INCa]%*%t(XXa)
  
  lra <- la1 - la0
  
  vlra <- va1 + va0
  
  sd <- matrix(sqrt(diag(vlra)))
  ci <- cbind(lra - 1.96*sd, lra + 1.96*sd)
  elra <- exp(lra)
  eci <- exp(ci)
  
  LAC <- cbind(a, elra, eci)
  dimnames(LAC) <- list(c(), c("Age", "LAC", "CILo", "CIHi"))
  
  
  XXc <- M1$Matrices$XCR
  INCc <- c(1, 3, M1$Matrices$Pt[[6]])
  
  cr1 <- XXc%*%matrix(B1[INCc,1])
  vc1 <- XXc%*%V1[INCc, INCc]%*%t(XXc)
  
  cr0 <- XXc%*%matrix(B0[INCc,1])
  vc0 <- XXc%*%V0[INCc, INCc]%*%t(XXc)
  
  lrc <- cr1 - cr0
  
  vlrc <- vc1 + vc0
  
  sd <- matrix(sqrt(diag(vlrc)))
  ci <- cbind(lrc - 1.96*sd, lrc + 1.96*sd)
  elrc <- exp(lrc)
  eci <- exp(ci)
  
  CRR <- cbind(c, elrc, eci)
  dimnames(CRR) <- list(c(), c("Coh", "CRR", "CILo", "CIHi"))
  
  
  
  XXp <- M1$Matrices$XPD
  INCp <- c(M1$Matrices$Pt[[5]])
  
  pd1 <- XXp%*%matrix(B1[INCp,1])
  vp1 <- XXp%*%V1[INCp, INCp]%*%t(XXp)
  
  pd0 <- XXp%*%matrix(B0[INCp,1])
  vp0 <- XXp%*%V0[INCp, INCp]%*%t(XXp)
  
  ldp <- pd1 - pd0
  
  vldp <- vp1 + vp0
  
  sd <- matrix(sqrt(diag(vldp)))
  ci <- cbind(ldp - 1.96*sd, ldp + 1.96*sd)
  eldp <- exp(ldp)
  eci <- exp(ci)
  
  EPD <- cbind(p, eldp, eci)
  dimnames(EPD) <- list(c(), c("Per", "EPD", "CILo", "CIHi"))
        
  FVAC = list(LAC = LAC, CRR = CRR, EPD = EPD)

  
  
  ###
  # FV(c|a) = FCP * ARL * EPD (fitted cohort pattern * LongAgeRR * EPD)
  #% PH-L if ARL and EPD are all equal across the two hazards but FCP is not.
  # IF PH-L holds, rate ratio depends only on cohort through
  #% RR(c | 1 vs 0) = FCP(c | hazard 1):FCP(c | hazard 0)
  ###
  # Includes a contrast of the age deviations in order to center the curve
  # on the referent age
  
  XXc <- M1$Matrices$XCT
  INCc <- c(1, 3, M1$Matrices$Pt[[6]], M1$Matrices$Pt[[4]])
  
  cr1 <- XXc%*%matrix(B1[INCc,1])
  vc1 <- XXc%*%V1[INCc, INCc]%*%t(XXc)
  
  cr0 <- XXc%*%matrix(B0[INCc,1])
  vc0 <- XXc%*%V0[INCc, INCc]%*%t(XXc)
  
  lrc <- cr1 - cr0
  
  vlrc <- vc1 + vc0
  
  sd <- matrix(sqrt(diag(vlrc)))
  ci <- cbind(lrc - 1.96*sd, lrc + 1.96*sd)
  elrc <- exp(lrc)
  eci <- exp(ci)
  
  FCP <- cbind(c, elrc, eci)
  dimnames(FCP) <- list(c(), c("Coh", "FCP", "CILo", "CIHi"))
  
  
  XXa <- M1$Matrices$LAR
  INCa <- c(1, 2, M1$Matrices$Pt[[4]])
  
  la1 <- XXa%*%matrix(B1[INCa,1])
  va1 <- XXa%*%V1[INCa, INCa]%*%t(XXa)
  
  la0 <- XXa%*%matrix(B0[INCa,1])
  va0 <- XXa%*%V0[INCa, INCa]%*%t(XXa)
  
  lra <- la1 - la0
  
  vlra <- va1 + va0
  
  sd <- matrix(sqrt(diag(vlra)))
  ci <- cbind(lra - 1.96*sd, lra + 1.96*sd)
  elra <- exp(lra)
  eci <- exp(ci)
  
  ARL <- cbind(a, elra, eci)
  dimnames(ARL) <- list(c(), c("Age", "ARL", "CILo", "CIHi"))
  
  
  
 FVCA = list(FCP = FCP, ARL = ARL, EPD = EPD)
  
  
  
  ###
  # FV(a|p) = CrossAge * PRR * ECD
  # PH-T if PRR and ECD are both equal across the two hazards but CrossAge is
  # not. If PH-T holds, rate ratio depends only on age through
  # RR(a|1 vs 0) = CrossAge(a, hazard 1):CrossAge(a,hazard 0)
  ###
  
  
  XXa <- M1$Matrices$XXA
  INCa <- c(1, 2, 3, M1$Matrices$Pt[[4]], M1$Matrices$Pt[[5]])
  
  la1 <- XXa%*%matrix(B1[INCa,1])
  va1 <- XXa%*%V1[INCa, INCa]%*%t(XXa)
  
  la0 <- XXa%*%matrix(B0[INCa,1])
  va0 <- XXa%*%V0[INCa, INCa]%*%t(XXa)
  
  lra <- la1 - la0
  
  vlra <- va1 + va0
  
  sd <- matrix(sqrt(diag(vlra)))
  ci <- cbind(lra - 1.96*sd, lra + 1.96*sd)
  elra <- exp(lra)
  eci <- exp(ci)
  
  CAC <- cbind(a, elra, eci)
  dimnames(CAC) <- list(c(), c("Age", "CAC", "CILo", "CIHi"))
  
  
  XXp <- M1$Matrices$XPR
  INCp <- c(1, 3, M1$Matrices$Pt[[5]])
  
  pd1 <- XXp%*%matrix(B1[INCp,1])
  vp1 <- XXp%*%V1[INCp, INCp]%*%t(XXp)
  
  pd0 <- XXp%*%matrix(B0[INCp,1])
  vp0 <- XXp%*%V0[INCp, INCp]%*%t(XXp)
  
  ldp <- pd1 - pd0
  
  vldp <- vp1 + vp0
  
  sd <- matrix(sqrt(diag(vldp)))
  ci <- cbind(ldp - 1.96*sd, ldp + 1.96*sd)
  eldp <- exp(ldp)
  eci <- exp(ci)
  
  PRR <- cbind(p, eldp, eci)
  dimnames(PRR) <- list(c(), c("Per", "PRR", "CILo", "CIHi"))
  
  
  
  XXc <- M1$Matrices$XCD
  INCc <- c(M1$Matrices$Pt[[6]])
  
  cr1 <- XXc%*%matrix(B1[INCc,1])
  vc1 <- XXc%*%V1[INCc, INCc]%*%t(XXc)
  
  cr0 <- XXc%*%matrix(B0[INCc,1])
  vc0 <- XXc%*%V0[INCc, INCc]%*%t(XXc)
  
  lrc <- cr1 - cr0
  
  vlrc <- vc1 + vc0
  
  sd <- matrix(sqrt(diag(vlrc)))
  ci <- cbind(lrc - 1.96*sd, lrc + 1.96*sd)
  elrc <- exp(lrc)
  eci <- exp(ci)
  
  ECD <- cbind(c, elrc, eci)
  dimnames(ECD) <- list(c(), c("Coh", "ECD", "CILo", "CIHi"))
  
  
  
  
  FVAP = list(CAC = CAC, PRR = PRR, ECD = ECD)
  
  
  
  ###
  # FV(p|a) = FTT * ARX * ECD (fitted temporal trends * CrossAgeRR * ECD)
  # PH-X if ARX and ECD are all equal across the two hazards but FTT is not.
  # If PH-X holds, then rate ratio depends only on period through
  # RR(p | 1 vs 0) = FTT(p, hazard 1):FTT(p, hazard 0)
  ###
  # Includes a contrast of the age deviations in order to center the FTT curve
  # on the referent age
  
  XXp <- M1$Matrices$XPT
  INCp <- c(1, 3, M1$Matrices$Pt[[5]], M1$Matrices$Pt[[4]])
  
  pd1 <- XXp%*%matrix(B1[INCp,1])
  vp1 <- XXp%*%V1[INCp, INCp]%*%t(XXp)
  
  pd0 <- XXp%*%matrix(B0[INCp,1])
  vp0 <- XXp%*%V0[INCp, INCp]%*%t(XXp)
  
  ldp <- pd1 - pd0
  
  vldp <- vp1 + vp0
  
  sd <- matrix(sqrt(diag(vldp)))
  ci <- cbind(ldp - 1.96*sd, ldp + 1.96*sd)
  eldp <- exp(ldp)
  eci <- exp(ci)
  
  FTT <- cbind(p, eldp, eci)
  dimnames(FTT) <- list(c(), c("Per", "FTT", "CILo", "CIHi"))
  
  
  XXa <- M1$Matrices$XLX
  # Includes a contrast of the cohort deviations to center the LongAge curve
  # on the referent cohort and a contrast of the period deviations to center
  # the CrossAge curve on the referent period
  
  INCa <- c(3, M1$Matrices$Pt[[6]], M1$Matrices$Pt[[5]])
  
  la1 <- XXa%*%matrix(B1[INCa,1])
  va1 <- XXa%*%V1[INCa, INCa]%*%t(XXa)
  
  la0 <- XXa%*%matrix(B0[INCa,1])
  va0 <- XXa%*%V0[INCa, INCa]%*%t(XXa)
  
  lra <- la1 - la0
  
  vlra <- va1 + va0
  
  sd <- matrix(sqrt(diag(vlra)))
  ci <- cbind(lra - 1.96*sd, lra + 1.96*sd)
  elra <- exp(lra)
  eci <- exp(ci)
  
  XLX <- cbind(a, elra, eci)
  dimnames(XLX) <- list(c(), c("Age", "XLX", "CILo", "CIHi"))
  
  
  # ECD defined above
  
  FVPA = list(FTT = FTT, XLX = XLX, ECD = ECD)
  
 
  ###
  # Ratio of intercepts only
  ###
  
  INC0 <- c(1)
  XX0 <- 1
  u1 <- XX0%*%matrix(B1[INC0,1])
  vu1 <- XX0%*%V1[INC0, INC0]%*%t(XX0)
  
  u0 <- XX0%*%matrix(B0[INC0,1])
  vu0 <- XX0%*%V0[INC0, INC0]%*%t(XX0)
  
  du <- u1 - u0
  
  vdu <- vu1 + vu0
  
  sd <- matrix(sqrt(diag(vdu)))
  ci <- cbind(du - 1.96*sd, du + 1.96*sd)
  edu <- exp(du)
  eci <- exp(ci)
  IO <- c(edu, eci)
  
  A <- length(a)
  P <- length(p)
  C <- length(c)
  aL <- a[cbind(1,A),1]
  pL <- p[cbind(1,P),1]
  cL <- c[cbind(1,C),1]
  
  IO <- cbind(rbind(aL,pL,cL), rbind(IO, IO, IO))

  
  dimnames(IO) <- list(c("age","period","cohort"), c("Lo", "HI", "IO", "CILo", "CIHi"))
  
  
  T = list(FVAC = FVAC, FVCA = FVCA, FVAP = FVAP, FVPA = FVPA, IO = IO)
  
}