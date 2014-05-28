
    module hermite
        contains
        subroutine HERZO(N,X,W)
!
!       ========================================================
!       Purpose : Compute the zeros of Hermite polynomial Ln(x)
!                 in the interval [-ì,ì], and the corresponding
!                 weighting coefficients for Gauss-Hermite
!                 integration
!       Input :   n    --- Order of the Hermite polynomial
!                 X(n) --- Zeros of the Hermite polynomial
!                 W(n) --- Corresponding weighting coefficients
!       ========================================================
!
        implicit double precision (A-H,O-Z)
        dimension X(N),W(N)
        HN=1.0D0/N
        ZL=-1.1611D0+1.46D0*N**0.5
        do 40 NR=1,N/2
           if (NR.EQ.1) Z=ZL
           if (NR.NE.1) Z=Z-HN*(N/2+1-NR)
           IT=0
10         IT=IT+1
           Z0=Z
           F0=1.0D0
           F1=2.0D0*Z
           do 15 K=2,N
              HF=2.0D0*Z*F1-2.0D0*(K-1.0D0)*F0
              HD=2.0D0*K*F1
              F0=F1
15            F1=HF
           P=1.0D0
           do 20 I=1,NR-1
20            P=P*(Z-X(I))
           FD=HF/P
           Q=0.0D0
           do 30 I=1,NR-1
              WP=1.0D0
              do 25 J=1,NR-1
                 if (J.EQ.I) go to 25
                 WP=WP*(Z-X(J))
25            continue
30            Q=Q+WP
           GD=(HD-Q*FD)/P
           Z=Z-FD/GD
           if (IT.LE.40.AND.DABS((Z-Z0)/Z).GT.1.0D-15) go to 10
           X(NR)=Z
           X(N+1-NR)=-Z
           R=1.0D0
           do 35 K=1,N
35            R=2.0D0*R*K
           W(NR)=3.544907701811D0*R/(HD*HD)
40         W(N+1-NR)=W(NR)
        if (N.NE.2*INT(N/2)) then
           R1=1.0D0
           R2=1.0D0
           do 45 J=1,N
              R1=2.0D0*R1*J
              if (J.GE.(N+1)/2) R2=R2*J
45              continue
                W(N/2+1)=0.88622692545276D0*R1/(R2*R2)
                X(N/2+1)=0.0D0
        endif
     
        end subroutine herzo
    end module hermite

