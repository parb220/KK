!sqsub --mpp=3G -q mpi -n 64 -o output30.log -r 5h ./socsec
!mpiexec -genvlist "C:\Program Files (x86)\VNI\imsl\fnl600\IA64\bin" -n 16 development
!Requin compiling
!module unload pathscale
!module load intel
!compile -intel -mpi constants.f90 distributions.f90 interpolate.f90 mathutils.f90 mherzo.f90 mtmod.f90 output.f90 discretizeAR1.f90 toms462.f90 parameters.f90 globals.f90 functions.f90 ComputeDistributions.f90 Policies.f90 simulations.f90 results.f90  results2.f90 OutputResults.f90 subroutines.f90 main.f90 -o socsec
!-ffree-line-length-0   this option removes the restriction on the number of characters per a line default is 132
!mpif90 -ffree-line-length-0 constants.f90 distributions.f90 interpolate.f90 mathutils.f90 mherzo.f90 mtmod.f90 output.f90 discretizeAR1.f90 toms462.f90 parameters.f90 globals.f90 functions.f90 ComputeDistributions.f90 Policies.f90 simulations.f90 results.f90  results2.f90 OutputResults.f90 subroutines.f90 main.f90 -o socsec
module subroutines    
    use functions
    use Results
    use Results2
    use OutputResults
    implicit none
    contains
    
subroutine master
    use Results
    use Results2
    use parameters
    use functions    
    use output
    use interpolate1     
    use mathutils, only : linspace    
    use distributions
    use discretizeAR1, only: Tauchen
    !use bcpol_int    
    implicit none
    include 'mpif.h'
   
    integer, intent(in) :: gbtype, getype 
    real(dbl) SumSquareMoments
    integer count1, count2, rate, i, j, ti, jm, married, s, h, jf, zf, zm
    real(dbl) ExpAveLifetimeProdfnorm
    real(dbl) ustatTemp(nef), check, uinit2(nef,nem), inc2, inc3, incrr, mweights(4)
    external SSM    
     

     
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    call system_clock(COUNT=count1, COUNT_RATE=rate)     
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !pause     
    !Guess on AggregateCapital
    !AggCapitalDemand = 2.1065d0 !1.888977658d0 !1.90799429d0  
    r= 0.115 !0.08957125d0   !baseline: 0.10302721d0 !no health exp baseline: 
    w = 1.0d0
    
    !Guess on AveEarningsAll
    AveEarningsAll = 0.727269132d0 !Baseline: 0.8367779d0 !NoSS: 0.837412599275927d0 !NoHealthExpense: 0.835439334d0 !no health exp No Medicaid No SS: 0.82436968d0
                                        !No heal exp Cfloorhalved No SS:0.830355301496494d0 
                                        
    !AveEarningsAll = 0.81643071d0 !NoHealthWelfareOnlyTaxFixed !0.8347964d0 NoHealthBaseline !0.83402111d0 NoHealthNoWelfare !0.828202878500065d0 NoHealthNoSSNoWelfare !0.832108557656853d0 NoHealthSSOnly
    !AveEarningsAll = 0.83974104d0 WelfareOnlyTaxFixed !0.8374126d0 WelfareOnly !0.828627852826614d0 NoProgram !0.816093071408190d0 SSOnly !0.8367779d0 Baseline

    AveEarningsAllNew = AveEarningsAll
    
    AveHHIncome = 1.400721580d0  !Baseline: 1.31884241d0 !NoSS: 1.344856143d0 !NoHealthExpense: 1.2905999568d0 !no health exp No Medicaid No SS: 1.434458956d0
                                        !No heal exp Cfloorhalved No SS:1.391988678d0 
                                        
    !AveHHIncome = 1.43281448d0 !NoHealthWelfareOnlyTaxFixed !1.29448015d0 NoHealthBaseline !1.40307982d0 NoHealthNoWelfare !1.446101305d0 NoHealthNoSSNoWelfare !1.296125394d0 NoHealthSSOnly
    !AveHHIncome = 1.30479099d0 WelfareOnlyTaxFixed !1.3448562d0 WelfareOnly !1.717272347d0 NoProgram !1.459366175d0 SSOnly !1.31884242d0 Baseline
                                                
    !Guess for government spending
    GovSpending = 0.05639641d0  !baseline: 0.10635448d0 !No HealthExp 1/2cfloors: 0.11099796d0 !No HealthNoMedicaidBaseline: 0.11146004d0 
    !PropTax = -0.001008786323954739d0  !NoSS: -0.040648062d0  !No heal exp NoSS: -0.099107840  !No health exp No Medicaid No SS: -0.26545746d0
                                        !No heal exp Cfloorhalved No SS: -0.210906281048865d0  
                                        
    PropTax = -0.196285699d0 !-0.000353247d0 !NoMedicaidTaxesFixed  !-0.07991605d0 NoHealthNoWelfare !-0.09392503780861847d0 NoHealthNoSSNoWelfare !-0.001008786323954739d0 NoHealthSSOnly  
    !PropTax = -0.010803193d0 !NoMedicaidTaxesFixed !-0.0406480d0 WelfareOnly -0.158129071d0 NoProgram !-0.043979071d0 SSOnly
                   
    EarnsTax =  0.00d0 !0.00780398d0                                    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (ScaleDownMedicaidBenefit == 1 .or. MoveSocSecToMedicaid == 1) then
        clowerbarMarried = MedicaidReduction*clowerbarMarried
        clowerbarWidow = MedicaidReduction*clowerbarWidow
        clowerbarWidower = MedicaidReduction*clowerbarWidower
    end if
    
    
    !Setup earnings grids  
    Cmat(1,1) = sqrt(2*(1+rhoe+rhoe**2))*sigma_epse
    Cmat(1,2)  = 0.0d0
    Cmat(2,1) = sqrt(2*(1+rhoe+rhoe**2))*sigma_epse*corr_epse
    Cmat(2,2) = sqrt(2*(1+rhoe+rhoe**2))*sigma_epse*sqrt(1 - corr_epse**2)  
        
    !ufinit(5) = 1.0d0 - sum(ufinit(1:4),1)   
    !uminit(5) = 1.0d0 - sum(uminit(1:4),1)   
    call Tauchen(rhoe**2, 0.0d0, 1.0d0, nef, 1.701758d0, uefgrid, Puefmat)
    call Tauchen(rhoe**2, 0.0d0, 1.0d0, nem, 1.400100d0, uemgrid, Puemmat)   
    !call Out(0,"uefgrid", uefgrid,ret)
    !call Out(0,"uemgrid", uemgrid,ret)
    !call Out(0,"Puefmat", Puefmat)
      
    !call GetInitialDistribution(uefgrid, uemgrid, sqrt(rhoe**2+(1+rhoe)**2+1)*sigma_epse, corr_epse0, uinit)
    !start with above. if lie outside 2/3 to .90 interval for ratio of initial to lifetime earnings variance of males
    !then deviate. Otherwise ok. closer to 2/3?? variance of lifetime earnings for males. 
    sigmaInitial(1) = sqrt(2.0d0)*sigma_0/Cmat(1,1)
    sigmaInitial(2) = sqrt(2.0d0*sigma_0**2*(Cmat(2,1)**2+Cmat(1,1)**2)-2*Cmat(1,1)*Cmat(2,1)*2.0d0*corr_epse0*sigma_0**2)/Cmat(1,1)/Cmat(2,2)
    corrInitial = (2.0d0*corr_epse0*sigma_0**2*Cmat(1,1)*Cmat(2,2)-2.0d0*sigma_0**2*Cmat(2,1)*Cmat(2,2))/Cmat(1,1)**2/Cmat(2,2)**2/sigmaInitial(1)/sigmaInitial(2)
    
    !call Out(0,"Cmat", Cmat)
    !call Out(0,"sigmaInitial", sigmaInitial, ret)
    !call Out(0,"corrInitial", corrInitial, ret)  
    
    call GetInitialDistribution(uefgrid, uemgrid, sigmaInitial(1), sigmaInitial(2), corr_epse0, uinit) 
    
    !uinit(1,1) = uinit2(1,1)
    !uinit(1,2) = uinit2(1,2)
    !uinit(2,1) = uinit2(2,1)
    !do i=1,nef
    !do j=1,nem
    !    if ((i==1 .and. j < 3) .or. (i==2 .and. j==1)) then
    !        continue
    !    else   
    !        uinit(i,j) = uinit2(i,j)*( 1-uinit(1,1)-uinit(1,2)-uinit(2,1) )/( sum(uinit2(2:,2:))+sum(uinit2(1,3:))+sum(uinit2(3:,1)))
    !    end if
    !end do
    !end do
    !call Out(0,"uinit", uinit)
    !call Out(0,"uefgrid", uefgrid, tab)
    !call Out(0,"uemgrid", uemgrid, tab)
    !call Out(0,"Puefmat", Puefmat)
    !call Out(0,"Puemmat", Puemmat)
    if (abs(sum(uinit)-1.0d0) > 1.d-7) then
        print *, "uinit not summing to 1!" 
        pause
    end if
    !pause 
      
      
    do i=1,nw
        efmat(:,:,1,i) = &
           log(sum((/(exp(betae0 + betae1*(4+j)+betae2*(4+j)**2+betae3*(4+j)**3),j=(i-1)*2,(i-1)*2+1)/),1)) + Cmat(1,1)*spread(uefgrid,2,nem)+Cmat(1,2)*spread(uemgrid,1,nef)
        efmat(:,:,2,i) =  &
           log(sum((/(exp(betae0 + betae4 + betae1*(j)+betae2*(j)**2+betae3*(j)**3),j=(i-1)*2,(i-1)*2+1)/),1)) + Cmat(1,1)*spread(uefgrid,2,nem)+Cmat(1,2)*spread(uemgrid,1,nef)
    end do    
    efmat = exp(efmat)    
    Puefmat = transpose(Puefmat) 

    do i=1,nw
        emmat(:,:,1,i) = &
           log(sum((/(exp(betae0 + betae1*(4+j)+betae2*(4+j)**2+betae3*(4+j)**3),j=(i-1)*2,(i-1)*2+1)/),1)) + Cmat(2,1)*spread(uefgrid,2,nem)+Cmat(2,2)*spread(uemgrid,1,nef)
        emmat(:,:,2,i) =  &
           log(sum((/(exp(betae0 + betae4 + betae1*(j)+betae2*(j)**2+betae3*(j)**3),j=(i-1)*2,(i-1)*2+1)/),1)) + Cmat(2,1)*spread(uefgrid,2,nem)+Cmat(2,2)*spread(uemgrid,1,nef)
    end do    
    emmat = exp(emmat)        
    Puemmat = transpose(Puemmat)  
    
    !call Out(0,"emmat(1,:,:,:)",emmat(1,:,:,:))
    !pause    
    !emmat(1,:,:,:) = 1.0d0
    
    !Renormalize productivities
    ProdDist = 0.0d0     
    ProdDist(:,:,1) = uinit
    do ti = 2,nw      
        do jf=1,nef
        do jm=1,nem                
            do zf=1,nef
            do zm=1,nem
                ProdDist(jf,jm,ti)  = ProdDist(jf,jm,ti) +   Puefmat(jf,zf) * Puemmat(jm,zm)*ProdDist(zf,zm,ti-1)                  
            end do
            end do
        end do
        end do
        check = sum(ProdDist(:,:,ti))
        if (abs(check - 1.0d0) > 1.e-8) then
            print *, "ProdDist", ti, "not summing to 1"
            print *, "check =", check
            pause
        end if
    end do
    
    !call Out(0,"ProdDist(1,:,:)",ProdDist(1,:,:))
    !pause
    
    ExpAveLifetimeProdfnorm=  sum(efmat(:,:,1,:)*ProdDist)/nw
    efmat = efmat/ExpAveLifetimeProdfnorm
    print *, "ExpAveLifetimeProdfnorm", ExpAveLifetimeProdfnorm  
    ExpAveLifetimeProdf(1) =  sum(efmat(:,:,1,:)*ProdDist)/nw
    ExpAveLifetimeProdf(2) =  sum(efmat(:,:,2,:)*ProdDist)/nw
    print *, "ExpAveLifetimeProdf", ExpAveLifetimeProdf         


    ExpAveLifetimeProdm(1) =  sum(emmat(:,:,1,:)*ProdDist)/nw
    emmat = emmat/ExpAveLifetimeProdm(1)
    print *, "ExpAveLifetimeProdm(1)", ExpAveLifetimeProdm(1) 
    ExpAveLifetimeProdm(1) =  sum(emmat(:,:,1,:)*ProdDist)/nw
    ExpAveLifetimeProdm(2) =  sum(emmat(:,:,2,:)*ProdDist)/nw
    print *, "ExpAveLifetimeProdm", ExpAveLifetimeProdm 
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
    call Tauchen(rhom**2, 0.0d0, sqrt(rhom**2+(1+rhom)**2+1)*sigma_epsm, npm-1, 1.3d0, umgrid(1:npm-1,1), Pummat(1:npm-1,1:npm-1,1)) !npm =5
    do j=2,nr-1
        Pummat(1:npm-1,1:npm-1,j) = Pummat(1:npm-1,1:npm-1,1)
    end do

    !incrr=0.015d0
    !do j=1,nr-1
    !    if (j<6) then
    !        inc2 = 1.2d0**(j-1)*0.0016d0
    !        inc3 = 0.5d0**(j-1)* 0.005d0
    !    else if (j<11) then
    !        inc2 = 1.2d0**(j-1)*0.0022d0
    !        inc3 = 0.5d0**(j-1)* 0.005d0      
    !    else
    !        inc2 = 1.2d0**(j-1)*0.0051d0
    !        inc3 = 0.5d0**(j-1)* 0.005d0       
    !    end if
    !    Pummat(1,5,j) = inc2
    !    Pummat(2,5,j) = 1.5*inc2
    !    Pummat(3,5,j) = 3.0*inc2
    !    Pummat(4,5,j) = 5.0*inc2    
    !    Pummat(5,1,j) = inc3
    !    Pummat(5,2,j) = inc3+incrr
    !    Pummat(5,3,j) = inc3+incrr*3
    !    Pummat(5,4,j) = inc3+incrr*6
    !    Pummat(5,5,j) = 1.0d0-sum(Pummat(5,1:4,j))
    !    
    !    Pummat(1,1:4,j) = Pummat(1,1:4,j)*(1-Pummat(1,5,j))
    !    Pummat(2,1:4,j) = Pummat(2,1:4,j)*(1-Pummat(2,5,j))
    !    Pummat(3,1:4,j) = Pummat(3,1:4,j)*(1-Pummat(3,5,j))
    !    Pummat(4,1:4,j) = Pummat(4,1:4,j)*(1-Pummat(4,5,j))
    !    Pummat(:,:,j) = transpose(Pummat(:,:,j))            
    !end do

    !initial distribution of medical expenses conditional on household education type        
    minit(:,1) = (/0.20d0,	0.20d0,	0.30d0,	0.30d0,	0.01d0/) !HH
    minit(1,1) = 1.0d0 - sum(minit(2:,1)) !HH
    !minit(:,1) = (/1.0d0,	0.0d0,	0.0d0,	0.0d0,	0.0d0/) !HH
    do i=2,4
        minit(:,i) = minit(:,1)!HC, CH, CC
    end do

    mweights=(/0.06d0, 0.1436d0, 0.2872d0, 0.4719d0/)
    mweights(1) = 1.0d0 - sum(mweights(2:4))
    MedDistSmall(1,:) = minit(:,1)    
    do j=1,nr-1
        Pummat(1,5,j) = mweights(1)*0.009*exp(0.228*j)*sum(MedDistSmall(j,:4))/sum(mweights*MedDistSmall(j,:4)) 
        Pummat(2,5,j) = mweights(2)*0.009*exp(0.228*j)*sum(MedDistSmall(j,:4))/sum(mweights*MedDistSmall(j,:4)) 
        Pummat(3,5,j) = mweights(3)*0.009*exp(0.228*j)*sum(MedDistSmall(j,:4))/sum(mweights*MedDistSmall(j,:4)) 
        Pummat(4,5,j) = mweights(4)*0.009*exp(0.228*j)*sum(MedDistSmall(j,:4))/sum(mweights*MedDistSmall(j,:4)) 
        Pummat(5,1,j) = 0.0d0
        Pummat(5,2,j) = 0.0d0
        Pummat(5,3,j) = 0.0d0
        Pummat(5,4,j) = 0.72d0
        Pummat(5,5,j) = 1.0d0-sum(Pummat(5,1:4,j))
        
        Pummat(1,1:4,j) = Pummat(1,1:4,j)*(1-Pummat(1,5,j))
        Pummat(2,1:4,j) = Pummat(2,1:4,j)*(1-Pummat(2,5,j))
        Pummat(3,1:4,j) = Pummat(3,1:4,j)*(1-Pummat(3,5,j))
        Pummat(4,1:4,j) = Pummat(4,1:4,j)*(1-Pummat(4,5,j))
        Pummat(:,:,j) = transpose(Pummat(:,:,j))    
        
        MedDistSmall(j+1,:) = matmul(Pummat(:,:,j),MedDistSmall(j,:))        
    end do    
    
    
    !umgrid(1:npm-1,1) = umgrid(1:npm-1,1)
    umgrid(npm,1) = NHshock !3.0d0
    
    umgrid(:,2) = umgrid(:,1)
    umgrid(:,3) = umgrid(:,1)

    !print *, "rho=", rhom**2
    !print *, "sigma=", sqrt(rhom**2+(1+rhom)**2+1)*sigma_epsm
    call Out(0,"umgrid", umgrid(:,1),ret)
    !call Out(0,"Prob 5 to 4", Pummat(4,5,:),tab)
    !call Out(0,"Prob x to 5", Pummat(5,:,:))
    call Out(0,"MedDistSmall", MedDistSmall(:,:))
    !call Out(0,"MedDistSmall", MedDistSmall(:,4),tab)
    !call Out(0,"prob entering", sum(Pummat(5,:4,:)*transpose(MedDistSmall(:nr-1,:4)),1)/sum(MedDistSmall(:nr-1,:4),2),tab)
    !pause

    open(12,file="betas.txt")
    read(12,*) betam, betahF, betahM, betasF, betasM
    close(12)    
 
    utmgrid(1) = sqrt(2.0d0)*sigma_tranm
    utmgrid(2) = -sqrt(2.0d0)*sigma_tranm
    Ptmvect(1) = 0.5d0
    Ptmvect(2) = 1.0d0 - Ptmvect(1)     
    
    open(13,file="hinits.txt")
    read(13,*) hinitF, hinitM
      !DO row = 1,max_rows
      !!    READ(11,*) (a(row,col),col=1,max_cols)
      !END DO    
    close(13)    
      
     
    !setup PTM's for health status    
    do ti=1,nr
    do s=1,3
        if (s==1) then 
            married = 1
        else 
            married = 0
        end if
    do h=1,nsht        
        PhmatF(2,h,s,ti) = 1/( 1+exp(-( betahF(1)+betahF(2)*(65.5+2*(ti-1))+betahF(3)*(65.5+2*(ti-1))**2+betahF(4)*married + &
            betahF(5)*married*(65.5+2*(ti-1)) + betahF(6)*(h-1) +betahF(7)*(h-1)*(65.5+2*(ti-1)) )) )
        PhmatM(2,h,s,ti) = 1/( 1+exp(-( betahM(1)+betahM(2)*(65.5+2*(ti-1))+betahM(3)*(65.5+2*(ti-1))**2+betahM(4)*married + &
            betahM(5)*married*(65.5+2*(ti-1)) +  betahM(6)*(h-1) +betahM(7)*(h-1)*(65.5+2*(ti-1)) )) )
        PhmatF(1,h,s,ti) = 1.0d0 - PhmatF(2,h,s,ti)
        PhmatM(1,h,s,ti) = 1.0d0 - PhmatM(2,h,s,ti)    
    end do
    end do
    end do
  
    !setup survival probabilities  
    do ti=1,nr-1
    do s=1,3
        if (s==1) then 
            married = 1
        else 
            married = 0
        end if   
    do h=1,nsht          
        survivalprobVectF(h,s,ti) = 1/( 1+exp(-( betasF(1)+betasF(2)*(65.5+2*(ti-1))+betasF(3)*(65.5+2*(ti-1))**2+betasF(4)*married + &
                betasF(5)*married*(65.5+2*(ti-1)) + betasF(6)*(h-1) +betasF(7)*(h-1)*(65.5+2*(ti-1)) )) )
        survivalprobVectM(h,s,ti) = 1/( 1+exp(-( betasM(1)+betasM(2)*(65.5+2*(ti-1))+betasM(3)*(65.5+2*(ti-1))**2+betasM(4)*married + &
                betasM(5)*married*(65.5+2*(ti-1)) + betasM(6)*(h-1) +betasM(7)*(h-1)*(65.5+2*(ti-1)) )) )            
    end do
    end do
    end do
    survivalprobVectF = survivalprobVectF*survAdjF
    survivalprobVectM = survivalprobVectM*survAdjM
    
       
    !pause
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
    !Setup asset grid            
    amin = 1.d-5
    !amax = sum( (/((1+r)**(nw-i) * efmat(nef,i), i=1,nw)/) )
    amax = 55.0d0 !110.0d0 !110.0d0 !55.0d0
    lnamin = log(amin)
    lnamax = log(amax)
    lnavect = (/(lnamin+(lnamax-lnamin)*i/(na-2),i=0,na-2)/)
    avect(1) = 0.0d0
    avect(2:) = exp(lnavect)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
                   
    call SSM(SumSquareMoments, gbtype, getype)
    
    !call bcpol(SSM, ibtype, xlb, xub, params, xguess = paramsGuess, ftol = ftol, fvalue = fvalue, maxfcn = maxfcn)
    !call Out(0,"params", params,tab)
    !call Out(0,"fvalue", fvalue, tab)
    !call SSM(n, params, SumSquareMoments)   
    
    call system_clock(COUNT=count2)
    print *, "total time =", real(count2 -count1)/real(rate)    
    
    call ComputeExPostStats1()
    call ComputeExPostStats2()
    call ComputeExPostStats3()
    call SaveResults()
end subroutine master

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine worker(my_rank, gbtype, getype)
    implicit none
    include 'mpif.h'
    
    real(dbl) postTaxWealth, preTaxIncomeF, preTaxIncomeM, c1R, alower, preTaxIncome, socsecIncome
    integer i, jf, jm, vf, vm, h, s, j, eh, em, ef, status(mpi_status_size), tag, ti, idef, idaef, idem, idaem, ideh, ides, idemed
    integer my_rank, wind(10), mStatus
    integer, intent(in):: gbtype, getype
    !print *, "my_rank =", my_rank
    
    !receive constant globals from master
    call mpi_bcast(mmat, 1, gbtype, 0, mpi_comm_world, ier)
    
    do  !do until no more work (curstat == 0)
        !receive curstat from master       
        call mpi_bcast(curstat, 1, mpi_integer, 0, mpi_comm_world, ier)
        if (curstat == 0) exit   
        !receive globals that change with every GE loop from master
        call mpi_bcast(efmat, 1, getype, 0, mpi_comm_world, ier)
        
        !retirees problem
        do  !do until this GE loop is completed (tag == 0) 
            !receive indices that dictate start and end point on asset grid for this worker 
            call mpi_recv(ind, 10, mpi_integer, 0, mpi_any_tag, mpi_comm_world, status, ier)
            tag = status(mpi_tag)
            !print *, "tag = ", tag
              
            if (tag == 0) exit !calculations for this GE loop are completed
            if (tag > 0) then
                if (tag == 1) then !ti has been updated
                    !obtain new value of ti from master
                    call mpi_bcast(ti,1,mpi_integer,0,mpi_comm_world,ier)
                    !receive ti+1 value functions from master                
                    call mpi_bcast(VnewRCubeMarried(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nhht*4, mpi_double_precision, 0, &
                        mpi_comm_world, ier)  
                    call mpi_bcast(UtilConsRCubeMarried(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nhht*4, mpi_double_precision, 0, &
                        mpi_comm_world, ier)               
                    call mpi_bcast(VnewRCubeWidow(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                        mpi_comm_world, ier)     
                    call mpi_bcast(UtilConsRCubeWidow(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                        mpi_comm_world, ier)     
                    call mpi_bcast(VnewRCubeWidower(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                        mpi_comm_world, ier)     
                    call mpi_bcast(UtilConsRCubeWidower(:,:,:,:,:,:,ti+1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                        mpi_comm_world, ier)                                                                  
                    !print *, "tag = 1 and my_rank =", my_rank                                     
                end if
                
                mStatus = max(tag-1,1)
                
                !regardless of tag compute optimal policies and value function for retiree's grid            
                do s = ind(1),ind(2) !s is the death status
                    ides = s - ind(1) + 1   
                do h = ind(3),ind(4)
                    ideh = h - ind(3) + 1    
                do vm = ind(5),ind(6) 
                    idem = vm - ind(5) + 1
                do vf = ind(7),ind(8)
                    idef = vf - ind(7) + 1
                    socsecIncome =  socsec(aveEarnFvect(vf),aveEarnMvect(vm),mStatus)
                do j = ind(9),ind(10)
                    idemed = j - ind(9) + 1                
                do i = 1,na
                    preTaxIncome = socsecIncome + avect(i) * r   
                    postTaxWealth = preTaxIncome + avect(i) - IncomeTax(avect(i)*r- max(0.0d0,avect(i)*r*capTax),socsecIncome,mmat(j,h,s,mStatus,ti),mStatus) - max(0.0d0,avect(i)*r*capTax)
                    c1R = postTaxWealth + transfersRetired(postTaxWealth, ti, j,h,s,mStatus,i) - mmat(j,h,s,mStatus,ti)                                                           
                    if (c1R < 1.e-8) then
                        print *, "neg here"
                        part_VRcube(i,idemed,idef,idem,ideh,ides) = -1d10
                        part_CURcube(i,idemed,idef,idem,ideh,ides) = -1d10
                    else if ((mStatus == 1 .and. s == 4) .or. (mStatus > 1 .and. s==2)) then
                        part_aPolicyRCube(i,idemed,idef,idem,ideh,ides) = 0.0d0
                        part_consRCube(i,idemed,idef,idem,ideh,ides) = c1R      
                        part_VRcube(i,idemed,idef,idem,ideh,ides) = U(part_consRCube(i,idemed,idef,idem,ideh,ides),0.0d0,mStatus,0)
                        part_CURcube(i,idemed,idef,idem,ideh,ides) = Ucons(part_consRCube(i,idemed,idef,idem,ideh,ides),mStatus)
                    else
                       if (i>1) then
                            alower = part_aPolicyRCube(i-1,idemed,idef,idem,ideh,ides)
                        else
                            alower = avect(1)
                        end if                     
                        call GoldenSectionSearchR(alower, minval((/avect(Na),c1R/)), c1R, ti, j, vf, vm, h, mStatus, s, &
                            part_aPolicyRCube(i,idemed,idef,idem,ideh,ides), part_VRcube(i,idemed,idef,idem,ideh,ides),part_CURcube(i,idemed,idef,idem,ideh,ides))                             
                        part_consRCube(i,idemed,idef,idem,ideh,ides) = c1R - part_aPolicyRCube(i,idemed,idef,idem,ideh,ides)   
                    end if                       
                               
                end do
                end do
                end do         
                end do 
                end do  
                end do
                !send policies and value function back to master 
                wind = ind
                !if (tag == 3 .and. ti == 16) then
                !    print *, "--------------------------"
                !    print *, "my_rank=", my_rank
                !    print *, "wind subroutines", wind
                !    print *, "--------------------------"
                !end if
                call mpi_send(wind,10,mpi_integer, 0, my_rank, mpi_comm_world, ier)              
                call mpi_send(part_aPolicyRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                   0, my_rank, mpi_comm_world, ier)
                call mpi_send(part_consRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                   0, my_rank, mpi_comm_world, ier)
                call mpi_send(part_VRcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                   0, my_rank, mpi_comm_world, ier)     
                call mpi_send(part_CURcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                   0, my_rank, mpi_comm_world, ier)                                                                                                                                                                         
            end if
        end do
        
        !workers problem
        do  !do until this GE loop is completed (tag == 0)                                
            !receive indices that dictate start and end point on asset grid for this worker 
            
            !print *, "my_rank =", my_rank, "loc= 1"
            
            call mpi_recv(ind, 10, mpi_integer, 0, mpi_any_tag, mpi_comm_world, status, ier)
            tag = status(mpi_tag)
            
            !print *, "my_rank =", my_rank, "loc= 2"
            
            if (tag == 0) exit !calculations for this GE loop are completed
            if (tag == 1) then !ti has been updated
                !obtain new value of ti from master
                
                !print *, "my_rank =", my_rank, "loc= 3"
                
                call mpi_bcast(ti,1,mpi_integer,0,mpi_comm_world,ier)
                
                !print *, "my_rank =", my_rank, "loc= 4 ti=", ti
                
                !receive ti+1 values functions from master
            if (ti == nw) then
                call mpi_bcast(VnewRCubeMarried(:,:,:,:,:,:,1), na*nm*naef*naem*nhht*4, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
                call mpi_bcast(VnewRCubeWidow(:,:,:,:,:,:,1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
                call mpi_bcast(VnewRCubeWidower(:,:,:,:,:,:,1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                    mpi_comm_world, ier)                                
            else
                call mpi_bcast(VnewWCube(:,:,:,:,:,:,ti+1), na*nef*nem*naef*naem*nhet, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
            end if
            if (ti == nw) then
                call mpi_bcast(UtilConsRCubeMarried(:,:,:,:,:,:,1), na*nm*naef*naem*nhht*4, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
                call mpi_bcast(UtilConsRCubeWidow(:,:,:,:,:,:,1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
                call mpi_bcast(UtilConsRCubeWidower(:,:,:,:,:,:,1), na*nm*naef*naem*nsht*2, mpi_double_precision, 0, &
                    mpi_comm_world, ier)                                
            else
                call mpi_bcast(UtilConsWCube(:,:,:,:,:,:,ti+1), na*nef*nem*naef*naem*nhet, mpi_double_precision, 0, &
                    mpi_comm_world, ier)
            end if                         
                
                !print *, "my_rank =", my_rank, "loc= 5"
                   
            end if
            !regardless of tag compute optimal policies and value function for worker's grid            
            do i=1,na
            do eh = ind(1),ind(2)  !education type                            
                ideh = eh - ind(1) + 1
                em = mod(eh+1,2)+1        
                ef = int((eh+1)/2)                                       
            do jf = ind(9),ind(10) !female earnings shock    
                idef = jf - ind(9) + 1
            do jm = ind(7),ind(8) !male earnings shock                     
                idem = jm - ind(7) + 1
                !compute for case of labor supply = 0.7d0
                preTaxIncomeF = w*efmat(jf,jm,ef,ti)*0.7d0 - SocSecTax(w*efmat(jf,jm,ef,ti)*0.7d0) + 0.5d0*r* avect(i) - EarnsTaxFn(w*efmat(jf,jm,ef,ti)*0.7d0)
                preTaxIncomeM = w*emmat(jf,jm,em,ti)*hbar - SocSecTax(w*emmat(jf,jm,em,ti)*hbar) + 0.5d0*r* avect(i) - EarnsTaxFn(w*emmat(jf,jm,em,ti)*hbar)
                postTaxWealth = preTaxIncomeF + preTaxIncomeM + avect(i) - IncomeTax(preTaxIncomeF+preTaxIncomeM- max(0.0d0,r*avect(i)*capTax)) - max(0.0d0,r*avect(i)*capTax)
                !if (ti == 1) postTaxWealth = postTaxWealth + btran(eh)
                c1R = postTaxWealth + transfers(postTaxWealth)   
            do vf = ind(5),ind(6)  !female average earnings   
                idaef = vf - ind(5) + 1   
            do vm = ind(3),ind(4)  !male average earnings   
                idaem = vm - ind(3) + 1       
         
                if (i>1) then       
                    alower = part_aPolicyCube(i-1,idef,idem,idaef,idaem,ideh)
                else
                    alower = avect(1)
                end if                                   
                call GoldenSectionSearchW(alower, minval((/avect(Na), c1R/)), part_aPolicyCube(i,idef,idem,idaef,idaem,ideh), &
                    part_Vcube(i,idef,idem,idaef,idaem,ideh), part_labCube(i,idef,idem,idaef,idaem,ideh),i,jf,jm,vf,vm,eh,ti,part_CUcube(i,idef,idem,idaef,idaem,ideh))
                preTaxIncomeF = w*efmat(jf,jm,ef,ti)*part_labCube(i,idef,idem,idaef,idaem,ideh) &
                    - SocSecTax(w*efmat(jf,jm,ef,ti)*part_labCube(i,idef,idem,idaef,idaem,ideh)) + 0.5d0*r* avect(i) &
                    - EarnsTaxFn(w*efmat(jf,jm,ef,ti)*part_labCube(i,idef,idem,idaef,idaem,ideh))                           
                postTaxWealth = preTaxIncomeF + preTaxIncomeM + avect(i) - IncomeTax(preTaxIncomeF+preTaxIncomeM- max(0.0d0,r*avect(i)*capTax)) - max(0.0d0,r*avect(i)*capTax)
                !if (ti == 1) postTaxWealth = postTaxWealth + btran(eh)            
                part_consCube(i,idef,idem,idaef,idaem,ideh) = postTaxWealth + transfers(postTaxWealth) - part_aPolicyCube(i,idef,idem,idaef,idaem,ideh)     
            end do                     
            end do  
            end do  
            end do
            end do
            end do
            !send policies and value function back to master 
            wind = ind
            call mpi_send(wind,10,mpi_integer, 0, my_rank, mpi_comm_world, ier)              
            call mpi_send(part_aPolicyCube,na*count_wef*count_wem*count_waef*count_waem*count_wet,mpi_double_precision, &
               0, my_rank, mpi_comm_world, ier)
            call mpi_send(part_consCube,na*count_wef*count_wem*count_waef*count_waem*count_wet,mpi_double_precision, &
               0, my_rank, mpi_comm_world, ier)
            call mpi_send(part_labCube,na*count_wef*count_wem*count_waef*count_waem*count_wet,mpi_double_precision, &
               0, my_rank, mpi_comm_world, ier)
            call mpi_send(part_Vcube,na*count_wef*count_wem*count_waef*count_waem*count_wet,mpi_double_precision, &
               0, my_rank, mpi_comm_world, ier)
            call mpi_send(part_CUcube,na*count_wef*count_wem*count_waef*count_waem*count_wet,mpi_double_precision, &
               0, my_rank, mpi_comm_world, ier)                    
         end do   
    
     end do
end subroutine worker

end module subroutines

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine SSM(SumSquareMoments, gbtype, getype)
    use ComputeDistributions
    use Policies
    use Results
    use Results2
    !use eqsolver
    use mathutils
    !use neqnf_int
    implicit none
    include 'mpif.h'

	interface
        subroutine EqmConds(Np, in, eqs, iflag, getype)  !for Powell
	        use ComputeDistributions
	        use Policies
	        use mathutils
	        use Results
	        use Results2    
            implicit none
            integer, intent(in):: Np
            real(dbl), intent(in) :: in(Np)
            real(dbl), intent(out):: eqs(Np) 
            integer iflag
            integer, intent(in) :: getype                   
        end subroutine EqmConds      
 	end interface  
 	
	interface
         subroutine BaseConds(N,input,consts,iflag, getype)       
            use globals
            implicit none
            integer N
            real(dbl), intent(in) :: input(2)
            real(dbl)  consts(2), input2(3), output(3)   
            integer iflag                
            integer, intent(in) :: getype
         end subroutine BaseConds   
         
         subroutine BaseCondsCapIter(N,input,consts,iflag, getype)       
            use globals
            implicit none
            integer N
            real(dbl), intent(in) :: input
            real(dbl)  consts(1), input2(2), output(2)   
            integer iflag              
            integer, intent(in) :: getype
         end subroutine BaseCondsCapIter                  
 	end interface 	
    
    !Variable declarations

    real(dbl) SumSquareMoments, check, TotalExps, total
    !parameters
    integer ti, i, j, z, v, s, h, iff, im, jf, jm, vf, vm, hf, hm, tm, pm, eh, OuterLoopIter1, OuterLoopIter2
    integer pj, tj, h1, hm1, hf1, pz, d, df, dm, em, ef, et
    integer, parameter:: Np = 3   
    real(dbl) aggvars(Np), eqs(Np), fnorm, denorm
    integer, intent(in): gbtype, getype
    
    integer iopt, nprint, info
    integer, parameter:: LWA = 100
    real(dbl) fvec(Np), wa(LWA)
    external Jac
           
        
    !pause    
        
    !autocorrMedExp = 0.0d0
    !do i =1,nm    
    !    autocorrMedExp = autocorrMedExp + sum(umgrid*PumMat(i,:),1)*mstat(i)*umgrid(i)
    !end do
    !autocorrMedExp = autocorrMedExp/sum(mstat*umgrid**2,1)    

    !Setup medical expense grid    
    !shock, health status, marital status, age      
    do i=1,nr              
    do tm = 1,ntm
    do pm = 1,npm   
    
        !widows, bad health, not deathyear
        lnmmat((tm-1)*npm+pm,1,1,2,i) = log(sum((/(exp(betam(1)+betam(2)*(65+j)+ betam(3)*(65+j)**2 + betam(4)*(65+j)**3 + betam(5)*(65+j)**4  &
             ),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,2) + utmgrid(tm)   
        lnmmat((tm-1)*npm+pm,3,1,2,i) = lnmmat((tm-1)*npm+pm,1,1,2,i)
        !widows, good health, not deathyear
        lnmmat((tm-1)*npm+pm,2,1,2,i) = log(sum((/(exp(betam(1)+betam(6)+(betam(2)+betam(7))*(65+j)+ betam(3)*(65+j)**2 + betam(4)*(65+j)**3 & 
            + betam(5)*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,2) + utmgrid(tm)              
        lnmmat((tm-1)*npm+pm,4,1,2,i) = lnmmat((tm-1)*npm+pm,2,1,2,i)       
        !widowers, bad health, not deathyear
        lnmmat((tm-1)*npm+pm,1,1,3,i) = log(sum((/(exp(betam(1)+betam(8)+(betam(2)+betam(9))*(65+j)+ (betam(3)+betam(10))*(65+j)**2 &
            + (betam(4)+betam(11))*(65+j)**3 + (betam(5)+betam(12))*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,3) + utmgrid(tm) 
        lnmmat((tm-1)*npm+pm,3,1,3,i) = lnmmat((tm-1)*npm+pm,1,1,3,i)        
        !widowers, good health, not deathyear
        lnmmat((tm-1)*npm+pm,2,1,3,i) = log(sum((/(exp(betam(1)+betam(6)+betam(8)+(betam(2)+betam(7)+betam(9))*(65+j)+ (betam(3)+betam(10))*(65+j)**2 &
            + (betam(4)+betam(11))*(65+j)**3 + (betam(5)+betam(12))*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1))  + umgrid(pm,3) + utmgrid(tm)                   
        lnmmat((tm-1)*npm+pm,4,1,3,i) = lnmmat((tm-1)*npm+pm,2,1,3,i) 
        
        !widows, bad health, deathyear
        lnmmat((tm-1)*npm+pm,1,2,2,i) = log(sum((/(exp(betam(1)+betam(18)+(betam(2)+betam(19))*(65+j)+ betam(3)*(65+j)**2 + betam(4)*(65+j)**3 + betam(5)*(65+j)**4  &
             ),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,2) + utmgrid(tm)   
        lnmmat((tm-1)*npm+pm,3,2,2,i) = lnmmat((tm-1)*npm+pm,1,2,2,i)
        !widows, good health, deathyear
        lnmmat((tm-1)*npm+pm,2,2,2,i) = log(sum((/(exp(betam(1)+betam(6)+betam(18)+(betam(2)+betam(7)+betam(19))*(65+j)+ betam(3)*(65+j)**2 + betam(4)*(65+j)**3 & 
            + betam(5)*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,2) + utmgrid(tm)              
        lnmmat((tm-1)*npm+pm,4,2,2,i) = lnmmat((tm-1)*npm+pm,2,2,2,i)       
        !widowers, bad health, deathyear
        lnmmat((tm-1)*npm+pm,1,2,3,i) = log(sum((/(exp(betam(1)+betam(8)+betam(18)+(betam(2)+betam(9)+betam(19))*(65+j)+ (betam(3)+betam(10))*(65+j)**2 &
            + (betam(4)+betam(11))*(65+j)**3 + (betam(5)+betam(12))*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,3) + utmgrid(tm) 
        lnmmat((tm-1)*npm+pm,3,2,3,i) = lnmmat((tm-1)*npm+pm,1,2,3,i)        
        !widowers, good health, deathyear
        lnmmat((tm-1)*npm+pm,2,2,3,i) = log(sum((/(exp(betam(1)+betam(6)+betam(8)+betam(18)+(betam(2)+betam(7)+betam(9)+betam(19))*(65+j)+ (betam(3)+betam(10))*(65+j)**2 &
            + (betam(4)+betam(11))*(65+j)**3 + (betam(5)+betam(12))*(65+j)**4),j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,3) + utmgrid(tm)                   
        lnmmat((tm-1)*npm+pm,4,2,3,i) = lnmmat((tm-1)*npm+pm,2,2,3,i)                
        
        do d=1,4 
            df = int((d+1)/2)-1
            dm = mod(d+1,2)               
        do h=1,nhht
            hf = int((h+1)/2)-1
            hm = mod(h+1,2)
          
            !married: BB, BG, GB, GG
            lnmmat((tm-1)*npm+pm,h,d,1,i) = log(sum((/(exp(betam(1)+betam(6)*hf+betam(13)+betam(18)*df+betam(20)*df+(betam(2)+betam(7)*hf+betam(14)+betam(19)*df)*(65+j) &
                 + (betam(3)+betam(15))*(65+j)**2 + (betam(4)+betam(16))*(65+j)**3 + (betam(5)+betam(17))*(65+j)**4) &
                 + exp(betam(1)+betam(6)*hm+betam(8)+betam(13)+betam(18)*dm+betam(20)*dm+(betam(2)+betam(7)*hm+betam(9)+betam(14)+betam(19)*dm)*(65+j) &
                 + (betam(3)+betam(10)+betam(15))*(65+j)**2 + (betam(4)+betam(11)+betam(16))*(65+j)**3 + (betam(5)+betam(12)+betam(17))*(65+j)**4) &            
                  ,j=(i-1)*2,(i-1)*2+1)/),1)) + umgrid(pm,1) + utmgrid(tm)        
        end do  
        end do
    end do
    end do  
    end do
    mmat = exp(lnmmat)  
    
    !call Out(0,"umgrid(:,1)", umgrid(:,1),ret)
    !call Out(0,"umgrid(:,1)", umgrid(:,3),ret)
    !call Out(0,"utmgrid", utmgrid,ret)
    !call Out(0,"betam", betam,ret)
    !call Out(0, "lnmmat", lnmmat(1,:,:,:,1) )
    !call Out(0, "mmat", mmat(1,:,:,:,1) )
    
    mmat = mmat/maxval(mmat)*medscale        
    
    if (ExpensesZero == 1) then
        mmat = 0.0d0
    end if          
    !call Out(0, "mmat", mmat(1,:,:,:,1)-umgrid(1,1) - utmgrid(1) )
    call Out(0, "mmat", mmat(1,1,1,1,:),ret)
    !pause
    
    do vf=1,nsht
    do jf=1,nset    
    do vm=1,nsht
    do jm=1,nset
        hinitH(vf+2*(vm-1),jf+2*(jm-1)) = hinitF(vf,jf)*hinitM(vm,jm)        
    end do
    end do 
    end do
    end do          
     
 !   MedExpDist = 0.0d0     
 !   do i=1,nm
 !       pm = i-npm*((i-1)/npm)
 !       tm = (i-1)/npm + 1     
 !   do h=1,nhht
 !   do s=1,3
 !   do eh=1,nhet
 !       MedExpDist(i,h,s,1) = MedExpDist(i,h,s,1) + minit(pm,eh) * hinitH(h,eh)*educDist(eh)*sinitH(s)*Ptmvect(tm)
 !   end do
 !   end do
 !   end do    
 !   end do
 !   check = sum(MedExpDist(:,:,:,1))
 !   if (abs(check - 1.0d0) > 1.e-8) then
 !       print *, "MedExpDist", 1, "not summing to 1"
 !      print *, "check =", check
 !       pause
 !   end if        
        
 !   do ti = 2,nr      
 !       do j=1,nm
 !           pj = j-npm*((j-1)/npm)
 !           tj = (j-1)/npm + 1  
 !       do h=1,nhht
 !           hm = mod(h+1,2)+1        
 !           hf = int((h+1)/2)         
 !       do s=1,3 
 !           do h1=1,nhht
 !               hm1 = mod(h1+1,2)+1        
 !               hf1 = int((h1+1)/2)                        
 !           do z=1,nm
 !               pz = z-npm*((z-1)/npm)                        
 !               if (s==1) then
 !                   MedExpDist(j,h1,1,ti)  = MedExpDist(j,h1,1,ti) + &
 !                       survivalProbVectF(hf,s,ti-1)*survivalProbVectM(hm,s,ti-1)*PumMat(pj,pz,s)*Ptmvect(tj)*PhmatF(hf1, hf, s, ti-1)*PhmatM(hm1, hm, s, ti-1)*MedExpDist(z,h,s,ti-1) 
                    !print *, "----------------------------------"
                    !print *, "sum=", survivalProbVectF(hf,s,ti-1)*survivalProbVectM(hm,s,ti-1)*PumMat(pj,pz,s)*Ptmvect(tj)*PhmatF(hf1, hf, s, ti-1)*PhmatM(hm1, hm, s, ti-1)*MedExpDist(z,h,s,ti-1) 
                    !print *, "s=", s, "j=", j, "h1=", h1, "pz=", pz, "pj=", pj, "tj=", tj, "hm=", hm, "hf=", hf
                    !print *, "survivalProbVectF(hf,s,ti-1)=", survivalProbVectF(hf,s,ti-1)
                    !print *, "survivalProbVectM(hm,s,ti-1)=", survivalProbVectM(hm,s,ti-1)
                    !print *, "PumMat(pj,pz,s)=" , PumMat(pj,pz,s)      
                    !print *, "Ptmvect(tj)=" , Ptmvect(tj)  
                    !print *, "PhmatF(hf1, hf, s, ti-1)=" , PhmatF(hf1, hf, s, ti-1)   
                   ! print *, "PhmatM(hm1, hm, s, ti-1)=" , PhmatM(hm1, hm, s, ti-1)   
                    !print *, "----------------------------------"                        
 !                   MedExpDist(j,h1,2,ti)  = MedExpDist(j,h1,2,ti) + survivalProbVectF(hf,s,ti-1)*(1-survivalProbVectM(hm,s,ti-1)) * PumMat(pj,pz,4)*Ptmvect(tj)*PhmatF(hf1, hf, s, ti-1)*PhmatM(hm1, hm, s, ti-1) *MedExpDist(z,h,s,ti-1) 
 !                   MedExpDist(j,h1,3,ti)  = MedExpDist(j,h1,3,ti) +  (1-survivalProbVectF(hf,s,ti-1))*survivalProbVectM(hm,s,ti-1) * PumMat(pj,pz,5)*Ptmvect(tj)*PhmatF(hf1, hf, s, ti-1)*PhmatM(hm1, hm, s, ti-1) *MedExpDist(z,h,s,ti-1) 
 !               else if (s==2) then
 !                   MedExpDist(j,h1,2,ti)  = MedExpDist(j,h1,2,ti) + survivalProbVectF(hf,2,ti-1) * PumMat(pj,pz,2)*Ptmvect(tj)*PhmatF(hf1, hf, 2, ti-1)*PhmatM(hm1, hm, 2, ti-1) *MedExpDist(z,h,s,ti-1) 
 !               else
 !                   MedExpDist(j,h1,3,ti)  = MedExpDist(j,h1,3,ti) + survivalProbVectM(hm,3,ti-1) * PumMat(pj,pz,3)*Ptmvect(tj)*PhmatF(hf1, hf, 3, ti-1)*PhmatM(hm1, hm, 3, ti-1) *MedExpDist(z,h,s,ti-1) 
 !               end if                                 
 !           end do
 !           end do
 !       end do
 !       end do
 !       end do
 !       total = 0.0d0
 !       do h=1,nhht
 !           hm = mod(h+1,2)+1        
 !           hf = int((h+1)/2)   
 !           total = total +  (survivalProbVectF(hf,1,ti-1)+survivalProbVectM(hm,1,ti-1)-survivalProbVectF(hf,1,ti-1)*survivalProbVectM(hm,1,ti-1))*dist(h,1,ti-1) &
 !                           + survivalProbVectF(hf,2,ti-1)*dist(h,2,ti-1)  +  survivalProbVectM(hm,3,ti-1)*dist(h,3,ti-1) 
 !       end do
        !print *, "total=", total       
 !       MedExpDist(:,:,:,ti) = MedExpDist(:,:,:,ti)/total       
       ! call Out(0,"MedExpDist(:,:,:,2)",MedExpDist(:,:,:,1)) 
        !call Out(0,"MedExpDist(:,:,:,2)",MedExpDist(:,:,:,2))
 !       check = sum(MedExpDist(:,:,:,ti))
        !print *, "check =", check
        !pause
 !       if (abs(check - 1.0d0) > 1.e-8) then
 !           print *, "MedExpDist", ti, "not summing to 1"
 !          print *, "check =", check
 !           pause
 !       end if
 !   end do       
      
    !call Out(0,"mmat(1)", mmat(:,:,:,1))
    
   ! call Out(0,"MedExpDist(1)", MedExpDist(:,:,:,1))
    
    !TotalExps =  sum(mmat(:,:,:,1)*MedExpDist(:,:,:,1))
    
    !call Out(0,"TotalExps", TotalExps,tab)
!    TotalExps = 0.0d0
!    do ti=1,nr
!        TotalExps = TotalExps + sum(mmat(:,:,:,ti)*MedExpDist(:,:,:,ti))*CohortWeights(nw+ti)
!    end do
!    call Out(0,"TotalExps", TotalExps,tab)
    !pause

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    AveMaleEarnings = 0.0d0
    do ti=1,nw    
        AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,1,ti)*ProdDist(:,:,ti))*(educDist(1)+educDist(3))/(1+ng)**(ti-1)
        AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,2,ti)*ProdDist(:,:,ti))*(educDist(2)+educDist(4))/(1+ng)**(ti-1)
    end do
    
    AveMaleEarnings = AveMaleEarnings/sum((/(1.0d0/(1+ng)**j,j=0,nw-1)/))
    emmat = emmat/AveMaleEarnings
    efmat = efmat*gendergap/AveMaleEarnings
    AveMaleEarnings = 0.0d0
    do ti=1,nw    
        AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,1,ti)*ProdDist(:,:,ti))*(educDist(1)+educDist(3))/(1+ng)**(ti-1)
        AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,2,ti)*ProdDist(:,:,ti))*(educDist(2)+educDist(4))/(1+ng)**(ti-1)
        AveMaleWagesByAgeEduc(ti,1) = sum(emmat(:,:,1,ti)*ProdDist(:,:,ti))
        AveMaleWagesByAgeEduc(ti,2) = sum(emmat(:,:,2,ti)*ProdDist(:,:,ti))
        AveFemaleWagesByAgeEduc(ti,1) = sum(efmat(:,:,1,ti)*ProdDist(:,:,ti))
        AveFemaleWagesByAgeEduc(ti,2) = sum(efmat(:,:,2,ti)*ProdDist(:,:,ti))        
    end do 
    AveMaleEarnings = AveMaleEarnings/sum((/(1.0d0/(1+ng)**j,j=0,nw-1)/))  
    AveEarningsInit = AveMaleEarnings    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    
    if (NoEarningsRisk == 1) then
        do em=1,nem
        do ef=1,nef
        do et = 1,nset
        do ti = 1,nw
           emmat(ef,em, et, ti) =  AveMaleWagesByAgeEduc(ti,et)
           efmat(ef,em, et, ti) =  AveFemaleWagesByAgeEduc(ti,et)
        end do
        end do
        end do
        end do
    end if
             
    !efmat = efmat*1.1d0
    !emmat = emmat*0.944d0
    !AveMaleEarnings = 0.0d0
    !do ti=1,nw    
    !    AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,1,ti)*ProdDist(:,:,ti))*(educDist(1)+educDist(3))/(1+ng)**(ti-1)
    !    AveMaleEarnings = AveMaleEarnings + sum(hbar*emmat(:,:,2,ti)*ProdDist(:,:,ti))*(educDist(2)+educDist(4))/(1+ng)**(ti-1)
    !end do
    
    !AveMaleEarnings = AveMaleEarnings/sum((/(1.0d0/(1+ng)**j,j=0,nw-1)/)) 
    !AveEarningsInit = AveMaleEarnings      
    
    !print *, "AveMaleEarnings=", AveMaleEarnings       
    !efmat(:,:,:,1) = efmat(:,:,:,1)*1.1d0
    !pause
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!           
    
    !call ComputeExAnteStats()
    !call Out(0,"Frac65plus", 0.18d0, Frac65plus,tab)      
    !pause
    !pause
    !call MedExpSimulations()
    !call WealthSimulations()
    !call simulations1()
    !call Out(0,"frac 65 year-olds NH", 0.27d0, fracNH,tab) 
    !call Out(0,"Fraction age 1st NH entry 65-74", 0.22d0, distNHentryage(1),tab)
    !call Out(0,"Fraction age 1st NH entry 75-84", 0.46d0, distNHentryage(2),tab)
    !call Out(0,"Fraction age 1st NH entry 85+", 0.31d0, distNHentryage(3),tab)
    !call Out(0,"aveyearsNH 65-74", 3.9d0, aveyearsNH(1),tab)
    !call Out(0,"aveyearsNH 75-84", 3.2d0, aveyearsNH(2),tab)
    !call Out(0,"aveyearsNH 85+", 2.9d0, aveyearsNH(3),tab)      
    !Frac65plusbs = sum(MedDistSmall(1:,5)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
    !Frac6574bs = sum(MedDistSmall(1:5,5)*CohortWeights(nw+1:nw+5))/sum(CohortWeights(nw+1:nw+5))
    !Frac7584bs = sum(MedDistSmall(6:10,5)*CohortWeights(nw+6:nw+10))/sum(CohortWeights(nw+6:nw+10))
    !Frac85plusbs = sum(MedDistSmall(11:,5)*CohortWeights(nw+11:))/sum(CohortWeights(nw+11:))
    !call Out(0,"Frac65plusbs", 0.086d0, Frac65plusbs,tab)
    !call Out(0,"Frac6574bs", 0.040d0, Frac6574bs,tab)
    !call Out(0,"Frac7584bs", 0.080d0, Frac7584bs,tab)
    !call Out(0,"Frac85plusbs", 0.21d0, Frac85plusbs,tab)  
    !pause    
    
    if (ResultsOnly == 1) then 
        open(1,file = "solution.dat")
        read (1,*) psiW
        read (1,*) psiRMarried
        read (1,*) psiRWidow
        read (1,*) psiRWidower
        read(1,*) aPolicyWCube
        read(1,*) aPolicyRCubeMarried
        read(1,*) aPolicyRCubeWidow
        read(1,*) aPolicyRCubeWidower
        read(1,*) consumptionWCube
        read(1,*) consumptionRCubeMarried
        read(1,*) consumptionRCubeWidow
        read(1,*) consumptionRCubeWidower        
        read(1,*) labWCube
        read(1,*) GovSpending
        read(1,*) AggCapitalSupply
        read(1,*) AggCons
        read(1,*) AggOutput
        read(1,*) TotMedByAgeID
        read(1,*) AggMedExp    
        read(1,*) MedExpDistMarried      
        read(1,*) MedExpDistWidow       
        read(1,*) MedExpDistWidower              
        read(1,*) r
        read(1,*) A
        read(1,*) w
        read(1,*) GovTransfers
        read(1,*) IncomeTaxes
        !read(1,*) AccBequests 
        read(1,*) AveMaleEarnings    
        read(1,*) aveEarnFvect
        read(1,*) aveEarnMvect
        
        !send constant globals to workers
        call mpi_bcast(mmat, 1, gbtype, 0, mpi_comm_world, ier)            
        !broadcast to workers that there is no more work (curstat = 0)
        curstat = 0
        call mpi_bcast(curstat, 1, mpi_integer, 0, mpi_comm_world, ier)             
    else  
        !send constant globals to workers
        call mpi_bcast(mmat, 1, gbtype, 0, mpi_comm_world, ier)
        
        aggvars(2) = AveEarningsAllNew        
        aggvars(3) = r
        
        curstat = 1
        
        if (OpenEconomy == 1) then        
            if (GEexperiment == 0) then            
                if (PartialEqm == 1 .or. RunOnce == 1) then 
                    call BaseConds(Np, aggvars(2:3), fvec(2:3), 0)                      
                else
                    iopt = 2   
                    nprint = 0 
                                        
                                      
                    OuterLoopIter2 = 0
                    aggvars(3) = r                    
                    tolOuterLoop = tolOuterLoop2
                    do
                        OuterLoopIter2 = OuterLoopIter2 + 1
                        print *, "OuterLoopIter2=", OuterLoopIter2
                        call BaseCondsCapIter(Np-2, aggvars(2), fvec(2), 0)
                        aggvars(2) = AveEarningsAllNew
                        print '(a,es10.2)', "fvec(2) = ", fvec(2)
                        if (abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do 
                    aggvars(3) = r                   
                    !call DNSQE(BaseCondsCapIter, Jac, iopt, Np-2, aggvars(2:3), fvec(2:3), tolOuterLoop, nprint, info, wa, lwa)
                    !fnorm = denorm(Np-2,fvec(2:3))
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)                                         
                    
                    !call DNSQE(BaseConds, Jac, iopt, Np-1, aggvars(2:4), fvec(2:4), tolGovBudget, nprint, info, wa, lwa)
                    !fnorm = denorm(Np-1,fvec(2:4))
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)          
                end if                         
                LaborMarketClearing = fvec(2)              
            else
                if (UseEarnsTax == 1) then
                    aggvars(1) = EarnsTax
                else
                    aggvars(1) = PropTax           
                end if
                if (RunOnce == 1) then            
                    call EqmConds(Np, aggvars, eqs, 0, getype)   
                else if (PartialEqm == 1) then  
                    iopt = 2   
                    nprint = 0 
                                        
                                      
                    OuterLoopIter2 = 0
                    aggvars(3) = r                    
                    tolOuterLoop = tolOuterLoop2
                    do
                        OuterLoopIter2 = OuterLoopIter2 + 1
                        print *, "OuterLoopIter2=", OuterLoopIter2
                        call BaseCondsCapIter(Np-2, aggvars(2), fvec(2), 0)
                        aggvars(2) = AveEarningsAllNew
                        print '(a,es10.2)', "fvec(2) = ", fvec(2)
                        if (abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do 
                    aggvars(3) = r                           
                else                
                    iopt = 2   
                    nprint = 0        
                                                          
                    OuterLoopIter2 = 0
                    aggvars(3) = r
                    tolCapital = tolCapital2
                    tolOuterLoop = tolOuterLoop2
                    do
                        OuterLoopIter2 = OuterLoopIter2 + 1
                        print *, "OuterLoopIter2=", OuterLoopIter2
                        call EqmConds(Np-1, aggvars(1:2), fvec(1:2), 0, getype)
                        if (UseEarnsTax == 1) then
                            aggvars(1) = EarnsTaxNew*1.0d0 + EarnsTax*0.0d0
                            EarnsTax = EarnsTaxNew                                                    
                        else
                            aggvars(1) = aggvars(1) - fvec(1)
                        end if
                        aggvars(2) = AveEarningsAllNew
                        print '(a,es10.2,a,es10.2)', "fvec(1) = ", fvec(1), "fvec(2) = ", fvec(2)
                        if (abs(fvec(1)) < tolOuterLoop .and. abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do 
                    aggvars(3) = r                 
                    
                    !call DNSQE(EqmConds, Jac, iopt, Np-1, aggvars(1:2), fvec(1:2), 1d-3, nprint, info, wa, lwa)
                    !fnorm = denorm(Np,fvec)
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)    
                    !aggvars(4) = r
                    
                    !call DNSQE(EqmConds, Jac, iopt, Np, aggvars, fvec, tolGovBudget, nprint, info, wa, lwa)
                    !fnorm = denorm(Np,fvec)
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)                          
                end if               
                GovBudgetConstraint = fvec(1)
                LaborMarketClearing = fvec(2)  
            end if        
        
        else        
            if (GEexperiment == 0) then            
                if (PartialEqm == 1 .or. RunOnce == 1) then 
                    call BaseConds(Np, aggvars(2:3), fvec(2:3), 0)                      
                else
                    iopt = 2   
                    nprint = 0 
                    
                    tolCapital = tolCapital1
                    tolOuterLoop = tolOuterLoop1
                    OuterLoopIter1 = 0
                    do
                        OuterLoopIter1 = OuterLoopIter1 + 1
                        print *, "OuterLoopIter1=", OuterLoopIter1
                        call BaseCondsCapIter(Np-2, aggvars(2), fvec(2), 0)
                        aggvars(2) = AveEarningsAllNew
                        print '(a,es10.2)', "fvec(2) = ", fvec(2)
                        if (abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do    
                    
                    OuterLoopIter2 = 0
                    aggvars(3) = r
                    tolCapital = tolCapital2
                    tolOuterLoop = tolOuterLoop2
                    do
                        OuterLoopIter2 = OuterLoopIter2 + 1
                        print *, "OuterLoopIter2=", OuterLoopIter2
                        call BaseCondsCapIter(Np-2, aggvars(2), fvec(2), 0)
                        aggvars(2) = AveEarningsAllNew
                        print '(a,es10.2)', "fvec(2) = ", fvec(2)
                        if (abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do 
                    aggvars(3) = r                   
                    !call DNSQE(BaseCondsCapIter, Jac, iopt, Np-2, aggvars(2:3), fvec(2:3), tolOuterLoop, nprint, info, wa, lwa)
                    !fnorm = denorm(Np-2,fvec(2:3))
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)                      
                   
                    
                    !call DNSQE(BaseConds, Jac, iopt, Np-1, aggvars(2:4), fvec(2:4), tolGovBudget, nprint, info, wa, lwa)
                    !fnorm = denorm(Np-1,fvec(2:4))
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)          
                end if                         
                LaborMarketClearing = fvec(2) 
                CapitalDiff = fvec(3)
                !BeqClearing = fvec(3)               
            else
                if (UseEarnsTax == 1) then
                    aggvars(1) = EarnsTax
                else
                    aggvars(1) = PropTax           
                end if
                if (PartialEqm == 1 .or. RunOnce == 1) then            
                    call EqmConds(Np, aggvars, eqs, 0, getype)            
                else                
                    iopt = 2   
                    nprint = 0        
                    
                    tolCapital = tolCapital1
                    tolOuterLoop = tolOuterLoop1
                    OuterLoopIter1 = 0
                    do
                        OuterLoopIter1 = OuterLoopIter1 + 1
                        print *, "OuterLoopIter1=", OuterLoopIter1
                        call EqmConds(Np-1, aggvars(1:2), fvec(1:2), 0, getype)                        
                        if (UseEarnsTax == 1) then
                            aggvars(1) = EarnsTax
                        else
                            aggvars(1) = aggvars(1) - fvec(1)
                        end if                        
                        aggvars(2) = AveEarningsAllNew
                        !aggvars(3) = aggvars(3) - fvec(3)
                        print '(a,es10.2,a,es10.2)', "fvec(1) = ", fvec(1), "fvec(2) = ", fvec(2)
                        if (abs(fvec(1)) < tolOuterLoop .and. abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do    
                    
                    OuterLoopIter2 = 0
                    aggvars(3) = r
                    tolCapital = tolCapital2
                    tolOuterLoop = tolOuterLoop2
                    do
                        OuterLoopIter2 = OuterLoopIter2 + 1
                        print *, "OuterLoopIter2=", OuterLoopIter2
                        call EqmConds(Np-1, aggvars(1:2), fvec(1:2), 0, getype)
                        if (UseEarnsTax == 1) then
                            aggvars(1) = EarnsTax
                        else
                            aggvars(1) = aggvars(1) - fvec(1)
                        end if                                                
                        aggvars(2) = AveEarningsAllNew
                        !aggvars(3) = aggvars(3) - fvec(3)
                        print '(a,es10.2,a,es10.2)', "fvec(1) = ", fvec(1), "fvec(2) = ", fvec(2)
                        if (abs(fvec(1)) < tolOuterLoop .and. abs(fvec(2)) < tolOuterLoop) then
                            exit
                        end if                    
                    end do 
                    aggvars(3) = r                 
                    !call DNSQE(EqmConds, Jac, iopt, Np-1, aggvars(1:3), fvec(1:3), 1d-3, nprint, info, wa, lwa)
                    !fnorm = denorm(Np,fvec)
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)    
                    !aggvars(4) = r
                    
                    !call DNSQE(EqmConds, Jac, iopt, Np, aggvars, fvec, tolGovBudget, nprint, info, wa, lwa)
                    !fnorm = denorm(Np,fvec)
                    !print *, "fnorm is", fnorm   
                    !call Out(0,"exit parameter", info,tab)                          
                end if               
                GovBudgetConstraint = fvec(1)
                LaborMarketClearing = fvec(2) 
                CapitalDiff = fvec(3) 
                !BeqClearing = fvec(3) 
            end if              
        end if
               
        !broadcast to workers that there is no more work (curstat = 0)
        curstat = 0
        call mpi_bcast(curstat, 1, mpi_integer, 0, mpi_comm_world, ier) 
     
        if (GEexperiment == 1) then
            if (UseEarnsTax == 1) then
                EarnsTax = aggvars(1)
            else
                PropTax = aggvars(1)           
            end if
        end if
        AveEarningsAll = aggvars(2)             

        print *, "GovBudgetConstraint =", GovBudgetConstraint         
        print *, "LaborMarketClearing =", LaborMarketClearing  
        print *, "CapitalDiff =", CapitalDiff      
        if (GEexperiment == 1) then
            print *, "PropTax =", PropTax    
            print *, "EarnsTax =", EarnsTax            
        else 
            print *, "GovSpending =", GovSpending 
        end if
        print *, "r =", r 
        print *, "AveEarningsAll =", AveEarningsAll 
               
        TotMedByAgeID = 0.0d0    
        do ti=1,nr 
            do d=1,4                                                            
            do h=1,nhht                
            do j=1,nm 
                TotMedByAgeID(ti) = TotMedByAgeID(ti) + mmat(j,h,d,1,ti)*sum(psiRMarried(:,j,:,:,h,d,ti))
            end do
            end do
            end do              
            do d=1,2                                                            
            do h=1,nsht                
            do j=1,nm 
                TotMedByAgeID(ti) = TotMedByAgeID(ti) + mmat(j,h,d,2,ti)*sum(psiRWidow(:,j,:,:,h,d,ti))
            end do
            end do           
            end do
            do d=1,2                                                         
            do h=1,nsht                
            do j=1,nm 
                TotMedByAgeID(ti) = TotMedByAgeID(ti) + mmat(j,h,d,3,ti)*sum(psiRWidower(:,j,:,:,h,d,ti))
            end do
            end do
            end do                                    
        end do
        
        AggMedExp = sum(CohortWeights(nw+1:) * TotMedByAgeID,1)
                       
        MedExpDistMarried  = sum(sum(sum(sum(psiRMarried,5),4),3),1) 
        MedExpDistWidow  = sum(sum(sum(sum(psiRWidow,5),4),3),1) 
        MedExpDistWidower  = sum(sum(sum(sum(psiRWidower,5),4),3),1)    
        
        print *, "AggMedExp =", AggMedExp      
        print *, "AggMedExp/AggOutput =", AggMedExp/AggOutput*100         
        !call Out(0, "TotMedByAgeID", TotMedByAgeID, ret)
        !pause    

        AggCons = sum(CohortWeights(:nw)*sum(sum(sum(sum(sum(sum(consumptionWcube * psiW,6),5),4),3),2),1),1) & 
                      + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(sum(consumptionRcubeMarried * psiRMarried,6),5),4),3),2),1),1) & 
                      + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(sum(consumptionRcubeWidow * psiRWidow,6),5),4),3),2),1),1) & 
                      + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(sum(consumptionRcubeWidower * psiRWidower,6),5),4),3),2),1),1)
        if (OpenEconomy == 1) then
            AggBudgetConst = AggCons + AggMedExp + IncomeTaxes + SSTaxes + CapitalTaxes  +EarnsTaxes &
                             - AggLaborSupply*w - GovTransfers  - TotalPayments - AggCapitalSupply*(r-ng)
            print '(a,es10.2)', "AggBudgetConst =", AggBudgetConst                        
        else 
            if (GEexperiment == 1) then
                RescourceConst = AggCons + AggMedExp+ GovSpendingBaseline*AggOutput + AddGovConsumption + &
                    (1+ng)*AggCapitalSupply - AggOutput - (1-delta) * AggCapitalSupply        
            else
                RescourceConst = AggCons + AggMedExp+  GovSpending*AggOutput + &
                    (1+ng)*AggCapitalSupply - AggOutput - (1-delta) * AggCapitalSupply
            end if  
            print '(a,es10.2)', "RescourceConst =", RescourceConst   
        end if    
        
        print *, "GovSpending=", IncomeTaxes + SSTaxes  +CapitalTaxes + EarnsTaxes - GovTransfers - TotalPayments
        !pause
              
        !pause        
        if (SaveSolution == 1) then
            open(1,file = "solution.dat")
            write (1,*) psiW
            write (1,*) psiRMarried
            write (1,*) psiRWidow
            write (1,*) psiRWidower
            write(1,*) aPolicyWCube
            write(1,*) aPolicyRCubeMarried
            write(1,*) aPolicyRCubeWidow
            write(1,*) aPolicyRCubeWidower
            write(1,*) consumptionWCube
            write(1,*) consumptionRCubeMarried
            write(1,*) consumptionRcubeWidow
            write(1,*) consumptionRcubeWidower
            write(1,*) labWCube            
            !write(1,*) GovSpending
            !write(1,*) AggCapitalSupply
            !write(1,*) AggCons
            !write(1,*) AggOutput
            !write(1,*) AggLaborSupply
            !write(1,*) TotMedByAgeID
            !write(1,*) AggMedExp  
            !write(1,*) MedExpDist                       
            write(1,*) r
            !write(1,*) A
            write(1,*) w
            !write(1,*) GovTransfers
            !write(1,*) IncomeTaxes
            !write(1,*) AccBequests
            !write(1,*) AveMaleEarnings
            write(1,*) aveEarnFvect
            write(1,*) aveEarnMvect
        end if
    end if    
    
    !call ComputeEarningsStats
    !call ComputeStatistics    
    
    open(12,file = "Targets.txt")
    read(12,*) DataVect
    close(12)            
    
    SumSquareMoments  = 0.0d0
    !SumSquareMoments = (8.37d0 - r*100)**2 + SumSquareMoments
    !SumSquareMoments = sum((DataVect(18:20) - OOPMedExpOverOutputByAge)**2,1) + SumSquareMoments 
    !SumSquareMoments = sum((DataVect(21:23) - MedicaidExpOverOutputByAge)**2,1) + SumSquareMoments
    !SumSquareMoments = (0.73d0-giniOOPmedexp)**2 + SumSquareMoments    
    !SumSquareMoments = sum((DataVect(39:43) - OOPmedShare)**2,1) + SumSquareMoments   
    !SumSquareMoments = sum((DataVect(44:46) - OOPmedShareUpper)**2,1) + SumSquareMoments      
    !SumSquareMoments = sum((DataVect(47:51) - OOPmedShareSSI)**2,1) + SumSquareMoments    
    !if (SumSquareMoments .ne. SumSquareMoments) then
    !    SumSquareMoments = 100.0d0
    !end if
      
    print *, "SumSquareMoments: ", SumSquareMoments    
    SumSquareMoments2 = SumSquareMoments
    
    !call SaveResults     
end subroutine SSM

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!subroutine EqmConds(in, eqs, Np) !for imsl routine 
subroutine EqmConds(Np, in, eqs, iflag, getype)  !for Powell
	use ComputeDistributions
	use Policies
	use mathutils
	use output
	use Results  
	use Results2  
    implicit none
    include 'mpif.h'
    integer, intent(in):: Np, getype
    real(dbl), intent(in) :: in(Np)
    real(dbl), intent(out):: eqs(Np)  
    integer ti, i, j, loc, vf, vm, jf, jm, zf, zm, s, nd, em, eh, ef, h, h1, hf, hm, im, iff, CapitalCounter, loc3, d
    integer iflag
    real(dbl) preTaxIncomeF, preTaxIncomeM, socsecIncome, AveEarningsM, eta3, QuintileCutoffs(5), fracPast, AveEarnsDist(naem), CumAveEarnsDist(naem)          
    integer initial_time, count_rate, count_max, final_time
    real(dbl) elapsed_time, check, cumdistGrid(naem), distGrid(naem)
    
    call Out(0,"in",in,ret)
    
    if (GEexperiment == 1) then
        if (UseEarnsTax == 1) then
            EarnsTax = in(1)
        else
            PropTax = in(1)
        end if
        GovSpending = GovSpendingBaseline   
    end if
    AveEarningsAll = in(2)
    
    eqs = 1.0d0
    
   
    CapitalCounter = 0        
    do                
                              
        !compute prices        
        if (OpenEconomy == 1) then
            !if (NormalizeWage == 1) then
                w = 1.0d0  
                A = (r+delta)**alpha*w**(1-alpha)/alpha**alpha/(1-alpha)**(1-alpha)                                                                                        
            !else                
            !    w = (1-alpha)*alpha**(alpha/(1-alpha))*A**(1/(1-alpha))*(r+delta)**(alpha/(alpha-1))
            !    AveMaleEarnings = AveEarningsInit*w                                 
            !end if                
        else if (Np == 3) then !partialEpm or runOnce
            r = in(3)
            if (NormalizeWage == 1) then
                w = 1.0d0  
                A = (r+delta)**alpha*w**(1-alpha)/alpha**alpha/(1-alpha)**(1-alpha)                                          
            else if (PartialEqm == 1) then                    
                AveMaleEarnings = AveEarningsInit*w                                
            else                
                w = (1-alpha)*alpha**(alpha/(1-alpha))*A**(1/(1-alpha))*(r+delta)**(alpha/(alpha-1))
                AveMaleEarnings = AveEarningsInit*w                                 
            end if        
        else
            print '(a,i5)', "Capital Iter =", CapitalCounter  
            if (NormalizeWage == 1) then
                w = 1.0d0  
                A = (r+delta)**alpha*w**(1-alpha)/alpha**alpha/(1-alpha)**(1-alpha)                               
            else
                w = (1-alpha)*alpha**(alpha/(1-alpha))*A**(1/(1-alpha))*(r+delta)**(alpha/(alpha-1))
                AveMaleEarnings = AveEarningsInit*w                          
            end if                      
        end if
                                               
                    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        !setup kink points  
        socseckinks(:,1) = (/tau1, tau2, tau3/)*AveEarningsAll   
        socseckinks(:,2) = socsec(socseckinks(:,1))
             
        !setup average earnings grids
        aveEarnMin = 1.d-2
        !aveEarnMax = sum(efmat(nef,:))*w/nw
        aveEarnMax = sum(efmat(nef,nem,2,:))*w*hbar/nw
        logaveEarnFvect = linspace(log(aveEarnMin), log(aveEarnMax), naef-4)
        aveEarnFvect(:naef-3) = (/0.0d0, exp(logaveEarnFvect)/)
        !aveEarnFvect(:naef-3) = linspace(0.0d0, aveEarnMax, naef-3)
        do i=1,3
            call Locate(aveEarnFvect(:naef+i-4), socseckinks(i,1), loc)
            aveEarnFvect(loc+2:naef+i-3) = aveEarnFvect(loc+1:naef+i-4)
            aveEarnFvect(loc+1) = socseckinks(i,1)
        end do     
        call Out(0, "aveEarnFvect", aveEarnFvect, tab)
        
        aveEarnMin = minval(emmat)*w*hbar*2.0d0
        !aveEarnMax = sum(emmat(nem,:))*w/nw
        aveEarnMax = sum(emmat(nef,nem,2,:))*w*hbar/nw*0.6d0
        logaveEarnMvect = linspace(log(aveEarnMin), log(aveEarnMax), naem-4)
        !aveEarnMvect(:naem-3) = (/0.0d0, exp(logaveEarnMvect)/)
        aveEarnMvect(:naem-3) = linspace(0.0d0, aveEarnMax, naem-3)
        do i=1,3
            call Locate(aveEarnMvect(:naem+i-4), socseckinks(i,1), loc)
            aveEarnMvect(loc+2:naem+i-3) = aveEarnMvect(loc+1:naem+i-4)
            aveEarnMvect(loc+1) = socseckinks(i,1)
        end do     
        
       !Create EarnLocsMcube and etaMcube cubes               
        EarnLocsMcube(:,:,:,:,1) = 0
        etaMCube(:,:,:,:,1) = 0.0d0
        do ti = 2,nw+1
            !print *, "ti=", ti      
            do em=1,nset
            do vm=1,naem
            do zf=1,nef
            do zm=1,nem
                AveEarningsM = (aveEarnMvect(vm)*(ti-2) + w*hbar*emmat(zf,zm,em,ti-1))/(ti-1)
                if (AveEarningsM > aveEarnMvect(naem)) then
                    AveEarningsM = aveEarnMvect(naem)
                    EarnLocsMcube(zf,zm,vm,em,ti) = naem-1
                    etaMcube(zf,zm,vm,em,ti) = 0.0d0
                else
                    call Locate(aveEarnMvect, AveEarningsM, EarnLocsMcube(zf,zm,vm,em,ti))  
                    if (EarnLocsMcube(zf,zm,vm,em,ti) == 0) then                    
                        EarnLocsMcube(zf,zm,vm,em,ti) = 1                    
                    end if
                    etaMcube(zf,zm,vm,em,ti) = ( aveEarnMvect(EarnLocsMcube(zf,zm,vm,em,ti)+1) - AveEarningsM )/( aveEarnMvect(EarnLocsMcube(zf,zm,vm,em,ti)+1) - aveEarnMvect(EarnLocsMcube(zf,zm,vm,em,ti)) )
                end if          
            end do
            end do
            end do
            end do
        end do   
        
        ProdDistW = 0.0d0
        !educDistM(1) = educDist(1) + educDist(3)
        !educDistM(2) = educDist(2) + educDist(4)    
        ProdDistW(:,:,1,:,1) = spread(uinit,3,nhet)*spread(spread(educDist,1,nef),2,nem)
        do ti = 2,nw+1            
            do eh = 1,nhet
                em = mod(eh+1,2)+1        
                ef = int((eh+1)/2)                               
            do vm=1,naem
            do zm=1,nem                                    
            do zf=1,nef
                loc3 = EarnLocsMcube(zf,zm,vm,em,ti)          
                eta3 = etaMcube(zf,zm,vm,em,ti)                                                                                    
                do jm=1,nem
                do jf=1,nef                
                    ProdDistW(jf,jm,loc3,eh,ti)   = ProdDistW(jf,jm,loc3,eh,ti)   +  eta3  * Puefmat(jf,zf) * Puemmat(jm,zm) * ProdDistW(zf,zm,vm,eh,ti-1)
                    ProdDistW(jf,jm,loc3+1,eh,ti) = ProdDistW(jf,jm,loc3+1,eh,ti) +  (1-eta3) * Puefmat(jf,zf) * Puemmat(jm,zm) * ProdDistW(zf,zm,vm,eh,ti-1)
                end do
                end do                                
            end do
            end do
            end do
            end do                      
            check = sum(ProdDistW(:,:,:,:,ti))
            if (abs(check - 1.0d0) > 1.e-8) then
                print *, "ProdDistW", ti, "not summing to 1"
                print *, "check =", check
                pause
            end if
        end do        
        
        !sinitHb = 0.0d0
        !sinitHbf = 0.0d0
        !setup initial distribution of married                   
        !sinitHa = sinitHuncond(1)/sum(exp(sinithb*aveEarnMvect)*sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1) )
        !sinitHaf = sinitHuncond(2)/sum(exp(sinithbf*aveEarnMvect)*sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1) )        
        !do i=1,naem
        !    sinitH(1,i) = sinitHa * exp(sinithb * aveEarnMvect(i))       
        !    sinitH(2,i) = sinitHaf * exp(sinithbf * aveEarnMvect(i))      
        !    sinitH(3,i) = 1.0d0 - sinitH(1,i) - sinitH(2,i)                 
        !end do
        
        QuintileCutoffs = (/0.00027d0, 0.45025597823d0, 0.693568331198d0, 1.018370057525d0, 1.529375858078d0/)
        MaritalDistAge65Data(:,1) = (/0.30d0, 0.37d0, 0.49d0, 0.56d0, 0.69d0/) 
        MaritalDistAge65Data(:,2) = (/0.51d0, 0.46d0, 0.36d0, 0.28d0, 0.18d0/) 
        MaritalDistAge65Data(:,3) = (/0.19d0, 0.18d0, 0.14d0, 0.16d0, 0.13d0/)  
        MaritalDistAge65SSIncData(:,1) = (/0.19d0, 0.24d0, 0.36d0, 0.67d0, 0.95d0/) 
        MaritalDistAge65SSIncData(:,2) = (/0.56d0, 0.55d0, 0.45d0, 0.21d0, 0.03d0/)           
        MaritalDistAge65SSIncData(:,3) = (/0.25d0, 0.21d0, 0.20d0, 0.12d0, 0.02d0/)     
        AveEarnsDist = sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1)
        CumAveEarnsDist(1) = AveEarnsDist(1)  
        do i=2,naem
            CumAveEarnsDist(i) = CumAveEarnsDist(i-1) + AveEarnsDist(i) 
        end do   
        !sinitH(1,1) = 0.0d0
        !sinitH(2,1) = 0.70d0
        !sinitH(1,2) = 0.0d0
        !sinitH(2,2) = 0.70d0   
        sinitH(1,1) = MaritalDistAge65SSIncData(1,1)
        sinitH(2,1) = MaritalDistAge65SSIncData(1,2)
        sinitH(1,2) = MaritalDistAge65SSIncData(1,1)
        sinitH(2,2) = MaritalDistAge65SSIncData(1,2)                
        fracPast = (CumAveEarnsDist(3)- 0.20d0)/0.20d0
        !print *, "fracPast=", fracPast
        !call Out(0,"CumAveEarnsDist",CumAveEarnsDist,ret)
        !pause
        sinitH(1,3) = (1-fracPast) * MaritalDistAge65SSIncData(1,1) + fracPast* MaritalDistAge65SSIncData(2,1) + 0.1d0
        sinitH(2,3) = (1-fracPast) * MaritalDistAge65SSIncData(1,2) + fracPast* MaritalDistAge65SSIncData(2,2) - 0.05d0   
        fracPast = (CumAveEarnsDist(4)- 0.40d0)/0.20d0
        sinitH(1,4) = (1-fracPast) * MaritalDistAge65SSIncData(2,1) + fracPast* MaritalDistAge65SSIncData(3,1) - 0.3d0
        sinitH(2,4) = (1-fracPast) * MaritalDistAge65SSIncData(2,2) + fracPast* MaritalDistAge65SSIncData(3,2) + 0.15d0       
        fracPast = (CumAveEarnsDist(5)- 0.60d0)/0.20d0
        sinitH(1,5) = (1-fracPast) * MaritalDistAge65SSIncData(3,1) + fracPast* MaritalDistAge65SSIncData(4,1) + 0.2d0
        sinitH(2,5) = (1-fracPast) * MaritalDistAge65SSIncData(3,2) + fracPast* MaritalDistAge65SSIncData(4,2) - 0.3d0
        fracPast = (CumAveEarnsDist(6)- 0.80d0)/0.20d0
        sinitH(1,6) = 0.9d0 !(1-fracPast) * MaritalDistAge65SSIncData(4,1) + fracPast* MaritalDistAge65SSIncData(5,1)
        sinitH(2,6) = 0.1d0 !(1-fracPast) * MaritalDistAge65SSIncData(4,2) + fracPast* MaritalDistAge65SSIncData(5,2)
        do i=7,naem
            sinitH(1,i) = 1.0d0
            sinitH(2,i) = 0.0d0
        end do
        do i=1,naem
            sinitH(3,i) = 1.0d0 - sinitH(1,i) - sinitH(2,i) 
        end do
        call Out(0,"sinitH",sinitH)
        call Out(0,"mart dist",sum(sinitH*spread(sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1),1,3),2),ret)
        distGrid = sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1)
        cumdistGrid(1) = distGrid(1)
        do i=2,naem
            cumdistGrid(i) = cumdistGrid(i-1)+distGrid(i)
        end do
        call Out(0,"dist across grid",distGrid,ret)
        call Out(0,"cumdistGrid",cumdistGrid,ret)
        call Out(0,"aveEarnMvect",aveEarnMvect,ret)
        !pause
        
        !!!!!!!!!!!
        !Compute cohort weights  
        dist = 0.0d0                     
        do eh = 1,nhet
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)          
        do vm = 1,naem  
        do h=1,nhht            
            !dist(i,s,1) = dist(i,s,1) + 1/(1+ng)**nw * sum(hinitH(i,:)*educDist)*sinitHuncond(s)  !As long as we set sinitH such that the distribution matches the data this is ok 
            !dist(i,s,1) = dist(i,s,1) + 1/(1+ng)**nw * hinitH(i,eh)*sinitH(s,vm)*sum(ProdDistW(:,:,vm,eh,nw))
            dist(h,1,1) = dist(h,1,1) + 1/(1+ng)**nw * hinitH(h,eh) * sinitH(1,vm) * sum(ProdDistW(:,:,vm,eh,nw))            
        end do
        do h=1,nsht            
            dist(h,2,1) = dist(h,2,1) + 1/(1+ng)**nw * hinitF(h,ef) * sinitH(2,vm) * sum(ProdDistW(:,:,vm,eh,nw)) 
            dist(h,3,1) = dist(h,3,1) + 1/(1+ng)**nw * hinitM(h,em) * sinitH(3,vm) * sum(ProdDistW(:,:,vm,eh,nw))             
        end do                     
        end do
        end do

        do ti=2,nr
        !married to married
        do j=1,nhht
            jm = mod(j+1,2)+1
            jf = int((j+1)/2)
        do h1=1,nhht 
            im = mod(h1+1,2)+1            
            iff = int((h1+1)/2)                        
            dist(h1,1,ti) = dist(h1,1,ti) + dist(j,1,ti-1) * survivalProbVectF(jf,1,ti-1) * survivalProbVectM(jm,1,ti-1) * &
                PhmatF(iff,jf,1,ti-1)* PhmatM(im,jm,1,ti-1) /(1+ng)                 
                                                                                              
        end do
        end do

        !married to widow
        do j=1,nhht
            jm = mod(j+1,2)+1
            jf = int((j+1)/2)
        do h1=1,nsht                                           
            dist(h1,2,ti) = dist(h1,2,ti) + (dist(j,1,ti-1) * survivalProbVectF(jf,1,ti-1) * (1-survivalProbVectM(jm,1,ti-1)) * PhmatF(h1,jf,1,ti-1)) /(1+ng)                                                   
        end do
        end do

        !widow to widow
        do h=1,nsht            
        do h1=1,nsht                                    
            dist(h1,2,ti) = dist(h1,2,ti) + dist(h,2,ti-1) * survivalProbVectF(h,2,ti-1) * PhmatF(h1,h,2,ti-1) /(1+ng)                                                        
        end do       
        end do
        
        !married to widower
        do j=1,nhht
            jm = mod(j+1,2)+1
            jf = int((j+1)/2)
        do h1=1,nsht                                           
            dist(h1,3,ti) = dist(h1,3,ti) + (dist(j,1,ti-1) * (1-survivalProbVectF(jf,1,ti-1)) * survivalProbVectM(jm,1,ti-1) * PhmatM(h1,jm,1,ti-1)) /(1+ng)                                                   
        end do
        end do    

        !widower to widower
        do h=1,nsht            
        do h1=1,nsht                                    
            dist(h1,3,ti) = dist(h1,3,ti) + dist(h,3,ti-1) * survivalProbVectM(h,3,ti-1) * PhmatM(h1,h,3,ti-1) /(1+ng)                                                        
        end do
        end do                
             
        end do    
        TotalPop = sum((/(1.0d0/(1+ng)**j,j=0,nw-1)/)) + sum(dist)
        CohortWeights(:nw) = (/(1.0d0/(1+ng)**j,j=0,nw-1)/)/TotalPop
        do ti=1,nr
            CohortWeights(nw+ti) = sum(dist(:,:,ti))/TotalPop
            dist(:,:,ti) = dist(:,:,ti)/sum(dist(:,:,ti))
            MaritalDist(1,ti) = sum(dist(:,1,ti))
            MaritalDist(2,ti) = sum(dist(:,2,ti))
            MaritalDist(3,ti) = sum(dist(:,3,ti))
        end do
        !!!!!!!!!!!                             

 
        !broadcast to workers that work is coming
        call mpi_bcast(curstat, 1, mpi_integer, 0, mpi_comm_world, ier) 
        !broadcast globals that change with GE loop to workers
        call mpi_bcast(efmat, 1, getype, 0, mpi_comm_world, ier)

        call SYSTEM_CLOCK(initial_time,count_rate,count_max)
        call ComputePolicies            
        call SYSTEM_CLOCK(final_time)
        elapsed_time = real(final_time - initial_time)/real(count_rate)    
        print *, "elapsed_time = ", elapsed_time
       
        call ComputeStationaryDistributions                        
        
        !Compute Updated Aggregate Capital
        AggregateCapitalVect = 0.0d0
        do ti=1,nw
            AggregateCapitalVect(ti) = sum(aPolicyWCube(:,:,:, : ,:,:,ti) * psiW(:,:,:, : ,:,:,ti))
        end do
        do ti=1,nr
            AggregateCapitalVect(nw+ti) = sum(aPolicyRCubeMarried(:,:, : ,:,:,:,ti) * psiRMarried(:,:, : ,:,:,:,ti)) + &
                sum(aPolicyRCubeWidow(:,:, : ,:,:,:,ti) * psiRWidow(:,:, : ,:,:,:,ti)) + sum(aPolicyRCubeWidower(:,:, : ,:,:,:,ti) * psiRWidower(:,:, : ,:,:,:,ti))
        end do   
        AggCapitalSupply = sum(CohortWeights * AggregateCapitalVect,1)/(1+ng)
         
        fracWwork = 0   
        AggLaborSupply = 0.0d0
        do ti = 1,nw
        do eh = 1,nhet
        do vm = 1,naem
        do vf = 1,naef
        do jm = 1,nem
        do jf = 1,nef
        do i = 1,na    
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)           
            AggLaborSupply = AggLaborSupply + (efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti) + emmat(jf,jm,em,ti)*hbar)*psiW(i,jf,jm,vf,vm,eh,ti)*CohortWeights(ti)
            if (labWcube(i,jf,jm,vf,vm,eh,ti) > 0.0d0) then
                fracWwork = fracWwork + psiW(i,jf,jm,vf,vm,eh,ti)*CohortWeights(ti)
            end if   
        end do
        end do
        end do
        end do
        end do
        end do   
        end do         
        fracWwork = fracWwork/sum(CohortWeights(:nw))             
 
        !Compute AveHHIncome
        AveHHIncome = 0.0d0        
        do ti=1,nw
            do eh = 1,nhet
            do vm = 1,naem
            do vf = 1,naef
            do jm = 1,nem
            do jf = 1,nef
            do i = 1,na  
                em = mod(eh+1,2)+1        
                ef = int((eh+1)/2)              
                preTaxIncomeF = w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti) + 0.5d0*r* avect(i)  
                preTaxIncomeM = w*emmat(jf,jm,em,ti)*hbar + 0.5d0*r* avect(i)                                 
                AveHHIncome = AveHHIncome + (preTaxIncomeF+preTaxIncomeM)* psiW(i,jf,jm,vf,vm,eh,ti)*CohortWeights(ti)
            end do
            end do
            end do 
            end do
            end do
            end do
        end do
        do ti=1,nr              
            do h = 1,nhht                    
            do vm = 1,naem
            do vf = 1,naef
                socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),1)
            do j = 1,nm
            do i = 1,na                             
                AveHHIncome = AveHHIncome + (socsecIncome + avect(i)*r) * sum(psiRMarried(i,j,vf,vm,h,:,ti))*CohortWeights(nw+ti)                
            end do
            end do
            end do
            end do   
            end do
                    
                     
            do h = 1,nsht                    
            do vm = 1,naem
            do vf = 1,naef
                socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),2)
            do j = 1,nm
            do i = 1,na                             
                AveHHIncome = AveHHIncome + (socsecIncome + avect(i)*r) * sum(psiRWidow(i,j,vf,vm,h,:,ti))*CohortWeights(nw+ti)                
            end do
            end do
            end do
            end do   
            end do       
                   
            do h = 1,nsht                    
            do vm = 1,naem
            do vf = 1,naef
                socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),3)
            do j = 1,nm
            do i = 1,na                             
                AveHHIncome = AveHHIncome + (socsecIncome + avect(i)*r) * sum(psiRWidower(i,j,vf,vm,h,:,ti))*CohortWeights(nw+ti)                
            end do
            end do
            end do
            end do   
            end do        
        end do      
       
       AveEarningsAllNew = w*AggLaborSupply/(1+fracWwork)/sum(CohortWeights(:nw))      

       AggCapitalDemand = ((r+delta)/alpha/A)**(1/(alpha-1))*AggLaborSupply
       
        if (Np == 3 .or. OpenEconomy==1) then
            eqs(3) = AggCapitalSupply - AggCapitalDemand
            CapitalDiff = eqs(3) 
            exit     
        else
            CapitalDiff = abs(AggCapitalSupply - AggCapitalDemand)/AggCapitalDemand
            
            print *, "--------------------"
            print '(a,es10.2,a,f13.9,a,f13.9,a,f13.9)', "AggCapitalSupply - AggCapitalDemand = ", CapitalDiff, " AggCapitalSupply = ", AggCapitalSupply, " AggCapitalDemand = ", AggCapitalDemand, "r=", r       
            if (GEexperiment ==1) then
                if (UseEarnsTax == 1) then
                    print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ",GovBudgetConstraint, " EarnsTax = ", EarnsTax
                else
                    print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ",GovBudgetConstraint, " PropTax = ", PropTax
                end if 
            else 
                print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ", GovBudgetConstraint, " GovSpending = ", GovSpending 
            end if
            print '(a,es10.2,a,f13.9,a,f13.9)', "AveEarningsAllNew - AveEarningsAll = ", LaborMarketClearing, " AveEarningsAllNew = ", AveEarningsAllNew, " AveEarningsAll = ", AveEarningsAll        
            print '(a,f13.9)', "fracWwork = ", fracWwork
            !print '(a,es10.2,a,f12.9)', "AccBequestsG - AccBequests = ", BeqClearing, " AccBequestsG = ", AccBequestsG
            print *, "--------------------"
            print *, ret
               
            if (PartialEqm == 1 .or. RunOnce == 1) then
                !AggCapitalDemand = AggCapitalSupply
                exit        
            end if     
            if (CapitalDiff < tolCapital) then
                AggCapitalSupply = AggCapitalDemand
                exit
            else if ( CapitalCounter >= 20) then
                !fracUpdatedCap = fracUpdatedCap*0.70d0
                !AggCapitalDemand = fracUpdatedCap * AggCapitalSupply + (1-fracUpdatedCap) * AggCapitalDemand
                print *, "CapitalCounter >= 20"
                CapitalCounter = 0
                AggCapitalSupply = AggCapitalDemand
                exit                
            else
                AggCapitalDemand = fracUpdatedCap * AggCapitalSupply + (1-fracUpdatedCap) * AggCapitalDemand
                r = (AggCapitalDemand/AggLaborSupply)**(alpha-1)*alpha*A-delta
            end if
            CapitalCounter = CapitalCounter + 1                 
        end if   
    
    end do   
                                 
    !Compute Income Taxes, GovtTransfers
    TotalTaxableIncome = 0.0d0
    IncTaxByAge = 0.0d0
    GovTransfersVect = 0.0d0
    SSTaxesVect = 0.0d0
    EarnsTaxesVect = 0.0d0
    do ti=1,nw
        do eh = 1,nhet
        do vm = 1,naem
        do vf = 1,naef
        do jm = 1,nem
        do jf = 1,nef
        do i = 1,na  
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)              
            preTaxIncomeF = w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti) - SocSecTax(w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti)) + 0.5d0*r* avect(i) &
                - EarnsTaxFn(w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti))  
            preTaxIncomeM = w*emmat(jf,jm,em,ti)*hbar - SocSecTax(w*emmat(jf,jm,em,ti)*hbar) + 0.5d0*r* avect(i) &
                - EarnsTaxFn(w*emmat(jf,jm,em,ti)*hbar)                  
            IncTaxByAge(ti) = IncTaxByAge(ti) + (IncomeTax(preTaxIncomeF+preTaxIncomeM-max(0.0d0,r*avect(i)*capTax)))* psiW(i,jf,jm,vf,vm,eh,ti)   
            GovTransfersVect(ti) = GovTransfersVect(ti) + transfers(preTaxIncomeF + preTaxIncomeM + avect(i) &
                 - IncomeTax(preTaxIncomeF+preTaxIncomeM-max(0.0d0,r*avect(i)*capTax)) -max(0.0d0,r*avect(i)*capTax) ) * psiW(i,jf,jm,vf,vm,eh,ti)                    
            SSTaxesVect(ti) = SSTaxesVect(ti) +  (SocSecTax(w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti)) + SocSecTax(w*emmat(jf,jm,em,ti)*hbar)) * psiW(i,jf,jm,vf,vm,eh,ti)         
            TotalTaxableIncome = TotalTaxableIncome + (preTaxIncomeF+preTaxIncomeM)* psiW(i,jf,jm,vf,vm,eh,ti)*CohortWeights(ti)
            EarnsTaxesVect(ti) = EarnsTaxesVect(ti) +  (EarnsTaxFn(w*efmat(jf,jm,ef,ti)*labWcube(i,jf,jm,vf,vm,eh,ti)) + EarnsTaxFn(w*emmat(jf,jm,em,ti)*hbar)) * psiW(i,jf,jm,vf,vm,eh,ti)         
        end do
        end do
        end do 
        end do
        end do
        end do
    end do
    do ti=1,nr    
        do d=1,4
        do h = 1,nhht                    
        do vm = 1,naem
        do vf = 1,naef
            socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),1)
        do j = 1,nm
        do i = 1,na             
            IncTaxByAge(nw+ti) = IncTaxByAge(nw+ti) + &
                IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,1,ti),1) * psiRMarried(i,j,vf,vm,h,d,ti)       
            GovTransfersVect(nw+ti) = GovTransfersVect(nw+ti) + transfersRetired(socsecIncome + avect(i) * r &
               + avect(i) - IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,1,ti),1)-max(0.0d0,r*avect(i)*capTax),ti,j,h,d,1,i) * psiRMarried(i,j,vf,vm,h,d,ti)
            TotalTaxableIncome = TotalTaxableIncome + TaxableIncome(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,1,ti),1) * psiRMarried(i,j,vf,vm,h,d,ti)*CohortWeights(nw+ti)                
        end do
        end do
        end do
        end do   
        end do
        end do
        
        do d = 1,2
        do h = 1,nsht                    
        do vm = 1,naem
        do vf = 1,naef
            socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),2)
        do j = 1,nm
        do i = 1,na             
            IncTaxByAge(nw+ti) = IncTaxByAge(nw+ti) + &
                IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,2,ti),2) * psiRWidow(i,j,vf,vm,h,d,ti)       
            GovTransfersVect(nw+ti) = GovTransfersVect(nw+ti) + transfersRetired(socsecIncome + avect(i) * r &
               + avect(i) - IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,2,ti),2)-max(0.0d0,r*avect(i)*capTax),ti,j,h,d,2,i) * psiRWidow(i,j,vf,vm,h,d,ti) 
            TotalTaxableIncome = TotalTaxableIncome + TaxableIncome(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,2,ti),2) * psiRWidow(i,j,vf,vm,h,d,ti) *CohortWeights(nw+ti)                
        end do
        end do
        end do
        end do   
        end do
        end do
        
        do d = 1,2
        do h = 1,nsht                    
        do vm = 1,naem
        do vf = 1,naef
            socsecIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),3)
        do j = 1,nm
        do i = 1,na             
            IncTaxByAge(nw+ti) = IncTaxByAge(nw+ti) + &
                IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,3,ti),3) * psiRWidower(i,j,vf,vm,h,d,ti)       
            GovTransfersVect(nw+ti) = GovTransfersVect(nw+ti) + transfersRetired(socsecIncome + avect(i) * r &
               + avect(i) - IncomeTax(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,3,ti),3)-max(0.0d0,r*avect(i)*capTax),ti,j,h,d,3,i) * psiRWidower(i,j,vf,vm,h,d,ti)
            TotalTaxableIncome = TotalTaxableIncome + TaxableIncome(avect(i)*r-max(0.0d0,r*avect(i)*capTax),socsecIncome,mmat(j,h,d,3,ti),3) * psiRWidower(i,j,vf,vm,h,d,ti)*CohortWeights(nw+ti)                
        end do
        end do
        end do
        end do   
        end do 
        end do            
    end do   
    
    CapitalTaxes = max(0.0d0,capTax*r*AggCapitalSupply)               
    IncomeTaxes = sum(CohortWeights * IncTaxByAge)
    SSTaxes = sum(CohortWeights(:nw) * SSTaxesVect)
    EarnsTaxes = sum(CohortWeights(:nw) * EarnsTaxesVect) 
    GovTransfers = sum(CohortWeights * GovTransfersVect)
    AggOutput = A*AggCapitalDemand ** alpha * AggLaborSupply**(1-alpha)
       
    TotalPayments = 0.0d0
    do ti = 1,nr
    do j = 1, naem   
    do i = 1, naef                                
        TotalPayments = TotalPayments + socsec(aveEarnFvect(i),aveEarnMvect(j),1)*sum(psiRMarried(:,:,i,j,:,:,ti))*CohortWeights(nw+ti) &
            + socsec(aveEarnFvect(i),aveEarnMvect(j),2)*sum(psiRWidow(:,:,i,j,:,:,ti))*CohortWeights(nw+ti) &
            + socsec(aveEarnFvect(i),aveEarnMvect(j),3)*sum(psiRWidower(:,:,i,j,:,:,ti))*CohortWeights(nw+ti)        
    end do      
    end do      
    end do
    
    AddGovConsumption = 0.0d0
    !if (ScaleDownMedicaidBenefit == 1 .and. ScaleDownSocSecBenefit == 1) then    
    !     AddGovConsumption = 0.003448877d0
    !else if (ScaleDownMedicaidBenefit == 1) then
    !     AddGovConsumption = 0.006897754d0
    !end if
    !if (ScaleDownSocSecBenefit == 1) then        
    !    do ti = 1,nr
    !    do j = 1, naem   
    !    do i = 1, naef                                
    !        AddGovConsumption = AddGovConsumption + govcons(aveEarnFvect(i),aveEarnMvect(j),1)*sum(psiRMarried(:,:,i,j,:,:,ti))*CohortWeights(nw+ti) &
    !            + govcons(aveEarnFvect(i),aveEarnMvect(j),2)*sum(psiRWidow(:,:,i,j,:,:,ti))*CohortWeights(nw+ti) &
    !            + govcons(aveEarnFvect(i),aveEarnMvect(j),3)*sum(psiRWidower(:,:,i,j,:,:,ti))*CohortWeights(nw+ti)        
    !    end do      
    !    end do      
    !    end do    
    !end if
    
    if (GEexperiment == 0) then
        !GovSpending = (IncomeTaxes + SSTaxes  + IncTaxBequests/(1.0d0+ng) + CapitalTaxes- GovTransfers - TotalPayments)/AggOutput
        !eqs(1) = IncomeTaxes + SSTaxes + IncTaxBequests/(1.0d0+ng) + CapitalTaxes - GovTransfers - GovSpending*AggOutput - TotalPayments
        GovSpending = (IncomeTaxes + SSTaxes  + CapitalTaxes + EarnsTaxes - GovTransfers - TotalPayments)/AggOutput
        eqs(1) = IncomeTaxes + SSTaxes  + CapitalTaxes + EarnsTaxes - GovTransfers - GovSpending*AggOutput - TotalPayments
    else               
        !eqs(1) = IncomeTaxes + SSTaxes + IncTaxBequests/(1.0d0+ng) + CapitalTaxes  - GovTransfers - GovSpendingBaseline*AggOutput - TotalPayments
        eqs(1) = IncomeTaxes + SSTaxes + CapitalTaxes + EarnsTaxes - GovTransfers - GovSpendingBaseline*AggOutput - TotalPayments - AddGovConsumption
    end if
    print *, "IncomeTaxes=", IncomeTaxes
    print *, "SSTaxes=", SSTaxes
    print *, "EarnsTaxes=", EarnsTaxes
    print *, "CapitalTaxes=", CapitalTaxes
    print *, "GovTransfers=", GovTransfers
    print *, "GovSpending=", GovSpending
    print *, "TotalPayments=", TotalPayments
 
    EarnsTaxNew = (EarnsTaxes - eqs(1))/w/AggLaborSupply
 
    eqs(2) = AveEarningsAllNew - AveEarningsAll        
    
    GovBudgetConstraint = eqs(1)    
    LaborMarketClearing = eqs(2)
    
    if (GEexperiment ==1) then
        if (UseEarnsTax == 1) then
            print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ", eqs(1), " EarnsTax = ", EarnsTax
        else
            print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ", eqs(1), " PropTax = ", PropTax        
        end if 
    else 
        print '(a,es10.2,a,f12.9)', "Govt Revenue - Expends = ", eqs(1), " GovSpending = ", GovSpending 
    end if
    print '(a,es10.2,a,f13.9,a,f13.9)', "AveEarningsAllNew - AveEarningsAll = ", eqs(2), " AveEarningsAllNew = ", AveEarningsAllNew, " AveEarningsAll = ", AveEarningsAll        
    print '(a,f13.9)', "fracWwork = ", fracWwork
    print '(a,f13.9)', "AveHHIncome = ", AveHHIncome
    print '(a,es10.2,a,f13.9,a,f13.9,a,f13.9)', "AggCapitalSupply - AggCapitalDemand = ", CapitalDiff, " AggCapitalSupply = ", AggCapitalSupply, " AggCapitalDemand = ", AggCapitalDemand, "r=", r       
    print '(a)', "*********************************************************"
    print *, ret        
    
 end subroutine EqmConds      
 
 
 subroutine BaseConds(N,input,consts,iflag, getype)       
    use globals
    implicit none
    integer N
    real(dbl), intent(in) :: input(2)
    integer, intent(in) :: getype
    real(dbl)  consts(2), input2(3), output(3)   
    integer iflag  
    
    if (UseEarnsTax == 1) then
        input2(1) = EarnsTax
    else
        input2(1) = PropTax
    end if
    input2(2:3) = input
    
    call EqmConds(3, input2, output, 0, getype) 
    consts = output(2:3)    
    print '(a,es10.2,a,f12.9)', "AveEarningsAllNew - AveEarningsAll  = ", consts(1), " AveEarningsAllNew = ", AveEarningsAllNew
    print '(a,es10.2,a,f12.9)', "AggCapitalSupply - AggCapitalDemand = ", consts(2), " r = ", r
    print '(a,f13.9)', "fracWwork = ", fracWwork
    
    GovBudgetConstraint = output(1) 
 end subroutine  BaseConds 

 subroutine BaseCondsCapIter(N,input,consts,iflag, getype)       
    use globals
    implicit none
    integer N
    real(dbl), intent(in) :: input
    integer, intent(in) :: getype
    real(dbl)  consts(1), input2(2), output(2)   
    integer iflag  
    
    if (UseEarnsTax == 1) then
        input2(1) = EarnsTax
    else
        input2(1) = PropTax
    end if
    input2(2) = input
    
    call EqmConds(2, input2, output, 0, getype) 
    consts = output(2)    
    print '(a,es10.2,a,f12.9)', "AveEarningsAllNew - AveEarningsAll  = ", consts(1), " AveEarningsAllNew = ", AveEarningsAllNew
    print '(a,es10.2,a,f12.9)', "AggCapitalSupply - AggCapitalDemand = ", CapitalDiff, " r = ", r
    print '(a,f13.9)', "fracWwork = ", fracWwork
    
    GovBudgetConstraint = output(1) 
 end subroutine  BaseCondsCapIter 
 
 
subroutine JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
    use constants
    integer N,LDFJAC,IFLAG, i, j
    real(dbl) X(N),FVEC(N),FJAC(LDFJAC,N)
    do i=1,N
    do j=1,N
    FJAC(i,j)=0
    end do
    end do	    
end subroutine JAC

