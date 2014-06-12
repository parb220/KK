module simulations
    use functions
    use output
    !use sorts
    use mathutils, only : linspace
    use interpolate1
    use mtmod
    implicit none    
    contains   
    
    subroutine simulations1
        !use rnun_int
        
        !use rnset_int
        !use svrgp_int
        !use svrgn_int
        
        !use m_mrgrnk
        implicit none
        
        Interface
           Subroutine mrgrnk(XVALT, IRNGT)
              use constants
              Real(dbl), Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine mrgrnk
           Subroutine I_mrgrnk(XVALT, IRNGT)
              use constants
              Integer, Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine I_mrgrnk                      
        End Interface        
        
        integer s, zf, zm, i, j, v, het, fet, met, set, h, iseed, status, Np, No, Nop, ti
        integer NumWidBad, NumWidGood, NumWidrBad, NumWidrGood, NumFWids(nr), NumMWids(nr), NumDied(5)
        integer pm, NumPeopleF, NumPeopleM
        integer NumYears, TotNum, NumZeroStd
        real(dbl) meanChangeWageF, meanChangeWageM, meanChangeHoursF, stdChangeWageF, stdChangeWageM, stdChangeHoursF
        real(dbl) changeWagesF(nw-1), changeWagesM(nw-1), changeHoursF(nw-1)
        real(dbl) wagem1, wagem0, wagef1, wagef0, covWageFMxtemp, covWageMHoursFxtemp
        real(dbl) cumuinit(nef,nem), cumuf(nef), cumum(nem), cumeducdist(nhet), cummeddist(npm,nhet), cumhealf(nsht,nset)
        real(dbl) cumhealm(nsht,nset), cummed(npm), cumsinitH(3,naem)
        real(dbl) preTaxIncome, postTaxWealth, preTaxIncomeM, preTaxIncomeF
        real(dbl),allocatable,dimension(:,:):: wealthMat, labfMat, earnsfMat, earnsmMat, aveearnsfMat, aveearnsmMat, consMat
        real(dbl),allocatable,dimension(:,:):: SocSecMat, TotMedExpMat, UtilMat, UtilConsMat, OOPExpMat, GovTransferMat                           
        integer, allocatable, dimension(:,:):: maritalMat, aliveHmat
        
        integer, allocatable, dimension(:,:):: alivefmat, alivemmat, prodflocMat, prodmlocMat, medlocMat, healHmat, healfMat, healmMat, TotMedExpRankMat, futureStatus
        integer, allocatable, dimension(:)::  HHeducloc, earnsmrank, maritalrank, medicalrank, aliverank, aveearnsmloc,  changeWagesFx, changeWagesMx, changeHoursFx
        real(dbl), allocatable, dimension(:)::aveearnsPrimary     
        !real(dbl), allocatable,dimension(:,:):: ChangeWage, ChangeFHours
        !real(dbl), allocatable,dimension(:):: ChangeWageVect, ChangeFHoursVect
        
        allocate (wealthMat(T,Ns),labfMat(nw,Ns),earnsfMat(nw,Ns),earnsmMat(nw,Ns), aveearnsfMat(nw+1,Ns),aveearnsmMat(nw+1,Ns), consMat(T,Ns), stat=status)
        allocate (SocSecMat(nr,Ns), TotMedExpMat(nr,Ns), UtilMat(T,Ns), UtilConsMat(T,Ns), OOPExpMat(nr,Ns), GovTransferMat(T,Ns), stat=status)
        allocate (aliveHmat(nr,Ns), maritalMat(nr,Ns), stat=status)
        
        allocate (prodflocMat(nw,Ns), prodmlocMat(nw,Ns), medlocMat(nr,Ns), stat=status)        
        allocate (alivefMat(nr,Ns),alivemMat(nr,Ns), healHmat(nr,Ns), healfMat(nr,Ns), healmMat(nr,Ns), stat=status)
        allocate (HHeducloc(Ns), earnsmrank(Ns), TotMedExpRankMat(nr,Ns), maritalrank(Ns), medicalrank(Ns), aliverank(Ns), stat=status)
        allocate (aveearnsmloc(Ns), futureStatus(nr,Ns))         
        !allocate (changeWagesF(nw-1), changeWagesM(nw-1), changeHoursF(nw-1)) 
        
        iseed = 803904654
        call sgrnd(iseed)
         
        !Workers
        !!!!!!!!!!!!!!!!!!!!!!!
            
        !Create cumulative initial distribution of earnings shocks
        do zf = 1,nef              
        do zm = 1,nem
            if (zf==1 .and. zm==1) then
                cumuinit(1,1) = uinit(1,1)
            else if (zm==1) then
                cumuinit(zf,zm) =  cumuinit(zf-1,nem) + uinit(zf,zm)
            else
                cumuinit(zf,zm) =  cumuinit(zf,zm-1) + uinit(zf,zm)
            end if
        end do
        end do
              
        !Create cumulative distribution for education distribution
        cumeducdist(1) = EducDist(1)    
        do s=2,nhet
            cumeducdist(s) = cumeducdist(s-1) + EducDist(s)                             
        end do 
                
        !Assign inital shocks to each observation
        !First to HH households
        do zf = 1,nef              
        do zm = 1,nem
            if (zf==1 .and. zm==1) then
                do i=1,nint(cumuinit(1,1)*EducDist(1)*Ns) 
                    prodflocMat(1,i) = 1
                    prodmlocMat(1,i) = 1
                end do             
            else if (zm==1) then
                do i=nint(cumuinit(zf-1,nem)*EducDist(1)*Ns+1),nint(cumuinit(zf,1)*EducDist(1)*Ns)
                    prodflocMat(1,i) = zf
                    prodmlocMat(1,i) = zm
                end do            
            else
                do i=nint(cumuinit(zf,zm-1)*EducDist(1)*Ns+1),nint(cumuinit(zf,zm)*EducDist(1)*Ns)
                    prodflocMat(1,i) = zf
                    prodmlocMat(1,i) = zm
                end do
            end if
        end do 
        end do        
           
        !Then to the rest of the households    
        do s=2,nhet
            do zf = 1,nef              
            do zm = 1,nem        
                if (zf==1 .and. zm==1) then
                    do i=nint(cumeducdist(s-1)*Ns)+1 ,nint(cumeducdist(s-1)*Ns)+nint(cumuinit(1,1)*EducDist(s)*Ns) 
                        prodflocMat(1,i) = 1
                        prodmlocMat(1,i) = 1
                    end do             
                else if (zm==1) then
                    do i=nint(cumeducdist(s-1)*Ns)+nint(cumuinit(zf-1,nem)*EducDist(s)*Ns)+1,nint(cumeducdist(s-1)*Ns)+nint(cumuinit(zf,1)*EducDist(s)*Ns)
                        prodflocMat(1,i) = zf
                        prodmlocMat(1,i) = zm
                    end do            
                else                                
                    do i=nint(cumeducdist(s-1)*Ns)+nint(cumuinit(zf,zm-1)*EducDist(s)*Ns)+1,nint(cumeducdist(s-1)*Ns)+nint(cumuinit(zf,zm)*EducDist(s)*Ns)
                        prodflocMat(1,i) = zf
                        prodmlocMat(1,i) = zm
                    end do
                end if       
            end do    
            end do
        end do    
        
        !Assign female earnings shocks for ages 2 to nw    
        do i =1,Ns  
            do j=2,nw
                cumuf(1) = Puefmat(1,prodflocMat(j-1,i))
                do v=2,nef
                    cumuf(v) = cumuf(v-1)+Puefmat(v,prodflocMat(j-1,i))
                end do           
                call Locate(cumuf, grnd(), prodflocMat(j,i))
                prodflocMat(j,i) = prodflocMat(j,i)+1                          
            end do                      
        end do        
        
        !Assign male earnings shocks for ages 2 to nw    
        do i =1,Ns  
            do j=2,nw
                cumum(1) = Puemmat(1,prodmlocMat(j-1,i))
                do v=2,nem
                    cumum(v) = cumum(v-1)+Puemmat(v,prodmlocMat(j-1,i))
                end do           
                call Locate(cumum, grnd(), prodmlocMat(j,i))
                prodmlocMat(j,i) = prodmlocMat(j,i)+1                          
            end do                      
        end do                               
        
        !Assign HH education location
        do i= 1,Ns 
            if (i<nint(Ns*cumeducdist(1))) then !HH
                HHeducloc(i)=1               
            else if (i<nint(Ns*cumeducdist(2))) then !HC 
                HHeducloc(i)=2                
            else if (i<nint(Ns*cumeducdist(3))) then !CH
                HHeducloc(i)=3                
            else !CC
                HHeducloc(i)=4                              
            end if   
        end do 
        
        FracEducLocH = 0.0d0       
        do i=1,Ns
            do j=1,nw
            do het = 1,nhet
                if (HHeducloc(i) == het) then
                    FracEducLocH(het,j) = FracEducLocH(het,j) + 1.0d0
                end if
            end do           
            end do
        end do
        FracEducLocH = FracEducLocH/Ns
               
        FracProdLocF = 0.0d0
        do i=1,Ns
            do j=1,nw
            do zf = 1,nef
                if (prodflocMat(j,i) == zf) then
                    FracProdLocF(zf,j) = FracProdLocF(zf,j) + 1.0d0
                end if
            end do           
            end do
        end do
        FracProdLocF = FracProdLocF/Ns
        
        FracProdLocM = 0.0d0
        do i=1,Ns
            do j=1,nw
            do zm = 1,nem
                if (prodmlocMat(j,i) == zm) then
                    FracProdLocM(zm,j) = FracProdLocM(zm,j) + 1.0d0
                end if
            end do           
            end do
        end do
        FracProdLocM = FracProdLocM/Ns               
        
        !Determine variable values for each observation at each age 1 to nw                   
        !Determine variable values for each observation at each age 1 to nw                   
        Nop = 0
        do i= 1,Ns 
            het = HHeducloc(i)                          
            fet = int((het+1)/2)
            met = mod(het+1,2)+1   
            labfMat(1,i) = labWCube(1,prodflocMat(1,i),prodmlocMat(1,i),1,1,het,1)
            earnsfMat(1,i) = w*efmat(prodflocMat(1,i), prodmlocMat(1,i),fet,1)*labfMat(1,i)
            earnsmMat(1,i) = w*emmat(prodflocMat(1,i), prodmlocMat(1,i),met,1)*hbar
            aveearnsfMat(1,i) = 0.0d0
            aveearnsmMat(1,i) = 0.0d0            
            consMat(1,i) = consumptionWCube(1,prodflocMat(1,i),prodmlocMat(1,i),1,1,het,1) 
            wealthMat(1,i) = 0.0d0
            UtilMat(1,i) = VnewWCube(1,prodflocMat(1,i),prodmlocMat(1,i),1,1,het,1)
            UtilConsMat(1,i) = UtilConsWCube(1,prodflocMat(1,i),prodmlocMat(1,i),1,1,het,1)
            !preTaxIncome = earnsMat(1,i) - SocSecTax(earnsMat(1,i)) + r*wealthMat(1,i)
            !postTaxWealth = preTaxIncome + wealthMat(1,i) - IncomeTax(preTaxIncome-r*wealthMat(1,i)*capTax) -r*wealthMat(1,i)*capTax + btran(s) - lstax
            !GovTransferMat(1,i) = transfers(postTaxWealth)
            !incomeMat(1,i) = postTaxWealth - wealthMat(1,i) + GovTransferMat(1,i)                                                       
            do j=2,nw
                !age-j wealth
                call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(j-1,i),prodmlocMat(j-1,i),:,:,het,j-1), &
                        wealthMat(j-1,i), aveearnsfMat(j-1,i), aveearnsmMat(j-1,i), wealthMat(j,i) )                  
                
                !age-j average earnings (average of earnings up to age j-1 earnings)                                      
                aveearnsfMat(j,i) = ((j-2)*aveearnsfMat(j-1,i) + earnsfMat(j-1,i))/(j-1)    
                aveearnsmMat(j,i) = ((j-2)*aveearnsmMat(j-1,i) + earnsmMat(j-1,i))/(j-1)       
                
                !age-j female labor supply
                call interpol3d(avect, aveEarnFvect, aveEarnMvect,labWCube(:,prodflocMat(j,i),prodmlocMat(j,i),:,:,het,j), &
                        wealthMat(j,i),aveearnsfMat(j,i), aveearnsmMat(j,i),labfMat(j,i))                   
                
                !earnings                                                             
                earnsfMat(j,i) = w*efmat(prodflocMat(j,i), prodmlocMat(j,i),fet,j)*labfMat(j,i)
                earnsmMat(j,i) = w*emmat(prodflocMat(j,i), prodmlocMat(j,i),met,j)*hbar  
                   
                !age-j consumption                            
                call interpol3d(avect, aveEarnFvect, aveEarnMvect, consumptionWCube(:,prodflocMat(j,i),prodmlocMat(j,i),:,:,het,j), &
                    wealthMat(j,i), aveearnsfMat(j,i),aveearnsmMat(j,i), consMat(j,i) ) 
                
                preTaxIncomeM = earnsmMat(j,i) - SocSecTax(earnsmMat(j,i)) + 0.5d0*r* wealthMat(j,i) - EarnsTaxFn(earnsmMat(j,i)) + 0.5d0*r* wealthMat(j,i)
                preTaxIncomeF = earnsfMat(j,i) - SocSecTax(earnsfMat(j,i)) + 0.5d0*r* wealthMat(j,i) - EarnsTaxFn(earnsfMat(j,i)) + 0.5d0*r* wealthMat(j,i)                     
                postTaxWealth = preTaxIncomeF + preTaxIncomeM + wealthMat(j,i) - IncomeTax(preTaxIncomeF+preTaxIncomeM- max(0.0d0,r*wealthMat(j,i)*capTax)) - max(0.0d0,r*wealthMat(j,i)*capTax)
                !if (j == 1) postTaxWealth = postTaxWealth + btran(het)            
                GovTransferMat(j,i) = transfers(postTaxWealth)
                
                call interpol3d(avect, aveEarnFvect, aveEarnMvect,VnewWCube(:,prodflocMat(j,i),prodmlocMat(j,i),:,:,het,j), &
                    wealthMat(j,i),aveearnsfMat(j,i), aveearnsmMat(j,i),UtilMat(j,i))       
                 call interpol3d(avect, aveEarnFvect, aveEarnMvect,UtilConsWCube(:,prodflocMat(j,i),prodmlocMat(j,i),:,:,het,j), &
                    wealthMat(j,i),aveearnsfMat(j,i), aveearnsmMat(j,i),UtilConsMat(j,i))  
                                   
                !ChangeWage(j-1,i) = earnsmMat(j,i)/earnsmMat(j-1,i) - 1
                !if (labfMat(j-1,i) > 1.d-8) then
                !    ChangeFHours(j-1,i) = labfMat(j,i)/labfMat(j-1,i) - 1
                !    Nop = Nop + 1
                !else
                !    ChangeFHours(j-1,i) = -100.0d0
                !end if
                                                              
            end do   
            aveearnsfMat(nw+1,i) = ((nw-1)*aveearnsfMat(nw,i) + earnsfMat(nw,i))/nw 
            aveearnsmMat(nw+1,i) = ((nw-1)*aveearnsmMat(nw,i) + earnsmMat(nw,i))/nw    
            
            call Locate(aveEarnMvect, aveearnsmMat(nw,i), aveearnsmloc(i))
            if (aveearnsmloc(i) == 0) aveearnsmloc(i) = 1
            !should we assign to lower or upper point based on probability?                                                       
        end do
        
        !!!!!!!!!!!!!!!!!!!!!!!
        !Retirees

        !Health Status 
        !Create cumulative distribution of female health status for each education type
        do set = 1,nset
            cumhealf(1,set) = hinitF(1,set)    
            do h=2,nsht
                cumhealf(h,set) = cumhealf(h-1,set) + hinitF(h,set)                             
            end do       
        end do    
        
        !Create cumulative distribution of male health status for each education type
        do set = 1,nset
            cumhealm(1,set) = hinitM(1,set)    
            do h=2,nsht
                cumhealm(h,set) = cumhealm(h-1,set) + hinitM(h,set)                             
            end do       
        end do              
        
        !Assign initial health status to each individual             
        do i= 1,Ns             
            het = HHeducloc(i)    
            fet = int((het+1)/2)
            met = mod(het+1,2)+1                          
            if (grnd() < cumhealf(1,fet)) then
                healfMat(1,i) = 1        
            else
                healfMat(1,i) = 2
            end if
            if (grnd() < cumhealm(1,met)) then
                healmMat(1,i) = 1        
            else
                healmMat(1,i) = 2
            end if                    
        end do      
        
        !Create cumulative distribution of marital statuses
        cumsinitH(1,:) = sinitH(1,:)
        do i = 2,3                           
            cumsinitH(i,:) = cumsinitH(i-1,:) + sinitH(i,:)                                   
        end do          
        
        !Assign alive flag to each individual of age nw+1
        alivefMat = 0
        alivemMat = 0
  
         
        !Assign marital flag to each household of age nw+1
        !maritalMat = 0
        !Assign 0 average earnings to husbands of some women who are widows
        maritalMat = 1
        do i =1,Ns                                            
            call Locate(cumsinitH(:,aveearnsmloc(i)), grnd(), maritalMat(1,i))
            maritalMat(1,i) = maritalMat(1,i)+1        
            maritalMat(2:,i) = maritalMat(1,i)
            if (maritalMat(1,i) == 1) then
                alivefMat(1,i) = 1
                alivemMat(1,i) = 1   
            else if (maritalMat(1,i) == 2) then
                alivefMat(1,i) = 1
                if (grnd() < probHusband0) then
                    aveearnsmMat(nw+1,i) = 0.0d0       
                end if
            else
                alivemMat(1,i) = 1 
            end if                                        
        end do
        deallocate(aveearnsmloc)
                 
        
        
        
        !Assign health status, alive status, marital status to each individual at ages nw+2 to T                     
        do i=1,Ns
        do j=2,nr    
            
            !survival           
            if (maritalMat(j-1,i) == 1 .and. alivefMat(j-1,i)==1 .and. alivemMat(j-1,i)==1) then  
                if (grnd() < survivalprobVectF(healfMat(j-1,i),1,j-1)) alivefMat(j,i) = 1     
                if (grnd() < survivalprobVectM(healmMat(j-1,i),1,j-1)) alivemMat(j,i) = 1     
            else if (maritalMat(j-1,i) == 2  .and. alivefMat(j-1,i)==1) then  !widow
                if (grnd() < survivalprobVectF(healfMat(j-1,i),2,j-1)) alivefMat(j,i) = 1
            else if (maritalMat(j-1,i) == 3  .and. alivemMat(j-1,i)==1) then  !widower
                if (grnd() < survivalprobVectM(healmMat(j-1,i),3,j-1)) alivemMat(j,i) = 1            
            end if 
            
            !marital status
            if (alivefMat(j,i) == 1 .and. alivemMat(j,i) == 1) then
                maritalMat(j:,i) = 1 
                futureStatus(j-1,i) = 1
            else if (alivefMat(j,i) == 1) then   
                maritalMat(j:,i) = 2
                if (maritalMat(j-1,i) == 1) then
                    futureStatus(j-1,i) = 2
                else
                    futureStatus(j-1,i) = 1
                end if
            else if (alivemMat(j,i) == 1) then   
                maritalMat(j:,i) = 3
                if (maritalMat(j-1,i) == 1) then
                    futureStatus(j-1,i) = 3
                else
                    futureStatus(j-1,i) = 1
                end if     
            else 
                if (maritalMat(j-1,i) == 1) then
                    futureStatus(j-1,i) = 4
                else
                    futureStatus(j-1,i) = 2
                end if            
            end if
            
            !health status                                    
            if (grnd() < PhmatF(1,healfMat(j-1,i),maritalMat(j-1,i),j-1)) then
                healfMat(j,i) = 1        
            else
                healfMat(j,i) = 2
            end if
            if (grnd() <PhmatM(1,healmMat(j-1,i),maritalMat(j-1,i),j-1)) then
                healmMat(j,i) = 1        
            else
                healmMat(j,i) = 2
            end if  
        end do        
        end do   
        do i=1,Ns
            if (maritalMat(nr,i) == 1) then
                futureStatus(nr,i) = 4
            else 
                futureStatus(nr,i) = 2
            end if
        end do             
        
        !Household health status
        do i=1,Ns
        do j=1,nr
            if (maritalMat(j,i) == 1) then            
                if (healfMat(j,i) == 1 .and. healmMat(j,i) == 1) then
                    healHmat(j,i) = 1 !BB
                else if (healfMat(j,i) == 1) then
                    healHmat(j,i) = 2 !BG
                else if (healmMat(j,i) == 1) then
                    healHmat(j,i) = 3 !GB
                else !GG
                    healHmat(j,i) = 4
                end if
            else  if (maritalMat(j,i) == 2) then                  
                    healHmat(j,i) =healfMat(j,i)  
            else
                healHmat(j,i) =healmMat(j,i)                                            
            end if
        end do
        end do
        
        !Medical shocks
        !Create cumulative distribution of medical shock for each education type
        do het = 1,nhet
            cummeddist(1,het) = minit(1,het)    
            do s=2,npm
                cummeddist(s,het) = cummeddist(s-1,het) + minit(s,het)                             
            end do       
        end do
        
        !Assign medical shocks for age nw+1    
        do i =1,Ns   
            het = HHeducloc(i)                                          
            call Locate(cummeddist(:,het), grnd(), medlocMat(1,i))
            medlocMat(1,i) = medlocMat(1,i)+1   
            if (grnd() > Ptmvect(1)) then
                medlocMat(1,i) = npm + medlocMat(1,i)
            end if                                                    
        end do 
        
        !Assign medical shocks for ages nw+2 to T
        !Need to condition this on martial status if want to have Pmmat differ by marital status
        do i =1,Ns  
            do j=2,nr
                pm = medlocMat(j-1,i)-npm*((medlocMat(j-1,i)-1)/npm)
                !tm = (medlocMat(j-1,i)-1)/npm + 1
                cummed(1) = Pummat(1,pm,j-1)
                do v=2,npm
                    cummed(v) = cummed(v-1)+Pummat(v,pm,j-1)
                end do           
                call Locate(cummed, grnd(), medlocMat(j,i))
                medlocMat(j,i) = medlocMat(j,i)+1   
                if (grnd() > Ptmvect(1)) then
                    medlocMat(j,i) = npm + medlocMat(j,i)
                end if                                        
            end do                      
        end do            
        
        aliveHmat = 0
        do i=1,Ns
            do j=1,nr
                if (alivefmat(j,i) == 1 .or. alivemmat(j,i)==1) aliveHmat(j,i) = 1                
            end do
        end do                  
        
        !Determine variable values for each observation at each age nw+1 to T
        
        do i=1,Ns
            do j=1,nr                
                !age-nw+j wealth
                if (j == 1) then                
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(nw,i),prodmlocMat(nw,i),:,:,HHeducloc(i),nw), &
                        wealthMat(nw,i), aveearnsfMat(nw,i), aveearnsmMat(nw,i), wealthMat(nw+1,i) )                     
                else
                    if (maritalMat(j-1,i) == 1) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeMarried(:,medlocMat(j-1,i),:,:,healHmat(j-1,i),futureStatus(j-1,i),j-1), &
                            wealthMat(nw+j-1,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), wealthMat(nw+j,i) )                      
                    else if (maritalMat(j-1,i) == 2) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidow(:,medlocMat(j-1,i),:,:,healfMat(j-1,i),futureStatus(j-1,i),j-1), &
                            wealthMat(nw+j-1,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), wealthMat(nw+j,i) )                                          
                    else
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidower(:,medlocMat(j-1,i),:,:,healmMat(j-1,i),futureStatus(j-1,i),j-1), &
                            wealthMat(nw+j-1,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), wealthMat(nw+j,i) )                                          
                    end if                                                                     
                end if
                

                if (maritalMat(j,i) == 1) then
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, consumptionRCubeMarried(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                       wealthMat(nw+j,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), consMat(nw+j,i) ) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,VnewRCubeMarried(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilMat(nw+j,i)) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,UtilConsRCubeMarried(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilConsMat(nw+j,i))    
                else if (maritalMat(j,i) == 2) then
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, consumptionRCubeWidow(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                       wealthMat(nw+j,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), consMat(nw+j,i) ) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,VnewRCubeWidow(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilMat(nw+j,i)) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,UtilConsRCubeWidow(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilConsMat(nw+j,i))                           
                else
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, consumptionRCubeWidower(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                       wealthMat(nw+j,i), aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i), consMat(nw+j,i) ) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,VnewRCubeWidower(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilMat(nw+j,i)) 
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect,UtilConsRCubeWidower(:,medlocMat(j,i),:,:,healHmat(j,i),futureStatus(j,i),j), &
                        wealthMat(nw+j,i),aveearnsfMat(nw+1,i), aveearnsmMat(nw+1,i),UtilConsMat(nw+j,i))                           
                end if                 

                TotMedExpMat(j,i) = mmat(medlocMat(j,i),healHmat(j,i),futureStatus(j,i),maritalMat(j,i),j)
                SocSecMat(j,i) = socsec(aveearnsfMat(nw+1,i),aveearnsmMat(nw+1,i),maritalMat(j,i))
                preTaxIncome = SocSecMat(j,i) + r*wealthMat(nw+j,i)
                postTaxWealth = preTaxIncome + wealthMat(nw+j,i) - IncomeTax(r*wealthMat(nw+j,i)- max(0.0d0,r*wealthMat(nw+j,i)*capTax),SocSecMat(j,i),TotMedExpMat(j,i),maritalMat(j,i)) - max(0.0d0,r*wealthMat(nw+j,i)*capTax)
                             
                GovTransferMat(nw+j,i) = transfersRetired(postTaxWealth, j,medlocMat(j,i),healHmat(j,i),futureStatus(j,i),maritalMat(j,i),1)
                !incomeMat(nw+j,i) = preTaxIncome  - IncomeTax(r*wealthMat(nw+j,i) - r*wealthMat(nw+j,i)*capTax,SocSecMat(j,i),TotMedExpMat(j,i),maritalMat(j,i)) + max(GovTransferMat(nw+j,i) - medMat(j,i),0.0d0) - r*wealthMat(nw+j,i)*capTax  
                
                if(GovTransferMat(nw+j,i) == 0.0d0) then
                    OOPExpMat(j,i) = TotMedExpMat(j,i)
                else
                !    if (medlocMat(j,i) == 5 .or. medlocMat(j,i) == 10) then
                !        OOPExpMat(j,i) = 0.0d0
                !    else 
                     if (maritalMat(j,i)==1) then
                        OOPExpMat(j,i) = min((1-FracMedicaidCovers)*TotMedExpMat(j,i),clowerbarMarried/2)
                     else if (maritalMat(j,i) == 2) then
                        OOPExpMat(j,i) = min((1-FracMedicaidCovers)*TotMedExpMat(j,i),clowerbarWidow/2)                     
                     else
                        OOPExpMat(j,i) = min((1-FracMedicaidCovers)*TotMedExpMat(j,i),clowerbarWidower/2)                     
                     end if
                !    end if    
                end if                                                                     
                                  
            end do
        end do        
       
  
  !     !AveYearsAlive
        AveYearsAliveF = 0.0d0;
        AveYearsAliveM = 0.0d0;
        do i = 1,Ns
            AveYearsAliveF = AveYearsAliveF + sum(alivefMat(:,i))  
            AveYearsAliveM = AveYearsAliveM + sum(alivemMat(:,i))  
        end do   
        AveYearsAliveF = AveYearsAliveF/Ns*2
        AveYearsAliveM = AveYearsAliveM/Ns*2
        
        AveYearsAliveF2 = 0.0d0
        AveYearsAliveM2 = 0.0d0
        NumPeopleF = 0
        NumPeopleM = 0
        do i = 1,Ns
            if (alivefMat(1,i) == 1) then
                AveYearsAliveF2 = AveYearsAliveF2 + sum(alivefMat(:,i))
                NumPeopleF = NumPeopleF + 1  
            end if
            if (alivemMat(1,i) == 1) then
                AveYearsAliveM2 = AveYearsAliveM2 + sum(alivemMat(:,i)) 
                NumPeopleM = NumPeopleM + 1  
            end if
        end do   
        AveYearsAliveF2 = AveYearsAliveF2/NumPeopleF*2
        AveYearsAliveM2 = AveYearsAliveM2/NumPeopleM*2       
        
        !Compute stats
        distNHentryage = 0.0d0
        aveyearsNH = 0.0d0
        do i=1,Ns
            do j=1,nr
                if ((medlocMat(j,i)==5 .or. medlocMat(j,i)==10) .and. aliveHmat(j,i) == 1) then
                    if (j<6) then
                        distNHentryage(1) = distNHentryage(1) + 1.0d0  
                        aveyearsNH(1) = aveyearsNH(1) + 2.0d0                      
                        do s=j+1,nr
                            if ((medlocMat(s,i)==5 .or. medlocMat(s,i)==10) .and. aliveHmat(s,i) == 1) then
                                aveyearsNH(1) = aveyearsNH(1) + 2.0d0 
                            end if 
                        end do
                    else if (j<11) then
                        distNHentryage(2) = distNHentryage(2) + 1.0d0
                        aveyearsNH(2) = aveyearsNH(2) + 2.0d0                      
                        do s=j+1,nr
                            if ((medlocMat(s,i)==5 .or. medlocMat(s,i)==10) .and. aliveHmat(s,i) == 1) then
                                aveyearsNH(2) = aveyearsNH(2) + 2.0d0 
                            end if 
                        end do                        
                    else 
                        distNHentryage(3) = distNHentryage(3) + 1.0d0
                        aveyearsNH(3) = aveyearsNH(3) + 2.0d0                      
                        do s=j+1,nr
                            if ((medlocMat(s,i)==5 .or. medlocMat(s,i)==10) .and. aliveHmat(s,i) == 1) then
                                aveyearsNH(3) = aveyearsNH(3) + 2.0d0 
                            end if 
                        end do                        
                    end if
                    exit
                end if
            end do
        end do
        fracNH = sum(distNHentryage)/dble(Ns)
        aveyearsNH = aveyearsNH/distNHentryage
        distNHentryage = distNHentryage/sum(distNHentryage)
        
        !time-series correlation, average across households
        TotNum = 0
        NumZeroStd = 0
        corrWageFM = 0.0d0
        corrWageMHoursF = 0.0d0
        do i=1,Ns
            NumYears = 0
            do ti=1,nw-1
                if (labfmat(ti,i) > 1.d-5 .and. labfmat(ti+1,i) > 1.d-5) then
                    wagef1 = earnsfmat(ti+1,i)/labfmat(ti+1,i)
                    wagef0 = earnsfmat(ti,i)/labfmat(ti,i)
                    changeWagesF(ti) = (wagef1 - wagef0)/wagef1
                    wagem1 = earnsmmat(ti+1,i)/hbar
                    wagem0 = earnsmmat(ti,i)/hbar
                    changeWagesM(ti) = (wagem1 - wagem0)/wagem1 
                    changeHoursF(ti) = (labfmat(ti+1,i) - labfmat(ti,i))/labfmat(ti+1,i)                                         
                    NumYears = NumYears + 1
                else
                    changeWagesF(ti) = -1.d7
                end if           
            end do
            if (NumYears > 1) then
                TotNum = TotNum + 1
                meanChangeWageM = sum(changeWagesF,changeWagesF>-1.d6)/NumYears
                meanChangeWageM = sum(changeWagesM,changeWagesF>-1.d6)/NumYears
                meanChangeHoursF = sum(changeHoursF,changeWagesF>-1.d6)/NumYears
                stdChangeWageF = sqrt(sum((changeWagesF - meanChangeWageF)**2,changeWagesF > -1.d6)/(NumYears-1))
                stdChangeWageM = sqrt(sum((changeWagesM - meanChangeWageM)**2,changeWagesF > -1.d6)/(NumYears-1))
                stdChangeHoursF = sqrt(sum((changeHoursF - meanChangeHoursF)**2,changeWagesF > -1.d6)/(NumYears-1))
                corrWageFM = corrWageFM + &
                    sum( (changeWagesF - meanChangeWageF)*(changeWagesM - meanChangeWageM),changeWagesF > -1.d6)/(NumYears-1)/stdChangeWageF/stdChangeWageM
                if (stdChangeHoursF > 1.d-5) then
                    corrWageMHoursF = corrWageMHoursF + &
                        sum( (changeHoursF - meanChangeHoursF)*(changeWagesM - meanChangeWageM),changeWagesF > -1.d6)/(NumYears-1)/stdChangeHoursF/stdChangeWageM
                else
                    NumZeroStd = NumZeroStd + 1    
                end if
                !print *, "NumYears =", NumYears
                !print *, "corrWageFM=",  sum( (changeWagesF - meanChangeWageF)*(changeWagesM - meanChangeWageM),changeWagesF > -1.d6)/(NumYears-1)/stdChangeWageF/stdChangeWageM
                !print *, "corrWageMHoursF=", sum( (changeHoursF - meanChangeHoursF)*(changeWagesM - meanChangeWageM),changeWagesF > -1.d6)/(NumYears-1)/stdChangeHoursF/stdChangeWageM
                !print *, "stdChangeWageF=", stdChangeWageF
               ! print *, "stdChangeWageM=", stdChangeWageM
                !print *, "stdChangeHoursF=", stdChangeHoursF
                !print *, "meanChangeWageM=", meanChangeWageM
                !print *, "meanChangeWageM=", meanChangeWageM
                !print *, "meanChangeHoursF=", meanChangeHoursF
                !pause
            end if
        end do     
        corrWageFM = corrWageFM/TotNum
        corrWageMHoursF = corrWageMHoursF/(TotNum-NumZeroStd)                
        !print *, "corrWageFM=", corrWageFM
        !print *, "corrWageMHoursF=", corrWageMHoursF
        !print *, "TotNum=", TotNum
        !print *, "NumZeroStd=", NumZeroStd
        !pause
                
        !cross-sectional correlation, averaged over time
        allocate (changeWagesFx(Ns), changeWagesMx(Ns), changeHoursFx(Ns)) 
        NumYears = 0
        corrWageFMx = 0.0d0
        corrWageMHoursFx = 0.0d0
        do ti=1,nw-1
            TotNum = 0
            meanChangeWageF = 0.0d0
            meanChangeWageM = 0.0d0
            meanChangeHoursF = 0.0d0
            stdChangeWageF = 0.0d0
            stdChangeWageM = 0.0d0
            stdChangeHoursF = 0.0d0
            covWageFMxtemp = 0.0d0
            covWageMHoursFxtemp = 0.0d0
            do i=1,Ns
                if (labfmat(ti,i) > 1.d-5 .and. labfmat(ti+1,i) > 1.d-5) then
                    wagef1 = earnsfmat(ti+1,i)/labfmat(ti+1,i)
                    wagef0 = earnsfmat(ti,i)/labfmat(ti,i)
                    changeWagesFx(i) = (wagef1 - wagef0)/wagef1
                    wagem1 = earnsmmat(ti+1,i)/hbar
                    wagem0 = earnsmmat(ti,i)/hbar
                    changeWagesMx(i) = (wagem1 - wagem0)/wagem1 
                    changeHoursFx(i) = (labfmat(ti+1,i) - labfmat(ti,i))/labfmat(ti+1,i)                                         
                    TotNum = TotNum + 1
                    
                    meanChangeWageF = meanChangeWageF + changeWagesFx(i)
                    meanChangeWageM = meanChangeWageM + changeWagesMx(i)
                    meanChangeHoursF = meanChangeHoursF + changeHoursFx(i)       
                end if   
            end do
            meanChangeWageF = meanChangeWageF/TotNum    
            meanChangeWageM = meanChangeWageM/TotNum    
            meanChangeHoursF = meanChangeHoursF/TotNum
            do i=1,Ns
                if (labfmat(ti,i) > 1.d-5 .and. labfmat(ti+1,i) > 1.d-5) then
                    stdChangeWageF = stdChangeWageF + (changeWagesFx(i) - meanChangeWageF)**2
                    stdChangeWageM = stdChangeWageM + (changeWagesMx(i) - meanChangeWageM)**2
                    stdChangeHoursF = stdChangeHoursF + (changeHoursFx(i) - meanChangeHoursF)**2
                    covWageFMxtemp = covWageFMxtemp + (changeWagesFx(i) - meanChangeWageF)*(changeWagesMx(i) - meanChangeWageM)
                    covWageMHoursFxtemp = covWageMHoursFxtemp + (changeHoursFx(i) - meanChangeHoursF)*(changeWagesMx(i) - meanChangeWageM)
                end if         
            end do 
            if (TotNum > 1) then
                stdChangeWageF = sqrt(stdChangeWageF/(TotNum-1))
                stdChangeWageM = sqrt(stdChangeWageM/(TotNum-1))
                stdChangeHoursF = sqrt(stdChangeHoursF/(TotNum-1))
                covWageFMxtemp = covWageFMxtemp/(TotNum-1)
                covWageMHoursFxtemp = covWageMHoursFxtemp/(TotNum-1)
                corrWageFMx = corrWageFMx + covWageFMxtemp/stdChangeWageF/stdChangeWageM
                corrWageMHoursFx = corrWageMHoursFx + covWageMHoursFxtemp/stdChangeHoursF/stdChangeWageM  
                NumYears = NumYears + 1
                !print *, "TotNum =", TotNum
                !print *, "covWageFMxtemp=",  covWageFMxtemp
                !print *, "covWageMHoursFxtemp=", covWageMHoursFxtemp
                !print *, "corrWageFMx=",  corrWageFMx
                !print *, "corrWageMHoursFx=", corrWageMHoursFx                
                !print *, "stdChangeWageF=", stdChangeWageF
                !print *, "stdChangeWageM=", stdChangeWageM
                !print *, "stdChangeHoursF=", stdChangeHoursF
                !print *, "meanChangeWageM=", meanChangeWageM
                !print *, "meanChangeWageM=", meanChangeWageM
                !print *, "meanChangeHoursF=", meanChangeHoursF 
                !pause               
            end if            
        end do     
        corrWageFMx = corrWageFMx/NumYears
        corrWageMHoursFx = corrWageMHoursFx/NumYears 
        print *, "corrWageFMx=", corrWageFMx
        print *, "corrWageMHoursFx=", corrWageMHoursFx
        print *, "NumYears=", NumYears
                
        deallocate (changeWagesFx, changeWagesMx, changeHoursFx) 
        
        AveEarnsFbyAge(:nw) = sum(aveearnsfMat(:nw,:),2)/Ns
        AveEarnsMbyAge(:nw) = sum(aveearnsmMat(:nw,:),2)/Ns
        AveEarnsFbyAge(nw+1) = sum(aveearnsfMat(nw+1,:)*aliveHMat(1,:))/sum(aliveHMat(1,:))
        AveEarnsMbyAge(nw+1) = sum(aveearnsmMat(nw+1,:)*aliveHMat(1,:))/sum(aliveHMat(1,:))
        EarnsFbyAge = sum(earnsfMat,2)/Ns
        EarnsMbyAge = sum(earnsmMat,2)/Ns
        AveUtilByAge(:nw) = sum(UtilMat(:nw,:),2)/Ns
        AveUtilByAge(nw+1:) = sum(UtilMat(nw+1:,:)*aliveHMat,2)/sum(aliveHMat,2)
        AveUtilConsByAge(:nw) = sum(UtilConsMat(:nw,:),2)/Ns
        AveUtilConsByAge(nw+1:) = sum(UtilConsMat(nw+1:,:)*aliveHMat,2)/sum(aliveHMat,2)        
        WealthByAge(:nw) = sum(wealthMat(:nw,:),2)/Ns
        WealthByAge(nw+1:) = sum(wealthMat(nw+1:,:)*aliveHMat,2)/sum(aliveHMat,2)
        ConsByAge(:nw) = sum(consMat(:nw,:),2)/Ns
        ConsByAge(nw+1:) = sum(consMat(nw+1:,:)*aliveHMat,2)/sum(aliveHMat,2)
        SocSecByAge = sum(SocSecMat*aliveHMat,2)/sum(aliveHMat,2)
        TotMedByAge = sum(TotMedExpMat*aliveHMat,2)/sum(aliveHMat,2)
        OOPMedByAge = sum(OOPExpMat*aliveHMat,2)/sum(aliveHMat,2)
        WealthByAgeEduc(:nw,1) = sum(wealthMat(:nw,:nint(Ns*cumeducdist(1))),2)/nint(Ns*cumeducdist(1))
        WealthByAgeEduc(nw+1:,1) = sum(wealthMat(nw+1:,:nint(Ns*cumeducdist(1))) *aliveHMat(:,:nint(Ns*cumeducdist(1))) ,2)/sum(aliveHMat(:,:nint(Ns*cumeducdist(1))),2)
        ConsByAgeEduc(:nw,1) = sum(consMat(:nw,:nint(Ns*cumeducdist(1))),2)/nint(Ns*cumeducdist(1))
        ConsByAgeEduc(nw+1:,1) = sum(consMat(nw+1:,:nint(Ns*cumeducdist(1))) *aliveHMat(:,:nint(Ns*cumeducdist(1))) ,2)/sum(aliveHMat(:,:nint(Ns*cumeducdist(1))),2)        
        AveUtilByAgeEduc(:nw,1) =  sum(UtilMat(:nw,:nint(Ns*cumeducdist(1))),2)/nint(Ns*cumeducdist(1))
        AveUtilByAgeEduc(nw+1:,1) = sum(UtilMat(nw+1:,:nint(Ns*cumeducdist(1))) *aliveHMat(:,:nint(Ns*cumeducdist(1))) ,2)/sum(aliveHMat(:,:nint(Ns*cumeducdist(1))),2) 
        AveUtilConsByAgeEduc(:nw,1) =  sum(UtilConsMat(:nw,:nint(Ns*cumeducdist(1))),2)/nint(Ns*cumeducdist(1))
        AveUtilConsByAgeEduc(nw+1:,1) = sum(UtilConsMat(nw+1:,:nint(Ns*cumeducdist(1))) *aliveHMat(:,:nint(Ns*cumeducdist(1))) ,2)/sum(aliveHMat(:,:nint(Ns*cumeducdist(1))),2) 

        AggWorkersAssets = sum(WealthByAge(:nw)*CohortWeights(:nw))
        AggRetireesAssets = sum(WealthByAge(nw+1:)*CohortWeights(nw+1:))        
  

        do het=2,nhet
           WealthByAgeEduc(:nw,het) = sum(wealthMat(:nw,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/(nint(Ns*cumeducdist(het))-nint(Ns*cumeducdist(het-1)))
           WealthByAgeEduc(nw+1:,het) = sum(wealthMat(nw+1:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het)))*aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/sum(aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)
           ConsByAgeEduc(:nw,het) = sum(consMat(:nw,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/(nint(Ns*cumeducdist(het))-nint(Ns*cumeducdist(het-1)))
           ConsByAgeEduc(nw+1:,het) = sum(consMat(nw+1:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het)))*aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/sum(aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)            
           AveUtilByAgeEduc(:nw,het) =  sum(UtilMat(:nw,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/(nint(Ns*cumeducdist(het))-nint(Ns*cumeducdist(het-1)))
           AveUtilByAgeEduc(nw+1:,het) = sum(UtilMat(nw+1:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het)))*aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/sum(aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)
           AveUtilConsByAgeEduc(:nw,het) =  sum(UtilConsMat(:nw,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/(nint(Ns*cumeducdist(het))-nint(Ns*cumeducdist(het-1)))
           AveUtilConsByAgeEduc(nw+1:,het) = sum(UtilConsMat(nw+1:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het)))*aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)/sum(aliveHMat(:,nint(Ns*cumeducdist(het-1))+1:nint(Ns*cumeducdist(het))),2)        
        end do
        
        FracRecGovTransByAgeEduc = 0.0d0        
        do j=1,nw
        do v=1,nint(Ns*cumeducdist(1))         
            if (GovTransferMat(j,v) > 0.0d0) then                
                FracRecGovTransByAgeEduc(j,1) = FracRecGovTransByAgeEduc(j,1) + 1                    
            end if                
        end do
        end do
        FracRecGovTransByAgeEduc(:nw,1) = FracRecGovTransByAgeEduc(:nw,1)/nint(Ns*cumeducdist(1))         
        do i=2,nhet
        do j=1,nw
        do v=1,nint(Ns*cumeducdist(i))-nint(Ns*cumeducdist(i-1))            
            if (GovTransferMat(j,nint(Ns*cumeducdist(i-1))+v) > 0.0d0) then                
                FracRecGovTransByAgeEduc(j,i) = FracRecGovTransByAgeEduc(j,i) + 1                    
            end if                
        end do
        end do
            FracRecGovTransByAgeEduc(:nw,i) = FracRecGovTransByAgeEduc(:nw,i)/(nint(Ns*cumeducdist(i))-nint(Ns*cumeducdist(i-1)))
        end do

        do j=1,nr
        do v=1,nint(Ns*cumeducdist(1))                                  
            if (GovTransferMat(nw+j,v) > 0.0d0 .and. aliveHmat(j,v) == 1) then                
                FracRecGovTransByAgeEduc(nw+j,1) = FracRecGovTransByAgeEduc(nw+j,1) + 1
            end if                                                       
        end do                        
        end do
        FracRecGovTransByAgeEduc(nw+1:,1) = FracRecGovTransByAgeEduc(nw+1:,1)/sum(aliveHmat(:,:nint(Ns*cumeducdist(1)) ),2)                     
        do i=2,nhet
        do j=1,nr
        do v=1,nint(Ns*cumeducdist(i))-nint(Ns*cumeducdist(i-1))                                   
            if (GovTransferMat(nw+j,nint(Ns*cumeducdist(i-1))+v) > 0.0d0 .and. aliveHmat(j,nint(Ns*cumeducdist(i-1))+v) == 1) then                
                FracRecGovTransByAgeEduc(nw+j,i) = FracRecGovTransByAgeEduc(nw+j,i) + 1
            end if                                                       
        end do                        
        end do
        end do  
        do i=2,nhet            
            FracRecGovTransByAgeEduc(nw+1:,i) = FracRecGovTransByAgeEduc(nw+1:,i)/sum(aliveHmat(:,nint(Ns*cumeducdist(i-1))+1:nint(Ns*cumeducdist(i)) ),2)             
        end do          
        
        NumWidBad = 0
        NumWidrBad = 0
        NumWidGood = 0
        NumWidrGood = 0
        do j=1,nr
        do i=1,Ns
            if (MaritalMat(j,i) == 2) then
                if (healFmat(j,i) == 1) then 
                    AveUtilByAgeHealMart(j,1,2) = AveUtilByAgeHealMart(j,1,2) + UtilMat(j,i)*aliveFMat(j,i)
                    NumWidBad = NumWidBad + aliveFMat(j,i)
                 else
                    AveUtilByAgeHealMart(j,2,2) = AveUtilByAgeHealMart(j,2,2) + UtilMat(j,i)*aliveFMat(j,i)
                    NumWidGood = NumWidGood + aliveFMat(j,i)
                 end if
             else if (MaritalMat(j,i) == 3) then
                if (healMmat(j,i) == 1) then 
                    AveUtilByAgeHealMart(j,1,3) = AveUtilByAgeHealMart(j,1,3) + UtilMat(j,i)*aliveMMat(j,i)
                    NumWidrBad = NumWidrBad + aliveMMat(j,i)
                 else
                    AveUtilByAgeHealMart(j,2,3) = AveUtilByAgeHealMart(j,2,3) + UtilMat(j,i)*aliveMMat(j,i)
                    NumWidrGood = NumWidrGood + aliveMMat(j,i)
                 end if             
             end if 
        end do
           AveUtilByAgeHealMart(j,1,2) = AveUtilByAgeHealMart(j,1,2)/NumWidBad
           AveUtilByAgeHealMart(j,2,2) = AveUtilByAgeHealMart(j,2,2)/NumWidGood
           AveUtilByAgeHealMart(j,1,3) = AveUtilByAgeHealMart(j,1,3)/NumWidrBad
           AveUtilByAgeHealMart(j,2,3) = AveUtilByAgeHealMart(j,2,3)/NumWidrGood 
        end do
        
        AveUtilFWidbyAgeWid = 0.0d0
        NumFWids = 0
        NumMWids = 0
        do i=1,Ns
            do j=1,nr
                if (maritalMat(j,i) == 2) then
                    AveUtilFWidbyAgeWid(j) = AveUtilFWidbyAgeWid(j) + UtilMat(1,i)
                    NumFWids(j) = NumFWids(j) + 1
                    exit
                else if (maritalMat(j,i) == 3) then
                    AveUtilMWidbyAgeWid(j) = AveUtilMWidbyAgeWid(j) + UtilMat(1,i)
                    NumMWids(j) = NumMWids(j) + 1
                    exit
                end if
            end do            
        end do
        AveUtilFWidbyAgeWid = AveUtilFWidbyAgeWid/NumFWids
        AveUtilMWidbyAgeWid = AveUtilMWidbyAgeWid/NumMWids

        !rank by average lifetime earnings of males
        call mrgrnk(aveearnsmMat(nw+1,:), earnsmrank)
        wealthMat = wealthMat(:,earnsmrank)
        consMat = consMat(:,earnsmrank)
        UtilMat = UtilMat(:,earnsmrank)
        UtilConsMat = UtilConsMat(:,earnsmrank)
        earnsmmat = earnsmmat(:,earnsmrank)
        earnsfmat = earnsfmat(:,earnsmrank)
        labfMat = labfMat(:,earnsmrank)
        SocSecMat = SocSecMat(:,earnsmrank)
        TotMedExpMat = TotMedExpMat(:,earnsmrank)
        OOPExpMat = OOPExpMat(:,earnsmrank)
        GovTransferMat = GovTransferMat(:,earnsmrank)
        aliveHmat = aliveHmat(:,earnsmrank)
        maritalMat = maritalMat(:,earnsmrank)
        aveearnsmMat = aveearnsmMat(:,earnsmrank)
        aveearnsfMat = aveearnsfMat(:,earnsmrank)
        
        Np = Ns/5
        do i=1,5
            AveEarnsQuintileCutoffs(i) = aveearnsmMat(nw+1,Np*(i-1)+1)
            WealthByAgePEM(:nw,i) = sum(wealthMat(:nw,Np*(i-1)+1:Np*i),2)/Np 
            WealthByAgePEM(nw+1:,i) = sum(wealthMat(nw+1:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            ConsByAgePEM(:nw,i) = sum(consMat(:nw,Np*(i-1)+1:Np*i),2)/Np
            ConsByAgePEM(nw+1:,i) = sum(consMat(nw+1:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            UtilByAgePEM(:nw,i) = sum(UtilMat(:nw,Np*(i-1)+1:Np*i),2)/Np                                        
            UtilByAgePEM(nw+1:,i) = sum(UtilMat(nw+1:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            UtilConsByAgePEM(:nw,i) = sum(UtilConsMat(:nw,Np*(i-1)+1:Np*i),2)/Np                                        
            UtilConsByAgePEM(nw+1:,i) = sum(UtilConsMat(nw+1:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)            
            EarnsMByAgePEM(:,i) = sum(earnsmmat(:,Np*(i-1)+1:Np*i),2)/Np 
            EarnsFByAgePEM(:,i) = sum(earnsfmat(:,Np*(i-1)+1:Np*i),2)/Np 
            LabFByAgePEM(:,i) = sum(labfMat(:,Np*(i-1)+1:Np*i),2)/Np 
            SocSecByAgePEM(:,i) = sum(SocSecMat(:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            
            SocSecRepRateAge65(i,1) =sum( SocSecMat(1,Np*(i-1)+1:Np*i)/(aveearnsfMat(nw+1,Np*(i-1)+1:Np*i)+aveearnsmMat(nw+1,Np*(i-1)+1:Np*i)) &
                *aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==1)/sum(aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==1)  
            SocSecRepRateAge65(i,2) =sum( SocSecMat(1,Np*(i-1)+1:Np*i)/(aveearnsfMat(nw+1,Np*(i-1)+1:Np*i)+aveearnsmMat(nw+1,Np*(i-1)+1:Np*i)) &
                *aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==2)/sum(aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==2)  
            SocSecRepRateAge65(i,3) =sum( SocSecMat(1,Np*(i-1)+1:Np*i)/(aveearnsfMat(nw+1,Np*(i-1)+1:Np*i)+aveearnsmMat(nw+1,Np*(i-1)+1:Np*i)) &
                *aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==3)/sum(aliveHmat(1,Np*(i-1)+1:Np*i),1,maritalMat(1,Np*(i-1)+1:Np*i)==3)                                      
            TotMedByAgePEM(:,i) = sum(TotMedExpMat(:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            OOPExpByAgePEM(:,i) = sum(OOPExpMat(:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            MedicaidExpByAgePEM(:,i) = sum((TotMedExpMat(:,Np*(i-1)+1:Np*i)-OOPExpMat(:,Np*(i-1)+1:Np*i))*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)
            GovTransferByAgePEM(:nw,i) = sum(GovTransferMat(:nw,Np*(i-1)+1:Np*i),2)/Np 
            GovTransferByAgePEM(nw+1:,i) = sum(GovTransferMat(nw+1:,Np*(i-1)+1:Np*i)*aliveHmat(:,Np*(i-1)+1:Np*i),2)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)  
            MaritalDistAge65PEM(i,1) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 1)
            MaritalDistAge65PEM(i,2) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 2)             
        end do 
        
        MaritalDistAge65PEM(:,1:2) = MaritalDistAge65PEM(:,1:2)/Np      
        MaritalDistAge65PEM(:,3) = 1.0d0 - MaritalDistAge65PEM(:,1) - MaritalDistAge65PEM(:,2)
              
                        
        FracRecGovTransByAgePEM = 0.0d0
        do i=1,5
        do j=1,nw
        do v=1,Np            
            if (GovTransferMat(j,Np*(i-1)+v) > 0.0d0) then                
                FracRecGovTransByAgePEM(j,i) = FracRecGovTransByAgePEM(j,i) + 1                    
            end if                
        end do
        end do
        end do
        FracRecGovTransByAgePEM(:nw,:) = FracRecGovTransByAgePEM(:nw,:)/Np        
        
        do i=1,5
        do j=1,nr
        do v=1,Np                                    
            if (GovTransferMat(nw+j,Np*(i-1)+v) > 0.0d0 .and. aliveHmat(j,Np*(i-1)+v) == 1) then                
                FracRecGovTransByAgePEM(nw+j,i) = FracRecGovTransByAgePEM(nw+j,i) + 1
            end if                                                       
        end do                        
        end do
        end do  
        do i=1,5            
            FracRecGovTransByAgePEM(nw+1:,i) = FracRecGovTransByAgePEM(nw+1:,i)/sum(aliveHmat(:,Np*(i-1)+1:Np*i),2)             
        end do                        

        
        do i=1,5
            AggregateWealthPEQ(i) = sum(WealthByAgePEM(:,i)*CohortWeights,1)
            AggWorkersAssetsPEQ(i) = sum(WealthByAgePEM(:nw,i)*CohortWeights(:nw),1) 
            AggRetireesAssetsPEQ(i) = sum(WealthByAgePEM(nw+1:,i)*CohortWeights(nw+1:),1) 
            AggregateOOPPEQ(i) = sum(OOPExpByAgePEM(:,i)*CohortWeights(nw+1:),1)                        
            AggregateConsPEQ(i) = sum(ConsByAgePEM(:,i)*CohortWeights,1)     
            AggregateRetConsPEQ(i) = sum(ConsByAgePEM(nw+1:,i)*CohortWeights(nw+1:),1)       
            MeanMedicaidPEQ(i) = sum(MedicaidExpByAgePEM(:,i)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:)) 
            FracWorkersRecGovTrans(i) = sum(FracRecGovTransByAgePEM(:nw,i)*CohortWeights(:nw))/sum(CohortWeights(:nw))
            FracRetireesRecGovTrans(i) = sum(FracRecGovTransByAgePEM(nw+1:,i)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
            Frac6574RecGovTrans(i) = sum(FracRecGovTransByAgePEM(nw+1:nw+5,i)*CohortWeights(nw+1:nw+5))/sum(CohortWeights(nw+1:nw+5))
            Frac7584RecGovTrans(i) = sum(FracRecGovTransByAgePEM(nw+6:nw+10,i)*CohortWeights(nw+6:nw+10))/sum(CohortWeights(nw+6:nw+10))
            Frac85plusRecGovTrans(i) = sum(FracRecGovTransByAgePEM(nw+11:,i)*CohortWeights(nw+11:))/sum(CohortWeights(nw+11:))
        end do          
        
        deallocate (HHeducloc, TotMedExpRankMat, maritalrank, medicalrank, aliverank)  
        deallocate(prodflocMat, prodmlocMat, medlocMat, UtilMat, UtilConsMat, OOPExpMat, GovTransferMat)   
        deallocate (wealthMat,labfMat,earnsfMat,earnsmMat, TotMedExpMat, consMat)                  
        !rank by average lifetime earnings of primary wage earner
        allocate(aveearnsPrimary(Ns))
        do i=1,Ns
            aveearnsPrimary(i) = max(aveearnsmMat(nw+1,i),aveearnsfMat(nw+1,i))
        end do
        call mrgrnk(aveearnsPrimary, earnsmrank)        
        maritalMat = maritalMat(:,earnsmrank)
        
        
        Np = Ns/5
        do i=1,5
            MaritalDistAge65PEM(i,1) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 1)
            MaritalDistAge65PEM(i,2) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 2)             
        end do 
        
        MaritalDistAge65PEM(:,1:2) = MaritalDistAge65PEM(:,1:2)/Np      
        MaritalDistAge65PEM(:,3) = 1.0d0 - MaritalDistAge65PEM(:,1) - MaritalDistAge65PEM(:,2)        
        
        !Rank by average lifetime medical expenses

        !rank by household social security income
        do i=1,Ns
            aveearnsPrimary(i) = SocSecMat(1,i)
        end do
        call mrgrnk(aveearnsPrimary, earnsmrank)        
        maritalMat = maritalMat(:,earnsmrank)
       
        
        Np = Ns/5
        do i=1,5
            MaritalDistAge65SSInc(i,1) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 1)
            MaritalDistAge65SSInc(i,2) = count(maritalMat(1,Np*(i-1)+1:Np*i) == 2)             
        end do 
        
        MaritalDistAge65SSInc(:,1:2) = MaritalDistAge65SSInc(:,1:2)/Np      
        MaritalDistAge65SSInc(:,3) = 1.0d0 - MaritalDistAge65SSInc(:,1) - MaritalDistAge65SSInc(:,2)        
              
        
        deallocate(earnsmrank)
        deallocate (aveearnsPrimary, SocSecMat)                
        deallocate (alivefMat,alivemMat,aliveHmat,healHmat, healfMat, healmMat, maritalMat, aveearnsfMat,aveearnsmMat)

        !No = (nw-1)*Ns
        !allocate (ChangeWageVect(No), ChangeFHoursVect(No))
        !ChangeWageVect = reshape(ChangeWage,(/No/))
        !ChangeFHoursVect = reshape(ChangeFHours,(/No/))
        !meanChangeWage = sum(ChangeWageVect/No,ChangeFHoursVect > -10.d0)
        !meanChangeFHours = sum(ChangeFHoursVect/No,ChangeFHoursVect > -10.d0)
        !stdChangeWage = sqrt(sum((ChangeWageVect - meanChangeWage)**2/(No-1),ChangeFHoursVect > -10.d0))
        !stdChangeFHours = sqrt(sum((ChangeFHoursVect - meanChangeFHours)**2/(No-1),ChangeFHoursVect > -10.d0))
        !corrWageMHoursF = sum( ((ChangeWageVect - meanChangeWage)/stdChangeWage) * ((ChangeFHoursVect - meanChangeFHours)/stdChangeFHours)/(No-1) ,ChangeFHoursVect > -10.d0)        
        !deallocate (ChangeWage, ChangeFHours)        

   end subroutine simulations1
      
    subroutine MedExpSimulations 
        !use interpolate1
        !use output
        !use mtmod
        implicit none
        
        Interface
           Subroutine mrgrnk(XVALT, IRNGT)
              use constants
              Real(dbl), Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine mrgrnk
           Subroutine I_mrgrnk(XVALT, IRNGT)
              use constants
              Integer, Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine I_mrgrnk                      
        End Interface 
                
        integer i, j, v, s, ic, n, iseed, status, count, Np, NumHouseholds(nr-1), NumHHAlive2(nr-1), NumIndAlive(nr), TotNum, het, zf, zm, fet, met, set, h
        integer TotNum6574, TotNum7584, TotNum85plus, TotNumNewMedicaid6574, TotNumNewMedicaid7584, TotNumNewMedicaid85plus
        integer NumPeopleNewMedicaid(nr), NumMarriedAlive(nr), NumMarriedNewMedicaid(nr)   
        integer NumHHAlive(nr), NumHHNewMedicaid(nr), NumWidowsAlive(nr), NumWidowsNewMedicaid(nr), NumWidowersAlive(nr), NumWidowersNewMedicaid(nr) 
        integer pm, tm, Totals(10)
        real(dbl) cumuinit(nef,nem), cumuf(nef), cumum(nem), cumeducdist(nhet), cummeddist(npm,nhet), cumhealf(nsht,nset), cumhealm(nsht,nset), cummed(npm), cumsinitH(3,naem)
        real(dbl) preTaxIncome, postTaxWealth, AverageOOPExpenses, htemp
        real(dbl),allocatable,dimension(:,:,:):: TotMedExpMat, OOPExpMat, GovTransferMat, earnsfMat, earnsmMat, aveearnsfMat, aveearnsmMat, wealthMat, labfMat, SocSecMat
        real(dbl),allocatable,dimension(:):: medVect, wealthVect
        integer, allocatable, dimension(:,:,:):: prodflocMat, prodmlocMat, medlocMat, alivefmat, alivemmat, aliveHmat, healHmat, healfMat, healmMat, maritalMat, futureStatus
        integer, allocatable, dimension(:):: InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus  
        integer, allocatable, dimension(:,:):: HHeducloc, aveearnsmloc, PEQuintile     
        iseed = 3464653
        call sgrnd(iseed)
                
        print *, "point 6a"  
        
        Np = Ns/(nr-1) !Ns should be divisible by nr-1=17
        
        !Create vector with number of people in each cohort        
        !NumHouseholds = nint( (/ CohortWeights(nw+1+ic)/sum(CohortWeights(nw+1:T-2)),ic=0,nr-3)/)*Np )    
        NumHouseholds = nint( (/(1.0d0/(1+ng)**ic,ic=0,nr-2)/)*Np )         
        
        !person, age, cohort        
        allocate (prodflocMat(Np,nw,nr-1), prodmlocMat(Np,nw,nr-1), aveearnsfMat(Np,nw+1,nr-1), aveearnsmMat(Np,nw+1,nr-1), wealthMat(Np,T,nr-1), stat=status)                                              
        allocate (labfMat(Np,nw,nr-1),earnsfMat(Np,nw,nr-1),earnsmMat(Np,nw,nr-1),HHeducloc(Np,nr-1),aveearnsmloc(Np,nr-1), stat = status)
        allocate (alivefMat(Np,nr,nr-1),alivemMat(Np,nr,nr-1),aliveHmat(Np,nr,nr-1), healHmat(Np,nr,nr-1), healfMat(Np,nr,nr-1), healmMat(Np,nr,nr-1), futureStatus(Np,nr,nr-1), stat=status)
        
        !Workers
        !!!!!!!!!!!!!!!!!!!!!!!

        print *, "point 6b"  
        
        !Create cumulative initial distribution of earnings shocks        
        do zf = 1,nef              
        do zm = 1,nem
            if (zf==1 .and. zm==1) then
                cumuinit(1,1) = uinit(1,1)
            else if (zm==1) then
                cumuinit(zf,zm) =  cumuinit(zf-1,nem) + uinit(zf,zm)
            else
                cumuinit(zf,zm) =  cumuinit(zf,zm-1) + uinit(zf,zm)
            end if
        end do
        end do
        
        !Create cumulative distribution for education distribution
        cumeducdist(1) = EducDist(1)    
        do s=2,nhet
            cumeducdist(s) = cumeducdist(s-1) + EducDist(s)                             
        end do                  
       
        !Each cohorts initial draws are set so that that cohort's initial productivity distribution is correct
        do ic=1,nr-1       
            !First to HH households                 
            do zf = 1,nef              
            do zm = 1,nem
                if (zf==1 .and. zm==1) then
                    do i=1,nint(cumuinit(1,1)*EducDist(1)*NumHouseholds(ic)) 
                        prodflocMat(i,1,ic) = 1
                        prodmlocMat(i,1,ic) = 1
                    end do             
                else if (zm==1) then
                    do i=nint(cumuinit(zf-1,nem)*EducDist(1)*NumHouseholds(ic)+1),nint(cumuinit(zf,1)*EducDist(1)*NumHouseholds(ic))
                        prodflocMat(i,1,ic) = zf
                        prodmlocMat(i,1,ic) = zm
                    end do            
                else
                    !print *, "nint(cumuinit(zf,zm-1)*EducDist(1)*NumHouseholds(ic)+1) =" , nint(cumuinit(zf,zm-1)*EducDist(1)*NumHouseholds(ic)+1)
                    !print *, "nint(cumuinit(zf,zm)*EducDist(1)*NumHouseholds(ic)) = ", nint(cumuinit(zf,zm)*EducDist(1)*NumHouseholds(ic))
                    do i=nint(cumuinit(zf,zm-1)*EducDist(1)*NumHouseholds(ic)+1),nint(cumuinit(zf,zm)*EducDist(1)*NumHouseholds(ic))
                        prodflocMat(i,1,ic) = zf
                        prodmlocMat(i,1,ic) = zm
                    end do                    
                end if
            end do 
            end do  
            
            !Then to the rest of the households    
            do s=2,nhet
                do zf = 1,nef              
                do zm = 1,nem        
                    if (zf==1 .and. zm==1) then
                        do i=nint(cumeducdist(s-1)*NumHouseholds(ic))+1 ,nint(cumeducdist(s-1)*NumHouseholds(ic)+cumuinit(1,1)*EducDist(s)*NumHouseholds(ic)) 
                            prodflocMat(i,1,ic) = 1
                            prodmlocMat(i,1,ic) = 1
                        end do             
                    else if (zm==1) then
                        do i=nint(cumeducdist(s-1)*NumHouseholds(ic)+cumuinit(zf-1,nem)*EducDist(s)*NumHouseholds(ic))+1,nint(cumeducdist(s-1)*NumHouseholds(ic)+cumuinit(zf,1)*EducDist(s)*NumHouseholds(ic))
                            prodflocMat(i,1,ic) = zf
                            prodmlocMat(i,1,ic) = zm
                        end do            
                    else                                
                        do i=nint(cumeducdist(s-1)*NumHouseholds(ic)+cumuinit(zf,zm-1)*EducDist(s)*NumHouseholds(ic))+1,nint(cumeducdist(s-1)*NumHouseholds(ic)+cumuinit(zf,zm)*EducDist(s)*NumHouseholds(ic))
                            prodflocMat(i,1,ic) = zf
                            prodmlocMat(i,1,ic) = zm
                        end do
                    end if       
                end do    
                end do
            end do                           
        end do
        
                
        !Assign female earnings shocks for ages 2 to nw    
        do ic=1,nr-1
        do i =1,NumHouseholds(ic)  
            do j=2,nw
                cumuf(1) = Puefmat(1,prodflocMat(i,j-1,ic))
                do v=2,nef
                    cumuf(v) = cumuf(v-1)+Puefmat(v,prodflocMat(i,j-1,ic))
                end do           
                call Locate(cumuf, grnd(), prodflocMat(i,j,ic))
                prodflocMat(i,j,ic) = prodflocMat(i,j,ic)+1                          
            end do                      
        end do   
        end do        
        
        !Assign male earnings shocks for ages 2 to nw    
        do ic=1,nr-1
        do i =1,NumHouseholds(ic)  
            do j=2,nw
                cumum(1) = Puemmat(1,prodmlocMat(i,j-1,ic))
                do v=2,nem
                    cumum(v) = cumum(v-1)+Puemmat(v,prodmlocMat(i,j-1,ic))
                end do           
                call Locate(cumum, grnd(), prodmlocMat(i,j,ic))
                prodmlocMat(i,j,ic) = prodmlocMat(i,j,ic)+1                          
            end do                      
        end do   
        end do        

        !Assign HH education location
        do ic=1,nr-1
        do i =1,NumHouseholds(ic) 
            if (i<nint(NumHouseholds(ic)*cumeducdist(1))) then !HH
                HHeducloc(i,ic)=1               
            else if (i<nint(NumHouseholds(ic)*cumeducdist(2))) then !HC 
                HHeducloc(i,ic)=2                
            else if (i<nint(NumHouseholds(ic)*cumeducdist(3))) then !CH
                HHeducloc(i,ic)=3                
            else !CC
                HHeducloc(i,ic)=4                              
            end if   
        end do   
        end do

        print *, "point 6c" 
        
        !average earnings and wealth of each individual in each cohort by age               
        do ic=1,nr-1 !cohort        
        do i=1,NumHouseholds(ic)   !individual   
            het = HHeducloc(i,ic)  
            fet = int((het+1)/2)
            met = mod(het+1,2)+1
            labfMat(i,1,ic) = labWCube(1,prodflocMat(i,1,ic),prodmlocMat(i,1,ic),1,1,het,1)
            earnsfMat(i,1,ic) = w*efmat(prodflocMat(i,1,ic), prodmlocMat(i,1,ic),fet,1)*labfMat(i,1,ic)
            earnsmMat(i,1,ic) = w*emmat(prodflocMat(i,1,ic), prodmlocMat(i,1,ic),met,1)*hbar   
            aveearnsfMat(i,1,ic) = 0.0d0
            aveearnsmMat(i,1,ic) = 0.0d0            
            wealthMat(i,1,ic) = 0.0d0                                                                                                    
        do j=2,nw    !age  
            !age-j wealth
            call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(i,j-1,ic),prodmlocMat(i,j-1,ic),:,:,het,j-1), &
                    wealthMat(i,j-1,ic), aveearnsfMat(i,j-1,ic), aveearnsmMat(i,j-1,ic), wealthMat(i,j,ic) )                  
            
            !age-j average earnings (average of earnings up to age j-1 earnings)                                      
            aveearnsfMat(i,j,ic) = ((j-2)*aveearnsfMat(i,j-1,ic) + earnsfMat(i,j-1,ic))/(j-1)    
            aveearnsmMat(i,j,ic) = ((j-2)*aveearnsmMat(i,j-1,ic) + earnsmMat(i,j-1,ic))/(j-1)       
            
            !age-j female labor supply
            call interpol3d(avect, aveEarnFvect, aveEarnMvect,labWCube(:,prodflocMat(i,j,ic),prodmlocMat(i,j,ic),:,:,het,j), &
                    wealthMat(i,j,ic),aveearnsfMat(i,j,ic), aveearnsmMat(i,j,ic),labfMat(i,j,ic))                   
            
            !earnings                                                             
            earnsfMat(i,j,ic) = w*efmat(prodflocMat(i,j,ic), prodmlocMat(i,j,ic),fet,j)*labfMat(i,j,ic)
            earnsmMat(i,j,ic) = w*emmat(prodflocMat(i,j,ic), prodmlocMat(i,j,ic),met,j)*hbar                                                                                                              
        end do  
            aveearnsfMat(i,nw+1,ic) = ((nw-1)*aveearnsfMat(i,nw,ic) + earnsfMat(i,nw,ic))/nw 
            aveearnsmMat(i,nw+1,ic) = ((nw-1)*aveearnsmMat(i,nw,ic) + earnsmMat(i,nw,ic))/nw 
                
            call Locate(aveEarnMvect, aveearnsmMat(i,nw,ic), aveearnsmloc(i,ic))
            if (aveearnsmloc(i,ic) == 0) aveearnsmloc(i,ic) = 1
            !should we assign to lower or upper point based on probability?                                   
        end do                                            
        end do                    
        
        deallocate(labfMat,earnsfMat,earnsmMat)       
        allocate (medlocMat(Np,nr,nr-1), stat=status)    
        !print *, "status =", status        
        allocate (SocSecMat(Np,nr,nr-1), maritalMat(Np,nr,nr-1), TotMedExpMat(Np,nr,nr-1), OOPExpMat(Np,nr,nr-1), GovTransferMat(Np,nr,nr-1), stat=status)
        !print *, "status =", status 
        print *, "point 6d" 
                               
        !!!!!!!!!!!!!!!!!!!!!!!        
        !Retirees
        
        !Health Status 
        !Create cumulative distribution of female health status for each education type
        do set = 1,nset
            cumhealf(1,set) = hinitF(1,set)    
            do h=2,nsht
                cumhealf(h,set) = cumhealf(h-1,set) + hinitF(h,set)                             
            end do       
        end do    
        
        !Create cumulative distribution of male health status for each education type
        do set = 1,nset
            cumhealm(1,set) = hinitM(1,set)    
            do h=2,nsht
                cumhealm(h,set) = cumhealm(h-1,set) + hinitM(h,set)                             
            end do       
        end do              
        
        !Assign initial health status to each individual             
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic)             
            het = HHeducloc(i,ic)    
            fet = int((het+1)/2)
            met = mod(het+1,2)+1                          
            if (grnd() < cumhealf(1,fet)) then
                healfMat(i,1,ic) = 1        
            else
                healfMat(i,1,ic) = 2
            end if
            if (grnd() < cumhealm(1,met)) then
                healmMat(i,1,ic) = 1        
            else
                healmMat(i,1,ic) = 2
            end if                    
        end do
        end do

        !Create cumulative distribution of marital statuses
        cumsinitH(1,:) = sinitH(1,:)
        do i = 2,3                           
            cumsinitH(i,:) = cumsinitH(i-1,:) + sinitH(i,:)                                   
        end do          
        
        !Assign alive flag to each individual of age nw+1
        alivefMat = 0
        alivemMat = 0
         
        !Assign marital flag to each household of age nw+1
        !maritalMat = 0
        maritalMat = 1
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic)                                                 
            call Locate(cumsinitH(:,aveearnsmloc(i,ic)), grnd(), maritalMat(i,1,ic))
            maritalMat(i,1,ic) = maritalMat(i,1,ic)+1        
            maritalMat(i,2:,ic) = maritalMat(i,1,ic)
            if (maritalMat(i,1,ic) == 1) then
                alivefMat(i,1,ic) = 1
                alivemMat(i,1,ic) = 1   
            else if (maritalMat(i,1,ic) == 2) then
                alivefMat(i,1,ic) = 1
                if (grnd() < probHusband0) then
                    aveearnsmMat(i,nw+1,ic) = 0.0d0       
                end if
            else
                alivemMat(i,1,ic) = 1 
            end if                                        
        end do
        end do  
        deallocate(aveearnsmloc)      
        
        !Assign health status, alive status, marital status to each individual at ages nw+2 to T                     
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic) 
        do j=2,nr    
            
            !survival           
            if (maritalMat(i,j-1,ic) == 1 .and. alivefMat(i,j-1,ic)==1 .and. alivemMat(i,j-1,ic)==1) then  
                if (grnd() < survivalprobVectF(healfMat(i,j-1,ic),1,j-1)) alivefMat(i,j,ic) = 1     
                if (grnd() < survivalprobVectM(healmMat(i,j-1,ic),1,j-1)) alivemMat(i,j,ic) = 1     
            else if (maritalMat(i,j-1,ic) == 2  .and. alivefMat(i,j-1,ic)==1) then  !widow
                if (grnd() < survivalprobVectF(healfMat(i,j-1,ic),2,j-1)) alivefMat(i,j,ic) = 1
            else if (maritalMat(i,j-1,ic) == 3  .and. alivemMat(i,j-1,ic)==1) then  !widower
                if (grnd() < survivalprobVectM(healmMat(i,j-1,ic),3,j-1)) alivemMat(i,j,ic) = 1            
            end if 
            
            !marital status
            if (alivefMat(i,j,ic) == 1 .and. alivemMat(i,j,ic) == 1) then
                maritalMat(i,j:,ic) = 1 
                futureStatus(i,j-1,ic) = 1
            else if (alivefMat(i,j,ic) == 1) then   
                maritalMat(i,j:,ic) = 2
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 2
                else
                    futureStatus(i,j-1,ic) = 1
                end if
            else if (alivemMat(i,j,ic) == 1) then   
                maritalMat(i,j:,ic) = 3
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 3
                else
                    futureStatus(i,j-1,ic) = 1
                end if
            else !both0
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 4
                else 
                    futureStatus(i,j-1,ic) = 2
                end if                    
            end if
                      
            
            !health status 
            htemp = grnd()
            !print *, "htemp=", htemp
            !print *, "PhmatF(1,healfMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)=", PhmatF(1,healfMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)
            if (htemp < PhmatF(1,healfMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)) then
                healfMat(i,j,ic) = 1        
            else
                healfMat(i,j,ic) = 2
            end if
            !print *, "healfMat(i,j,ic)=", healfMat(i,j,ic)                                   
            !pause
            if (grnd() <PhmatM(1,healmMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)) then
                healmMat(i,j,ic) = 1        
            else
                healmMat(i,j,ic) = 2
            end if  
        end do        
        end do 
        end do             
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic)
            if (maritalMat(i,nr,ic) == 1) then
                futureStatus(i,nr,ic) = 4
            else 
                futureStatus(i,nr,ic) = 2
            end if
        end do  
        end do         
        
        !Household health status
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic)
        do j=1,nr
            if (maritalMat(i,j,ic) == 1) then      
                if (healfMat(i,j,ic) == 1 .and. healmMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 1 !BB
                else if (healfMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 2 !BG
                else if (healmMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 3 !GB
                else !GG
                    healHmat(i,j,ic) = 4
                end if
            else if (maritalMat(i,j,ic) == 2) then                  
                healHmat(i,j,ic) =healfMat(i,j,ic)  
            else
                healHmat(i,j,ic) =healmMat(i,j,ic)                                            
            end if                
        end do
        end do
        end do        
        
        !Medical shocks
        !Create cumulative distribution of medical shock for each education type
        do het = 1,nhet
            cummeddist(1,het) = minit(1,het)    
            do s=2,npm
                cummeddist(s,het) = cummeddist(s-1,het) + minit(s,het)                             
            end do       
        end do       
        
        !Assign medical shocks for age nw+1    
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic)   
            het = HHeducloc(i,ic)                                          
            call Locate(cummeddist(:,het), grnd(), medlocMat(i,1,ic))
            medlocMat(i,1,ic) = medlocMat(i,1,ic)+1   
            if (grnd() > Ptmvect(1)) then
                medlocMat(i,1,ic) = npm + medlocMat(i,1,ic)
            end if                                                    
        end do 
        end do
        
        !Assign medical shocks for ages nw+2 to T
        !Need to condition this on martial status if want to have Pmmat differ by marital status
        do ic=1,nr-1
        do i= 1,NumHouseholds(ic) 
            do j=2,nr
                pm = medlocMat(i,j-1,ic)-npm*((medlocMat(i,j-1,ic)-1)/npm)
                tm = (medlocMat(i,j-1,ic)-1)/npm + 1                
                cummed(1) = Pummat(1,pm,j-1)
                do v=2,npm
                    cummed(v) = cummed(v-1)+Pummat(v,pm,j-1)
                end do           
                call Locate(cummed, grnd(), medlocMat(i,j,ic))
                medlocMat(i,j,ic) = medlocMat(i,j,ic)+1   
                if (grnd() > Ptmvect(1)) then
                    medlocMat(i,j,ic) = npm + medlocMat(i,j,ic)
                end if                                        
            end do                      
        end do  
        end do
       
        
        aliveHmat = 0
        do ic=1,nr-1        
        do i=1,NumHouseholds(ic)   !individual                                           
            do j=1,nr
                !age-nw+j wealth
                if (j == 1) then                
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(i,nw,ic),prodmlocMat(i,nw,ic),:,:,HHeducloc(i,ic),nw), &
                        wealthMat(i,nw,ic), aveearnsfMat(i,nw,ic), aveearnsmMat(i,nw,ic), wealthMat(i,nw+1,ic) )                     
                else
                    if (maritalMat(i,j-1,ic) == 1) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeMarried(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                     
                    else if (maritalMat(i,j-1,ic) == 2) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidow(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                                            
                    else
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidower(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                                            
                    end if                                                                                                      
                end if
                 
                TotMedExpMat(i,j,ic) = mmat(medlocMat(i,j,ic),healHmat(i,j,ic),futureStatus(i,j,ic),maritalMat(i,j,ic),j)
                SocSecMat(i,j,ic) = socsec(aveearnsfMat(i,nw+1,ic),aveearnsmMat(i,nw+1,ic),maritalMat(i,j,ic))
                preTaxIncome = SocSecMat(i,j,ic) + r*wealthMat(i,nw+j,ic)
                postTaxWealth = preTaxIncome + wealthMat(i,nw+j,ic) - IncomeTax(r*wealthMat(i,nw+j,ic)- max(0.0d0,r*wealthMat(i,nw+j,ic)*capTax),SocSecMat(i,j,ic),TotMedExpMat(i,j,ic),maritalMat(i,j,ic)) - max(0.0d0,r*wealthMat(i,nw+j,ic)*capTax)
                             
                GovTransferMat(i,j,ic) = transfersRetired(postTaxWealth, j,medlocMat(i,j,ic),healHmat(i,j,ic),futureStatus(i,j,ic),maritalMat(i,j,ic),1)
                !incomeMat(nw+j,i) = preTaxIncome  - IncomeTax(r*wealthMat(nw+j,i) - r*wealthMat(i,nw+j,ic)*capTax,SocSecMat(j,i),TotMedExpMat(j,i),maritalMat(j,i)) + max(GovTransferMat(nw+j,i) - medMat(j,i),0.0d0)  - r*wealthMat(i,nw+j,ic)*capTax
                
                if(GovTransferMat(i,j,ic) == 0.0d0) then
                    OOPExpMat(i,j,ic) = TotMedExpMat(i,j,ic)
                else
                    !if (medlocMat(i,j,ic) == 5 .or. medlocMat(i,j,ic) == 10) then
                    !    OOPExpMat(i,j,ic) = 0.0d0
                    !else
                     if (maritalMat(i,j,ic)==1) then
                        OOPExpMat(i,j,ic) = min((1-FracMedicaidCovers)*TotMedExpMat(i,j,ic),clowerbarMarried/2)
                     else if (maritalMat(i,j,ic) == 2) then
                        OOPExpMat(i,j,ic) = min((1-FracMedicaidCovers)*TotMedExpMat(i,j,ic),clowerbarWidow/2)                     
                     else
                        OOPExpMat(i,j,ic) = min((1-FracMedicaidCovers)*TotMedExpMat(i,j,ic),clowerbarWidower/2)                     
                     end if                                  
                    !end if    
                end if                 
                if (alivefmat(i,j,ic) == 1 .or. alivemmat(i,j,ic)==1) aliveHmat(i,j,ic) = 1                                           
            end do
        end do        
        end do

        deallocate (prodflocMat,prodmlocMat,aveearnsfMat,futureStatus,  stat=status)
       
        !average OOP expenses by marital status
        do ic=1,nr-1    
            NumHHAlive2(ic) = sum(aliveHmat(1:NumHouseholds(ic),ic,ic))                   
        end do
        TotNum = sum(NumHHAlive2) + sum(aliveHmat(1:NumHouseholds(nr-1),nr,nr-1))
                
        AverageOOPExpenses = 0.0d0
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic,ic) == 1) then
                AverageOOPExpenses = AverageOOPExpenses + OOPExpMat(i,ic,ic)                
            end if            
        end do
        end do  
        do i=1,NumHouseholds(nr-1)
            if (aliveHmat(i,nr,nr-1) == 1) then
                AverageOOPExpenses = AverageOOPExpenses + OOPExpMat(i,nr,nr-1)        
            end if            
        end do                   
        AverageOOPExpenses = AverageOOPExpenses/TotNum
        !call Out(0,"AverageOOPExpenses",AverageOOPExpenses,ret)
        !pause
        
        do ic=1,nr-1    
            NumHHAlive2(ic) = sum(alivefmat(1:NumHouseholds(ic),ic,ic)*alivemmat(1:NumHouseholds(ic),ic,ic))                   
        end do
        TotNum = sum(NumHHAlive2) + sum(alivefmat(1:NumHouseholds(nr-1),nr,nr-1)*alivemmat(1:NumHouseholds(nr-1),nr,nr-1))
        
        allocate (medVect(TotNum), wealthVect(TotNum), AgentNum(TotNum), stat=status)    
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic,ic)*alivemmat(i,ic,ic) == 1) then
                medVect(j) = OOPExpMat(i,ic,ic)
                wealthVect(j) = SocSecMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do   
        do i=1,NumHouseholds(nr-1)
            if (alivefmat(i,nr,nr-1)*alivemmat(i,nr,nr-1) == 1) then
                medVect(j) = OOPExpMat(i,nr,nr-1)
                wealthVect(j) = SocSecMat(i,nr,nr-1)
                j = j + 1
            end if            
        end do             
                        
        do i=1,TotNum
            AgentNum(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        medVect = medVect(AgentNum)        
        
        do i=1,5
            AggregateOOPSSQ(i,1) = sum(medVect((i-1)*TotNum/5+1:i*TotNum/5))/(TotNum/5)
        end do  
        deallocate (medVect, wealthVect, AgentNum)  
        
        do ic=1,nr-1    
            NumHHAlive2(ic) = sum(alivefmat(1:NumHouseholds(ic),ic,ic)*(1-alivemmat(1:NumHouseholds(ic),ic,ic)))                   
        end do
        TotNum = sum(NumHHAlive2) + sum(alivefmat(1:NumHouseholds(nr-1),nr,nr-1)*(1-alivemmat(1:NumHouseholds(nr-1),nr,nr-1)))
        
        allocate (medVect(TotNum), wealthVect(TotNum), AgentNum(TotNum), stat=status)    
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic,ic)*(1-alivemmat(i,ic,ic)) == 1) then
                medVect(j) = OOPExpMat(i,ic,ic)
                wealthVect(j) = SocSecMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do   
        do i=1,NumHouseholds(nr-1)
            if (alivefmat(i,nr,nr-1)*(1-alivemmat(i,nr,nr-1)) == 1) then
                medVect(j) = OOPExpMat(i,nr,nr-1)
                wealthVect(j) = SocSecMat(i,nr,nr-1)
                j = j + 1
            end if            
        end do             
                        
        do i=1,TotNum
            AgentNum(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        medVect = medVect(AgentNum)        
        
        do i=1,5
            AggregateOOPSSQ(i,2) = sum(medVect((i-1)*TotNum/5+1:i*TotNum/5))/(TotNum/5)
        end do  
        deallocate (medVect, wealthVect, AgentNum)     
        
        do ic=1,nr-1    
            NumHHAlive2(ic) = sum(alivemmat(1:NumHouseholds(ic),ic,ic)*(1-alivefmat(1:NumHouseholds(ic),ic,ic)))                   
        end do
        TotNum = sum(NumHHAlive2) + sum(alivemmat(1:NumHouseholds(nr-1),nr,nr-1)*(1-alivefmat(1:NumHouseholds(nr-1),nr,nr-1)))
        
        allocate (medVect(TotNum), wealthVect(TotNum), AgentNum(TotNum), stat=status)    
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic,ic)*(1-alivefmat(i,ic,ic)) == 1) then
                medVect(j) = OOPExpMat(i,ic,ic)
                wealthVect(j) = SocSecMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do   
        do i=1,NumHouseholds(nr-1)
            if (alivemmat(i,nr,nr-1)*(1-alivefmat(i,nr,nr-1)) == 1) then
                medVect(j) = OOPExpMat(i,nr,nr-1)
                wealthVect(j) = SocSecMat(i,nr,nr-1)
                j = j + 1
            end if            
        end do             
                        
        do i=1,TotNum
            AgentNum(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        medVect = medVect(AgentNum)        
        
        do i=1,5
            AggregateOOPSSQ(i,3) = sum(medVect((i-1)*TotNum/5+1:i*TotNum/5))/(TotNum/5)
        end do  
        deallocate (medVect, wealthVect, AgentNum)              
        AggregateOOPSSQ = AggregateOOPSSQ/AverageOOPExpenses
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Mobility Matrices
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
        print *, "point 6e" 
              
        do ic=1,nr-1    
            NumHHAlive2(ic) = sum(aliveHmat(1:NumHouseholds(ic),ic+1,ic))                   
        end do
        TotNum = sum(NumHHAlive2)
        !TotNum = sum(NumHouseholds)                 
        
        !medical expense mobility matrices
        allocate (medVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = TotMedExpMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do        
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                            
        medVect = medVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = TotMedExpMat(i,ic+1,ic)
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                  
        medVect = medVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            MedMobMat(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        MedMobMat = dble(MedMobMat)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (medVect, InitQuintile, FinalQuintile, AgentNum, ip)  
        
        allocate (medVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = OOPExpMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                            
        medVect = medVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = OOPExpMat(i,ic+1,ic)
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                  
        medVect = medVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            OOPMobMat(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        OOPMobMat = dble(OOPMobMat)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)
        
        deallocate (medVect, InitQuintile, FinalQuintile, AgentNum, ip)  
        print *, "point 6f"
        
        TotNum = sum(NumHHAlive2(11:))
        !TotNum = sum(NumHouseholds)                 
        
     
        allocate (medVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = TotMedExpMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do        
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                            
        medVect = medVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = TotMedExpMat(i,ic+1,ic)
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                  
        medVect = medVect(AgentNum)        
        
        !assign period 3 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            MedMobMat85plus(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        MedMobMat85plus = dble(MedMobMat85plus)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (medVect, InitQuintile, FinalQuintile, AgentNum, ip)  
        
        allocate (medVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = OOPExpMat(i,ic,ic)
                j = j + 1
            end if            
        end do
        end do
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                            
        medVect = medVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                medVect(j) = OOPExpMat(i,ic+1,ic)
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(medVect, AgentNum)                  
        medVect = medVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            OOPMobMat85plus(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        OOPMobMat85plus = dble(OOPMobMat85plus)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)
        
        deallocate (medVect, InitQuintile, FinalQuintile, AgentNum, ip)                          
        
        
        deallocate(TotMedExpMat, OOPExpMat)  
             
        
!open(12,file="simout.txt")                
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !2-period wealth mobility matrices
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        print *, "point 6g"
        
        !65 plus                
        TotNum = sum(NumHHAlive2)        
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)                
                j = j + 1
            end if            
        end do
        end do        
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)             
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr65plus(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        WealthMobMatHH2yr65plus = dble(WealthMobMatHH2yr65plus)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip)  
        
         
        !start conditonal PEQ
        allocate (PEQuintile(Np,nr-1), stat=status)    
        
        do ic=1,nr-1
            allocate( AgentNum(NumHouseholds(ic)), ip(NumHouseholds(ic)) )
            do i=1,NumHouseholds(ic)
                AgentNum(i) = i
                ip(i) = i
            end do        
            call mrgrnk(aveearnsmMat(:NumHouseholds(ic),nw+1,ic), AgentNum)
            do i=1,5
                PEQuintile((i-1)*NumHouseholds(ic)/5+1:i*NumHouseholds(ic)/5,ic) = i                          
            end do  
            call I_mrgrnk(AgentNum, ip)      
            PEQuintile(:NumHouseholds(ic),ic) = PEQuintile(ip,ic)  
            deallocate(AgentNum, ip)      
        end do
      
        
        do s=1,5
            
            
            TotNum = sum(NumHouseholds/5) 
            allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)   
            j = 1
            do ic=1,nr-1
            do i=1,NumHouseholds(ic)
                if (aliveHmat(i,ic+1,ic) == 1 .and. PEQuintile(i,ic)==s) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)                
                    j = j + 1
                end if            
            end do
            end do   
                
                            
            do i=1,TotNum
                AgentNum(i) = i
                ip(i) = i
            end do
            call mrgrnk(wealthVect(:j-1), AgentNum(:j-1))                            
            wealthVect = wealthVect(AgentNum)        
              
                         
            !assign period 1 quintile
            do i=1,5
                InitQuintile((i-1)*(j-1)/5+1:i*(j-1)/5) = i                          
            end do  
            call I_mrgrnk(AgentNum(:j-1), ip(:j-1))      
            InitQuintile = InitQuintile(ip)
            
            !extract expenses in period 2 for each individual       
            j = 1
            do ic=1,nr-1
            do i=1,NumHouseholds(ic)
                if (aliveHmat(i,ic+1,ic) == 1 .and. PEQuintile(i,ic)==s) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)             
                    j = j + 1
                end if            
            end do
            end do
    
                    
            do i=1,TotNum
                AgentNum(i) = i
                ip(i) = i
            end do
            call mrgrnk(wealthVect(:j-1), AgentNum(:j-1))                  
            wealthVect = wealthVect(AgentNum)        
            
            !assign period 2 quintile
            do i=1,5
                FinalQuintile((i-1)*(j-1)/5+1:i*(j-1)/5) = i                          
            end do  
            call I_mrgrnk(AgentNum(:j-1), ip(:j-1))    
            FinalQuintile = FinalQuintile(ip)                
      
            
            do i=1,5
            do n=1,5
                WealthMobMatHH2yr65plusCondPEQ(i,n,s) = count(FinalQuintile(:j-1)==n .and. InitQuintile(:j-1)==i)                 
            end do
            end do
            WealthMobMatHH2yr65plusCondPEQ(:,:,s) = 5.0d0*dble(WealthMobMatHH2yr65plusCondPEQ(:,:,s))/dble(j-1)
            !call Out(0,"MedMobMat",MedMobMat)        
     
                                         
            deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip)  
        end do  
        deallocate(PEQuintile)      
        !end conditional PEQ
        
                
        !wealth mobility matrices 65-74
        TotNum = sum(NumHHAlive2(:5))
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)    
        InitNHstatus = 0       
        FinalNHstatus = 0                   
        !extract expenses in period 1 for each household
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (maritalMat(i,ic,ic) == 1 .and. maritalMat(i,ic+1,ic) > 1) then
                    InitNHstatus(j) = 1
                end if
                j = j + 1
            end if            
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)                
                if (maritalMat(i,ic+1,ic) == 1 .and. maritalMat(i,ic+2,ic) > 1) then
                    InitNHstatus(j) = 1
                end if                   
                j = j + 1
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr6574(i,j) = count(FinalQuintile==j .and. InitQuintile==i)  
            WealthMobMatCondSpousalDeath6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondSpousalDeath6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondSpousalDeath6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondSpousalDeath6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                              
        end do
        end do
        WealthMobMatHH2yr6574 = dble(WealthMobMatHH2yr6574)/dble(TotNum/5)       
        Totals = sum(WealthMobMatCondSpousalDeath6574,2)
        do i = 1,10
            WealthMobMatCondSpousalDeath6574(i,:) = dble(WealthMobMatCondSpousalDeath6574(i,:))/dble(Totals(i))                   
        end do
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)  
                
        !wealth mobility matrices 75-84
        TotNum = sum(NumHHAlive2(6:10))
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0  
 !call Out(12,"TotNum", TotNum, ret)
         
        !extract expenses in period 1 for each household
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (maritalMat(i,ic,ic) == 1 .and. maritalMat(i,ic+1,ic) > 1) then
                    InitNHstatus(j) = 1
                end if                
                j = j + 1
            end if            
        end do
        end do        
                
!call Out(12,"wealth7584Vect", wealthVect, ret)                 
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (maritalMat(i,ic+1,ic) == 1 .and. maritalMat(i,ic+2,ic) > 1) then
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1
            end if            
        end do
        end do        

!call Out(12,"wealth7584Vect2", wealthVect, ret)              
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 3 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr7584(i,j) = count(FinalQuintile==j .and. InitQuintile==i)         
            WealthMobMatCondSpousalDeath7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondSpousalDeath7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondSpousalDeath7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondSpousalDeath7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                    
        end do
        end do
        WealthMobMatHH2yr7584 = dble(WealthMobMatHH2yr7584)/dble(TotNum/5)
        Totals = sum(WealthMobMatCondSpousalDeath7584,2)
        do i = 1,10
            WealthMobMatCondSpousalDeath7584(i,:) = dble(WealthMobMatCondSpousalDeath7584(i,:))/dble(Totals(i))                   
        end do      
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)       
          
        !wealth mobility matrices 85+
        TotNum = sum(NumHHAlive2(11:))
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)    
        InitNHstatus = 0       
        FinalNHstatus = 0    
        !print *, "status=", status
 
 !call Out(12,"TotNum", TotNum, ret)
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
               if (maritalMat(i,ic,ic) == 1 .and. maritalMat(i,ic+1,ic) > 1) then
                    InitNHstatus(j) = 1
                end if                
                j = j + 1
            end if            
        end do
        end do 

!call Out(12,"wealth85plusVect", wealthVect, ret)          
               
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (ic < nr-1) then                        
                    if (maritalMat(i,ic+1,ic) == 1 .and. maritalMat(i,ic+2,ic) > 1) then
                        InitNHstatus(j) = 1
                    end if
                end if                 
                j = j + 1
            end if            
        end do
        end do
                
!call Out(12,"wealth85plusVect2", wealthVect, ret)                    
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 3 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr85plus(i,j) = count(FinalQuintile==j .and. InitQuintile==i)
            WealthMobMatCondSpousalDeath85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondSpousalDeath85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondSpousalDeath85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondSpousalDeath85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                              
                             
        end do
        end do
        WealthMobMatHH2yr85plus = dble(WealthMobMatHH2yr85plus)/dble(TotNum/5)
        Totals = sum(WealthMobMatCondSpousalDeath85plus,2)
        do i = 1,10
            WealthMobMatCondSpousalDeath85plus(i,:) = dble(WealthMobMatCondSpousalDeath85plus(i,:))/dble(Totals(i))                   
        end do        
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)

!close(12)

        !wealth mobility matrices 85-94
        TotNum = sum(NumHHAlive2(11:15))
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=11,15
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                j = j + 1
            end if            
        end do
        end do        
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,15
        do i=1,NumHouseholds(ic)
            if (aliveHmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                j = j + 1
            end if            
        end do
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 3 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr8594(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        WealthMobMatHH2yr8594 = dble(WealthMobMatHH2yr8594)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip)              
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !WealthMobMatCondNH
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !wealth mobility matrices 65-74
        !TotNum = sum(NumHHAlive2(:5))
        !allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        
        do ic=1,nr-1               
            NumMarriedAlive(ic) = 2*sum(alivefmat(1:NumHouseholds(ic),ic+1,ic)*alivemmat(1:NumHouseholds(ic),ic+1,ic))      
            NumWidowsAlive(ic) = sum(alivefmat(1:NumHouseholds(ic),ic+1,ic)*(1-alivemmat(1:NumHouseholds(ic),ic+1,ic)))      
            NumWidowersAlive(ic) = sum((1-alivefmat(1:NumHouseholds(ic),ic+1,ic))*alivemmat(1:NumHouseholds(ic),ic+1,ic))                  
        end do
        NumIndAlive = NumMarriedAlive + NumWidowsAlive + NumWidowersAlive        
                
        TotNum = sum(NumIndAlive(:5))   
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                            
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                 
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                j = j + 1               
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then     
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                 
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                j = j + 1 
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                                  
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatInd2yr6574(i,j) = count(FinalQuintile==j .and. InitQuintile==i) 
            WealthMobMatCondNH6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondNH6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondNH6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondNH6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                
        end do
        end do
        WealthMobMatInd2yr6574 = dble(WealthMobMatInd2yr6574)/dble(TotNum/5)       
        Totals = sum(WealthMobMatCondNH6574,2)
        do i = 1,10
            WealthMobMatCondNH6574(i,:) = dble(WealthMobMatCondNH6574(i,:))/dble(Totals(i))                   
        end do
        
        !Do hospital with the same variables
        InitNHstatus = 0       
        FinalNHstatus = 0                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                 
                j = j + 2                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if             
        end do
        end do     
                              
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                     
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                 
                j = j + 2
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then               
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                                  
            end if            
        end do
        end do                
                
        do i=1,5
        do j=1,5
            WealthMobMatCondHosp6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondHosp6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondHosp6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondHosp6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                
        end do
        end do
        Totals = sum(WealthMobMatCondHosp6574,2)
        do i = 1,10
            WealthMobMatCondHosp6574(i,:) = dble(WealthMobMatCondHosp6574(i,:))/dble(Totals(i))                   
        end do
        
        !Do health status with the same variables
        InitNHstatus = 0       
        FinalNHstatus = 0                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if             
        end do
        end do     
                              
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                     
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                                       
            end if            
        end do
        end do                
                
        do i=1,5
        do j=1,5
            WealthMobMatCondBadHeal6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondBadHeal6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondBadHeal6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondBadHeal6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                
        end do
        end do
        Totals = sum(WealthMobMatCondBadHeal6574,2)
        do i = 1,10
            WealthMobMatCondBadHeal6574(i,:) = dble(WealthMobMatCondBadHeal6574(i,:))/dble(Totals(i))                   
        end do 
        
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)      
        
        print *, "here 1"
        
        !Redo with women's marital status
        TotNum = sum(NumWidowsAlive(1:5)) + sum(NumMarriedAlive(1:5))/2   
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then
                
                if (maritalMat(i,ic+1,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidow6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidow6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidow6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidow6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidow6574,2)
        do i = 1,10
            WealthMobMatCondWidow6574(i,:) = dble(WealthMobMatCondWidow6574(i,:))/dble(Totals(i))                   
        end do                 
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)  

        !Redo with men's marital status
        TotNum = sum(NumWidowersAlive(1:5)) + sum(NumMarriedAlive(1:5))/2   
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else 
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,5
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic+1,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidower6574(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidower6574(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidower6574(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidower6574(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidower6574,2)
        do i = 1,10
            WealthMobMatCondWidower6574(i,:) = dble(WealthMobMatCondWidower6574(i,:))/dble(Totals(i))                   
        end do                 
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)  
        
        print *, "here 2"
                
        !wealth mobility matrices 75-84
        TotNum = sum(NumIndAlive(6:10))   
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                            
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                  
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                j = j + 1
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then     
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                    
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                j = j + 1 
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                    
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                    
                j = j + 1                                  
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatInd2yr7584(i,j) = count(FinalQuintile==j .and. InitQuintile==i)   
            WealthMobMatCondNH7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondNH7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondNH7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondNH7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                
        end do
        end do
        WealthMobMatInd2yr7584 = dble(WealthMobMatInd2yr7584)/dble(TotNum/5)      
                Totals = sum(WealthMobMatCondNH7584,2)
        do i = 1,10
            WealthMobMatCondNH7584(i,:) = dble(WealthMobMatCondNH7584(i,:))/dble(Totals(i))                   
        end do 
        
        !Do hospital with same variables
        InitNHstatus = 0       
        FinalNHstatus = 0    
                                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                  
                j = j + 2 
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                
            end if             
        end do
        end do     
                        

        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                     
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                    
                j = j + 2
            else if (alivefmat(i,ic+1,ic) == 1) then               
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                    
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                    
                j = j + 1                                  
            end if            
        end do
        end do                                                
        
        do i=1,5
        do j=1,5
            WealthMobMatCondHosp7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondHosp7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondHosp7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondHosp7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                
        end do
        end do        
        Totals = sum(WealthMobMatCondHosp7584,2)
        do i = 1,10
            WealthMobMatCondHosp7584(i,:) = dble(WealthMobMatCondHosp7584(i,:))/dble(Totals(i))                   
        end do            
        
        !Do health status with same variables
        InitNHstatus = 0       
        FinalNHstatus = 0    
                                
        !extract expenses in period 1 for each individual
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if         
        end do
        end do     
                        

        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if                     
        end do
        end do                                                
        
        do i=1,5
        do j=1,5
            WealthMobMatCondBadHeal7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondBadHeal7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondBadHeal7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondBadHeal7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                
        end do
        end do        
        Totals = sum(WealthMobMatCondBadHeal7584,2)
        do i = 1,10
            WealthMobMatCondBadHeal7584(i,:) = dble(WealthMobMatCondBadHeal7584(i,:))/dble(Totals(i))                   
        end do    

        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)      
        
        print *, "here 3"
        
        !Redo with women's marital status
        TotNum = sum(NumWidowsAlive(6:10))  + sum(NumMarriedAlive(6:10))/2  
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic+1,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidow7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidow7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidow7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidow7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidow7584,2)
        do i = 1,10
            WealthMobMatCondWidow7584(i,:) = dble(WealthMobMatCondWidow7584(i,:))/dble(Totals(i))                   
        end do         
                                                       
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)                
        
        
        !Redo with men's marital status
        TotNum = sum(NumWidowersAlive(6:10))  + sum(NumMarriedAlive(6:10))/2  
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=6,10
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic+1,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidower7584(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidower7584(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidower7584(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidower7584(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidower7584,2)
        do i = 1,10
            WealthMobMatCondWidower7584(i,:) = dble(WealthMobMatCondWidower7584(i,:))/dble(Totals(i))                   
        end do         
                                                       
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)        
        
        print *, "here 4"  
        
        !wealth mobility matrices 85+
        TotNum = sum(NumIndAlive(11:))   
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                            
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                 
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                j = j + 1
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                   
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,ic)
                if (medlocMat(i,ic,ic) == 5 .or. medlocMat(i,ic,ic) == 10) then
                    InitNHstatus(j) = 1
                end if                    
                j = j + 1                
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then     
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                 
                j = j + 1
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2
                j = j + 1 
            else if (alivefmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                if (medlocMat(i,ic+1,ic) == 5 .or. medlocMat(i,ic+1,ic) == 10) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                                  
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatInd2yr85plus(i,j) = count(FinalQuintile==j .and. InitQuintile==i)    
            WealthMobMatCondNH85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondNH85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondNH85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondNH85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do
        WealthMobMatInd2yr85plus = dble(WealthMobMatInd2yr85plus)/dble(TotNum/5)   
        Totals = sum(WealthMobMatCondNH85plus,2)
        do i = 1,10
            WealthMobMatCondNH85plus(i,:) = dble(WealthMobMatCondNH85plus(i,:))/dble(Totals(i))                   
        end do    

        !Redo with hospital
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    if (grnd() < 0.5d0) then
                        InitNHstatus(j) = 1
                    else
                        InitNHstatus(j+1) = 1
                    end if
                end if                 
                j = j + 2
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                   
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic,ic) == 4 .or. medlocMat(i,ic,ic) == 9) then
                    InitNHstatus(j) = 1
                end if                    
                j = j + 1                
            end if             
        end do
        end do     
                        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                     
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    !if (InitNHstatus(j) == 1) then
                        FinalNHstatus(j) = 1
                    !else if (InitNHstatus(j+1) == 1) then
                        FinalNHstatus(j+1) = 1
                    !else if (grnd() < 0.5d0) then
                    !    FinalNHstatus(j) = 1
                    !else
                    !    FinalNHstatus(j+1) = 1
                    !end if
                end if                 
                j = j + 2
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (medlocMat(i,ic+1,ic) == 4 .or. medlocMat(i,ic+1,ic) == 9) then
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                                  
            end if            
        end do
        end do                                              
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondHosp85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondHosp85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondHosp85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondHosp85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do        
        Totals = sum(WealthMobMatCondHosp85plus,2)
        do i = 1,10
            WealthMobMatCondHosp85plus(i,:) = dble(WealthMobMatCondHosp85plus(i,:))/dble(Totals(i))                   
        end do         
        
        !Redo with health status
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic,ic) == 1) then
                    InitNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic,ic) == 1) then   
                    InitNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if                   
        end do
        end do     
                        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1 .and. alivemmat(i,ic+1,ic) == 1) then                                            
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if
                j = j + 1
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                               
            else if (alivefmat(i,ic+1,ic) == 1) then                
                if (healfMat(i,ic+1,ic) == 1) then
                    FinalNHstatus(j) = 1
                end if                  
                j = j + 1                    
            else if (alivemmat(i,ic+1,ic) == 1) then                
                if (healmMat(i,ic+1,ic) == 1) then   
                    FinalNHstatus(j) = 1
                end if                 
                j = j + 1                
            end if                
        end do
        end do                                              
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondBadHeal85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondBadHeal85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondBadHeal85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondBadHeal85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do        
        Totals = sum(WealthMobMatCondBadHeal85plus,2)
        do i = 1,10
            WealthMobMatCondBadHeal85plus(i,:) = dble(WealthMobMatCondBadHeal85plus(i,:))/dble(Totals(i))                   
        end do    
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)      
        
        print *, "here 5"  
        
        !Redo with women's marital status
        TotNum = sum(NumWidowsAlive(11:)) + sum(NumMarriedAlive(11:))/2  
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivefmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic+1,ic) == 2) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2    
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidow85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidow85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidow85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidow85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidow85plus,2)
        do i = 1,10
            WealthMobMatCondWidow85plus(i,:) = dble(WealthMobMatCondWidow85plus(i,:))/dble(Totals(i))                   
        end do    

        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)      
        
        !Redo with women's marital status
        TotNum = sum(NumWidowsAlive(11:)) + sum(NumMarriedAlive(11:))/2  
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), InitNHstatus(TotNum), FinalNHstatus(TotNum), stat=status)
        InitNHstatus = 0       
        FinalNHstatus = 0    
                        
        !extract expenses in period 1 for each individual
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic,ic)
                    InitNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic,ic)/2
                end if                   
                j = j + 1                 
            end if             
        end do
        end do     
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                                       
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=11,nr-1
        do i=1,NumHouseholds(ic)
            if (alivemmat(i,ic+1,ic) == 1) then                
                if (maritalMat(i,ic+1,ic) == 3) then
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)
                    FinalNHstatus(j) = 1
                else
                    wealthVect(j) = wealthMat(i,nw+ic+1,ic)/2    
                end if                 
                j = j + 1                                          
            end if            
        end do
        end do                
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5            
            WealthMobMatCondWidower85plus(2*i-1,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==0)
            WealthMobMatCondWidower85plus(2*i-1,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==0 .and. FinalNHstatus==1)   
            WealthMobMatCondWidower85plus(2*i,2*j-1) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==0)   
            WealthMobMatCondWidower85plus(2*i,2*j) = count(FinalQuintile==j .and. InitQuintile==i .and. InitNHstatus==1 .and. FinalNHstatus==1)                                                         
        end do
        end do       
        Totals = sum(WealthMobMatCondWidower85plus,2)
        do i = 1,10
            WealthMobMatCondWidower85plus(i,:) = dble(WealthMobMatCondWidower85plus(i,:))/dble(Totals(i))                   
        end do    

        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip, InitNHstatus, FinalNHstatus)              
        print *, "here 6"  

        deallocate(wealthMat)
        deallocate(medlocMat)   
        deallocate(aveearnsmMat)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Medicaid Entry Transitions
        do ic=1,nr-1   
            NumHHAlive(ic) = sum(aliveHmat(1:NumHouseholds(ic),ic,ic))
            NumMarriedAlive(ic) = 2*sum(alivefmat(1:NumHouseholds(ic),ic,ic)*alivemmat(1:NumHouseholds(ic),ic,ic))      
            NumWidowsAlive(ic) = sum(alivefmat(1:NumHouseholds(ic),ic,ic)*(1-alivemmat(1:NumHouseholds(ic),ic,ic)))      
            NumWidowersAlive(ic) = sum((1-alivefmat(1:NumHouseholds(ic),ic,ic))*alivemmat(1:NumHouseholds(ic),ic,ic))                  
        end do
        NumHHAlive(nr) = sum(aliveHmat(1:NumHouseholds(nr-1),nr,nr-1))
        NumMarriedAlive(nr) = 2*sum(alivefmat(1:NumHouseholds(nr-1),nr,nr-1)*alivemmat(1:NumHouseholds(nr-1),nr,nr-1))      
        NumWidowsAlive(nr) = sum(alivefmat(1:NumHouseholds(nr-1),nr,nr-1)*(1-alivemmat(1:NumHouseholds(nr-1),nr,nr-1)))      
        NumWidowersAlive(nr) = sum((1-alivefmat(1:NumHouseholds(nr-1),nr,nr-1))*alivemmat(1:NumHouseholds(nr-1),nr,nr-1))         
        NumIndAlive = NumMarriedAlive + NumWidowsAlive + NumWidowersAlive

        NumHHNewMedicaid = 0
        NumMarriedNewMedicaid = 0
        NumWidowsNewMedicaid = 0
        NumWidowersNewMedicaid = 0
        do ic=1,nr-1                 
            do i=1,NumHouseholds(ic)   !individual                                                                      
                if (ic == 1) then
                    if (GovTransferMat(i,ic,ic) > 0.0d0 .and. aliveHmat(i,ic,ic) == 1) then
                        NumHHNewMedicaid(ic) = NumHHNewMedicaid(ic) + 1 
                    end if
                    if (GovTransferMat(i,ic,ic) > 0.0d0 .and. alivefmat(i,ic,ic)*alivemmat(i,ic,ic) == 1) then
                        NumMarriedNewMedicaid(ic) = NumMarriedNewMedicaid(ic) + 2 
                    end if
                    if (GovTransferMat(i,ic,ic) > 0.0d0 .and. alivefmat(i,ic,ic)*(1-alivemmat(i,ic,ic)) == 1) then
                        NumWidowsNewMedicaid(ic) = NumWidowsNewMedicaid(ic) + 1 
                    end if
                    if (GovTransferMat(i,ic,ic) > 0.0d0 .and. (1-alivefmat(i,ic,ic))*alivemmat(i,ic,ic) == 1) then
                        NumWidowersNewMedicaid(ic) = NumWidowersNewMedicaid(ic) + 1 
                    end if                
                else
                    if (GovTransferMat(i,ic-1,ic) == 0.0d0 .and. GovTransferMat(i,ic,ic) > 0.0d0 .and. aliveHmat(i,ic,ic) == 1) then
                        NumHHNewMedicaid(ic) = NumHHNewMedicaid(ic) + 1 
                    end if
                    if (GovTransferMat(i,ic-1,ic) == 0.0d0 .and. GovTransferMat(i,ic,ic) > 0.0d0 .and. alivefmat(i,ic,ic)*alivemmat(i,ic,ic) == 1) then
                        NumMarriedNewMedicaid(ic) = NumMarriedNewMedicaid(ic) + 2 
                    end if
                    if (GovTransferMat(i,ic-1,ic) == 0.0d0 .and. GovTransferMat(i,ic,ic) > 0.0d0 .and. alivefmat(i,ic,ic)*(1-alivemmat(i,ic,ic)) == 1) then
                        NumWidowsNewMedicaid(ic) = NumWidowsNewMedicaid(ic) + 1 
                    end if
                    if (GovTransferMat(i,ic-1,ic) == 0.0d0 .and. GovTransferMat(i,ic,ic) > 0.0d0 .and. (1-alivefmat(i,ic,ic))*alivemmat(i,ic,ic) == 1) then
                        NumWidowersNewMedicaid(ic) = NumWidowersNewMedicaid(ic) + 1 
                    end if
                end if
            end do
        end do
        if (GovTransferMat(i,nr-1,nr-1) == 0.0d0 .and. GovTransferMat(i,nr,nr-1) > 0.0d0 .and. aliveHmat(i,nr,nr-1) == 1) then
            NumHHNewMedicaid(nr) = NumHHNewMedicaid(nr) + 1 
        end if
        if (GovTransferMat(i,nr-1,nr-1) == 0.0d0 .and. GovTransferMat(i,nr,nr-1) > 0.0d0 .and. alivefmat(i,nr,nr-1)*alivemmat(i,nr,nr-1) == 1) then
            NumMarriedNewMedicaid(nr) = NumMarriedNewMedicaid(nr) + 2 
        end if
        if (GovTransferMat(i,nr-1,nr-1) == 0.0d0 .and. GovTransferMat(i,nr,nr-1) > 0.0d0 .and. alivefmat(i,nr,nr-1)*(1-alivemmat(i,nr,nr-1)) == 1) then
            NumWidowsNewMedicaid(nr) = NumWidowsNewMedicaid(nr) + 1 
        end if
        if (GovTransferMat(i,nr-1,nr-1) == 0.0d0 .and. GovTransferMat(i,nr,nr-1) > 0.0d0 .and. (1-alivefmat(i,nr,nr-1))*alivemmat(i,nr,nr-1) == 1) then
            NumWidowersNewMedicaid(nr) = NumWidowersNewMedicaid(nr) + 1 
        end if        
        
        NumPeopleNewMedicaid = NumMarriedNewMedicaid + NumWidowsNewMedicaid + NumWidowersNewMedicaid
       
        
        !households
        TotNum6574 = sum(NumHHAlive(:5))
        TotNum7584 = sum(NumHHAlive(6:10))
        TotNum85plus = sum(NumHHAlive(11:))
        
        TotNumNewMedicaid6574 = sum(NumHHNewMedicaid(:5))
        TotNumNewMedicaid7584 = sum(NumHHNewMedicaid(6:10))
        TotNumNewMedicaid85plus = sum(NumHHNewMedicaid(11:))       
        
        FracHHEnterMedicaid(1) =  dble(TotNumNewMedicaid6574)/dble(TotNum6574)
        FracHHEnterMedicaid(2) =  dble(TotNumNewMedicaid7584)/dble(TotNum7584)
        FracHHEnterMedicaid(3) =  dble(TotNumNewMedicaid85plus)/dble(TotNum85plus)   
        
        !married individuals
        TotNum6574 = sum(NumMarriedAlive(:5))
        TotNum7584 = sum(NumMarriedAlive(6:10))
        TotNum85plus = sum(NumMarriedAlive(11:))
        
        TotNumNewMedicaid6574 = sum(NumMarriedNewMedicaid(:5))
        TotNumNewMedicaid7584 = sum(NumMarriedNewMedicaid(6:10))
        TotNumNewMedicaid85plus = sum(NumMarriedNewMedicaid(11:))       
        
        FracMarriedEnterMedicaid(1) =  dble(TotNumNewMedicaid6574)/dble(TotNum6574)
        FracMarriedEnterMedicaid(2) =  dble(TotNumNewMedicaid7584)/dble(TotNum7584)
        FracMarriedEnterMedicaid(3) =  dble(TotNumNewMedicaid85plus)/dble(TotNum85plus)   
        
        !widows
        TotNum6574 = sum(NumWidowsAlive(:5))
        TotNum7584 = sum(NumWidowsAlive(6:10))
        TotNum85plus = sum(NumWidowsAlive(11:))
        
        TotNumNewMedicaid6574 = sum(NumWidowsNewMedicaid(:5))
        TotNumNewMedicaid7584 = sum(NumWidowsNewMedicaid(6:10))
        TotNumNewMedicaid85plus = sum(NumWidowsNewMedicaid(11:))       
        
        FracWidowsEnterMedicaid(1) =  dble(TotNumNewMedicaid6574)/dble(TotNum6574)
        FracWidowsEnterMedicaid(2) =  dble(TotNumNewMedicaid7584)/dble(TotNum7584)
        FracWidowsEnterMedicaid(3) =  dble(TotNumNewMedicaid85plus)/dble(TotNum85plus)                   
        
        !widowers
        TotNum6574 = sum(NumWidowersAlive(:5))
        TotNum7584 = sum(NumWidowersAlive(6:10))
        TotNum85plus = sum(NumWidowersAlive(11:))
        
        TotNumNewMedicaid6574 = sum(NumWidowersNewMedicaid(:5))
        TotNumNewMedicaid7584 = sum(NumWidowersNewMedicaid(6:10))
        TotNumNewMedicaid85plus = sum(NumWidowersNewMedicaid(11:))       
        
        FracWidowersEnterMedicaid(1) =  dble(TotNumNewMedicaid6574)/dble(TotNum6574)
        FracWidowersEnterMedicaid(2) =  dble(TotNumNewMedicaid7584)/dble(TotNum7584)
        FracWidowersEnterMedicaid(3) =  dble(TotNumNewMedicaid85plus)/dble(TotNum85plus)     
        
        !individuals
        TotNum6574 = sum(NumIndAlive(:5))
        TotNum7584 = sum(NumIndAlive(6:10))
        TotNum85plus = sum(NumIndAlive(11:))
        
        TotNumNewMedicaid6574 = sum(NumPeopleNewMedicaid(:5))
        TotNumNewMedicaid7584 = sum(NumPeopleNewMedicaid(6:10))
        TotNumNewMedicaid85plus = sum(NumPeopleNewMedicaid(11:))       
        
        FracIndEnterMedicaid(1) =  dble(TotNumNewMedicaid6574)/dble(TotNum6574)
        FracIndEnterMedicaid(2) =  dble(TotNumNewMedicaid7584)/dble(TotNum7584)
        FracIndEnterMedicaid(3) =  dble(TotNumNewMedicaid85plus)/dble(TotNum85plus)                   
    end subroutine MedExpSimulations      
      
    subroutine WealthSimulations 
        !use interpolate1
        !use output
        !use mtmod
        implicit none
        
        Interface
           Subroutine mrgrnk(XVALT, IRNGT)
              use constants
              Real(dbl), Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine mrgrnk
           Subroutine I_mrgrnk(XVALT, IRNGT)
              use constants
              Integer, Dimension (:), Intent (In) :: XVALT
              Integer, Dimension (:), Intent (Out) :: IRNGT
           End Subroutine I_mrgrnk                      
        End Interface 
                
        integer i, j, v, s, ic, iseed, status, count, Np, NumPeople(T-1), NumPeopleAlive(nr), TotNum, het, zf, zm, fet, met, set, h
        integer pm, tm
        real(dbl) cumuinit(nef,nem), cumuf(nef), cumum(nem), cumeducdist(nhet), cummeddist(npm,nhet), cumhealf(nsht,nset), cumhealm(nsht,nset), cummed(npm), cumsinitH(3,naem)
        real(dbl),allocatable,dimension(:,:,:):: earnsfMat, earnsmMat, aveearnsfMat, aveearnsmMat, wealthMat, labfMat
        real(dbl),allocatable,dimension(:):: wealthVect
        integer, allocatable, dimension(:,:,:):: prodflocMat, prodmlocMat, medlocMat, alivefmat, alivemmat, aliveHmat, healHmat, healfMat, healmMat, maritalMat, futureStatus
        integer, allocatable, dimension(:):: InitQuintile, FinalQuintile, AgentNum, ip  
        integer, allocatable, dimension(:,:):: HHeducloc, aveearnsmloc     
        iseed = 3464653
        call sgrnd(iseed)
                
        print *, "point 7a"  
        
        Np = Ns/(T-1) !Ns should be divisible by T-1=39
        
        !Create vector with number of people in each cohort                    
        NumPeople = nint( (/(1.0d0/(1+ng)**ic,ic=0,T-2)/)*Np )         
        
        !person, age, cohort        
        allocate (prodflocMat(Np,nw,T-1), prodmlocMat(Np,nw,T-1), aveearnsfMat(Np,nw+1,T-1), aveearnsmMat(Np,nw+1,T-1), wealthMat(Np,T,T-1), stat=status)                                              
        allocate (labfMat(Np,nw,T-1),earnsfMat(Np,nw,T-1),earnsmMat(Np,nw,T-1),HHeducloc(Np,T-1), aveearnsmloc(Np,T-1), stat = status)
        allocate (alivefMat(Np,nr,T-1),alivemMat(Np,nr,T-1),aliveHmat(Np,nr,T-1), healHmat(Np,nr,T-1), healfMat(Np,nr,T-1), healmMat(Np,nr,T-1), futureStatus(Np,nr,T-1), stat=status)
        
        print *, "point 7b"  
        
        !Workers
        !!!!!!!!!!!!!!!!!!!!!!!        
        !Create cumulative initial distribution of earnings shocks        
        do zf = 1,nef              
        do zm = 1,nem
            if (zf==1 .and. zm==1) then
                cumuinit(1,1) = uinit(1,1)
            else if (zm==1) then
                cumuinit(zf,zm) =  cumuinit(zf-1,nem) + uinit(zf,zm)
            else
                cumuinit(zf,zm) =  cumuinit(zf,zm-1) + uinit(zf,zm)
            end if
        end do
        end do
        
        !Create cumulative distribution for education distribution
        cumeducdist(1) = EducDist(1)    
        do s=2,nhet
            cumeducdist(s) = cumeducdist(s-1) + EducDist(s)                             
        end do                  
       
        !Each cohorts initial draws are set so that that cohort's initial productivity distribution is correct
        do ic=1,T-1       
            !First to HH households                 
            do zf = 1,nef              
            do zm = 1,nem
                if (zf==1 .and. zm==1) then
                    do i=1,nint(cumuinit(1,1)*EducDist(1)*NumPeople(ic)) 
                        prodflocMat(i,1,ic) = 1
                        prodmlocMat(i,1,ic) = 1
                    end do             
                else if (zm==1) then
                    do i=nint(cumuinit(zf-1,nem)*EducDist(1)*NumPeople(ic)+1),nint(cumuinit(zf,1)*EducDist(1)*NumPeople(ic))
                        prodflocMat(i,1,ic) = zf
                        prodmlocMat(i,1,ic) = zm
                    end do            
                else
                    !print *, "nint(cumuinit(zf,zm-1)*EducDist(1)*NumPeople(ic)+1) =" , nint(cumuinit(zf,zm-1)*EducDist(1)*NumPeople(ic)+1)
                    !print *, "nint(cumuinit(zf,zm)*EducDist(1)*NumPeople(ic)) = ", nint(cumuinit(zf,zm)*EducDist(1)*NumPeople(ic))
                    do i=nint(cumuinit(zf,zm-1)*EducDist(1)*NumPeople(ic)+1),nint(cumuinit(zf,zm)*EducDist(1)*NumPeople(ic))
                        prodflocMat(i,1,ic) = zf
                        prodmlocMat(i,1,ic) = zm
                    end do                    
                end if
            end do 
            end do  
            
            !Then to the rest of the households    
            do s=2,nhet
                do zf = 1,nef              
                do zm = 1,nem        
                    if (zf==1 .and. zm==1) then
                        do i=nint(cumeducdist(s-1)*NumPeople(ic))+1 ,nint(cumeducdist(s-1)*NumPeople(ic)+cumuinit(1,1)*EducDist(s)*NumPeople(ic)) 
                            prodflocMat(i,1,ic) = 1
                            prodmlocMat(i,1,ic) = 1
                        end do             
                    else if (zm==1) then
                        do i=nint(cumeducdist(s-1)*NumPeople(ic)+cumuinit(zf-1,nem)*EducDist(s)*NumPeople(ic))+1,nint(cumeducdist(s-1)*NumPeople(ic)+cumuinit(zf,1)*EducDist(s)*NumPeople(ic))
                            prodflocMat(i,1,ic) = zf
                            prodmlocMat(i,1,ic) = zm
                        end do            
                    else                                
                        do i=nint(cumeducdist(s-1)*NumPeople(ic)+cumuinit(zf,zm-1)*EducDist(s)*NumPeople(ic))+1,nint(cumeducdist(s-1)*NumPeople(ic)+cumuinit(zf,zm)*EducDist(s)*NumPeople(ic))
                            prodflocMat(i,1,ic) = zf
                            prodmlocMat(i,1,ic) = zm
                        end do
                    end if       
                end do    
                end do
            end do                           
        end do
        
        print *, "point 7c"
                
        !Assign female earnings shocks for ages 2 to nw    
        do ic=1,T-1
        do i =1,NumPeople(ic)  
            do j=2,nw
                cumuf(1) = Puefmat(1,prodflocMat(i,j-1,ic))
                do v=2,nef
                    cumuf(v) = cumuf(v-1)+Puefmat(v,prodflocMat(i,j-1,ic))
                end do           
                call Locate(cumuf, grnd(), prodflocMat(i,j,ic))
                prodflocMat(i,j,ic) = prodflocMat(i,j,ic)+1                          
            end do                      
        end do   
        end do        
        
        !Assign male earnings shocks for ages 2 to nw    
        do ic=1,T-1
        do i =1,NumPeople(ic)  
            do j=2,nw
                cumum(1) = Puemmat(1,prodmlocMat(i,j-1,ic))
                do v=2,nem
                    cumum(v) = cumum(v-1)+Puemmat(v,prodmlocMat(i,j-1,ic))
                end do           
                call Locate(cumum, grnd(), prodmlocMat(i,j,ic))
                prodmlocMat(i,j,ic) = prodmlocMat(i,j,ic)+1                          
            end do                      
        end do   
        end do        

        !Assign HH education location
        do ic=1,T-1
        do i =1,NumPeople(ic) 
            if (i<nint(NumPeople(ic)*cumeducdist(1))) then !HH
                HHeducloc(i,ic)=1               
            else if (i<nint(NumPeople(ic)*cumeducdist(2))) then !HC 
                HHeducloc(i,ic)=2                
            else if (i<nint(NumPeople(ic)*cumeducdist(3))) then !CH
                HHeducloc(i,ic)=3                
            else !CC
                HHeducloc(i,ic)=4                              
            end if   
        end do   
        end do

        print *, "point 7e"
        
        !average earnings and wealth of each individual in each cohort by age               
        do ic=1,T-1 !cohort        
        do i=1,NumPeople(ic)   !individual   
            het = HHeducloc(i,ic)  
            fet = int((het+1)/2)
            met = mod(het+1,2)+1
            labfMat(i,1,ic) = labWCube(1,prodflocMat(i,1,ic),prodmlocMat(i,1,ic),1,1,het,1)
            earnsfMat(i,1,ic) = w*efmat(prodflocMat(i,1,ic), prodmlocMat(i,1,ic),fet,1)*labfMat(i,1,ic)
            earnsmMat(i,1,ic) = w*emmat(prodflocMat(i,1,ic), prodmlocMat(i,1,ic),met,1)*hbar   
            aveearnsfMat(i,1,ic) = 0.0d0
            aveearnsmMat(i,1,ic) = 0.0d0            
            wealthMat(i,1,ic) = 0.0d0                                                                                                    
        do j=2,nw    !age  
            !age-j wealth
            call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(i,j-1,ic),prodmlocMat(i,j-1,ic),:,:,het,j-1), &
                    wealthMat(i,j-1,ic), aveearnsfMat(i,j-1,ic), aveearnsmMat(i,j-1,ic), wealthMat(i,j,ic) )                  
            
            !age-j average earnings (average of earnings up to age j-1 earnings)                                      
            aveearnsfMat(i,j,ic) = ((j-2)*aveearnsfMat(i,j-1,ic) + earnsfMat(i,j-1,ic))/(j-1)    
            aveearnsmMat(i,j,ic) = ((j-2)*aveearnsmMat(i,j-1,ic) + earnsmMat(i,j-1,ic))/(j-1)       
            
            !age-j female labor supply
            call interpol3d(avect, aveEarnFvect, aveEarnMvect,labWCube(:,prodflocMat(i,j,ic),prodmlocMat(i,j,ic),:,:,het,j), &
                    wealthMat(i,j,ic),aveearnsfMat(i,j,ic), aveearnsmMat(i,j,ic),labfMat(i,j,ic))                   
            
            !earnings                                                             
            earnsfMat(i,j,ic) = w*efmat(prodflocMat(i,j,ic), prodmlocMat(i,j,ic),fet,j)*labfMat(i,j,ic)
            earnsmMat(i,j,ic) = w*emmat(prodflocMat(i,j,ic), prodmlocMat(i,j,ic),met,j)*hbar                                                                                                              
        end do  
            aveearnsfMat(i,nw+1,ic) = ((nw-1)*aveearnsfMat(i,nw,ic) + earnsfMat(i,nw,ic))/nw 
            aveearnsmMat(i,nw+1,ic) = ((nw-1)*aveearnsmMat(i,nw,ic) + earnsmMat(i,nw,ic))/nw 
                 
            call Locate(aveEarnMvect, aveearnsmMat(i,nw,ic), aveearnsmloc(i,ic))
            if (aveearnsmloc(i,ic) == 0) aveearnsmloc(i,ic) = 1
            !should we assign to lower or upper point based on probability?                                      
        end do                                            
        end do                    
        
        deallocate(labfMat,earnsfMat,earnsmMat)       
        allocate (medlocMat(Np,nr,T-1), stat=status)    
        !print *, "status =", status        
        allocate (maritalMat(Np,nr,T-1), stat=status)
        !print *, "status =", status 
        !print *, "point 6d" 
         print *, "point 7f"                      
        !!!!!!!!!!!!!!!!!!!!!!!
        !Retirees
        
        !Health Status 
        !Create cumulative distribution of female health status for each education type
        do set = 1,nset
            cumhealf(1,set) = hinitF(1,set)    
            do h=2,nsht
                cumhealf(h,set) = cumhealf(h-1,set) + hinitF(h,set)                             
            end do       
        end do    
        
        !Create cumulative distribution of male health status for each education type
        do set = 1,nset
            cumhealm(1,set) = hinitM(1,set)    
            do h=2,nsht
                cumhealm(h,set) = cumhealm(h-1,set) + hinitM(h,set)                             
            end do       
        end do              
        
        !Assign initial health status to each individual             
        do ic=1,T-1
        do i= 1,NumPeople(ic)             
            het = HHeducloc(i,ic)    
            fet = int((het+1)/2)
            met = mod(het+1,2)+1                          
            if (grnd() < cumhealf(1,fet)) then
                healfMat(i,1,ic) = 1        
            else
                healfMat(i,1,ic) = 2
            end if
            if (grnd() < cumhealm(1,met)) then
                healmMat(i,1,ic) = 1        
            else
                healmMat(i,1,ic) = 2
            end if                    
        end do
        end do

        !Create cumulative distribution of marital statuses
        cumsinitH(1,:) = sinitH(1,:)
        do i = 2,3                           
            cumsinitH(i,:) = cumsinitH(i-1,:) + sinitH(i,:)                                   
        end do          
        
        !Assign alive flag to each individual of age nw+1
        alivefMat = 0
        alivemMat = 0
         
        !Assign marital flag to each household of age nw+1
        !maritalMat = 0
        maritalMat = 1
        do ic=1,T-1
        do i= 1,NumPeople(ic)                                                 
            call Locate(cumsinitH(:,aveearnsmloc(i,ic)), grnd(), maritalMat(i,1,ic))
            maritalMat(i,1,ic) = maritalMat(i,1,ic)+1        
            maritalMat(i,2:,ic) = maritalMat(i,1,ic)
            if (maritalMat(i,1,ic) == 1) then
                alivefMat(i,1,ic) = 1
                alivemMat(i,1,ic) = 1   
            else if (maritalMat(i,1,ic) == 2) then
                alivefMat(i,1,ic) = 1
                if (grnd() < probHusband0) then
                    aveearnsmMat(i,nw+1,ic) = 0.0d0       
                end if                
            else
                alivemMat(i,1,ic) = 1 
            end if                                        
        end do
        end do        
        deallocate(aveearnsmloc)
        
        print *, "point 7g"
        
        !Assign health status, alive status, marital status to each individual at ages nw+2 to T                     
        do ic=1,T-1
        do i= 1,NumPeople(ic) 
        do j=2,nr    
            
            !survival           
            if (maritalMat(i,j-1,ic) == 1 .and. alivefMat(i,j-1,ic)==1 .and. alivemMat(i,j-1,ic)==1) then  
                if (grnd() < survivalprobVectF(healfMat(i,j-1,ic),1,j-1)) alivefMat(i,j,ic) = 1     
                if (grnd() < survivalprobVectM(healmMat(i,j-1,ic),1,j-1)) alivemMat(i,j,ic) = 1     
            else if (maritalMat(i,j-1,ic) == 2  .and. alivefMat(i,j-1,ic)==1) then  !widow
                if (grnd() < survivalprobVectF(healfMat(i,j-1,ic),2,j-1)) alivefMat(i,j,ic) = 1
            else if (maritalMat(i,j-1,ic) == 3  .and. alivemMat(i,j-1,ic)==1) then  !widower
                if (grnd() < survivalprobVectM(healmMat(i,j-1,ic),3,j-1)) alivemMat(i,j,ic) = 1            
            end if 
            
            !marital status
            if (alivefMat(i,j,ic) == 1 .and. alivemMat(i,j,ic) == 1) then
                maritalMat(i,j:,ic) = 1 
                futureStatus(i,j-1,ic) = 1
            else if (alivefMat(i,j,ic) == 1) then   
                maritalMat(i,j:,ic) = 2
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 2
                else
                    futureStatus(i,j-1,ic) = 1
                end if
            else if (alivemMat(i,j,ic) == 1) then   
                maritalMat(i,j:,ic) = 3
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 3
                else
                    futureStatus(i,j-1,ic) = 1
                end if
            else !both0
                if (maritalMat(i,j-1,ic) == 1) then
                    futureStatus(i,j-1,ic) = 4
                else 
                    futureStatus(i,j-1,ic) = 2
                end if                  
            end if
            
            !health status                                    
            if (grnd() < PhmatF(1,healfMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)) then
                healfMat(i,j,ic) = 1        
            else
                healfMat(i,j,ic) = 2
            end if
            if (grnd() <PhmatM(1,healmMat(i,j-1,ic),maritalMat(i,j-1,ic),j-1)) then
                healmMat(i,j,ic) = 1        
            else
                healmMat(i,j,ic) = 2
            end if  
        end do        
        end do 
        end do     
        do ic=1,T-1
        do i= 1,NumPeople(ic)
            if (maritalMat(i,nr,ic) == 1) then
                futureStatus(i,nr,ic) = 4
            else 
                futureStatus(i,nr,ic) = 2
            end if
        end do  
        end do                   
        
        !Household health status
        do ic=1,T-1
        do i= 1,NumPeople(ic)
        do j=1,nr
            if (maritalMat(i,j,ic) == 1) then      
                if (healfMat(i,j,ic) == 1 .and. healmMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 1 !BB
                else if (healfMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 2 !BG
                else if (healmMat(i,j,ic) == 1) then
                    healHmat(i,j,ic) = 3 !GB
                else !GG
                    healHmat(i,j,ic) = 4
                end if
            else if (maritalMat(i,j,ic) == 2) then                  
                healHmat(i,j,ic) =healfMat(i,j,ic)  
            else
                healHmat(i,j,ic) =healmMat(i,j,ic)                                            
            end if    
        end do
        end do
        end do        
        
        !Medical shocks
        !Create cumulative distribution of medical shock for each education type
        do het = 1,nhet
            cummeddist(1,het) = minit(1,het)    
            do s=2,npm
                cummeddist(s,het) = cummeddist(s-1,het) + minit(s,het)                             
            end do       
        end do       
        
        !Assign medical shocks for age nw+1    
        do ic=1,T-1
        do i= 1,NumPeople(ic)   
            het = HHeducloc(i,ic)                                          
            call Locate(cummeddist(:,het), grnd(), medlocMat(i,1,ic))
            medlocMat(i,1,ic) = medlocMat(i,1,ic)+1   
            if (grnd() > Ptmvect(1)) then
                medlocMat(i,1,ic) = npm + medlocMat(i,1,ic)
            end if                                                    
        end do 
        end do
        
        !Assign medical shocks for ages nw+2 to T
        !Need to condition this on martial status if want to have Pmmat differ by marital status
        do ic=1,T-1
        do i= 1,NumPeople(ic) 
            do j=2,nr
                pm = medlocMat(i,j-1,ic)-npm*((medlocMat(i,j-1,ic)-1)/npm)
                tm = (medlocMat(i,j-1,ic)-1)/npm + 1                
                cummed(1) = Pummat(1,pm,j-1)
                do v=2,npm
                    cummed(v) = cummed(v-1)+Pummat(v,pm,j-1)
                end do           
                call Locate(cummed, grnd(), medlocMat(i,j,ic))
                medlocMat(i,j,ic) = medlocMat(i,j,ic)+1   
                if (grnd() > Ptmvect(1)) then
                    medlocMat(i,j,ic) = npm + medlocMat(i,j,ic)
                end if                                        
            end do                      
        end do  
        end do
        
        print *, "point 7h"
        
        aliveHmat = 0
        do ic=1,T-1        
        do i=1,NumPeople(ic)   !individual                                           
            do j=1,nr
                !age-nw+j wealth
                if (j == 1) then                
                    call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyWCube(:,prodflocMat(i,nw,ic),prodmlocMat(i,nw,ic),:,:,HHeducloc(i,ic),nw), &
                        wealthMat(i,nw,ic), aveearnsfMat(i,nw,ic), aveearnsmMat(i,nw,ic), wealthMat(i,nw+1,ic) )                     
                else
                    if (maritalMat(i,j-1,ic) == 1) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeMarried(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                     
                    else if (maritalMat(i,j-1,ic) == 2) then
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidow(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                                            
                    else
                        call interpol3d(avect, aveEarnFvect, aveEarnMvect, aPolicyRCubeWidower(:,medlocMat(i,j-1,ic),:,:,healHmat(i,j-1,ic),futureStatus(i,j-1,ic),j-1), &
                            wealthMat(i,nw+j-1,ic), aveearnsfMat(i,nw+1,ic), aveearnsmMat(i,nw+1,ic), wealthMat(i,nw+j,ic) )                                            
                    end if                  
                end if                                                
                if (alivefmat(i,j,ic) == 1 .or. alivemmat(i,j,ic)==1) aliveHmat(i,j,ic) = 1                                           
            end do
        end do        
        end do

        deallocate (prodflocMat,prodmlocMat,aveearnsfMat, aveearnsmMat,futureStatus, stat=status)
        deallocate(medlocMat)      
        
        print *, "point 7i"  
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Mobility Matrices                              
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !2-period wealth mobility matrices
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
        !All HH's       
        do ic=0,nr-1    
            NumPeopleAlive(ic+1) = sum(aliveHmat(1:NumPeople(nw+ic),ic+1,nw+ic))            
        end do        
        
        TotNum = sum(NumPeople(:nw-1)) + sum(NumPeopleAlive)
        
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=1,nw-1
        do i=1,NumPeople(ic)
            wealthVect(j) = wealthMat(i,ic,ic)
            j = j + 1
        end do
        end do        
        do ic=0,nr-1
        do i=1,NumPeople(nw+ic)
            if (aliveHmat(i,ic+1,nw+ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic,nw+ic)
                j = j + 1
            end if            
        end do
        end do        
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,nw-1
        do i=1,NumPeople(ic)
            wealthVect(j) = wealthMat(i,ic+1,ic)
            j = j + 1
        end do
        end do        
        do ic=0,nr-1
        do i=1,NumPeople(nw+ic)
            if (aliveHmat(i,ic+1,nw+ic) == 1) then
                wealthVect(j) = wealthMat(i,nw+ic+1,nw+ic)
                j = j + 1
            end if            
        end do
        end do          
                       
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        WealthMobMatHH2yr = dble(WealthMobMatHH2yr)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip)  
                
        !wealth mobility matrix 21-64
        TotNum = sum(NumPeople(:nw-1)) + NumPeopleAlive(1)
        allocate (wealthVect(TotNum), InitQuintile(TotNum), FinalQuintile(TotNum), AgentNum(TotNum), ip(TotNum), stat=status)    
        !print *, "status=", status
        
        !extract expenses in period 1 for each household
        j = 1
        do ic=1,nw-1
        do i=1,NumPeople(ic)
            wealthVect(j) = wealthMat(i,ic,ic)
            j = j + 1
        end do
        end do                
        do i=1,NumPeople(ic)
            if (aliveHmat(i,1,nw) == 1) then
                wealthVect(j) = wealthMat(i,nw,nw)
                j = j + 1
            end if            
        end do      
                        
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                            
        wealthVect = wealthVect(AgentNum)        
                     
        !assign period 1 quintile
        do i=1,5
            InitQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)      
        InitQuintile = InitQuintile(ip)
        
        !extract expenses in period 2 for each individual       
        j = 1
        do ic=1,nw-1
        do i=1,NumPeople(ic)
            wealthVect(j) = wealthMat(i,ic+1,ic)
            j = j + 1
        end do
        end do                
        do i=1,NumPeople(ic)
            if (aliveHmat(i,1,nw) == 1) then
                wealthVect(j) = wealthMat(i,nw+1,nw)
                j = j + 1
            end if            
        end do
                
        do i=1,TotNum
            AgentNum(i) = i
            ip(i) = i
        end do
        call mrgrnk(wealthVect, AgentNum)                  
        wealthVect = wealthVect(AgentNum)        
        
        !assign period 2 quintile
        do i=1,5
            FinalQuintile((i-1)*TotNum/5+1:i*TotNum/5) = i                          
        end do  
        call I_mrgrnk(AgentNum, ip)    
        FinalQuintile = FinalQuintile(ip)                
        
        do i=1,5
        do j=1,5
            WealthMobMatHH2yr2164(i,j) = count(FinalQuintile==j .and. InitQuintile==i)                 
        end do
        end do
        WealthMobMatHH2yr2164 = dble(WealthMobMatHH2yr2164)/dble(TotNum/5)
        !call Out(0,"MedMobMat",MedMobMat)        
                                     
        deallocate (wealthVect, InitQuintile, FinalQuintile, AgentNum, ip)                      
               
                   
        deallocate(wealthMat)
        print *, "point 7f" 
    end subroutine WealthSimulations            
      
end module simulations


Subroutine mrgrnk(XDONT, IRNGT)
    use constants
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Real (kind=dbl), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Real (kind=dbl) :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine mrgrnk

Subroutine I_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (In)  :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Integer :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine I_mrgrnk        