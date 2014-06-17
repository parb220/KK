module results
    use functions
    use output
    use simulations
    !use sorts
    use mathutils, only : linspace
    
    !use ieee_arithmetic
    
    implicit none    
    contains
    
   
    subroutine ComputeExPostStats1()
        integer eh, ti, s, vm, vf, h, j, i, ht, em, ef, het, fet, met, zm, zf, d, dt, jm, jf
        real(dbl) preTaxIncome, PostTaxWealth, preTaxIncomeF, preTaxIncomeM, tempR, OOPExpenses
        real(dbl) TransferRet, IncTaxRet, AveMaleEarningsCheck
        !logical isnan 
        !
            
        MedicareRevenue = 0.0d0
        do ti=1,nw
            do eh = 1,nhet
            do vm = 1,naem
            do vf = 1,naef
            do jm = 1,nem
            do jf = 1,nef
            do i = 1,na  
                em = mod(eh+1,2)+1        
                ef = int((eh+1)/2)              
                MedicareRevenue = MedicareRevenue +  (MedicareTax(w*efmat(jf,jm,ef,ti)* &
labWcube(i,jf,jm,vf,vm,eh,ti)) + MedicareTax(w*emmat(jf,jm,em,ti)*hbar)) * psiW(i,jf,jm,vf,vm,eh,ti) * & 
CohortWeights(ti)        
            end do
            end do
            end do 
            end do
            end do
            end do
        end do  
       call Out(0,"SSTaxes - MedicareRevenue", SSTaxes- MedicareRevenue, ret)
       call Out(0,"TotalPayments", TotalPayments, ret)

        
        do i=1,naem
            AveEarnsM65dist(i) = sum(psiRMarried(:,:,:,i,:,:,1)) + sum(psiRWidow(:,:,:,i,:,:,1)) + sum(psiRWidower(:,:,:,i,:,:,1))
        end do
        call Out(0,"AveEarnsM65dist", AveEarnsM65dist, ret)
        !call Out(0,"AveEarnsM65dist2",sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1),ret)
            
        
        Frac65plus = sum(MaritalDist(1,:)*CohortWeights(nw+1:)) *2.0d0
        Frac65plus = Frac65plus + sum(sum(MaritalDist(2:3,:),1)*CohortWeights(nw+1:))
        TotalPop = Frac65plus + sum(CohortWeights(1:nw))*2.0d0
        Frac65plus = Frac65plus/TotalPop        
        
        do ti=1,nr
            FracIndByMaritalStatusAge(1,ti) = MaritalDist(1,ti)*2.0d0/(MaritalDist(1,ti)*2.0d0 + & 
MaritalDist(2,ti) + MaritalDist(3,ti)) 
            FracIndByMaritalStatusAge(2,ti) = MaritalDist(2,ti)/(MaritalDist(1,ti)*2.0d0 + & 
MaritalDist(2,ti) + MaritalDist(3,ti))
            FracIndByMaritalStatusAge(3,ti) = MaritalDist(3,ti)/(MaritalDist(1,ti)*2.0d0 + & 
MaritalDist(2,ti) + MaritalDist(3,ti)) 
        end do
        
        do ti=1,nw
        do zf=1,nef
            FracProdLocFID(zf,ti) = sum(psiW(:,zf,:,:,:,:,ti))
        end do
        end do
                
        do ti=1,nw
        do zm=1,nem
            FracProdLocMID(zm,ti) = sum(psiW(:,:,zm,:,:,:,ti))
        end do
        end do
        
        do ti=1,nw
        do het=1,nhet
            FracEducLocHID(het,ti) = sum(psiW(:,:,:,:,:,het,ti))
        end do
        end do        
              

        print *, "here 1"

        FracWomenLabForByAgeEduc = 0.0d0
        AveFemaleHoursByAgeEduc = 0.0d0
        AveFemaleHoursCondWorkByEduc = 0.0d0
        do ti=1,nw
        do het=1,nhet
        do vm=1,naem
        do vf=1,naef
        do em=1,nem
        do ef=1,nef
        do i=1,na
            if (labWcube(i,ef,em,vf,vm,het,ti) > 0.0d0) then
                FracWomenLabForByAgeEduc(ti,het) = FracWomenLabForByAgeEduc(ti,het) + & 
psiW(i,ef,em,vf,vm,het,ti)/educDist(het) 
                AveFemaleHoursCondWorkByEduc(het) = AveFemaleHoursCondWorkByEduc(het) + &
labWcube(i,ef,em,vf,vm,het,ti)*psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti)/educDist(het)                                                               
            end if
            AveFemaleHoursByAgeEduc(ti,het) = AveFemaleHoursByAgeEduc(ti,het) + labWcube(i,ef,em,vf,vm,het,ti)* & 
psiW(i,ef,em,vf,vm,het,ti)/educDist(het)           
        end do
        end do
        end do
        end do
        end do
        end do
        end do       
        
       
        print *, "here 1a"
        
        FracWomenLabForByEduc = 0.0d0
        AveFemaleHoursByEduc = 0.0d0        
        do eh=1,nhet
        do ti=1,nw            
            FracWomenLabForByEduc(eh) = FracWomenLabForByEduc(eh) + FracWomenLabForByAgeEduc(ti,eh)*CohortWeights(ti)            
            AveFemaleHoursByEduc(eh) = AveFemaleHoursByEduc(eh) + AveFemaleHoursByAgeEduc(ti,eh)*CohortWeights(ti)
        end do
        end do            
        FracWomenLabForByEduc = FracWomenLabForByEduc/sum(CohortWeights(:nw))   
        AveFemaleHoursByEduc = AveFemaleHoursByEduc/sum(CohortWeights(:nw))   
        FracWomenLaborForce = sum(FracWomenLabForByEduc*educDist)
        
        AveFemaleHoursCondWorkByEduc = AveFemaleHoursCondWorkByEduc/(FracWomenLabForByEduc*sum(CohortWeights(:nw))) 
        
        print *, "here 1b"
        
        AveFrischElasticityFW = 0.0d0
        AveFrischElasticityF = 0.0d0
        FracWomenLabForByAge = 0.0d0
        AveFemaleHoursCondWorkByAge = 0.0d0
        AveFemaleHoursByAge = 0.0d0
        AveFemaleEarnings = 0.0d0
        AveHHEarnings = 0.0d0
        do ti=1,nw
        do het=1,nhet
        do vm=1,naem
        do vf=1,naef
        do em=1,nem
        do ef=1,nef
        do i=1,na
            if (labWcube(i,ef,em,vf,vm,het,ti) > 0.0d0) then
                FracWomenLabForByAge(ti) = FracWomenLabForByAge(ti) + psiW(i,ef,em,vf,vm,het,ti)
            end if                                             
        end do
        end do
        end do
        end do
        end do
        end do
        end do
        
        print *, "here 1c"
        
        do ti=1,nw
        do het=1,nhet
            fet = int((het+1)/2) 
            met = mod(het+1,2)+1      
        do vm=1,naem
        do vf=1,naef
        do em=1,nem
        do ef=1,nef
        do i=1,na
            AveFemaleHoursByAge(ti) = AveFemaleHoursByAge(ti) + labWcube(i,ef,em,vf,vm,het,ti)*psiW(i,ef,em,vf,vm,het,ti)               
            if (labWcube(i,ef,em,vf,vm,het,ti) > 0.0d0) then  
                tempR = labWcube(i,ef,em,vf,vm,het,ti)*psiW(i,ef,em,vf,vm,het,ti)/FracWomenLabForByAge(ti)
                !if (.NOT. ieee_is_nan(tempR)) then
                !if (.NOT. isnan(tempR)) then
                if (tempR .ne. tempR) then
                    AveFemaleHoursCondWorkByAge(ti) = AveFemaleHoursCondWorkByAge(ti) + tempR          
                end if                                       
                tempR = (1-labWcube(i,ef,em,vf,vm,het,ti))/labWcube(i,ef,em,vf,vm,het,ti)/gamma* &
psiW(i,ef,em,vf,vm,het,ti)/FracWomenLabForByAge(ti)*CohortWeights(ti) 
                !if (.NOT. ieee_is_nan(tempR)) then
                !if (.NOT. isnan(tempR)) then
                if (tempR .ne. tempR) then
                    AveFrischElasticityFW = AveFrischElasticityFW + tempR
                end if            
            end if     
            AveFemaleEarnings = AveFemaleEarnings + w*labWcube(i,ef,em,vf,vm,het,ti)* & 
efmat(ef,em,fet,ti)*psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti) 
            AveHHEarnings = AveHHEarnings + w*labWcube(i,ef,em,vf,vm,het,ti)*efmat(ef,em,fet,ti)* &
psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti) &
             +  w*hbar*emmat(ef,em,met,ti)*psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti) 
            AveMaleEarningsCheck = AveMaleEarningsCheck + w*hbar*emmat(ef,em,met,ti)* & 
psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti) 
        end do
        end do
        end do
        end do
        end do
        end do
        end do   
        AveFemaleEarnings = AveFemaleEarnings/sum(CohortWeights(:nw))
        AveHHEarnings = AveHHEarnings/sum(CohortWeights(:nw))
        AveMaleEarningsCheck = AveMaleEarningsCheck/sum(CohortWeights(:nw))
        print *, "AveMaleEarningsCheck=", AveMaleEarningsCheck
        !print *, "here 1d"
        !pause
             
        AveFrischElasticityFW = AveFrischElasticityFW/sum(CohortWeights(:nw))
        AveFemaleHours = sum(AveFemaleHoursByAge*CohortWeights(:nw))/sum(CohortWeights(:nw))
        !print *, "AveFemaleHoursCondWorkByAge", AveFemaleHoursCondWorkByAge
        !print *, "CohortWeights(:nw)", CohortWeights(:nw)
        !print *, "numerater=", sum(AveFemaleHoursCondWorkByAge*CohortWeights(:nw))
        !print *, "denominator=", sum(CohortWeights(:nw))
        AveFemaleHoursCondWork = sum(AveFemaleHoursCondWorkByAge*CohortWeights(:nw))/sum(CohortWeights(:nw))
        AveFrischElasticityF = (1-AveFemaleHours)/AveFemaleHours/gamma
        AveFrischElasticityM = (1-hbar)/hbar/gamma    
        
         

        AggConsAtDeath = sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeMarried(:,:,:,:,:,4,:) * &
 psiRMarried(:,:,:,:,:,4,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidow(:,:,:,:,:,2,:) * &
psiRWidow(:,:,:,:,:,2,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidower(:,:,:,:,:,2,:) * & 
psiRWidower(:,:,:,:,:,2,:),5),4),3),2),1),1)
        
        
        AggConsExcludeDeath = AggCons - AggConsAtDeath  
        
        AggConsNHResidents = sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeMarried(:,5,:,:,:,:,:) * & 
psiRMarried(:,5,:,:,:,:,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidow(:,5,:,:,:,:,:) * & 
psiRWidow(:,5,:,:,:,:,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidower(:,5,:,:,:,:,:) * &
 psiRWidower(:,5,:,:,:,:,:),5),4),3),2),1),1)
        AggConsNHResidents = AggConsNHResidents + sum(CohortWeights(nw+1:)* & 
sum(sum(sum(sum(sum(consumptionRcubeMarried(:,10,:,:,:,:,:) * & 
psiRMarried(:,10,:,:,:,:,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidow(:,10,:,:,:,:,:) * &
 psiRWidow(:,10,:,:,:,:,:),5),4),3),2),1),1) & 
                  + sum(CohortWeights(nw+1:)*sum(sum(sum(sum(sum(consumptionRcubeWidower(:,10,:,:,:,:,:) * &
 psiRWidower(:,10,:,:,:,:,:),5),4),3),2),1),1)
        
        
        print *, "here 2"    
        
        WealthByAgeID2 = 0.0d0
        do ti=1,nw
        do i=1,na
            WealthByAgeID2(ti) = WealthByAgeID2(ti) + sum(avect(i) * psiW(i,:,:,:,:,:,ti))
        end do
            ConsByAgeID(ti) = sum(consumptionWCube(:,:,:,:,:,:,ti) * psiW(:,:,:,:,:,:,ti))
        end do
                
        
        do ti = 1,nr            
        do s = 1,3
            if (s==1) then
                ht = nhht
            else
                ht = nsht
            end if
         do h = 1,ht
         do vm = 1,naem
         do vf = 1,naef
         do j = 1,nm
         do i = 1,na 
             if (s==1) then
                WealthByAgeID2(nw+ti) = WealthByAgeID2(nw+ti) + avect(i) * sum(psiRMarried(i,j,vf,vm,h,:,ti))  
                ConsByAgeID(nw+ti) = ConsByAgeID(nw+ti)  +  sum(consumptionRCubeMarried(i,j,vf,vm,h,:,ti) * & 
psiRMarried(i,j,vf,vm,h,:,ti)) 
            else if (s==2) then
                WealthByAgeID2(nw+ti) = WealthByAgeID2(nw+ti) + avect(i) * sum(psiRWidow(i,j,vf,vm,h,:,ti))   
                ConsByAgeID(nw+ti) = ConsByAgeID(nw+ti)  + sum(consumptionRCubeWidow(i,j,vf,vm,h,:,ti) * &
 psiRWidow(i,j,vf,vm,h,:,ti))     
            else
                WealthByAgeID2(nw+ti) = WealthByAgeID2(nw+ti) + avect(i) * sum(psiRWidower(i,j,vf,vm,h,:,ti)) 
                ConsByAgeID(nw+ti) =  ConsByAgeID(nw+ti)  + sum(consumptionRCubeWidower(i,j,vf,vm,h,:,ti) * & 
psiRWidower(i,j,vf,vm,h,:,ti))       
            end if                         
         end do
         end do
         end do 
         end do
         end do
         end do       
         end do               
        
         print *, "here 3"  
        
        EarnsFbyAgeID =0.0d0 
        EarnsMbyAgeID =0.0d0 
        AveEarnsFbyAgeID =0.0d0 
        AveEarnsMbyAgeID =0.0d0     
        FracIndGovtTransByAgeID = 0.0d0    
        do ti=1,nw
        do het=1,nhet
            fet = int((het+1)/2)
            met = mod(het+1,2)+1
        do vm=1,naem
        do vf=1,naef
        do em=1,nem
        do ef=1,nef
        do i=1,na
            EarnsFbyAgeID(ti) = EarnsFbyAgeID(ti) + w*efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti)* & 
psiW(i,ef,em,vf,vm,het,ti) 
            EarnsMbyAgeID(ti) = EarnsMbyAgeID(ti) + w*emmat(ef,em,met,ti)*hbar*psiW(i,ef,em,vf,vm,het,ti) 
            AveEarnsFbyAgeID(ti) = AveEarnsFbyAgeID(ti) +aveEarnFvect(vf)*psiW(i,ef,em,vf,vm,het,ti) 
            AveEarnsMbyAgeID(ti) = AveEarnsMbyAgeID(ti) + aveEarnMvect(vm)*psiW(i,ef,em,vf,vm,het,ti) 
            preTaxIncomeF = w*efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti) &
                - SocSecTax(w*efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti)) + 0.5d0*r* avect(i)  &
                - EarnsTaxFn(w*efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti))
            preTaxIncomeM = w*emmat(ef,em,met,ti)*hbar - SocSecTax(w*emmat(ef,em,met,ti)*hbar) + 0.5d0*r* & 
avect(i) - EarnsTaxFn(w*emmat(ef,em,met,ti)*hbar)                                     
            postTaxWealth = preTaxIncomeF + preTaxIncomeM + avect(i) - & 
IncomeTax(preTaxIncomeF+preTaxIncomeM- max(0.0d0,r*avect(i)*capTax)) - & 
max(0.0d0,r*avect(i)*capTax)                                 
            !if (ti == 1) postTaxWealth = postTaxWealth + btran(het)
            if (transfers(postTaxWealth) > 0.0d0) then
                FracIndGovtTransByAgeID(ti) = FracIndGovtTransByAgeID(ti) + psiW(i,ef,em,vf,vm,het,ti)                          
            end if
        end do
        end do
        end do
        end do
        end do
        end do 
        end do
        
        print *, "here 4"  
        
        do s=1,3
        do vm=1,naem
        do vf=1,naef
            SocSecArray(vf,vm,s)  = socsec(aveEarnFvect(vf),aveEarnMvect(vm),s)   
        end do
        end do
        end do
                
        SocSecByAgeID = 0.0d0  
        ConsNHMedicaid = 0.0d0 
        MedExpsNHMedicaid = 0.0d0 
        FracNHPrivate = 0.0d0  
        ConsNHPrivate = 0.0d0 
        MedExpsNHPrivate = 0.0d0 
        FracNHPrivate = 0.0d0    
        s=1 !currently married
        do ti=1,nr 
            do d=1,4                                                       
            do h=1,nhht  
            do vm=1,naem
            do vf=1,naef            
            do j=1,nm
            do i=1,na                        
                preTaxIncome = SocSecArray(vf,vm,s) + avect(i) * r      
                IncTaxRet = IncomeTax(avect(i)*r- max(0.0d0,avect(i)*r* & 
capTax),SocSecArray(vf,vm,s),mmat(j,h,d,s,ti),s)            
                postTaxWealth = preTaxIncome + avect(i) - IncTaxRet - max(0.0d0,avect(i)*r*capTax)            
                TransferRet = transfersRetired(postTaxWealth, ti, j,h,d,s,i)                                                             
                if(TransferRet == 0.0d0) then
                    OOPExpArrayMarried(i,j,vf,vm,h,d,ti) = mmat(j,h,d,s,ti) 
                    if (j==5 .or. j==10) then
                        ConsNHPrivate(ti) = ConsNHPrivate(ti) + consumptionRCubeMarried(i,j,vf,vm,h,d,1) * & 
psiRMarried(i,j,vf,vm,h,d,ti)
                        MedExpsNHPrivate(ti) = MedExpsNHPrivate(ti) + mmat(j,h,d,s,ti)*psiRMarried(i,j,vf,vm,h,d,ti) 
                        FracNHPrivate(ti) =  FracNHPrivate(ti) + psiRMarried(i,j,vf,vm,h,d,ti)             
                    end if                      
                else
                !    if (j==5 .or. j==10) then
                !        OOPExpArrayMarried(i,j,vf,vm,h,d,ti) = 0.0d0
                !    else
                         OOPExpArrayMarried(i,j,vf,vm,h,d,ti) = min((1-FracMedicaidCovers)*mmat(j,h,d,s,ti) , & 
clowerbarMarried/2)                
                !    end if
                    if (j==5 .or. j==10) then
                        ConsNHMedicaid(ti) = ConsNHMedicaid(ti) + consumptionRCubeMarried(i,j,vf,vm,h,d,1)* & 
psiRMarried(i,j,vf,vm,h,d,ti)
                        MedExpsNHMedicaid(ti) = MedExpsNHMedicaid(ti) + mmat(j,h,d,s,ti)*psiRMarried(i,j,vf,vm,h,d,ti)
                        FracNHMedicaid(ti) =  FracNHMedicaid(ti) + psiRMarried(i,j,vf,vm,h,d,ti)                             
                    end if  
                                   
                end if  
                SocSecByAgeID(ti) = SocSecByAgeID(ti) + SocSecArray(vf,vm,s)*psiRMarried(i,j,vf,vm,h,d,ti)
                if (ti==1) then
                    AveEarnsFbyAgeID(nw+1) = AveEarnsFbyAgeID(nw+1) +aveEarnFvect(vf)*psiRMarried(i,j,vf,vm,h,d,1) 
                    AveEarnsMbyAgeID(nw+1) = AveEarnsMbyAgeID(nw+1) + aveEarnMvect(vm)*psiRMarried(i,j,vf,vm,h,d,1)             
                end if                                         
            end do
            end do                
            end do  
            end do        
            end do  
            end do         
                                      
            s=2  !currently widow     
            do d=1,2                                          
            do h=1,nsht  
            do vm=1,naem
            do vf=1,naef            
            do j=1,nm
            do i=1,na             
                preTaxIncome = SocSecArray(vf,vm,s) + avect(i) * r      
                IncTaxRet = IncomeTax(avect(i)*r- max(0.0d0,avect(i)*r*capTax),SocSecArray(vf,vm,s),mmat(j,h,d,s,ti),s)            
                postTaxWealth = preTaxIncome + avect(i) - IncTaxRet - max(0.0d0,avect(i)*r*capTax)            
                TransferRet = transfersRetired(postTaxWealth, ti, j,h,d,s,i)                                                             
                if(TransferRet == 0.0d0) then
                    OOPExpArrayWidow(i,j,vf,vm,h,d,ti) = mmat(j,h,d,s,ti)
                    if (j==5 .or. j==10) then
                        ConsNHPrivate(ti) = ConsNHPrivate(ti) + consumptionRCubeWidow(i,j,vf,vm,h,d,1)* &
psiRWidow(i,j,vf,vm,h,d,ti)
                        MedExpsNHPrivate(ti) = MedExpsNHPrivate(ti) + mmat(j,h,d,s,ti)*psiRWidow(i,j,vf,vm,h,d,ti) 
                        FracNHPrivate(ti) =  FracNHPrivate(ti) + psiRWidow(i,j,vf,vm,h,d,ti)             
                    end if  
                else
                !    if (j==5 .or. j==10) then
                !        OOPExpArrayWidow(i,j,vf,vm,h,d,ti) = 0.0d0
                !    else
                        OOPExpArrayWidow(i,j,vf,vm,h,d,ti) = min((1-FracMedicaidCovers)*mmat(j,h,d,s,ti) , & 
clowerbarWidow/2)      
                !    end if   
                    if (j==5 .or. j==10) then
                        ConsNHMedicaid(ti) = ConsNHMedicaid(ti) + consumptionRCubeWidow(i,j,vf,vm,h,d,1)* & 
psiRWidow(i,j,vf,vm,h,d,ti)
                        MedExpsNHMedicaid(ti) = MedExpsNHMedicaid(ti) + mmat(j,h,d,s,ti)*psiRWidow(i,j,vf,vm,h,d,ti) 
                        FracNHMedicaid(ti) =  FracNHMedicaid(ti) + psiRWidow(i,j,vf,vm,h,d,ti)             
                    end if                  
                end if  
                SocSecByAgeID(ti) = SocSecByAgeID(ti) + SocSecArray(vf,vm,s)*psiRWidow(i,j,vf,vm,h,d,ti)
                if (ti==1) then
                    AveEarnsFbyAgeID(nw+1) = AveEarnsFbyAgeID(nw+1) +aveEarnFvect(vf)*psiRWidow(i,j,vf,vm,h,d,1)
                    AveEarnsMbyAgeID(nw+1) = AveEarnsMbyAgeID(nw+1) + aveEarnMvect(vm)*psiRWidow(i,j,vf,vm,h,d,1)   
                end if         
            end do
            end do
            end do                
            end do  
            end do 
            end do                   
            
            s=3 !currently widower                                        
            do d=1,2
            do h=1,nsht  
            do vm=1,naem
            do vf=1,naef            
            do j=1,nm
            do i=1,na             
                preTaxIncome = SocSecArray(vf,vm,s) + avect(i) * r      
                IncTaxRet = IncomeTax(avect(i)*r- max(0.0d0,avect(i)*r*capTax),SocSecArray(vf,vm,s),mmat(j,h,d,s,ti),s)            
                postTaxWealth = preTaxIncome + avect(i) - IncTaxRet - max(0.0d0,avect(i)*r*capTax)            
                TransferRet = transfersRetired(postTaxWealth, ti, j,d,h,s,i)                                                             
                if(TransferRet == 0.0d0) then
                    OOPExpArrayWidower(i,j,vf,vm,h,d,ti) = mmat(j,h,d,s,ti) 
                    if (j==5 .or. j==10) then
                        ConsNHPrivate(ti) = ConsNHPrivate(ti) + consumptionRCubeWidower(i,j,vf,vm,h,d,1)* & 
psiRWidower(i,j,vf,vm,h,d,ti)
                        MedExpsNHPrivate(ti) = MedExpsNHPrivate(ti) + mmat(j,h,d,s,ti)*psiRWidower(i,j,vf,vm,h,d,ti) 
                        FracNHPrivate(ti) =  FracNHPrivate(ti) + psiRWidower(i,j,vf,vm,h,d,ti)                      
                    end if                     
                else
                !    if (j==5 .or. j==10) then
                !        OOPExpArrayWidower(i,j,vf,vm,h,d,ti) = 0.0d0
                !    else
                        OOPExpArrayWidower(i,j,vf,vm,h,d,ti) = min((1-FracMedicaidCovers)*mmat(j,h,d,s,ti) , & 
clowerbarWidower/2)      
                !    end if
                    if (j==5 .or. j==10) then
                        ConsNHMedicaid(ti) = ConsNHMedicaid(ti) + consumptionRCubeWidower(i,j,vf,vm,h,d,1)* & 
psiRWidower(i,j,vf,vm,h,d,ti)
                        MedExpsNHMedicaid(ti) = MedExpsNHMedicaid(ti) + mmat(j,h,d,s,ti)* &
psiRWidower(i,j,vf,vm,h,d,ti) 
                        FracNHMedicaid(ti) =  FracNHMedicaid(ti) + psiRWidower(i,j,vf,vm,h,d,ti)                      
                    end if                 
                end if  
                SocSecByAgeID(ti) = SocSecByAgeID(ti) + SocSecArray(vf,vm,s)*psiRWidower(i,j,vf,vm,h,d,ti)
                if (ti==1) then
                    AveEarnsFbyAgeID(nw+1) = AveEarnsFbyAgeID(nw+1) +aveEarnFvect(vf)*psiRWidower(i,j,vf,vm,h,d,1)
                    AveEarnsMbyAgeID(nw+1) = AveEarnsMbyAgeID(nw+1) + aveEarnMvect(vm)*psiRWidower(i,j,vf,vm,h,d,1)    
                end if                                      
            end do
            end do
            end do                
            end do  
            end do    
            end do    
        end do           

        !ByAge stats
        do ti = 1,nr     
            FracHHTopMedShockByAge(ti) = FracHHTopMedShockByAge(ti) + & 
sum(psiRMarried(:,5,:,:,:,:,ti)) + sum(psiRMarried(:,10,:,:,:,:,ti)) + &
                                         sum(psiRWidow(:,5,:,:,:,:,ti)) + sum(psiRWidow(:,10,:,:,:,:,ti)) + &
                                         sum(psiRWidower(:,5,:,:,:,:,ti)) + sum(psiRWidower(:,10,:,:,:,:,ti))               
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else
                ht = nsht
                dt = 2
            end if
         do d=1,dt
         do h = 1,ht
         do vm = 1,naem
         do vf = 1,naef
         do j = 1,nm
         do i = 1,na
            if (s==1) then
                OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
            else if (s==2) then
                OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
            else
                OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
            end if            
            if (OOPExpenses>0.0d0) then
                if (s==1) then
                    FracPosOOPByAge(ti) =  FracPosOOPByAge(ti) +psiRMarried(i,j,vf,vm,h,d,ti)   
                else if (s==2) then
                    FracPosOOPByAge(ti) =  FracPosOOPByAge(ti) +psiRWidow(i,j,vf,vm,h,d,ti)  
                else
                    FracPosOOPByAge(ti) =  FracPosOOPByAge(ti) +psiRWidower(i,j,vf,vm,h,d,ti)  
                end if
            end if
            preTaxIncome = SocSecArray(vf,vm,1) + avect(i) * r       
            IncTaxRet = IncomeTax(avect(i)*r- max(0.0d0,avect(i)*r*capTax),SocSecArray(vf,vm,s),mmat(j,h,d,s,ti),s)                    
            postTaxWealth = preTaxIncome + avect(i) - IncTaxRet - max(0.0d0,avect(i)*r*capTax) 
            if (transfersRetired(postTaxWealth, ti, j,d,h,s,i) >0.0d0) then
                if (s==1) then
                    FracMedicaidByAge(ti) =  FracMedicaidByAge(ti) +psiRMarried(i,j,vf,vm,h,d,ti)                    
                    FracMarriedMedicaidByAge(ti) = FracMarriedMedicaidByAge(ti) +psiRMarried(i,j,vf,vm,h,d,ti)    
                    MeanOOPExpsMedicaidByAge(ti) =  MeanOOPExpsMedicaidByAge(ti) + &
OOPExpenses*psiRMarried(i,j,vf,vm,h,d,ti)    
                else if (s==2) then
                    FracMedicaidByAge(ti) =  FracMedicaidByAge(ti) +psiRWidow(i,j,vf,vm,h,d,ti)  
                    FracWidowsMedicaidByAge(ti) = FracWidowsMedicaidByAge(ti) +psiRWidow(i,j,vf,vm,h,d,ti)  
                    MeanOOPExpsMedicaidByAge(ti) = MeanOOPExpsMedicaidByAge(ti) + & 
OOPExpenses*psiRWidow(i,j,vf,vm,h,d,ti)  
                else
                    FracMedicaidByAge(ti) =  FracMedicaidByAge(ti) +psiRWidower(i,j,vf,vm,h,d,ti) 
                    FracWidowersMedicaidByAge(ti) = FracWidowersMedicaidByAge(ti) +psiRWidower(i,j,vf,vm,h,d,ti)   
                    MeanOOPExpsMedicaidByAge(ti) = MeanOOPExpsMedicaidByAge(ti) + & 
OOPExpenses*psiRWidower(i,j,vf,vm,h,d,ti)   
                end if                
            end if              
            if (s==1) then
                MeanOOPExpByAge(ti) =  MeanOOPExpByAge(ti) + OOPExpenses*psiRMarried(i,j,vf,vm,h,d,ti)                  
            else if (s==2) then
                MeanOOPExpByAge(ti) =  MeanOOPExpByAge(ti) + OOPExpenses*psiRWidow(i,j,vf,vm,h,d,ti)                        
            else
                MeanOOPExpByAge(ti) =  MeanOOPExpByAge(ti) + OOPExpenses*psiRWidower(i,j,vf,vm,h,d,ti)                  
            end if                     
        end do
        end do 
        end do
        end do
        end do 
        end do
        end do        
        end do
        FracWidowsMedicaidByAge = FracWidowsMedicaidByAge/MaritalDist(2,:)
        FracWidowersMedicaidByAge = FracWidowersMedicaidByAge/MaritalDist(3,:)
        FracMarriedMedicaidByAge = FracMarriedMedicaidByAge/MaritalDist(1,:)
        MeanOOPExpsMedicaidByAge = MeanOOPExpsMedicaidByAge/FracMedicaidByAge
        FracIndMedicaidByAge = FracWidowsMedicaidByAge*FracIndByMaritalStatusAge(2,:) + &
 FracWidowersMedicaidByAge*FracIndByMaritalStatusAge(3,:) + &
            FracMarriedMedicaidByAge*FracIndByMaritalStatusAge(1,:)
        FracIndGovtTransByAgeID(nw+1:) = FracIndMedicaidByAge
        FracWidowsMedicaid = sum(FracWidowsMedicaidByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
        Frac6566WidowsMedicaid = sum(FracWidowsMedicaidByAge(1:2)*CohortWeights(nw+1:nw+2))/ & 
sum(CohortWeights(nw+1:nw+2))
        Frac6574WidowsMedicaid = sum(FracWidowsMedicaidByAge(1:5)*CohortWeights(nw+1:nw+5))/ &
sum(CohortWeights(nw+1:nw+5))    
        Frac7584WidowsMedicaid = sum(FracWidowsMedicaidByAge(6:10)*CohortWeights(nw+6:nw+10))/ & 
sum(CohortWeights(nw+6:nw+10))    
        Frac85plusWidowsMedicaid = sum(FracWidowsMedicaidByAge(11:)*CohortWeights(nw+11:))/ &
sum(CohortWeights(nw+11:))    
        
        FracWidowersMedicaid = sum(FracWidowersMedicaidByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))  
        Frac6566WidowersMedicaid = sum(FracWidowersMedicaidByAge(1:2)*CohortWeights(nw+1:nw+2))/& 
sum(CohortWeights(nw+1:nw+2))  
        Frac6574WidowersMedicaid = sum(FracWidowersMedicaidByAge(1:5)*CohortWeights(nw+1:nw+5))/& 
sum(CohortWeights(nw+1:nw+5))
        Frac7584WidowersMedicaid = sum(FracWidowersMedicaidByAge(6:10)*CohortWeights(nw+6:nw+10))/ & 
sum(CohortWeights(nw+6:nw+10))    
        Frac85plusWidowersMedicaid = sum(FracWidowersMedicaidByAge(11:)*CohortWeights(nw+11:))/ & 
sum(CohortWeights(nw+11:))    
        
        FracMarriedMedicaid = sum(FracMarriedMedicaidByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
        Frac6566MarriedMedicaid = sum(FracMarriedMedicaidByAge(1:2)*CohortWeights(nw+1:nw+2))/ & 
sum(CohortWeights(nw+1:nw+2))
        Frac6574MarriedMedicaid = sum(FracMarriedMedicaidByAge(1:5)*CohortWeights(nw+1:nw+5))/ & 
sum(CohortWeights(nw+1:nw+5))
        Frac7584MarriedMedicaid = sum(FracMarriedMedicaidByAge(6:10)*CohortWeights(nw+6:nw+10))/ & 
sum(CohortWeights(nw+6:nw+10))    
        Frac85plusMarriedMedicaid = sum(FracMarriedMedicaidByAge(11:)*CohortWeights(nw+11:))/ & 
sum(CohortWeights(nw+11:))    
        
        Frac65plusMedicaid = sum(FracMedicaidByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
        Frac6574Medicaid = sum(FracMedicaidByAge(1:5)*CohortWeights(nw+1:nw+5))/sum(CohortWeights(nw+1:nw+5))
        Frac7584Medicaid = sum(FracMedicaidByAge(6:10)*CohortWeights(nw+6:nw+10))/sum(CohortWeights(nw+6:nw+10))
        Frac85plusMedicaid = sum(FracMedicaidByAge(11:)*CohortWeights(nw+11:))/sum(CohortWeights(nw+11:))
        
        Frac65plusHHTopMedShock = sum(FracHHTopMedShockByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
        Frac6574HHTopMedShock = sum(FracHHTopMedShockByAge(1:5)*CohortWeights(nw+1:nw+5))/ & 
sum(CohortWeights(nw+1:nw+5))
        Frac7584HHTopMedShock = sum(FracHHTopMedShockByAge(6:10)*CohortWeights(nw+6:nw+10))/ & 
sum(CohortWeights(nw+6:nw+10))
        Frac85plusHHTopMedShock = sum(FracHHTopMedShockByAge(11:)*CohortWeights(nw+11:))/ & 
sum(CohortWeights(nw+11:))        
        
        GovTransfersTo65Plus = sum(CohortWeights(nw+1:) * GovTransfersVect(nw+1:))
        MedicaidTransfers = sum(CohortWeights(nw+1:) * (TotMedByAgeID - MeanOOPExpByAge))
        
        MeanOOPExpByAgeCond = MeanOOPExpByAge/FracPosOOPByAge 
        
        OOPExpOverOutputByAge(1) = sum(CohortWeights(nw+1:nw+5) * MeanOOPExpByAge(:5),1)/AggOutput*100
        OOPExpOverOutputByAge(2) = sum(CohortWeights(nw+6:nw+10) * MeanOOPExpByAge(6:10),1)/AggOutput*100
        OOPExpOverOutputByAge(3) = sum(CohortWeights(nw+11:) * MeanOOPExpByAge(11:),1)/AggOutput*100         
        MedicaidExpOverOutputByAge(1) = sum(CohortWeights(nw+1:nw+5) * (TotMedByAgeID(:5) - MeanOOPExpByAge(:5)),1)/ & 
AggOutput*100
        MedicaidExpOverOutputByAge(2) = sum(CohortWeights(nw+6:nw+10) * (TotMedByAgeID(6:10) - MeanOOPExpByAge(6:10)),1)/ &
AggOutput*100
        MedicaidExpOverOutputByAge(3) = sum(CohortWeights(nw+11:) * (TotMedByAgeID(11:) - MeanOOPExpByAge(11:)),1)/ & 
AggOutput*100                        
        
        FracIndMedicaid = sum(FracIndMedicaidByAge*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
        Frac6574IndMedicaid = sum(FracIndMedicaidByAge(:5)*CohortWeights(nw+1:nw+5))/sum(CohortWeights(nw+1:nw+5))
        Frac7584IndMedicaid = sum(FracIndMedicaidByAge(6:10)*CohortWeights(nw+6:nw+10))/sum(CohortWeights(nw+6:nw+10))
        Frac85plusIndMedicaid = sum(FracIndMedicaidByAge(11:)*CohortWeights(nw+11:))/sum(CohortWeights(nw+11:))
        FracIndRetsGovtTrans = FracIndMedicaid
        FracIndWorksGovtTrans = sum(FracIndGovtTransByAgeID(:nw)*CohortWeights(:nw))/sum(CohortWeights(:nw))        
        
        AveNHExpensesWidows = 0.0d0
        AveNonNHExpensesWidows = 0.0d0
        AveExpensesWidowers = 0.0d0
        AveNHExpensesWidowers = 0.0d0
        AveNonNHExpensesWidowers = 0.0d0
        AveExpensesWidowers = 0.0d0
        AveNHExpensesMarried = 0.0d0
        AveNonNHExpensesMarried = 0.0d0
        AveExpensesMarried = 0.0d0                
        AggNHExp = 0.0d0
        do ti=1,nr
            do d=1,2
            do h = 1,nsht
                AveNHExpensesWidows(ti) = AveNHExpensesWidows(ti) + mmat(5,h,d,2,ti)* & 
                sum(psiRWidow(:,5,:,:,h,d,ti)) + mmat(10,h,d,2,ti)*sum(psiRWidow(:,10,:,:,h,d,ti))   
                do j=1,4
                AveNonNHExpensesWidows(ti) = AveNonNHExpensesWidows(ti) + mmat(j,h,d,2,ti)* & 
sum(psiRWidow(:,j,:,:,h,d,ti)) + mmat(5+j,h,d,2,ti)*sum(psiRWidow(:,5+j,:,:,h,d,ti)) 
                end do
                do j=1,10
                    AveExpensesWidows(ti) = AveExpensesWidows(ti) + mmat(j,h,d,2,ti)*sum(psiRWidow(:,j,:,:,h,d,ti))      
                end do
                
                AveNHExpensesWidowers(ti) = AveNHExpensesWidowers(ti) + mmat(5,h,d,3,ti)*sum(psiRWidower(:,5,:,:,h,d,ti)) &
                    + mmat(10,h,d,3,ti)*sum(psiRWidower(:,10,:,:,h,d,ti))   
                do j=1,4
                AveNonNHExpensesWidowers(ti) = AveNonNHExpensesWidowers(ti) + mmat(j,h,d,3,ti)*sum(psiRWidower(:,j,:,:,h,d,ti)) &
                    + mmat(5+j,h,d,3,ti)*sum(psiRWidower(:,5+j,:,:,h,d,ti)) 
                end do
                do j=1,10
                    AveExpensesWidowers(ti) = AveExpensesWidowers(ti) + mmat(j,h,d,3,ti)*sum(psiRWidower(:,j,:,:,h,d,ti))      
                end do
                                                                
            end do
            end do
            
            do d=1,4
            do h = 1,nhht               
                AveNHExpensesMarried(ti) = AveNHExpensesMarried(ti) + mmat(5,h,d,1,ti)*sum(psiRMarried(:,5,:,:,h,d,ti)) &
                    + mmat(10,h,d,1,ti)*sum(psiRMarried(:,10,:,:,h,d,ti))   
                do j=1,4
                AveNonNHExpensesMarried(ti) = AveNonNHExpensesMarried(ti) + mmat(j,h,d,1,ti)*sum(psiRMarried(:,j,:,:,h,d,ti)) &
                    + mmat(5+j,h,d,1,ti)*sum(psiRMarried(:,5+j,:,:,h,d,ti)) 
                end do
                do j=1,10
                    AveExpensesMarried(ti) = AveExpensesMarried(ti) + mmat(j,h,d,1,ti)*sum(psiRMarried(:,j,:,:,h,d,ti))      
                end do
                                
            end do
            end do       
            
            AggNHExp = AggNHExp + CohortWeights(nw+ti) * (AveNHExpensesWidows(ti)+AveNHExpensesWidowers(ti) & 
+AveNHExpensesMarried(ti))
                 
            AveNHExpensesWidows(ti) = AveNHExpensesWidows(ti)/(sum(psiRWidow(:,5,:,:,1:2,1:2,ti))+ & 
sum(psiRWidow(:,10,:,:,1:2,1:2,ti)))
            AveNonNHExpensesWidows(ti) = AveNonNHExpensesWidows(ti)/(sum(psiRWidow(:,1:4,:,:,1:2,1:2,ti))+ & 
sum(psiRWidow(:,6:10,:,:,1:2,1:2,ti)))
            AveExpensesWidows(ti) = AveExpensesWidows(ti)/sum(psiRWidow(:,:,:,:,1:2,1:2,ti)) 
            
            AveNHExpensesWidowers(ti) = AveNHExpensesWidowers(ti)/(sum(psiRWidower(:,5,:,:,1:2,1:2,ti))+ & 
sum(psiRWidower(:,10,:,:,1:2,1:2,ti)))
            AveNonNHExpensesWidowers(ti) = AveNonNHExpensesWidowers(ti)/(sum(psiRWidower(:,1:4,:,:,1:2,1:2,ti))+ &
sum(psiRWidower(:,6:10,:,:,1:2,1:2,ti)))
            AveExpensesWidowers(ti) = AveExpensesWidowers(ti)/sum(psiRWidower(:,:,:,:,1:2,1:2,ti))  
            
            AveNHExpensesMarried(ti) = AveNHExpensesMarried(ti)/(sum(psiRMarried(:,5,:,:,:,:,ti))+ & 
sum(psiRMarried(:,10,:,:,:,:,ti)))
            AveNonNHExpensesMarried(ti) = AveNonNHExpensesMarried(ti)/(sum(psiRMarried(:,1:4,:,:,:,:,ti))+  &
sum(psiRMarried(:,6:10,:,:,:,:,ti)))
            AveExpensesMarried(ti) = AveExpensesMarried(ti)/sum(psiRMarried(:,:,:,:,:,:,ti))                                       
        end do

        print *, "AveNHExpensesWidows", AveNHExpensesWidows
        print *, "AveNonNHExpensesWidows", AveNonNHExpensesWidows
        print *, "AveExpensesWidows", AveExpensesWidows
        print *, "AveNHExpensesWidowers", AveNHExpensesWidowers
        print *, "AveNonNHExpensesWidowers", AveNonNHExpensesWidowers
        print *, "AveExpensesWidowers", AveExpensesWidowers
        print *, "AveNHExpensesMarried", AveNHExpensesMarried
        print *, "AveNonNHExpensesMarried", AveNonNHExpensesMarried
        print *, "AveExpensesMarried", AveExpensesMarried                
        !pause
        print *, "here 6"
        
        !aggregates
        AggOOPExp = sum(CohortWeights(nw+1:) * MeanOOPExpByAge)  
        MeanOOPExpCond = sum(CohortWeights(nw+1:) * MeanOOPExpByAgeCond,1)/sum(CohortWeights(nw+1:),1)        
        MeanOOPExp = AggOOPExp/sum(CohortWeights(nw+1:),1)      
        MeanOOPExpMedicaid = sum(CohortWeights(nw+1:) * MeanOOPExpsMedicaidByAge)/sum(CohortWeights(nw+1:),1) 
        print *, "MeanOOPExpMedicaid=", MeanOOPExpMedicaid
        
        AveTotExpsByMart = 0.0d0
        AveOOPExpsByMart = 0.0d0
        !AveTotExpsByMart(1) = sum(mmat(:,:,:,1,1)*MedExpDist(:,:,:,1,1))*CohortWeights(nw+1)        
        !AveOOPExpsByMart(1) = sum(OOPExpArray(:,:,:,:,:,1,1)*psiR(:,:,:,:,:,1,1))*CohortWeights(nw+1)
        AveTotExpsByHealMart = 0.0d0
        AveOOPExpsByHealMart = 0.0d0
        !do h=1,nhht     
        !    AveTotExpsByHealMart(h,1) = AveTotExpsByHealMart(h,1) + sum(mmat(:,h,:,1,1)*MedExpDist(:,h,:,1,1))/sum(MedExpDist(:,h,:,1,1))*CohortWeights(nw+1)   
        !    AveOOPExpsByHealMart(h,1) = AveOOPExpsByHealMart(h,1) + sum(OOPExpArray(:,:,:,:,h,1,1)*psiR(:,:,:,:,h,1,1))/sum(psiR(:,:,:,:,h,1,1))*CohortWeights(nw+1)          
        !end do        
        do ti=1,nr
        
            AveTotExpsByMart(1) = AveTotExpsByMart(1) + sum(mmat(:,:,:,1,ti)* & 
sum(sum(sum(psiRMarried(:,:,:,:,:,:,ti),4),3),1))/MaritalDist(1,ti)*CohortWeights(nw+ti)        
            AveOOPExpsByMart(1) = AveOOPExpsByMart(1) + sum(OOPExpArrayMarried(:,:,:,:,:,:,ti)* & 
psiRMarried(:,:,:,:,:,:,ti))/MaritalDist(1,ti)*CohortWeights(nw+ti)              
            do h=1,nhht   
                AveTotExpsByHealMart(h,1) = AveTotExpsByHealMart(h,1) + sum(mmat(:,h,:,1,ti)* & 
sum(sum(sum(psiRMarried(:,:,:,:,h,:,ti),4),3),1)/sum(sum(sum(psiRMarried(:,:,:,:,h,:,ti),4),3),1)) & 
*CohortWeights(nw+ti)   
                AveOOPExpsByHealMart(h,1) = AveOOPExpsByHealMart(h,1) + & 
sum(OOPExpArrayMarried(:,:,:,:,h,:,ti)*psiRMarried(:,:,:,:,h,:,ti)/ & 
psiRMarried(:,:,:,:,h,:,ti))*CohortWeights(nw+ti)          
            end do    
            AveTotExpsByMart(2) = AveTotExpsByMart(2) + sum(mmat(:,1:2,1:2,2,ti)* & 
sum(sum(sum(psiRWidow(:,:,:,:,:,:,ti),4),3),1))/MaritalDist(2,ti)*CohortWeights(nw+ti)        
            AveOOPExpsByMart(2) = AveOOPExpsByMart(2) + sum(OOPExpArrayWidow(:,:,:,:,:,:,ti)* & 
psiRWidow(:,:,:,:,:,:,ti))/MaritalDist(2,ti)*CohortWeights(nw+ti)              
            do h=1,nsht   
                AveTotExpsByHealMart(h,2) = AveTotExpsByHealMart(h,2) + sum(mmat(:,h,1:2,2,ti)* & 
sum(sum(sum(psiRWidow(:,:,:,:,h,:,ti),4),3),1)/sum(sum(sum(psiRWidow(:,:,:,:,h,:,ti),4),3),1))*CohortWeights(nw+ti)   
                AveOOPExpsByHealMart(h,2) = AveOOPExpsByHealMart(h,2) + sum(OOPExpArrayWidow(:,:,:,:,h,:,ti)* & 
psiRWidow(:,:,:,:,h,:,ti)/psiRWidow(:,:,:,:,h,:,ti))*CohortWeights(nw+ti)          
            end do  
            AveTotExpsByMart(3) = AveTotExpsByMart(3) + sum(mmat(:,1:2,1:2,3,ti)* & 
sum(sum(sum(psiRWidower(:,:,:,:,:,:,ti),4),3),1))/MaritalDist(3,ti)*CohortWeights(nw+ti)        
            AveOOPExpsByMart(3) = AveOOPExpsByMart(3) + sum(OOPExpArrayWidower(:,:,:,:,1:2,:,ti)* & 
psiRWidower(:,:,:,:,:,:,ti))/MaritalDist(3,ti)*CohortWeights(nw+ti)              
            do h=1,nsht   
                AveTotExpsByHealMart(h,3) = AveTotExpsByHealMart(h,3) + sum(mmat(:,h,1:2,3,ti)* & 
sum(sum(sum(psiRWidower(:,:,:,:,h,:,ti),4),3),1)/sum(sum(sum(psiRWidower(:,:,:,:,h,:,ti),4),3),1))* &
CohortWeights(nw+ti)   
                AveOOPExpsByHealMart(h,3) = AveOOPExpsByHealMart(h,3) + sum(OOPExpArrayWidower(:,:,:,:,h,:,ti) * &
psiRWidower(:,:,:,:,h,:,ti)/psiRWidower(:,:,:,:,h,:,ti))*CohortWeights(nw+ti)          
            end do                                       
        
        end do
        
        AveTotExpsByMart = AveTotExpsByMart/sum(CohortWeights(nw+1:))
        AveOOPExpsByMart = AveOOPExpsByMart/sum(CohortWeights(nw+1:))
        AveTotExpsByHealMart = AveTotExpsByHealMart/sum(CohortWeights(nw+1:))              
        AveOOPExpsByHealMart = AveOOPExpsByHealMart/sum(CohortWeights(nw+1:))       

        print *, "end of ComputeExPostStats1"

    end subroutine ComputeExPostStats1  

   
end module results

  
   
