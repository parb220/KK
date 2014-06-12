module results2
    use functions
    use output
    use simulations
    !use sorts
    use mathutils, only : linspace
    
    implicit none    
    contains
    
  
    subroutine ComputeExPostStats2()
        integer ti, s, vm, vf, j, i, st, nt, ht, h, dt, d
        real(dbl) preTaxIncomeF, preTaxIncomeM, tempR
        real(dbl) Xoop(GSm+1), Yoop(GSm+1), OOPgrid(GSm), weight, weight2, weight3, weight4
        real(dbl) AggOOPExpTemp, TotalWealth, TotalEarns, TotalLifeEarns, OOPExpenses
        real(dbl) XtotWealth(na+1), YtotWealth(na+1), XtotEarns(GSe+1), YtotEarns(GSe+1)
        
        
        !Welfare
        !Calculate average utility by age for welfare comparisions
        do ti=1,nw
            AveUtilByAgeID(ti) = sum(VnewWCube(:,:,:,:,:,:,ti)*psiW(:,:,:,:,:,:,ti))
        end do
        do ti=1,nr
            AveUtilByAgeID(nw+ti) = sum(VnewRCubeMarried(:,:,:,:,:,:,ti)*psiRMarried(:,:,:,:,:,:,ti)) + &
                                    sum(VnewRCubeWidow(:,:,:,:,:,:,ti)*psiRWidow(:,:,:,:,:,:,ti)) + &
                                    sum(VnewRCubeWidower(:,:,:,:,:,:,ti)*psiRWidower(:,:,:,:,:,:,ti))
        end do        
        do s=1,nhet
            AveUtilByEducID(s) = sum(VnewWCube(:,:,:,:,:,s,1)*psiW(:,:,:,:,:,s,1))/sum(psiW(:,:,:,:,:,s,1))
        end do     
        do ti=1,nw
            AveUtilConsByAgeID(ti) = sum(UtilConsWCube(:,:,:,:,:,:,ti)*psiW(:,:,:,:,:,:,ti))
        end do
        do ti=1,nr
            AveUtilConsByAgeID(nw+ti) = sum(UtilConsRCubeMarried(:,:,:,:,:,:,ti)*psiRMarried(:,:,:,:,:,:,ti)) + &
                                        sum(UtilConsRCubeWidow(:,:,:,:,:,:,ti)*psiRWidow(:,:,:,:,:,:,ti)) + &
                                        sum(UtilConsRCubeWidower(:,:,:,:,:,:,ti)*psiRWidower(:,:,:,:,:,:,ti))
        end do        
        do s=1,nhet
            AveUtilConsByEducID(s) = sum(UtilConsWCube(:,:,:,:,:,s,1)*psiW(:,:,:,:,:,s,1))/sum(psiW(:,:,:,:,:,s,1))
        end do             
        
        do ti=1,nw
            FracAtMaxAvectByAge(ti) = sum(psiW(na,:,:,:,:,:,ti))
            FracAtSecMaxAvectByAge(ti) = sum(psiW(na-1,:,:,:,:,:,ti))             
        end do
        do ti=1,nr
            FracAtMaxAvectByAge(nw+ti) = sum(psiRMarried(na,:,:,:,:,:,ti)) + sum(psiRWidow(na,:,:,:,:,:,ti)) + &
                                         sum(psiRWidower(na,:,:,:,:,:,ti))
            FracAtSecMaxAvectByAge(nw+ti) = sum(psiRMarried(na-1,:,:,:,:,:,ti)) + sum(psiRWidow(na-1,:,:,:,:,:,ti)) + &
                                            sum(psiRWidower(na-1,:,:,:,:,:,ti))
        end do
        do i=1,naef
            FracRetsAtAveEarnsF(i) = sum(psiRMarried(:,:,i,:,:,:,1)) + sum(psiRWidow(:,:,i,:,:,:,1)) + &
                                     sum(psiRWidower(:,:,i,:,:,:,1))       
        end do
        do i=1,naem
            FracRetsAtAveEarnsM(i) = sum(psiRMarried(:,:,:,i,:,:,1))  + sum(psiRWidow(:,:,:,i,:,:,1)) + &
                                     sum(psiRWidower(:,:,:,i,:,:,1))      
        end do
        
        !OOP Health Expense Distribution for 65-68 Year-olds
        OOPgrid = linspace(0.0d0, maxval((/maxval(OOPExpArrayMarried(:,:,:,:,:,:,:2)),maxval(OOPExpArrayWidow(:,:,:,:,:,:,:2)),maxval(OOPExpArrayWidower(:,:,:,:,:,:,:2))/)), GSm)
        AggOOPExpTemp = sum(MeanOOPExpByAge(:2)*CohortWeights(nw+1:nw+2))             
        Xoop = 0.0d0
        Yoop = 0.0d0
        do ti = 1,2
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else 
                ht = nsht
                dt = 2
            end if
        do d = 1,dt
        do h = 1,ht
        do vm = 1,naem
        do vf = 1,naef
        do j = 1,nm
        do i = 1,na         
           if (s==1) then
               tempR = psiRMarried(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
           else if (s==2) then
               tempR = psiRWidow(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
           else
               tempR = psiRWidower(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
           end if
           st = 1
           do 
                if (OOPExpenses <= OOPgrid(st) .or. st==GSm) then
                    Xoop(st+1) = Xoop(st+1) + tempR*CohortWeights(nw+ti)/sum(CohortWeights(nw+1:nw+2),1)
                    Yoop(st+1) = Yoop(st+1) + OOPExpenses/AggOOPExpTemp*tempR*CohortWeights(nw+ti)
                    exit
                else
                    st = st + 1
                end if
            end do           
        end do
        end do
        end do      
        end do   
        end do
        end do
        end do
        end do
        
        !Gini  
        do st = 2,GSm+1
            Xoop(st) = Xoop(st-1) + Xoop(st)
            Yoop(st) = Yoop(st-1) + Yoop(st)
        end do 
        OOPExpGiniYoung = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)       
        
        OOPmedShareYoung = 0.0d0
        OOPmedShareUpperYoung = 0.0d0
        j = 1
        do i = 1, GSm
            if (Xoop(i+1) > 0.2d0*j .and. j < 5) then
                if (Xoop(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (Xoop(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (Xoop(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1     
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight3
                            j = j + 1                                                 
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight4
                            OOPmedShareYoung(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight4 -weight3-weight2-weight)
                            j = j + 1                        
                        else                    
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1                        
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight3
                            OOPmedShareYoung(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                    
                    else
                        weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                        OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                        OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight2
                        OOPmedShareYoung(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareYoung(j) = OOPmedShareYoung(j) + (Yoop(i+1)-Yoop(i))*weight
                    OOPmedShareYoung(j+1) = (Yoop(i+1)-Yoop(i))*(1.0d0-weight)
                    j = j+1      
                end if       
            else
                OOPmedShareYoung(j) = OOPmedShareYoung(j) + Yoop(i+1)-Yoop(i)            
            end if  
            if (Xoop(i+1) > 0.9d0) then 
                if (Xoop(i) <= 0.9d0) then              
                    weight = (0.9d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpperYoung(1) = OOPmedShareUpperYoung(1) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpperYoung(1) = OOPmedShareUpperYoung(1) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if   
            if (Xoop(i+1) > 0.95d0) then 
                if (Xoop(i) <= 0.95d0) then              
                    weight = (0.95d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpperYoung(2) = OOPmedShareUpperYoung(2) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpperYoung(2) = OOPmedShareUpperYoung(2) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if 
            if (Xoop(i+1) > 0.99d0) then 
                if (Xoop(i) <= 0.99d0) then              
                    weight = (0.99d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpperYoung(3) = OOPmedShareUpperYoung(3) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpperYoung(3) = OOPmedShareUpperYoung(3) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if       
        end do           
        
      
        !OOP Health Expense Gini by Age
        OOPgrid = linspace(0.0d0, maxval((/maxval(OOPExpArrayMarried),maxval(OOPExpArrayWidow),maxval(OOPExpArrayWidower)/)), GSm)

        do ti = 1,nr
            Xoop = 0.0d0
            Yoop = 0.0d0
            do s = 1,3
                if (s==1) then
                    ht = nhht
                    dt = 4
                else
                    ht = nsht
                    dt = 2
                end if
            do d = 1,dt
            do h = 1,ht
            do vm = 1,naem
            do vf = 1,naef
            do j = 1,nm
            do i = 1,na                               
                if (s==1) then
                    tempR = psiRMarried(i,j,vf,vm,h,d,ti)
                    OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
                else if (s==2) then
                    tempR = psiRWidow(i,j,vf,vm,h,d,ti)
                    OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
                else
                    tempR = psiRWidower(i,j,vf,vm,h,d,ti)
                    OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
                end if                        
                st = 1
                do 
                    if (OOPExpenses <= OOPgrid(st) .or. st==GSm) then
                        Xoop(st+1) = Xoop(st+1) + tempR
                        Yoop(st+1) = Yoop(st+1) + OOPExpenses/MeanOOPExpByAge(ti)*tempR
                        exit
                    else
                        st = st + 1
                    end if
                end do   
            end do
            end do      
            end do   
            end do
            end do
            end do
            end do
                do st = 2,GSm+1
                    Xoop(st) = Xoop(st-1) + Xoop(st)
                    Yoop(st) = Yoop(st-1) + Yoop(st)
                end do 
                OOPExpGiniByAge(ti) = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)            
        end do 
        
        !OOP Health Expense Concentrations (All Retirees)
        Xoop = 0.0d0
        Yoop = 0.0d0
        do ti = 1,nr
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else 
                ht = nsht
                dt = 2
            end if
        do d = 1,dt
        do h = 1,ht
        do vm = 1,naem
        do vf = 1,naef
        do j = 1,nm
        do i = 1,na
           if (s==1) then
               tempR = psiRMarried(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
           else if (s==2) then
               tempR = psiRWidow(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
           else
               tempR = psiRWidower(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
           end if            
           st = 1
           do 
                if (OOPExpenses <= OOPgrid(st) .or. st==GSm) then
                    Xoop(st+1) = Xoop(st+1) + tempR*CohortWeights(nw+ti)/sum(CohortWeights(nw+1:),1)
                    Yoop(st+1) = Yoop(st+1) + OOPExpenses/AggOOPExp*tempR*CohortWeights(nw+ti)
                    exit
                else
                    st = st + 1
                end if
            end do           
        end do
        end do
        end do
        end do      
        end do   
        end do
        end do
        end do
        do st = 2,GSm+1
            Xoop(st) = Xoop(st-1) + Xoop(st)
            Yoop(st) = Yoop(st-1) + Yoop(st)
        end do 
        OOPExpGini = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)                                   
        
        OOPmedShare = 0.0d0
        OOPmedShareUpper = 0.0d0
        j = 1
        do i = 1, GSm
            if (Xoop(i+1) > 0.2d0*j .and. j < 5) then
                if (Xoop(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (Xoop(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (Xoop(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1     
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight3
                            j = j + 1                                                 
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight4
                            OOPmedShare(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight4 -weight3-weight2-weight)
                            j = j + 1                        
                        else                    
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1                        
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight3
                            OOPmedShare(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                    
                    else
                        weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight2
                        OOPmedShare(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShare(j) = OOPmedShare(j) + (Yoop(i+1)-Yoop(i))*weight
                    OOPmedShare(j+1) = (Yoop(i+1)-Yoop(i))*(1.0d0-weight)
                    j = j+1      
                end if       
            else
                OOPmedShare(j) = OOPmedShare(j) + Yoop(i+1)-Yoop(i)            
            end if  
            if (Xoop(i+1) > 0.9d0) then 
                if (Xoop(i) <= 0.9d0) then              
                    weight = (0.9d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper(1) = OOPmedShareUpper(1) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper(1) = OOPmedShareUpper(1) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if   
            if (Xoop(i+1) > 0.95d0) then 
                if (Xoop(i) <= 0.95d0) then              
                    weight = (0.95d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper(2) = OOPmedShareUpper(2) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper(2) = OOPmedShareUpper(2) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if 
            if (Xoop(i+1) > 0.99d0) then 
                if (Xoop(i) <= 0.99d0) then              
                    weight = (0.99d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper(3) = OOPmedShareUpper(3) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper(3) = OOPmedShareUpper(3) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if       
        end do         
        
        !OOP Health Expense Distribution for 65-74 Year-olds
        OOPgrid = linspace(0.0d0, maxval((/maxval(OOPExpArrayMarried(:,:,:,:,:,:,:5)),maxval(OOPExpArrayWidow(:,:,:,:,:,:,:5)),maxval(OOPExpArrayWidower(:,:,:,:,:,:,:5))/)), GSm)
        AggOOPExpTemp = sum(MeanOOPExpByAge(:5)*CohortWeights(nw+1:nw+5))             
        Xoop = 0.0d0
        Yoop = 0.0d0
        do ti = 1,5
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else
                ht = nsht
                dt = 2
            end if
        do d = 1,dt
        do h = 1,ht        
        do vm = 1,naem
        do vf = 1,naef
        do j = 1,nm
        do i = 1,na
           if (s==1) then
               tempR = psiRMarried(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
           else if (s==2) then
               tempR = psiRWidow(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
           else
               tempR = psiRWidower(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
           end if         
           st = 1
           do 
                if (OOPExpenses <= OOPgrid(st) .or. st==GSm) then
                    Xoop(st+1) = Xoop(st+1) + tempR*CohortWeights(nw+ti)/sum(CohortWeights(nw+1:nw+5),1)
                    Yoop(st+1) = Yoop(st+1) + OOPExpenses/AggOOPExpTemp*tempR*CohortWeights(nw+ti)
                    exit
                else
                    st = st + 1
                end if
            end do 
        end do          
        end do
        end do
        end do      
        end do   
        end do
        end do
        end do

        !Gini  
        do st = 2,GSm+1
            Xoop(st) = Xoop(st-1) + Xoop(st)
            Yoop(st) = Yoop(st-1) + Yoop(st)
        end do 
        OOPExpGini6574 = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)       
        
        OOPmedShare6574 = 0.0d0
        OOPmedShareUpper6574 = 0.0d0
        j = 1
        do i = 1, GSm
            if (Xoop(i+1) > 0.2d0*j .and. j < 5) then
                if (Xoop(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (Xoop(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (Xoop(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1     
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight3
                            j = j + 1                                                 
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight4
                            OOPmedShare6574(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight4 -weight3-weight2-weight)
                            j = j + 1                        
                        else                    
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1                        
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight3
                            OOPmedShare6574(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                    
                    else
                        weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight2
                        OOPmedShare6574(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShare6574(j) = OOPmedShare6574(j) + (Yoop(i+1)-Yoop(i))*weight
                    OOPmedShare6574(j+1) = (Yoop(i+1)-Yoop(i))*(1.0d0-weight)
                    j = j+1      
                end if       
            else
                OOPmedShare6574(j) = OOPmedShare6574(j) + Yoop(i+1)-Yoop(i)            
            end if  
            if (Xoop(i+1) > 0.9d0) then 
                if (Xoop(i) <= 0.9d0) then              
                    weight = (0.9d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper6574(1) = OOPmedShareUpper6574(1) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper6574(1) = OOPmedShareUpper6574(1) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if   
            if (Xoop(i+1) > 0.95d0) then 
                if (Xoop(i) <= 0.95d0) then              
                    weight = (0.95d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper6574(2) = OOPmedShareUpper6574(2) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper6574(2) = OOPmedShareUpper6574(2) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if 
            if (Xoop(i+1) > 0.99d0) then 
                if (Xoop(i) <= 0.99d0) then              
                    weight = (0.99d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper6574(3) = OOPmedShareUpper6574(3) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper6574(3) = OOPmedShareUpper6574(3) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if       
        end do   
        
        !OOP Health Expense Distribution for 75-84 Year-olds
        OOPgrid = linspace(0.0d0, maxval((/maxval(OOPExpArrayMarried(:,:,:,:,:,:,6:10)),maxval(OOPExpArrayWidow(:,:,:,:,:,:,6:10)),maxval(OOPExpArrayWidower(:,:,:,:,:,:,6:10))/)), GSm)
        AggOOPExpTemp = sum(MeanOOPExpByAge(6:10)*CohortWeights(nw+6:nw+10))             
        Xoop = 0.0d0
        Yoop = 0.0d0
        do ti = 6,10
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else
                ht = nsht
                dt = 2
            end if
        do d = 1,dt
        do h = 1,ht       
        do vm = 1,naem
        do vf = 1,naef
        do j = 1,nm
        do i = 1,na
           if (s==1) then
               tempR = psiRMarried(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
           else if (s==2) then
               tempR = psiRWidow(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
           else
               tempR = psiRWidower(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
           end if         
           st = 1
           do 
                if (OOPExpenses <= OOPgrid(st) .or. st==GSm) then
                    Xoop(st+1) = Xoop(st+1) + tempR*CohortWeights(nw+ti)/sum(CohortWeights(nw+6:nw+10),1)
                    Yoop(st+1) = Yoop(st+1) + OOPExpenses/AggOOPExpTemp*tempR*CohortWeights(nw+ti)
                    exit
                else
                    st = st + 1
                end if
            end do
        end do           
        end do
        end do
        end do      
        end do   
        end do
        end do
        end do
        
        !Gini  
        do st = 2,GSm+1
            Xoop(st) = Xoop(st-1) + Xoop(st)
            Yoop(st) = Yoop(st-1) + Yoop(st)
        end do 
        OOPExpGini7584 = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)       
        
        OOPmedShare7584 = 0.0d0
        OOPmedShareUpper7584 = 0.0d0
        j = 1
        do i = 1, GSm
            if (Xoop(i+1) > 0.2d0*j .and. j < 5) then
                if (Xoop(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (Xoop(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (Xoop(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1     
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight3
                            j = j + 1                                                 
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight4
                            OOPmedShare7584(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight4 -weight3-weight2-weight)
                            j = j + 1                        
                        else                    
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1                        
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight3
                            OOPmedShare7584(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                    
                    else
                        weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight2
                        OOPmedShare7584(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShare7584(j) = OOPmedShare7584(j) + (Yoop(i+1)-Yoop(i))*weight
                    OOPmedShare7584(j+1) = (Yoop(i+1)-Yoop(i))*(1.0d0-weight)
                    j = j+1      
                end if       
            else
                OOPmedShare7584(j) = OOPmedShare7584(j) + Yoop(i+1)-Yoop(i)            
            end if  
            if (Xoop(i+1) > 0.9d0) then 
                if (Xoop(i) <= 0.9d0) then              
                    weight = (0.9d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper7584(1) = OOPmedShareUpper7584(1) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper7584(1) = OOPmedShareUpper7584(1) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if   
            if (Xoop(i+1) > 0.95d0) then 
                if (Xoop(i) <= 0.95d0) then              
                    weight = (0.95d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper7584(2) = OOPmedShareUpper7584(2) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper7584(2) = OOPmedShareUpper7584(2) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if 
            if (Xoop(i+1) > 0.99d0) then 
                if (Xoop(i) <= 0.99d0) then              
                    weight = (0.99d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper7584(3) = OOPmedShareUpper7584(3) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper7584(3) = OOPmedShareUpper7584(3) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if       
        end do  
        
        !OOP Health Expense Distribution for 85+ Year-olds
        OOPgrid = linspace(0.0d0, maxval((/maxval(OOPExpArrayMarried(:,:,:,:,:,:,11:)),maxval(OOPExpArrayWidow(:,:,:,:,:,:,11:)),maxval(OOPExpArrayWidower(:,:,:,:,:,:,11:))/)), GSm)
        AggOOPExpTemp = sum(MeanOOPExpByAge(11:)*CohortWeights(nw+11:))             
        Xoop = 0.0d0
        Yoop = 0.0d0
        do ti = 11,nr
        do s = 1,3
            if (s==1) then
                ht = nhht
                dt = 4
            else
                ht = nsht
                dt = 2
            end if
        do d = 1,dt
        do h = 1,ht       
        do vm = 1,naem
        do vf = 1,naef
        do j = 1,nm
        do i = 1,na
           if (s==1) then
               tempR = psiRMarried(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayMarried(i,j,vf,vm,h,d,ti)
           else if (s==2) then
               tempR = psiRWidow(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidow(i,j,vf,vm,h,d,ti)
           else
               tempR = psiRWidower(i,j,vf,vm,h,d,ti)
               OOPExpenses = OOPExpArrayWidower(i,j,vf,vm,h,d,ti)
           end if         
           st = 1
           do 
                if (OOPExpenses<= OOPgrid(st) .or. st==GSm) then
                    Xoop(st+1) = Xoop(st+1) + tempR*CohortWeights(nw+ti)/sum(CohortWeights(nw+11:),1)
                    Yoop(st+1) = Yoop(st+1) + OOPExpenses/AggOOPExpTemp*tempR*CohortWeights(nw+ti)
                    exit
                else
                    st = st + 1
                end if
            end do  
        end do             
        end do
        end do
        end do      
        end do   
        end do
        end do
        end do
        
        !Gini  
        do st = 2,GSm+1
            Xoop(st) = Xoop(st-1) + Xoop(st)
            Yoop(st) = Yoop(st-1) + Yoop(st)
        end do 
        OOPExpGini85plus = 1-sum( (Xoop(2:) - Xoop(:GSm))*(Yoop(2:) + Yoop(:GSm)) ,1)       
        
        OOPmedShare85plus = 0.0d0
        OOPmedShareUpper85plus = 0.0d0
        j = 1
        do i = 1, GSm
            if (Xoop(i+1) > 0.2d0*j .and. j < 5) then
                if (Xoop(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (Xoop(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (Xoop(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1     
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight3
                            j = j + 1                                                 
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight4
                            OOPmedShare85plus(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight4 -weight3-weight2-weight)
                            j = j + 1                        
                        else                    
                            weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight2
                            j = j + 1                        
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                            OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight3
                            OOPmedShare85plus(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                    
                    else
                        weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(Xoop(i+1) - Xoop(i))
                        OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight2
                        OOPmedShare85plus(j+1) = (Yoop(i+1)-Yoop(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShare85plus(j) = OOPmedShare85plus(j) + (Yoop(i+1)-Yoop(i))*weight
                    OOPmedShare85plus(j+1) = (Yoop(i+1)-Yoop(i))*(1.0d0-weight)
                    j = j+1      
                end if       
            else
                OOPmedShare85plus(j) = OOPmedShare85plus(j) + Yoop(i+1)-Yoop(i)            
            end if  
            
            
            if (Xoop(i+1) > 0.9d0) then 
                if (Xoop(i) <= 0.9d0) then              
                    weight = (0.9d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper85plus(1) = OOPmedShareUpper85plus(1) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper85plus(1) = OOPmedShareUpper85plus(1) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if   
            if (Xoop(i+1) > 0.95d0) then 
                if (Xoop(i) <= 0.95d0) then              
                    weight = (0.95d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper85plus(2) = OOPmedShareUpper85plus(2) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper85plus(2) = OOPmedShareUpper85plus(2) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if 
            if (Xoop(i+1) > 0.99d0) then 
                if (Xoop(i) <= 0.99d0) then              
                    weight = (0.99d0 - Xoop(i))/(Xoop(i+1) - Xoop(i))
                    OOPmedShareUpper85plus(3) = OOPmedShareUpper85plus(3) + (Yoop(i+1)-Yoop(i))*(1.0d0-weight)                
                else
                    OOPmedShareUpper85plus(3) = OOPmedShareUpper85plus(3) + (Yoop(i+1)-Yoop(i))  
                end if          
            end if       
        end do               
   
    
    end subroutine ComputeExPostStats2
    subroutine ComputeExPostStats3()
        integer ti, s, vm, vf, h, j, i, ht, em, ef, het, fet, met, zm, zf
        real(dbl) PostTaxWealth, preTaxIncomeF, preTaxIncomeM, tempR
        real(dbl) weight, weight2, weight3, weight4
        real(dbl) TotalWealth, TotalEarns, TotalLifeEarns, TotalSocSecIncome
        real(dbl) XtotWealth(na+1), YtotWealth(na+1), XtotEarns(GSe+1), YtotEarns(GSe+1), EarnsGrid(GSe)     
        

       
        !Ginis and Distributions
        !Calculate wealth ginis and shares
        TotalWealth = sum(sum( spread(avect,2,nw)* sum(sum(sum(sum(sum(psiW,6),5),4),3),2)  ,1)*CohortWeights(:nw))
        TotalWealth = TotalWealth + sum(sum(spread(avect,2,nr)*sum(sum(sum(sum(sum(psiRMarried,6),5),4),3),2) ,1)*CohortWeights(nw+1:)) &
                                  + sum(sum(spread(avect,2,nr)*sum(sum(sum(sum(sum(psiRWidow,6),5),4),3),2) ,1)*CohortWeights(nw+1:)) &
                                  + sum(sum(spread(avect,2,nr)*sum(sum(sum(sum(sum(psiRWidower,6),5),4),3),2) ,1)*CohortWeights(nw+1:))
        XtotWealth = 0.0d0
        YtotWealth = 0.0d0       
        WealthShares = 0.0d0        
        Frac65plusShares = 0.0d0
        WealthSharesUpper = 0.0d0
        j = 1
        do i=1,na
            
            XwealthVect(i) = sum ( sum(sum(sum(sum(sum(avect(i)*psiW(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(:nw) ,1) + &
             sum ( sum(sum(sum(sum(sum(avect(i)*psiRMarried(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1) + &
             sum ( sum(sum(sum(sum(sum(avect(i)*psiRWidow(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1) + &
             sum ( sum(sum(sum(sum(sum(avect(i)*psiRWidower(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1)                     
            XtotWealth(i+1) = XtotWealth(i) + sum ( sum(sum(sum(sum(sum(psiW(i,:,:,:,:,:,:),5),4),3),2),1) * CohortWeights(:nw) ,1) + &
             sum ( sum(sum(sum(sum(sum(psiRMarried(i,:,:,:,:,:,:),5),4),3),2),1) * CohortWeights(nw+1:) ,1) +&
             sum ( sum(sum(sum(sum(sum(psiRWidow(i,:,:,:,:,:,:),5),4),3),2),1) * CohortWeights(nw+1:) ,1) + &
             sum ( sum(sum(sum(sum(sum(psiRWidower(i,:,:,:,:,:,:),5),4),3),2),1) * CohortWeights(nw+1:) ,1)
            YtotWealth(i+1) = YtotWealth(i) + XwealthVect(i)/TotalWealth                                                                                                            
            Frac65plusVect(i) = sum ( sum(sum(sum(sum(sum(psiRMarried(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1) + &
                                sum ( sum(sum(sum(sum(sum(psiRWidow(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1) + &
                                sum ( sum(sum(sum(sum(sum(psiRWidower(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(nw+1:) ,1)
             
            if (XtotWealth(i+1) > 0.2d0*j .and. j < 5) then
                if (XtotWealth(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (XtotWealth(i+1) > 0.2d0*(j+2) .and. j < 3) then  
                        if (XtotWealth(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight/TotalWealth                            
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight2/TotalWealth                           
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight2
                            j = j + 1                            
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight3/TotalWealth                           
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight3
                            j = j + 1                            
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight4/TotalWealth                           
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight4
                            WealthShares(j+1) = XwealthVect(i)*(1-weight4-weight3-weight2-weight)/TotalWealth                           
                            Frac65plusShares(j+1) = Frac65plusShares(j+1) + Frac65plusVect(i)*(1-weight4-weight3-weight2-weight)
                            j = j + 1   
                        else                                                 
                            weight = (0.2d0*j - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight/TotalWealth                           
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight2/TotalWealth                           
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                            WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight2/TotalWealth
                            Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight3
                            WealthShares(j+1) = XwealthVect(i)*(1-weight3-weight2-weight)/TotalWealth                           
                            Frac65plusShares(j+1) = Frac65plusVect(i)*(1-weight3-weight2-weight)
                            j = j + 1
                        end if                        
                     else                               
                        weight = (0.2d0*j - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                        WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight/TotalWealth                       
                        Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotWealth(i+1) - XtotWealth(i))
                        WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight2/TotalWealth                       
                        Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight2
                        WealthShares(j+1) = XwealthVect(i)*(1-weight2-weight)/TotalWealth                       
                        Frac65plusShares(j+1) = Frac65plusVect(i)*(1-weight2-weight)
                        j = j + 1
                     end if
                else
                    weight = (0.2d0*j - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                    WealthShares(j) = WealthShares(j) + XwealthVect(i)*weight/TotalWealth
                    Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i)*weight2
                    WealthShares(j+1) = XwealthVect(i)*(1.0d0-weight)/TotalWealth                   
                    Frac65plusShares(j+1) = Frac65plusVect(i)*(1.0d0-weight)
                    j = j+1      
                end if       
             else
                WealthShares(j) = WealthShares(j) + XwealthVect(i)/TotalWealth                        
                Frac65plusShares(j) = Frac65plusShares(j) + Frac65plusVect(i) 
             end if  
             if (XtotWealth(i+1) > 0.9d0) then 
                if (XtotWealth(i) <= 0.9d0) then              
                    weight = (0.9d0 - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                    WealthSharesUpper(1) = WealthSharesUpper(1) + XwealthVect(i)*(1.0d0-weight)/TotalWealth                
                else
                    WealthSharesUpper(1) = WealthSharesUpper(1) + XwealthVect(i)/TotalWealth  
                end if          
             end if   
             if (XtotWealth(i+1) > 0.95d0) then 
                if (XtotWealth(i) <= 0.95d0) then              
                    weight = (0.95d0 - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                    WealthSharesUpper(2) = WealthSharesUpper(2) + XwealthVect(i)*(1.0d0-weight)/TotalWealth                
                else
                    WealthSharesUpper(2) = WealthSharesUpper(2) + XwealthVect(i)/TotalWealth  
                end if          
             end if 
             if (XtotWealth(i+1) > 0.99d0) then 
                if (XtotWealth(i) <= 0.99d0) then              
                    weight = (0.99d0 - XtotWealth(i))/(XtotWealth(i+1) - XtotWealth(i))
                    WealthSharesUpper(3) = WealthSharesUpper(3) + XwealthVect(i)*(1.0d0-weight)/TotalWealth                
                else
                    WealthSharesUpper(3) = WealthSharesUpper(3) + XwealthVect(i)/TotalWealth  
                end if          
             end if                                                               
        end do
        Frac65plusShares = Frac65plusShares/0.20d0
        WealthGini = 1-sum( (XtotWealth(2:) - XtotWealth(:na))*(YtotWealth(2:) + YtotWealth(:na)) ,1)       
         
        
         
        !Wealth Gini for working age households
        TotalWealth = sum(sum( spread(avect,2,nw)* sum(sum(sum(sum(sum(psiW,6),5),4),3),2)  ,1)*CohortWeights(:nw))        
        XtotWealth = 0.0d0
        YtotWealth = 0.0d0        
        j = 1
        do i=1,na            
            XwealthVect(i) = sum ( sum(sum(sum(sum(sum(avect(i)*psiW(i,:,:,:,:,:,:),5),4),3),2),1)* CohortWeights(:nw) ,1)             
            XtotWealth(i+1) = XtotWealth(i) + sum ( sum(sum(sum(sum(sum(psiW(i,:,:,:,:,:,:),5),4),3),2),1) * CohortWeights(:nw) ,1)
            YtotWealth(i+1) = YtotWealth(i) + XwealthVect(i)/TotalWealth                                                                                                                                                                                      
        end do        
        WealthGiniWorkingHHs = 1-sum( (XtotWealth(2:) - XtotWealth(:na))*(YtotWealth(2:) + YtotWealth(:na)) ,1)
        
        !Wealth Gini 65-66 year-olds        
        TotalWealth = sum(avect*sum(sum(sum(sum(sum(psiRMarried(:,:,:,:,:,:,1),6),5),4),3),2),1)*CohortWeights(nw+1) &
                                  + sum(avect*sum(sum(sum(sum(sum(psiRWidow(:,:,:,:,:,:,1),6),5),4),3),2) ,1)*CohortWeights(nw+1) &
                                  + sum(avect*sum(sum(sum(sum(sum(psiRWidower(:,:,:,:,:,:,1),6),5),4),3),2) ,1)*CohortWeights(nw+1)
        XtotWealth = 0.0d0
        YtotWealth = 0.0d0                
        j = 1
        do i=1,na
            
            XwealthVect(i) =  sum(sum(sum(sum(sum(avect(i)*psiRMarried(i,:,:,:,:,:,1),5),4),3),2),1)* CohortWeights(nw+1) + &
              sum(sum(sum(sum(sum(avect(i)*psiRWidow(i,:,:,:,:,:,1),5),4),3),2),1)* CohortWeights(nw+1)  + &
              sum(sum(sum(sum(sum(avect(i)*psiRWidower(i,:,:,:,:,:,1),5),4),3),2),1)* CohortWeights(nw+1)                     
            XtotWealth(i+1) = XtotWealth(i) +  sum(sum(sum(sum(sum(psiRMarried(i,:,:,:,:,:,1),5),4),3),2),1) * CohortWeights(nw+1)  +&
              sum(sum(sum(sum(sum(psiRWidow(i,:,:,:,:,:,1),5),4),3),2),1) * CohortWeights(nw+1) + &
              sum(sum(sum(sum(sum(psiRWidower(i,:,:,:,:,:,1),5),4),3),2),1) * CohortWeights(nw+1) 
            YtotWealth(i+1) = YtotWealth(i) + XwealthVect(i)/TotalWealth                                                                                                                       
        end do        
        WealthGini6566 = 1-sum( (XtotWealth(2:) - XtotWealth(:na))*(YtotWealth(2:) + YtotWealth(:na)) ,1)              
         
        !Earnings Gini including retirees
        TotalEarns = sum((EarnsFbyAgeID+EarnsMbyAgeID)*CohortWeights(:nw))
        
        XtotEarns = 0.0d0
        YtotEarns = 0.0d0    
        EarnsGrid = linspace(0.001d0, maxval(w*emmat*hbar+w*efmat*hbar), GSe)            
       
        do ti=1,nw
        do het=1,nhet
            fet = int((het+1)/2)
            met = mod(het+1,2)+1
        do vm=1,naem
        do vf=1,naef
        do em=1,nem
        do ef=1,nef   
        do i=1,na  
           s = 1
           do 
                if (w*(efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti)+emmat(ef,em,met,ti)*hbar) <= EarnsGrid(s) .or. s==GSe) then
                    XtotEarns(s+1) = XtotEarns(s+1) + psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti)
                    YtotEarns(s+1) = YtotEarns(s+1) + w*(efmat(ef,em,fet,ti)*labWcube(i,ef,em,vf,vm,het,ti)+emmat(ef,em,met,ti)*hbar)/TotalEarns*psiW(i,ef,em,vf,vm,het,ti)*CohortWeights(ti)
                    exit
                else
                    s = s + 1
                end if
            end do           
        end do
        end do
        end do
        end do
        end do
        end do    
        end do   
        XtotEarns(1) = XtotEarns(1) + sum(CohortWeights(nw+1:))
        do s = 2,GSe+1
            XtotEarns(s) = XtotEarns(s-1) + XtotEarns(s)
            YtotEarns(s) = YtotEarns(s-1) + YtotEarns(s)
        end do 
        EarnsGini = 1-sum( (XtotEarns(2:) - XtotEarns(:GSe))*(YtotEarns(2:) + YtotEarns(:GSe)) ,1)                        
            
        EarnsShares = 0.0d0        
        EarnsSharesUpper = 0.0d0       
        j = 1        

        do i = 1, GSe
            if (XtotEarns(i+1) > 0.2d0*j .and. j < 5) then
                if (XtotEarns(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (XtotEarns(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (XtotEarns(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            j = j + 1                            
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight4
                            EarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight4-weight3 -weight2-weight)
                            j = j + 1                        
                        else                        
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            EarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight3 -weight2-weight)
                            j = j + 1   
                         end if                     
                    else
                        weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                        EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                        EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                        EarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    EarnsShares(j) = EarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                    EarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                      
                    j = j+1      
                end if       
             else
                EarnsShares(j) = EarnsShares(j) + YtotEarns(i+1)-YtotEarns(i)                    
             end if  
             if (XtotEarns(i+1) > 0.9d0) then 
                if (XtotEarns(i) <= 0.9d0) then              
                    weight = (0.9d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    EarnsSharesUpper(1) = EarnsSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    EarnsSharesUpper(1) = EarnsSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if   
             if (XtotEarns(i+1) > 0.95d0) then 
                if (XtotEarns(i) <= 0.95d0) then              
                    weight = (0.95d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    EarnsSharesUpper(2) = EarnsSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    EarnsSharesUpper(2) = EarnsSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if 
             if (XtotEarns(i+1) > 0.99d0) then 
                if (XtotEarns(i) <= 0.99d0) then              
                    weight = (0.99d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    EarnsSharesUpper(3) = EarnsSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    EarnsSharesUpper(3) = EarnsSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if       
        end do                                            


        !Lifetime Earnings Gini 65-66 year-olds      
        TotalLifeEarns = 0.0d0
        do vf =1,naef
        do vm =1,naem
            TotalLifeEarns = TotalLifeEarns + sum( (aveEarnFvect(vf)+aveEarnMvect(vm))*psiRMarried(:,:,vf,vm,:,:,1) ) + &
              sum( (aveEarnFvect(vf)+aveEarnMvect(vm))*psiRWidow(:,:,vf,vm,:,:,1))  + &
              sum((aveEarnFvect(vf)+aveEarnMvect(vm))*psiRWidower(:,:,vf,vm,:,:,1))
        end do
        end do
        XtotEarns = 0.0d0
        YtotEarns = 0.0d0    
        EarnsGrid = linspace(aveEarnFvect(1)+aveEarnMvect(1), aveEarnFvect(naef)+aveEarnMvect(naem), GSe)                   
        
        do vm=1,naem
        do vf=1,naef        
           s = 1
           do 
            if (aveEarnFvect(vf)+aveEarnMvect(vm) <= EarnsGrid(s) .or. s==GSe) then
                XtotEarns(s+1) = XtotEarns(s+1) + sum(psiRMarried(:,:,vf,vm,:,:,1))+sum(psiRWidow(:,:,vf,vm,:,:,1))+sum(psiRWidower(:,:,vf,vm,:,:,1))
                YtotEarns(s+1) = YtotEarns(s+1) + (aveEarnFvect(vf)+aveEarnMvect(vm))/TotalLifeEarns*(sum(psiRMarried(:,:,vf,vm,:,:,1)) &
                                + sum(psiRWidow(:,:,vf,vm,:,:,1)) + sum(psiRWidower(:,:,vf,vm,:,:,1)) )
                exit
            else
                s = s + 1
            end if
           end do           
        end do
        end do
       
        do s = 2,GSe+1
            XtotEarns(s) = XtotEarns(s-1) + XtotEarns(s)
            YtotEarns(s) = YtotEarns(s-1) + YtotEarns(s)
        end do 
        LifetimeEarnsGini6566 = 1-sum( (XtotEarns(2:) - XtotEarns(:GSe))*(YtotEarns(2:) + YtotEarns(:GSe)) ,1)                        
            
        LifetimeEarnsShares = 0.0d0        
        LifetimeEarnsSharesUpper = 0.0d0       
        j = 1        

        do i = 1, GSe
            if (XtotEarns(i+1) > 0.2d0*j .and. j < 5) then
                if (XtotEarns(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (XtotEarns(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (XtotEarns(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            j = j + 1                            
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight4
                            LifetimeEarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight4-weight3 -weight2-weight)
                            j = j + 1                        
                        else                        
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            LifetimeEarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight3 -weight2-weight)
                            j = j + 1   
                         end if                     
                    else
                        weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                        LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                        LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                        LifetimeEarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                    LifetimeEarnsShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                      
                    j = j+1      
                end if       
             else
                LifetimeEarnsShares(j) = LifetimeEarnsShares(j) + YtotEarns(i+1)-YtotEarns(i)                    
             end if  
             if (XtotEarns(i+1) > 0.9d0) then 
                if (XtotEarns(i) <= 0.9d0) then              
                    weight = (0.9d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    LifetimeEarnsSharesUpper(1) = LifetimeEarnsSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    LifetimeEarnsSharesUpper(1) = LifetimeEarnsSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if   
             if (XtotEarns(i+1) > 0.95d0) then 
                if (XtotEarns(i) <= 0.95d0) then              
                    weight = (0.95d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    LifetimeEarnsSharesUpper(2) = LifetimeEarnsSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    LifetimeEarnsSharesUpper(2) = LifetimeEarnsSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if 
             if (XtotEarns(i+1) > 0.99d0) then 
                if (XtotEarns(i) <= 0.99d0) then              
                    weight = (0.99d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    LifetimeEarnsSharesUpper(3) = LifetimeEarnsSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    LifetimeEarnsSharesUpper(3) = LifetimeEarnsSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if       
        end do   
                    
 
        !Soc Sec Income Gini 65-66 year-olds      
        TotalSocSecIncome = 0.0d0
        do vf =1,naef
        do vm =1,naem
            TotalSocSecIncome = TotalSocSecIncome + sum( socsec(aveEarnFvect(vf),aveEarnMvect(vm),1)*psiRMarried(:,:,vf,vm,:,:,1) ) + &
              sum( socsec(aveEarnFvect(vf),aveEarnMvect(vm),2)*psiRWidow(:,:,vf,vm,:,:,1))  + &
              sum(socsec(aveEarnFvect(vf),aveEarnMvect(vm),3)*psiRWidower(:,:,vf,vm,:,:,1))
        end do
        end do
        XtotEarns = 0.0d0
        YtotEarns = 0.0d0    
        EarnsGrid = linspace(0.0d0, socsec(aveEarnFvect(naef),aveEarnMvect(naem),1), GSe)                   
        
        do vm=1,naem
        do vf=1,naef
        do i=1,3        
           s = 1
           do 
            if (socsec(aveEarnFvect(vf),aveEarnMvect(vm),i) <= EarnsGrid(s) .or. s==GSe) then
                if (i==1) then
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(psiRMarried(:,:,vf,vm,:,:,1))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome*sum(psiRMarried(:,:,vf,vm,:,:,1)) 
                          
                else if (i==2) then
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(psiRWidow(:,:,vf,vm,:,:,1))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome*sum(psiRWidow(:,:,vf,vm,:,:,1))
                else
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(psiRWidower(:,:,vf,vm,:,:,1))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome*sum(psiRWidower(:,:,vf,vm,:,:,1))                               
                end if
                exit
            else
                s = s + 1
            end if
           end do           
        end do
        end do
        end do
       
        do s = 2,GSe+1
            XtotEarns(s) = XtotEarns(s-1) + XtotEarns(s)
            YtotEarns(s) = YtotEarns(s-1) + YtotEarns(s)
        end do 
        SocSecIncGini6566 = 1-sum( (XtotEarns(2:) - XtotEarns(:GSe))*(YtotEarns(2:) + YtotEarns(:GSe)) ,1)                        
            
        SocSecIncShares6566 = 0.0d0        
        SocSecIncSharesUpper6566 = 0.0d0       
        j = 1        

        do i = 1, GSe
            if (XtotEarns(i+1) > 0.2d0*j .and. j < 5) then
                if (XtotEarns(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (XtotEarns(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (XtotEarns(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            j = j + 1                            
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight4
                            SocSecIncShares6566(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight4-weight3 -weight2-weight)
                            j = j + 1                        
                        else                        
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            SocSecIncShares6566(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight3 -weight2-weight)
                            j = j + 1   
                         end if                     
                    else
                        weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                        SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                        SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                        SocSecIncShares6566(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncShares6566(j) = SocSecIncShares6566(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                    SocSecIncShares6566(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                      
                    j = j+1      
                end if       
             else
                SocSecIncShares6566(j) = SocSecIncShares6566(j) + YtotEarns(i+1)-YtotEarns(i)                    
             end if  
             if (XtotEarns(i+1) > 0.9d0) then 
                if (XtotEarns(i) <= 0.9d0) then              
                    weight = (0.9d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper6566(1) = SocSecIncSharesUpper6566(1) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper6566(1) = SocSecIncSharesUpper6566(1) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if   
             if (XtotEarns(i+1) > 0.95d0) then 
                if (XtotEarns(i) <= 0.95d0) then              
                    weight = (0.95d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper6566(2) = SocSecIncSharesUpper6566(2) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper6566(2) = SocSecIncSharesUpper6566(2) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if 
             if (XtotEarns(i+1) > 0.99d0) then 
                if (XtotEarns(i) <= 0.99d0) then              
                    weight = (0.99d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper6566(3) = SocSecIncSharesUpper6566(3) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper6566(3) = SocSecIncSharesUpper6566(3) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if       
        end do   

        !Soc Sec Income Gini All retirees
        TotalSocSecIncome = 0.0d0
        do vf =1,naef
        do vm =1,naem
            TotalSocSecIncome = TotalSocSecIncome + socsec(aveEarnFvect(vf),aveEarnMvect(vm),1)*sum(sum(sum(sum(sum(psiRMarried(:,:,vf,vm,:,:,:),4),3),2),1)*CohortWeights(nw+1:)) &
                + socsec(aveEarnFvect(vf),aveEarnMvect(vm),2)*sum(sum(sum(sum(sum(psiRWidow(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:)) &
                + socsec(aveEarnFvect(vf),aveEarnMvect(vm),3)*sum(sum(sum(sum(sum(psiRWidower(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:))              
              
        end do
        end do
        XtotEarns = 0.0d0
        YtotEarns = 0.0d0    
        EarnsGrid = linspace(0.0d0, socsec(aveEarnFvect(naef),aveEarnMvect(naem),1), GSe)                   
        
        do vm=1,naem
        do vf=1,naef
        do i=1,3        
           s = 1
           do 
            if (socsec(aveEarnFvect(vf),aveEarnMvect(vm),i) <= EarnsGrid(s) .or. s==GSe) then
                if (i==1) then
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(sum(sum(sum(sum(psiRMarried(:,:,vf,vm,:,:,:),4),3),2),1)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome*sum(sum(sum(sum(sum(psiRMarried(:,:,vf,vm,:,:,:),4),3),2),1)*CohortWeights(nw+1:))
                          
                else if (i==2) then
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(sum(sum(sum(sum(psiRWidow(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome* sum(sum(sum(sum(sum(psiRWidow(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:))
                else
                    XtotEarns(s+1) = XtotEarns(s+1) + sum(sum(sum(sum(sum(psiRWidower(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:))/sum(CohortWeights(nw+1:))
                    YtotEarns(s+1) = YtotEarns(s+1) + socsec(aveEarnFvect(vf),aveEarnMvect(vm),i)/TotalSocSecIncome*sum(sum(sum(sum(sum(psiRWidower(:,:,vf,vm,:,:,:),4),3),2) ,1)*CohortWeights(nw+1:))                           
                end if
                exit
            else
                s = s + 1
            end if
           end do           
        end do
        end do
        end do
                      
        do s = 2,GSe+1
            XtotEarns(s) = XtotEarns(s-1) + XtotEarns(s)
            YtotEarns(s) = YtotEarns(s-1) + YtotEarns(s)
        end do 

        SocSecIncGini = 1-sum( (XtotEarns(2:) - XtotEarns(:GSe))*(YtotEarns(2:) + YtotEarns(:GSe)) ,1)                        
            
        SocSecIncShares = 0.0d0        
        SocSecIncSharesUpper = 0.0d0       
        j = 1        

        do i = 1, GSe
            if (XtotEarns(i+1) > 0.2d0*j .and. j < 5) then
                if (XtotEarns(i+1) > 0.2d0*(j+1) .and. j < 4) then
                    if (XtotEarns(i+1) > 0.2d0*(j+2) .and. j < 3) then
                        if (XtotEarns(i+1) > 0.2d0*(j+3) .and. j < 2) then
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            j = j + 1                            
                            weight4 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight4
                            SocSecIncShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight4-weight3 -weight2-weight)
                            j = j + 1                        
                        else                        
                            weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                            j = j + 1
                            weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                            j = j + 1
                            weight3 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                            SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight3
                            SocSecIncShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight3 -weight2-weight)
                            j = j + 1   
                         end if                     
                    else
                        weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                        SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                        j = j + 1
                        weight2 = (0.2d0*j - 0.2d0*(j-1))/(XtotEarns(i+1) - XtotEarns(i))
                        SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight2
                        SocSecIncShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1-weight2-weight)
                        j = j + 1
                    end if
                else
                    weight = (0.2d0*j - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncShares(j) = SocSecIncShares(j) + (YtotEarns(i+1)-YtotEarns(i))*weight
                    SocSecIncShares(j+1) = (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                      
                    j = j+1      
                end if       
             else
                SocSecIncShares(j) = SocSecIncShares(j) + YtotEarns(i+1)-YtotEarns(i)                    
             end if  
             if (XtotEarns(i+1) > 0.9d0) then 
                if (XtotEarns(i) <= 0.9d0) then              
                    weight = (0.9d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper(1) = SocSecIncSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper(1) = SocSecIncSharesUpper(1) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if   
             if (XtotEarns(i+1) > 0.95d0) then 
                if (XtotEarns(i) <= 0.95d0) then              
                    weight = (0.95d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper(2) = SocSecIncSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper(2) = SocSecIncSharesUpper(2) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if 
             if (XtotEarns(i+1) > 0.99d0) then 
                if (XtotEarns(i) <= 0.99d0) then              
                    weight = (0.99d0 - XtotEarns(i))/(XtotEarns(i+1) - XtotEarns(i))
                    SocSecIncSharesUpper(3) = SocSecIncSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))*(1.0d0-weight)                
                else
                    SocSecIncSharesUpper(3) = SocSecIncSharesUpper(3) + (YtotEarns(i+1)-YtotEarns(i))  
                end if          
             end if       
        end do   
                                  
        print *, "First set of simulations..."
        call simulations1()
        print *, "Second set of simulations..."
        call MedExpSimulations()
        print *, "Third set of simulations..."
        call WealthSimulations()
        print *, "Finished with simulations..."        
        
    end subroutine ComputeExPostStats3      
end module results2

  
   
