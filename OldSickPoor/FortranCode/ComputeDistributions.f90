module ComputeDistributions
    use functions
    use output
    use interpolate1 
    implicit none
    contains
    
    subroutine ComputeStationaryDistributions()   
        real(dbl) AveEarningsF, eta, eta2, eta3, check, probs, tempx, totalAlive
        integer loc, loc2, loc3, i, ti, jf, jm, j, zf, zm, z, vf, vm, s, s1, eh, ef, em, h , h1, hf, hm, hm1, hf1, pm, tm, pz, pj, tj 
           
        !compute stationary distribution for working agents
        print '(a)', "     Computing stationary distribution for working agents..."
        psiW = 0.0d0
        
        !Age 1
        !psiW(1,:,:,1,1,:,1) = spread(spread(ufinit,2,nem)*spread(uminit,1,nef),3,nhet)*spread(spread(educDist,1,nef),2,nem)     
        psiW(1,:,:,1,1,:,1) = spread(uinit,3,nhet)*spread(spread(educDist,1,nef),2,nem)     
        !na,nef,nem,naef,naem,nhet,nw
       
       !Ages 2 to nw
        do ti = 2,nw
            !print *, "ti=", ti      
            do eh = 1,nhet
                em = mod(eh+1,2)+1        
                ef = int((eh+1)/2)
            do vm=1,naem
            do zm=1,nem                
            do vf=1,naef            
            do zf=1,nef
                loc3 = EarnLocsMcube(zf,zm,vm,em,ti)          
                eta3 = etaMcube(zf,zm,vm,em,ti)            
            do i = 1,na
                AveEarningsF = (aveEarnFvect(vf)*(ti-2) + w*efmat(zf,zm,ef,ti-1)*labWCube(i,zf,zm,vf,vm,eh,ti-1))/(ti-1)
                if (AveEarningsF > aveEarnFvect(naef)) then
                    AveEarningsF = aveEarnFvect(naef)
                    loc = naef-1
                    eta = 0.0d0
                 else
                    call Locate(aveEarnFvect, AveEarningsF, loc)  
                    eta = ( aveEarnFvect(loc+1) - AveEarningsF )/( aveEarnFvect(loc+1) - aveEarnFvect(loc) )
                end if                
                call Locate(avect, aPolicyWCube(i,zf,zm,vf,vm,eh,ti-1), loc2)
                            
                if (loc2 == na) then
                    do jm=1,nem
                    do jf=1,nef      
                        tempx = Puefmat(jf,zf) * Puemmat(jm,zm) * psiW(i,zf,zm,vf,vm,eh,ti-1)          
                        psiW(loc2,jf,jm,loc,loc3,eh,ti)     = psiW(loc2,jf,jm,loc,loc3,eh,ti)     +  eta * eta3  * tempx
                        psiW(loc2,jf,jm,loc+1,loc3,eh,ti)   = psiW(loc2,jf,jm,loc+1,loc3,eh,ti)   + (1-eta)* eta3 * tempx
                        psiW(loc2,jf,jm,loc,loc3+1,eh,ti)   = psiW(loc2,jf,jm,loc,loc3+1,eh,ti)   +  eta * (1-eta3) * tempx
                        psiW(loc2,jf,jm,loc+1,loc3+1,eh,ti) = psiW(loc2,jf,jm,loc+1,loc3+1,eh,ti) + (1-eta)* (1-eta3) * tempx
                    end do
                    end do
                else    
                    eta2 = ( avect(loc2+1) - aPolicyWCube(i,zf,zm,vf,vm,eh,ti-1) )/( avect(loc2+1) - avect(loc2) )
                    do jm=1,nem
                    do jf=1,nef
                        tempx = Puefmat(jf,zf) * Puemmat(jm,zm) * psiW(i,zf,zm,vf,vm,eh,ti-1)            
                        psiW(loc2,jf,jm,loc,loc3,eh,ti)     = psiW(loc2,jf,jm,loc,loc3,eh,ti)     +  eta * eta2 * eta3* tempx
                        psiW(loc2,jf,jm,loc+1,loc3,eh,ti)   = psiW(loc2,jf,jm,loc+1,loc3,eh,ti)   + (1-eta) * eta2 * eta3* tempx
                        psiW(loc2+1,jf,jm,loc,loc3,eh,ti)   = psiW(loc2+1,jf,jm,loc,loc3,eh,ti)   +  eta * (1-eta2) * eta3* tempx
                        psiW(loc2+1,jf,jm,loc+1,loc3,eh,ti) = psiW(loc2+1,jf,jm,loc+1,loc3,eh,ti) + (1-eta) * (1-eta2) * eta3* tempx
                        psiW(loc2,jf,jm,loc,loc3+1,eh,ti)     = psiW(loc2,jf,jm,loc,loc3+1,eh,ti)     +  eta * eta2 * (1-eta3)* tempx
                        psiW(loc2,jf,jm,loc+1,loc3+1,eh,ti)   = psiW(loc2,jf,jm,loc+1,loc3+1,eh,ti)   + (1-eta) * eta2 * (1-eta3)* tempx
                        psiW(loc2+1,jf,jm,loc,loc3+1,eh,ti)   = psiW(loc2+1,jf,jm,loc,loc3+1,eh,ti)   +  eta * (1-eta2) * (1-eta3)* tempx
                        psiW(loc2+1,jf,jm,loc+1,loc3+1,eh,ti) = psiW(loc2+1,jf,jm,loc+1,loc3+1,eh,ti) + (1-eta) * (1-eta2) * (1-eta3)* tempx
                    end do
                    end do
                end if                
            
            end do
            end do
            end do
            end do
            end do
            end do
            check = sum(psiW(:,:,:,:,:,:,ti))
            if (abs(check - 1.0d0) > 1.e-8) then
                print *, "psiW", ti, "not summing to 1"
                print *, "check =", check
                pause
            end if
        end do
       
        !compute stationary distribution for retired agents
        print '(a)', "     Computing stationary distribution for retired agents..."
        psiRMarried = 0.0d0
        psiRWidow = 0.0d0
        psiRWidower = 0.0d0
        
        !First retired age
        !Married
        do eh = 1,nhet
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)        
        do vm=1,naem
        do zm=1,nem                
        do vf=1,naef
        do zf=1,nef
                loc3 = EarnLocsMcube(zf,zm,vm,em,nw+1)          
                eta3 = etaMcube(zf,zm,vm,em,nw+1)         
        do j=1,na        
            AveEarningsF = (aveEarnFvect(vf)*(nw-1) + w*efmat(zf,zm,ef,nw)*labWCube(j,zf,zm,vf,vm,eh,nw))/nw
            if (AveEarningsF > aveEarnFvect(naef)) then
                AveEarningsF = aveEarnFvect(naef)
                loc = naef-1
                eta = 0.0d0
            else
                call Locate(aveEarnFvect, AveEarningsF, loc)  
                eta = ( aveEarnFvect(loc+1) - AveEarningsF )/( aveEarnFvect(loc+1) - aveEarnFvect(loc) )
            end if                        
            call Locate(avect, aPolicyWCube(j,zf,zm,vf,vm,eh,nw), loc2)       
        do h=1,nhht
            hm = mod(h+1,2)+1        
            hf = int((h+1)/2)  
        do i=1,nm
            pm = i-npm*((i-1)/npm)
            tm = (i-1)/npm + 1            
        do s=1,4
            if (s==1) then
                probs = survivalprobVectF(hf,1,1)*survivalprobVectM(hm,1,1)
            else if (s==2) then
                probs = survivalprobVectF(hf,1,1)*(1-survivalprobVectM(hm,1,1))
            else if (s==3) then
                probs = (1-survivalprobVectF(hf,1,1))*survivalprobVectM(hm,1,1)
            else
                probs = (1-survivalprobVectF(hf,1,1))*(1-survivalprobVectM(hm,1,1))
            end if
            tempx = hinitH(h,eh) * minit(pm,eh) * Ptmvect(tm) * sinitH(1,vm) * probs * psiW(j,zf,zm,vf,vm,eh,nw)
            if (loc2 == na) then 
                psiRMarried(loc2 , i,loc, loc3, h,s, 1)     = psiRMarried(loc2 , i,loc, loc3, h,s, 1) + eta*eta3 * tempx
                psiRMarried(loc2 , i,loc+1, loc3, h,s, 1)   = psiRMarried(loc2 , i,loc+1, loc3, h,s, 1) + (1-eta)*eta3 * tempx
                psiRMarried(loc2 , i,loc, loc3+1, h,s, 1)    = psiRMarried(loc2 , i,loc, loc3+1, h,s, 1) + eta*(1-eta3) * tempx
                psiRMarried(loc2 , i,loc+1, loc3+1, h,s, 1)   = psiRMarried(loc2 , i,loc+1, loc3+1, h,s, 1) + (1-eta)*(1-eta3) * tempx                                 
            else    
                eta2 = ( avect(loc2+1) - aPolicyWCube(j,zf,zm,vf,vm,eh,nw) )/( avect(loc2+1) - avect(loc2) )
                psiRMarried(loc2 , i,loc,loc3, h,s, 1) = psiRMarried(loc2 , i,loc,loc3, h,s, 1) + eta * eta2 *eta3  * tempx
                psiRMarried(loc2+1, i,loc,loc3, h,s, 1) = psiRMarried(loc2+1, i,loc,loc3, h,s, 1) + eta * (1-eta2)*eta3  * tempx
                psiRMarried(loc2 , i,loc+1,loc3, h,s, 1) = psiRMarried(loc2 , i,loc+1,loc3, h,s, 1) + (1-eta) * eta2 *eta3* tempx
                psiRMarried(loc2+1, i,loc+1,loc3, h,s, 1) = psiRMarried(loc2+1, i,loc+1,loc3,h,s, 1) + (1-eta) * (1-eta2) *eta3  * tempx
                psiRMarried(loc2 , i,loc,loc3+1, h,s, 1) = psiRMarried(loc2 , i,loc,loc3+1, h,s, 1) + eta * eta2 *(1-eta3) * tempx
                psiRMarried(loc2+1, i,loc,loc3+1, h,s, 1) = psiRMarried(loc2+1, i,loc,loc3+1, h,s, 1) + eta * (1-eta2) *(1-eta3) * tempx
                psiRMarried(loc2 , i,loc+1,loc3+1, h,s, 1) = psiRMarried(loc2 , i,loc+1,loc3+1, h,s, 1) + (1-eta) * eta2*(1-eta3)  * tempx
                psiRMarried(loc2+1, i,loc+1,loc3+1, h,s, 1) = psiRMarried(loc2+1, i,loc+1,loc3+1, h,s, 1) + (1-eta) * (1-eta2) *(1-eta3) * tempx
                !s,a,m,ebar
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
        
        !First retired age
        !Widow
          do eh = 1,nhet
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)        
        do vm=1,naem
        do zm=1,nem                
        do vf=1,naef
        do zf=1,nef
                loc3 = EarnLocsMcube(zf,zm,vm,em,nw+1)          
                eta3 = etaMcube(zf,zm,vm,em,nw+1)         
        do j=1,na        
            AveEarningsF = (aveEarnFvect(vf)*(nw-1) + w*efmat(zf,zm,ef,nw)*labWCube(j,zf,zm,vf,vm,eh,nw))/nw
            if (AveEarningsF > aveEarnFvect(naef)) then
                AveEarningsF = aveEarnFvect(naef)
                loc = naef-1
                eta = 0.0d0
            else
                call Locate(aveEarnFvect, AveEarningsF, loc)  
                eta = ( aveEarnFvect(loc+1) - AveEarningsF )/( aveEarnFvect(loc+1) - aveEarnFvect(loc) )
            end if                        
            call Locate(avect, aPolicyWCube(j,zf,zm,vf,vm,eh,nw), loc2)       
        do h=1,nsht
        do i=1,nm
            pm = i-npm*((i-1)/npm)
            tm = (i-1)/npm + 1            
        do s=1,2
            if (s==1) then
                probs = survivalprobVectF(h,2,1)
            else if (s==2) then
                probs = 1-survivalprobVectF(h,2,1)
            end if   
            tempx = hinitF(h,ef) * minit(pm,eh) * Ptmvect(tm) * sinitH(2,vm) * probs * psiW(j,zf,zm,vf,vm,eh,nw)   
                       
            if (loc2 == na) then 
                psiRWidow(loc2 , i,loc, loc3, h,s, 1)     = psiRWidow(loc2 , i,loc, loc3, h,s, 1) + eta*eta3  * tempx * (1-probHusband0)
                psiRWidow(loc2 , i,loc+1, loc3, h,s, 1)   = psiRWidow(loc2 , i,loc+1, loc3, h,s, 1) + (1-eta)*eta3  * tempx * (1-probHusband0)
                psiRWidow(loc2 , i,loc, loc3+1, h,s, 1)    = psiRWidow(loc2 , i,loc, loc3+1, h,s, 1) + eta*(1-eta3)   * tempx * (1-probHusband0)
                psiRWidow(loc2 , i,loc+1, loc3+1, h,s, 1)   = psiRWidow(loc2 , i,loc+1, loc3+1, h,s, 1) + (1-eta)*(1-eta3) * tempx * (1-probHusband0)                                        
            else    
                eta2 = ( avect(loc2+1) - aPolicyWCube(j,zf,zm,vf,vm,eh,nw) )/( avect(loc2+1) - avect(loc2) )
                psiRWidow(loc2 , i,loc,loc3, h,s, 1) = psiRWidow(loc2 , i,loc,loc3, h,s, 1) + eta * eta2 *eta3  * tempx * (1-probHusband0)
                psiRWidow(loc2+1, i,loc,loc3, h,s, 1) = psiRWidow(loc2+1, i,loc,loc3, h,s, 1) + eta * (1-eta2)*eta3  * tempx * (1-probHusband0)
                psiRWidow(loc2 , i,loc+1,loc3, h,s, 1) = psiRWidow(loc2 , i,loc+1,loc3, h,s, 1) + (1-eta) * eta2 *eta3* tempx * (1-probHusband0)
                psiRWidow(loc2+1, i,loc+1,loc3, h,s, 1) = psiRWidow(loc2+1, i,loc+1,loc3,h,s, 1) + (1-eta) * (1-eta2) *eta3  * tempx * (1-probHusband0)      
                psiRWidow(loc2 , i,loc,loc3+1, h,s, 1) = psiRWidow(loc2 , i,loc,loc3+1, h,s, 1) + eta * eta2 *(1-eta3) * tempx * (1-probHusband0)
                psiRWidow(loc2+1, i,loc,loc3+1, h,s, 1) = psiRWidow(loc2+1, i,loc,loc3+1, h,s, 1) + eta * (1-eta2) *(1-eta3) * tempx * (1-probHusband0)
                psiRWidow(loc2 , i,loc+1,loc3+1, h,s, 1) = psiRWidow(loc2 , i,loc+1,loc3+1, h,s, 1) + (1-eta) * eta2*(1-eta3)  * tempx * (1-probHusband0)
                psiRWidow(loc2+1, i,loc+1,loc3+1, h,s, 1) = psiRWidow(loc2+1, i,loc+1,loc3+1, h,s, 1) + (1-eta) * (1-eta2) *(1-eta3) * tempx * (1-probHusband0)
                !s,a,m,ebar
            end if
            
            if (loc2 == na) then 
                psiRWidow(loc2 , i,loc, 1, h,s, 1)     = psiRWidow(loc2 , i,loc, 1, h,s, 1) + eta*  tempx * probHusband0
                psiRWidow(loc2 , i,loc+1, 1, h,s, 1)   = psiRWidow(loc2 , i,loc+1, 1, h,s, 1) + (1-eta)  * tempx * probHusband0
                                                   
            else    
                eta2 = ( avect(loc2+1) - aPolicyWCube(j,zf,zm,vf,vm,eh,nw) )/( avect(loc2+1) - avect(loc2) )
                psiRWidow(loc2 , i,loc,1, h,s, 1) = psiRWidow(loc2 , i,loc,1, h,s, 1) + eta * eta2 * tempx * probHusband0
                psiRWidow(loc2+1, i,loc,1, h,s, 1) = psiRWidow(loc2+1, i,loc,1, h,s, 1) + eta * (1-eta2) * tempx * probHusband0
                psiRWidow(loc2 , i,loc+1,1, h,s, 1) = psiRWidow(loc2 , i,loc+1,1, h,s, 1) + (1-eta) * eta2 * tempx * probHusband0
                psiRWidow(loc2+1, i,loc+1,1, h,s, 1) = psiRWidow(loc2+1, i,loc+1,1,h,s, 1) + (1-eta) * (1-eta2) * tempx * probHusband0     
                !s,a,m,ebar
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
        
        !First retired age
        !Widower
        do eh = 1,nhet
            em = mod(eh+1,2)+1        
            ef = int((eh+1)/2)        
        do vm=1,naem
        do zm=1,nem                
        do vf=1,naef
        do zf=1,nef
                loc3 = EarnLocsMcube(zf,zm,vm,em,nw+1)          
                eta3 = etaMcube(zf,zm,vm,em,nw+1)         
        do j=1,na        
            AveEarningsF = (aveEarnFvect(vf)*(nw-1) + w*efmat(zf,zm,ef,nw)*labWCube(j,zf,zm,vf,vm,eh,nw))/nw
            if (AveEarningsF > aveEarnFvect(naef)) then
                AveEarningsF = aveEarnFvect(naef)
                loc = naef-1
                eta = 0.0d0
            else
                call Locate(aveEarnFvect, AveEarningsF, loc)  
                eta = ( aveEarnFvect(loc+1) - AveEarningsF )/( aveEarnFvect(loc+1) - aveEarnFvect(loc) )
            end if                        
            call Locate(avect, aPolicyWCube(j,zf,zm,vf,vm,eh,nw), loc2)       
        do h=1,nsht
        do i=1,nm
            pm = i-npm*((i-1)/npm)
            tm = (i-1)/npm + 1            
        do s=1,2
            if (s==1) then
                probs = survivalprobVectM(h,3,1)
            else if (s==2) then
                probs = 1-survivalprobVectM(h,3,1)
            end if 
            tempx = hinitM(h,em) * minit(pm,eh) * Ptmvect(tm) * sinitH(3,vm) * probs * psiW(j,zf,zm,vf,vm,eh,nw)       
            if (loc2 == na) then 
                psiRWidower(loc2 , i,loc, loc3, h,s, 1)     = psiRWidower(loc2 , i,loc, loc3, h,s, 1) + eta*eta3  * tempx
                psiRWidower(loc2 , i,loc+1, loc3, h,s, 1)   = psiRWidower(loc2 , i,loc+1, loc3, h,s, 1) + (1-eta)*eta3  * tempx
                psiRWidower(loc2 , i,loc, loc3+1, h,s, 1)    = psiRWidower(loc2 , i,loc, loc3+1, h,s, 1) + eta*(1-eta3) * tempx
                psiRWidower(loc2 , i,loc+1, loc3+1, h,s, 1)   = psiRWidower(loc2 , i,loc+1, loc3+1, h,s, 1) + (1-eta)*(1-eta3) * tempx            
            else    
                eta2 = ( avect(loc2+1) - aPolicyWCube(j,zf,zm,vf,vm,eh,nw) )/( avect(loc2+1) - avect(loc2) )
                psiRWidower(loc2 , i,loc,loc3, h,s, 1) = psiRWidower(loc2 , i,loc,loc3, h,s, 1) + eta * eta2 *eta3  * tempx
                psiRWidower(loc2+1, i,loc,loc3, h,s, 1) = psiRWidower(loc2+1, i,loc,loc3, h,s, 1) + eta * (1-eta2)*eta3  * tempx
                psiRWidower(loc2 , i,loc+1,loc3, h,s, 1) = psiRWidower(loc2 , i,loc+1,loc3, h,s, 1) + (1-eta) * eta2 *eta3* tempx
                psiRWidower(loc2+1, i,loc+1,loc3, h,s, 1) = psiRWidower(loc2+1, i,loc+1,loc3,h,s, 1) + (1-eta) * (1-eta2) *eta3  * tempx       
                psiRWidower(loc2 , i,loc,loc3+1, h,s, 1) = psiRWidower(loc2 , i,loc,loc3+1, h,s, 1) + eta * eta2 *(1-eta3) * tempx
                psiRWidower(loc2+1, i,loc,loc3+1, h,s, 1) = psiRWidower(loc2+1, i,loc,loc3+1, h,s, 1) + eta * (1-eta2) *(1-eta3) * tempx
                psiRWidower(loc2 , i,loc+1,loc3+1, h,s, 1) = psiRWidower(loc2 , i,loc+1,loc3+1, h,s, 1) + (1-eta) * eta2*(1-eta3)  * tempx
                psiRWidower(loc2+1, i,loc+1,loc3+1, h,s, 1) = psiRWidower(loc2+1, i,loc+1,loc3+1, h,s, 1) + (1-eta) * (1-eta2) *(1-eta3) * tempx                           
                !s,a,m,ebar
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
        check = sum(psiRMarried(:,:,:,:,:,:,1))+sum(psiRWidow(:,:,:,:,:,:,1))+sum(psiRWidower(:,:,:,:,:,:,1))
        if (abs(check - 1.0d0) > 1.e-7) then
            print *, "psiR 1 not summing to 1"
            print *, "check =", check
            pause
        end if        
        
        !do i=1,naem
        !    AveEarnsM65dist(i) = sum(psiW(:,:,:,:,i,:,nw))
        !end do
        !call Out(0,"AveEarnsM65dist", AveEarnsM65dist, ret)
        !call Out(0,"AveEarnsM65dist2",sum(sum(sum(ProdDistW(:,:,:,:,nw) ,4),2),1),ret)        
        
        !Retirement ages 2 to nr
        do ti = 2,nr
            !print *, "ti=", ti    
            !totalAliveMarried = sum(psiRMarried(:,:,:,:,:,1:3,ti-1))
            !totalAliveWidow = sum(psiRWidow(:,:,:,:,:,1,ti-1))
            !totalAliveWidower = sum(psiRWidower(:,:,:,:,:,1,ti-1)) 
            totalAlive = sum(psiRMarried(:,:,:,:,:,1:3,ti-1)) + sum(psiRWidow(:,:,:,:,:,1,ti-1)) + sum(psiRWidower(:,:,:,:,:,1,ti-1))        
            do s=1,3
            
                if (s==1) then !currently married, next period married
                
                    do h=1,nhht
                        hm = mod(h+1,2)+1        
                        hf = int((h+1)/2)              
                    do vm=1,naem
                    do vf=1,naef            
                    do z=1,nm
                        pz = z-npm*((z-1)/npm)        
                    do i = 1,na
                        call Locate(avect, aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1), loc2)
                    do s1=1,4
                    do h1=1,nhht
                        hm1 = mod(h1+1,2)+1        
                        hf1 = int((h1+1)/2)              
                    do j=1,nm   
                        pj = j-npm*((j-1)/npm)
                        tj = (j-1)/npm + 1   
                        
                        if (ti==nr) then
                            if (s1<=3) then                                
                                probs = 0.0d0
                            else
                                probs = 1.0d0
                            end if                        
                        else                        
                            if (s1==1) then !probability married at date ti+1 given married at date ti
                                probs = survivalprobVectF(hf1,1,ti)*survivalprobVectM(hm1,1,ti)
                            else if (s1==2) then
                                probs = survivalprobVectF(hf1,1,ti)*(1-survivalprobVectM(hm1,1,ti))
                            else if (s1==3) then
                                probs = (1-survivalprobVectF(hf1,1,ti))*survivalprobVectM(hm1,1,ti)
                            else
                                probs = (1-survivalprobVectF(hf1,1,ti))*(1-survivalprobVectM(hm1,1,ti))
                            end if
                        end if
                        tempx = PumMat(pj,pz,ti-1)*Ptmvect(tj)*PhmatF(hf1, hf, 1, ti-1)*PhmatM(hm1, hm, 1, ti-1)*probs*psiRMarried(i,z,vf,vm,h,s,ti-1)/totalAlive 
                        if (loc2 == na) then                                                          
                            psiRMarried(loc2,j,vf,vm,h1,s1,ti) = psiRMarried(loc2,j,vf,vm,h1,s1,ti) + tempx                                                                                                                        
                        else    
                            eta2 = ( avect(loc2+1) - aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1) )/( avect(loc2+1) - avect(loc2) )                                                             
                            psiRMarried(loc2,j,vf,vm,h1,s1,ti) = psiRMarried(loc2,j,vf,vm,h1,s1,ti) + eta2 * tempx                                                
                            psiRMarried(loc2+1,j,vf,vm,h1,s1,ti) = psiRMarried(loc2+1,j,vf,vm,h1,s1,ti) + (1-eta2) * tempx                                                                                                                                                    
                    end if 
                    end do    
                    end do           
                    end do
                    end do
                    end do
                    end do
                    end do
                    end do                   
                
                
                
                else if (s==2) then !currently married, next period widow
                
                    do h=1,nhht
                        hm = mod(h+1,2)+1        
                        hf = int((h+1)/2)              
                    do vm=1,naem
                    do vf=1,naef            
                    do z=1,nm
                        pz = z-npm*((z-1)/npm)        
                    do i = 1,na
                        call Locate(avect, aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1), loc2)
                    do s1=1,2
                    do hf1=1,nsht
                    do j=1,nm   
                        pj = j-npm*((j-1)/npm)
                        tj = (j-1)/npm + 1   
                        
                        if (ti==nr) then
                            if (s1==1) then                                
                                probs = 0.0d0
                            else
                                probs = 1.0d0
                            end if                        
                        else                                                
                            if (s1==1) then
                                probs = survivalprobVectF(hf1,2,ti)
                            else if (s1==2) then
                                probs = 1-survivalprobVectF(hf1,2,ti)
                            end if                        
                        end if                        
                        tempx = PumMat(pj,pz,ti-1)*Ptmvect(tj)*PhmatF(hf1, hf, 1, ti-1)*probs*psiRMarried(i,z,vf,vm,h,s,ti-1)/totalAlive 
                        if (loc2 == na) then                                                          
                            psiRWidow(loc2,j,vf,vm,hf1,s1,ti) = psiRWidow(loc2,j,vf,vm,hf1,s1,ti) + tempx                                                                                                                        
                        else    
                            eta2 = ( avect(loc2+1) - aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1) )/( avect(loc2+1) - avect(loc2) )                                                             
                            psiRWidow(loc2,j,vf,vm,hf1,s1,ti) = psiRWidow(loc2,j,vf,vm,hf1,s1,ti) + eta2 * tempx                                                
                            psiRWidow(loc2+1,j,vf,vm,hf1,s1,ti) = psiRWidow(loc2+1,j,vf,vm,hf1,s1,ti) + (1-eta2) * tempx                                                                                                                                                    
                    end if 
                    end do    
                    end do           
                    end do
                    end do
                    end do
                    end do
                    end do
                    end do                 
                
                else if (s==3) then !currently married, next period widower
                
                    do h=1,nhht
                        hm = mod(h+1,2)+1        
                        hf = int((h+1)/2)              
                    do vm=1,naem
                    do vf=1,naef            
                    do z=1,nm
                        pz = z-npm*((z-1)/npm)        
                    do i = 1,na
                        call Locate(avect, aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1), loc2)
                    do s1=1,2
                    do hm1=1,nsht
                    do j=1,nm   
                        pj = j-npm*((j-1)/npm)
                        tj = (j-1)/npm + 1   

                        if (ti==nr) then
                            if (s1==1) then                                
                                probs = 0.0d0
                            else
                                probs = 1.0d0
                            end if                        
                        else                           
                            if (s1==1) then
                                probs = survivalprobVectM(hm1,3,ti)
                            else if (s1==2) then
                                probs = 1-survivalprobVectM(hm1,3,ti)
                            end if
                        end if
                        
                        tempx = PumMat(pj,pz,ti-1)*Ptmvect(tj)*PhmatM(hm1, hm, 1, ti-1)*probs*psiRMarried(i,z,vf,vm,h,s,ti-1)/totalAlive  
                        if (loc2 == na) then                                                          
                            psiRWidower(loc2,j,vf,vm,hm1,s1,ti) = psiRWidower(loc2,j,vf,vm,hm1,s1,ti) + tempx                                                                                                                        
                        else    
                            eta2 = ( avect(loc2+1) - aPolicyRCubeMarried(i,z,vf,vm,h,s,ti-1) )/( avect(loc2+1) - avect(loc2) )                                                             
                            psiRWidower(loc2,j,vf,vm,hm1,s1,ti) = psiRWidower(loc2,j,vf,vm,hm1,s1,ti) + eta2 * tempx                                                
                            psiRWidower(loc2+1,j,vf,vm,hm1,s1,ti) = psiRWidower(loc2+1,j,vf,vm,hm1,s1,ti) + (1-eta2) * tempx                                                                                                                                                    
                    end if 
                    end do    
                    end do           
                    end do
                    end do
                    end do
                    end do
                    end do
                    end do                    
                
                end if            

            end do      
            
            !this period: widow, next period: widow            
            do hf=1,nsht              
            do vm=1,naem
            do vf=1,naef            
            do z=1,nm
                pz = z-npm*((z-1)/npm)        
            do i = 1,na
                call Locate(avect, aPolicyRCubeWidow(i,z,vf,vm,hf,1,ti-1), loc2)
            do s1=1,2
            do hf1=1,nsht              
            do j=1,nm   
                pj = j-npm*((j-1)/npm)
                tj = (j-1)/npm + 1   
                
                if (ti==nr) then
                    if (s1==1) then                                
                        probs = 0.0d0
                    else
                        probs = 1.0d0
                    end if                        
                else 
                    if (s1==1) then
                        probs = survivalprobVectF(hf1,2,ti)
                    else if (s1==2) then
                        probs = 1-survivalprobVectF(hf1,2,ti)
                    end if
                end if
                tempx = PumMat(pj,pz,ti-1)*Ptmvect(tj)*PhmatF(hf1, hf, 2, ti-1)*probs*psiRWidow(i,z,vf,vm,hf,1,ti-1)/totalAlive 
                if (loc2 == na) then                                      
                    psiRWidow(loc2,j,vf,vm,hf1,s1,ti) =  psiRWidow(loc2,j,vf,vm,hf1,s1,ti) + tempx                                                                                               
                else    
                    eta2 = ( avect(loc2+1) - aPolicyRCubeWidow(i,z,vf,vm,hf,1,ti-1) )/( avect(loc2+1) - avect(loc2) )                                                             
                    psiRWidow(loc2,j,vf,vm,hf1,s1,ti) = psiRWidow(loc2,j,vf,vm,hf1,s1,ti) + eta2 * tempx   
                    psiRWidow(loc2+1,j,vf,vm,hf1,s1,ti) = psiRWidow(loc2+1,j,vf,vm,hf1,s1,ti) + (1-eta2) * tempx                                                                                                                            
                end if 
            end do               
            end do
            end do            
            end do
            end do
            end do   
            end do  
            end do   
            
            !this period: widower, next period: widower
            do hm=1,nsht              
            do vm=1,naem
            do vf=1,naef            
            do z=1,nm
                pz = z-npm*((z-1)/npm)        
            do i = 1,na
                call Locate(avect, aPolicyRCubeWidower(i,z,vf,vm,hm,1,ti-1), loc2)
            do s1=1,2
            do hm1=1,nsht              
            do j=1,nm   
                pj = j-npm*((j-1)/npm)
                tj = (j-1)/npm + 1   

                if (ti==nr) then
                    if (s1==1) then                                
                        probs = 0.0d0
                    else
                        probs = 1.0d0
                    end if                        
                else 
                    if (s1==1) then
                        probs = survivalprobVectM(hm1,3,ti)
                    else if (s1==2) then
                        probs = 1-survivalprobVectM(hm1,3,ti)
                    end if
                end if
                tempx = PumMat(pj,pz,ti-1)*Ptmvect(tj)*PhmatM(hm1, hm, 3, ti-1)*probs*psiRWidower(i,z,vf,vm,hm,1,ti-1)/totalAlive  
                if (loc2 == na) then                                      
                    psiRWidower(loc2,j,vf,vm,hm1,s1,ti) =  psiRWidower(loc2,j,vf,vm,hm1,s1,ti) + tempx                                                                                               
                else    
                    eta2 = ( avect(loc2+1) - aPolicyRCubeWidower(i,z,vf,vm,hm,1,ti-1) )/( avect(loc2+1) - avect(loc2) )                                                             
                    psiRWidower(loc2,j,vf,vm,hm1,s1,ti) = psiRWidower(loc2,j,vf,vm,hm1,s1,ti) + eta2 * tempx   
                    psiRWidower(loc2+1,j,vf,vm,hm1,s1,ti) = psiRWidower(loc2+1,j,vf,vm,hm1,s1,ti) + (1-eta2) * tempx                                                                                                                            
                end if 
            end do               
            end do
            end do            
            end do
            end do
            end do   
            end do  
            end do                     
           
            !total = 0.0d0
            !do h=1,nhht
            !   hm = mod(h+1,2)+1        
            !    hf = int((h+1)/2)   
            !    total = total +  (survivalProbVectF(hf,1,ti-1)+survivalProbVectM(hm,1,ti-1)-survivalProbVectF(hf,1,ti-1)*survivalProbVectM(hm,1,ti-1))*dist(h,1,ti-1) &
            !                    + survivalProbVectF(hf,2,ti-1)*dist(h,2,ti-1)  +  survivalProbVectM(hm,3,ti-1)*dist(h,3,ti-1) 
            !end do
            !psiRMarried(:,:,:,:,:,:,ti) = psiRMarried(:,:,:,:,:,:,ti)/total
           ! psiRWidow(:,:,:,:,:,:,ti) = psiRWidow(:,:,:,:,:,:,ti)/total
           ! psiRWidower(:,:,:,:,:,:,ti) = psiRWidower(:,:,:,:,:,:,ti)/total
           !check1 = sum(psiRMarried(:,:,:,:,:,:,ti))
           !check2 = sum(psiRWidow(:,:,:,:,:,:,ti))
           !check3 = sum(psiRWidower(:,:,:,:,:,:,ti))            
            check = sum(psiRMarried(:,:,:,:,:,:,ti))+sum(psiRWidow(:,:,:,:,:,:,ti))+sum(psiRWidower(:,:,:,:,:,:,ti))
            !psiRMarried(:,:,:,:,:,:,ti) = psiRMarried(:,:,:,:,:,:,ti)/check
            !psiRWidow(:,:,:,:,:,:,ti) = psiRWidow(:,:,:,:,:,:,ti)/check
            !psiRWidower(:,:,:,:,:,:,ti) = psiRwidower(:,:,:,:,:,:,ti)/check
            if (abs(check - 1.0d0) > 1.e-7) then
                print *, "psiR", ti, "not summing to 1"
                print *, "check =", check  
                !call Out(0,"dist 1", dist(:,:,1))              
                !call Out(0,"dist 2", sum(sum(sum(sum(psiR(:,:,:,:,:,:,1),4),3),2),1))
                pause
            end if            
        end do
    end subroutine ComputeStationaryDistributions
        
end module ComputeDistributions