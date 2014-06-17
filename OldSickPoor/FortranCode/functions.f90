module functions
    use globals
    implicit none    
    interface U
        module procedure Uscalar
    end interface
    interface transfers
        module procedure transfersScalar, transfersVect, transfersMat, transfersCube
    end interface
    interface transfersRetired
        module procedure transfersRetiredScalar
    end interface
    interface IncomeTax
        module procedure IncomeTaxWorkers, IncomeTaxRetirees
    end interface 
    interface SocSecTax
        module procedure SocSecTaxScalar, SocSecTaxVect
    end interface     
    interface socsec
        module procedure socsecScalar, socsecVect, socsec_joint_s, socsec_joint_v
    end interface 
    
    contains

     !Computes utility for scalar value of consumption c
    function Uscalar(cscalar,labscalar,hhtype,het)
        use globals
        implicit none
        real(dbl), intent(in):: cscalar, labscalar
        integer, intent(in):: hhtype, het
        real(dbl) Uscalar
        
        !hhtype = 1 married retired, 2 widow retired, 3 widower retired, 4 married working        
        if (cscalar < 0) then
            Uscalar = -1d10
        else
            if (NonSeparableUtility == 1) then      
                 if (sigma == 1) then
                    if (hhtype == 1) then
                        Uscalar =2*gamma*log(cscalar/(1+lambda))
                    else if (hhtype == 4) then
                        if (labscalar > 0) then
                            Uscalar =2*gamma*log(cscalar/(1+lambda))+(1-gamma)*(log(1-labscalar)+log(1-hbar)) - phi1(het) !- phi(het)*labscalar**2 
                        else
                            Uscalar =2*gamma*log(cscalar/(1+lambda))+(1-gamma)*(log(1-labscalar)+log(1-hbar))
                        end if
                    else
                        Uscalar = gamma*log(cscalar)
                    end if
                 else
                    if (hhtype == 1) then
                        Uscalar = 2*((cscalar/(1+lambda))**gamma)**(1-sigma)/(1-sigma)
                    else if (hhtype == 4) then
                        if (labscalar > 0) then
                            Uscalar = ((cscalar/(1+lambda))**gamma*(1-labscalar)**(1-gamma))**(1-sigma)/(1-sigma) + & 
 ((cscalar/(1+lambda))**gamma*(1-hbar)**(1-gamma))**(1-sigma)/(1-sigma) - phi1(het) !- phi(het)*labscalar**2 
                        else
                            Uscalar = ((cscalar/(1+lambda))**gamma*(1-labscalar)**(1-gamma))**(1-sigma)/(1-sigma) + & 
((cscalar/(1+lambda))**gamma*(1-hbar)**(1-gamma))**(1-sigma)/(1-sigma)
                        end if
                    else
                        Uscalar = (cscalar**gamma)**(1-sigma)/(1-sigma)
                    end if             
                 end if
             else
                 if (sigma == 1) then
                    if (hhtype == 1) then
                        Uscalar =2*log(cscalar/(1+lambda))+ psiR/(1-gamma)
                    else if (hhtype == 4) then
                        if (labscalar > 0) then
                            Uscalar =2*log(cscalar/(1+lambda))+psi(het)*(1-labscalar)**(1-gamma)/(1-gamma)  - phi1(het)!*labscalar**upsilon 
                        else
                            Uscalar =2*log(cscalar/(1+lambda))+psi(het)/(1-gamma)
                        end if
                    else
                        Uscalar = log(cscalar)+psi(het)/(1-gamma)
                    end if
                 else
                    if (hhtype == 1) then
                        Uscalar = 2*(cscalar/(1+lambda))**(1-sigma)/(1-sigma)+ psiR/(1-gamma)
                    else if (hhtype == 4) then
                        if (labscalar > 0) then
                            Uscalar = 2*(cscalar/(1+lambda))**(1-sigma)/(1-sigma)+ psi(het)* &
(1-labscalar)**(1-gamma)/(1-gamma) - phi1(het)!*labscalar**upsilon 
                        else
                            Uscalar = 2*(cscalar/(1+lambda))**(1-sigma)/(1-sigma)+ psi(het)/(1-gamma)
                        end if
                    else
                        Uscalar = (cscalar)**(1-sigma)/(1-sigma) +psiR/(1-gamma)
                    end if             
                 end if
            end if
        end if
        
     end function Uscalar
      

    function Ucons(cscalar,hhtype)
        use globals
        implicit none
        real(dbl), intent(in):: cscalar
        integer, intent(in):: hhtype
        real(dbl) Ucons
        
        !hhtype = 1 married retired, 2 widow retired, 3 widower retired, 4 married working        
        if (cscalar < 0) then
            Ucons = -1d6
        else
             if (sigma == 1) then
                if (hhtype == 1) then
                    Ucons =2*log(cscalar/(1+lambda))
                else if (hhtype == 4) then                    
                    Ucons =2*log(cscalar/(1+lambda))                    
                else
                    Ucons = log(cscalar)
                end if
             else
                if (hhtype == 1) then
                    Ucons = 2*(cscalar/(1+lambda))**(1-sigma)/(1-sigma)
                else if (hhtype == 4) then                    
                    Ucons = 2*(cscalar/(1+lambda))**(1-sigma)/(1-sigma)                  
                else
                    Ucons = (cscalar)**(1-sigma)/(1-sigma)
                end if             
             end if
        end if               
     end function Ucons
     
     
    function Ulabor(labscalar,hhtype,het)
        use globals
        implicit none
        real(dbl), intent(in):: labscalar
        integer, intent(in):: hhtype,het
        real(dbl) Ulabor
        
        !hhtype = 1 married retired, 2 widow retired, 3 widower retired, 4 married working              
  
        if (hhtype == 1) then
            Ulabor = psi(het)/(1-gamma)
        else if (hhtype == 4) then  
            if (labscalar > 0) then
                Ulabor = psi(het)*(1-labscalar)**(1-gamma)/(1-gamma) - phi1(het) 
            else
                Ulabor = psi(het)/(1-gamma)
            end if                                                  
        else
            Ulabor = psi(het)/(1-gamma)
        end if             
   
     end function Ulabor   

     function transfersScalar(wscalar)
        use globals
        implicit none
        real(dbl), intent(in):: wscalar
        real(dbl) transfersScalar
        
        if (clowerbarW > wscalar)then
            transfersScalar = clowerbarW-wscalar
        else
            transfersScalar =0.0d0
        end if
        
    end function transfersScalar
         
     function transfersVect(wvect)
        use globals
        implicit none
        real(dbl), intent(in):: wvect(:)
        real(dbl) transfersVect(size(wvect,1))
        
        transfersVect = merge(clowerbarW-wvect, 0.0d0, clowerbarW > wvect)
        
    end function transfersVect
         
     function transfersMat(wmat)
        use globals
        implicit none
        real(dbl), intent(in):: wmat(:,:)
        real(dbl) transfersMat(size(wmat,1),size(wmat,2))
        
        transfersMat = merge(clowerbarW-wmat, 0.0d0, clowerbarW > wmat)

    end function transfersMat
    
     function transfersCube(wcube)
        use globals
        implicit none
        real(dbl), intent(in):: wcube(:,:,:)
        real(dbl) transfersCube(size(wcube,1),size(wcube,2),size(wcube,3))
        
        transfersCube = merge(clowerbarW-wcube, 0.0d0, clowerbarW > wcube)

    end function transfersCube    
 
     function transfersRetiredScalar(wscalar, age, shock, health, dying, status, aloc)
        use globals
        implicit none
        real(dbl), intent(in):: wscalar
        integer, intent(in):: age, shock, health, dying, status, aloc
        real(dbl) transfersRetiredScalar, wadj 
        
        wadj = wscalar - avect(aloc) + maxval((/avect(aloc)-ceiling,0.0d0/))
        if (status==1) then            
            if (clowerbarMarried > wadj - mmat(shock,health,dying,status,age)) then
                !if (shock==5 .or. shock==10) then
                !    transfersRetiredScalar = clowerbarMarried + mmat(shock,health,dying,status,age) - wadj 
                !else
                    transfersRetiredScalar = max(clowerbarMarried + FracMedicaidCovers*mmat(shock,health,dying,status,age) - wadj, &
                            clowerbarMarried/2 +mmat(shock,health,dying,status,age) - wadj,0.0d0 )  
                !end if
            else
                transfersRetiredScalar = 0.0d0
            end if       
        else if (status == 2) then                
            if (clowerbarWidow > wadj - mmat(shock,health,dying,status,age)) then
                !if (shock==5 .or. shock==10) then
                !    transfersRetiredScalar = clowerbarWidow + mmat(shock,health,dying,status,age) - wadj
                !else
                    transfersRetiredScalar = max(clowerbarWidow + FracMedicaidCovers*mmat(shock,health,dying,status,age) - wadj, &
                            clowerbarWidow/2 +mmat(shock,health,dying,status,age) - wadj,0.0d0 )  
                !end if
            else
                transfersRetiredScalar = 0.0d0
            end if
        else    
            if (clowerbarWidower > wadj - mmat(shock,health,dying,status,age)) then
               ! if (shock==5 .or. shock==10) then
               !     transfersRetiredScalar = clowerbarWidower + mmat(shock,health,dying,status,age) - wadj
               ! else
                    transfersRetiredScalar = max(clowerbarWidower + FracMedicaidCovers*mmat(shock,health,dying,status,age) - wadj, &
                            clowerbarWidower/2 +mmat(shock,health,dying,status,age) - wadj,0.0d0 )  
               ! end if
            else
                transfersRetiredScalar = 0.0d0
            end if            
        end if
    end function transfersRetiredScalar 

     function IncomeTaxWorkers(iscalar)
        use globals
        implicit none
        real(dbl), intent(in):: iscalar
        real(dbl) IncomeTaxWorkers
        
        if (iscalar < 1.e-8) then
            IncomeTaxWorkers = 0.0d0
            return
        end if
                
        IncomeTaxWorkers = max(0.0d0,(a_mw+PropTax+b_mw*log(iscalar/AveHHIncome))*iscalar + incshifter)
        !IncomeTaxWorkers = max(0.0d0,(a_mw+b_mw*log(iscalar/AveHHIncome))*iscalar + incshifter + PropTax)
        !IncomeTaxWorkers = (a_mw+b_mw*log(iscalar))*iscalar + PropTax*iscalar
        
    end function IncomeTaxWorkers 
        
     function IncomeTaxRetirees(assetinc, ssinc, medexp, hhtype)
        use globals
        implicit none
        real(dbl), intent(in):: assetinc, ssinc, medexp
        integer, intent(in):: hhtype
        real(dbl) IncomeTaxRetirees, iscalar, taxablebenefits, provinc
        
        if (hhtype == 1) then !married retirees
            provinc = assetinc + ssinc/2
            if (provinc < incbase1m) then
                taxablebenefits = 0.0d0
            else if (provinc < incbase2m) then
                taxablebenefits = min(ssinc/2, (provinc-incbase1m)/2)
            else
               taxablebenefits = min(0.85d0*ssinc,0.85d0*(provinc-incbase2m)+min(incmin1m,ssinc/2))
            end if
        else !single retirees
            provinc = assetinc + ssinc/2
            if (provinc < incbase1s) then
                taxablebenefits = 0.0d0
            else if (provinc < incbase2s) then
                taxablebenefits = min(ssinc/2, (provinc-incbase1s)/2)
            else
                taxablebenefits = min(0.85d0*ssinc,0.85d0*(provinc-incbase2s)+min(incmin1s,ssinc/2))
            end if
        end if                    
        
        iscalar = max(0.0d0,((assetinc+taxablebenefits)-max(0.0d0,medexp-medded*(assetinc+taxablebenefits))))
        !iscalar = max(0.0d0,((assetinc)-max(0.0d0,medexp-medded*(assetinc))))
        if (iscalar < 1.e-4) then
            IncomeTaxRetirees = 0.0d0
            return
        end if
        
        if (hhtype == 1) then !married retirees
            IncomeTaxRetirees = max(0.0d0,(a_mr+PropTax+b_mr*log(iscalar/AveHHIncome))*iscalar + incshifter)
            !IncomeTaxRetirees = max(0.0d0,(a_mr+b_mr*log(iscalar/AveHHIncome))*iscalar + incshifter + PropTax)
            !IncomeTaxRetirees = (a_mr+b_mr*log(iscalar))*iscalar + PropTax*iscalar
        else !single retirees
            IncomeTaxRetirees = max(0.0d0,(a_sr+PropTax+b_sr*log(iscalar/AveHHIncome))*iscalar + incshifter)
            !IncomeTaxRetirees = max(0.0d0,(a_sr+b_sr*log(iscalar/AveHHIncome))*iscalar + incshifter + PropTax)
            !IncomeTaxRetirees = (a_sr+b_sr*log(iscalar))*iscalar + PropTax*iscalar
        end if
        
    end function IncomeTaxRetirees
    
    
   function TaxableIncome(assetinc, ssinc, medexp, hhtype)       
        use globals
        implicit none
        real(dbl), intent(in):: assetinc, ssinc, medexp
        integer, intent(in):: hhtype
        real(dbl) TaxableIncome, provinc, taxablebenefits
        
        if (hhtype == 1) then !married retirees
            provinc = assetinc + ssinc/2
            if (provinc < incbase1m) then
                taxablebenefits = 0.0d0
            else if (provinc < incbase2m) then
                taxablebenefits = min(ssinc/2, (provinc-incbase1m)/2)
            else
                taxablebenefits = min(0.85d0*ssinc,0.85d0*(provinc-incbase2m)+min(incmin1m,ssinc/2))
            end if
        else !single retirees
            provinc = assetinc + ssinc/2
            if (provinc < incbase1s) then
                taxablebenefits = 0.0d0
            else if (provinc < incbase2s) then
                taxablebenefits = min(ssinc/2, (provinc-incbase1s)/2)
            else
                taxablebenefits = min(0.85d0*ssinc,0.85d0*(provinc-incbase2s)+min(incmin1s,ssinc/2))
            end if
        end if           
        
        TaxableIncome = max(0.0d0,((assetinc+taxablebenefits)-max(0.0d0,medexp-medded*(assetinc+taxablebenefits))))
                
   end function TaxableIncome        
    
     function SocSecTaxScalar(iscalar)
        use globals
        implicit none
        real(dbl), intent(in):: iscalar
        real(dbl) SocSecTaxScalar
        
        SocSecTaxScalar = tauhatS * minval((/iscalar,emax*AveEarningsAll/))
        
    end function SocSecTaxScalar 
    
     function EarnsTaxFn(iscalar)
        use globals
        implicit none
        real(dbl), intent(in):: iscalar
        real(dbl) EarnsTaxFn
        
        EarnsTaxFn = EarnsTax * iscalar
        
    end function EarnsTaxFn     
    
     function MedicareTax(iscalar)
        use globals
        implicit none
        real(dbl), intent(in):: iscalar
        real(dbl) MedicareTax
        
        MedicareTax = 0.029 * minval((/iscalar,emax*AveEarningsAll/))
        
    end function MedicareTax      
        
     function SocSecTaxVect(ivect)
        use globals
        implicit none
        integer i
        real(dbl), intent(in):: ivect(:)
        real(dbl) SocSecTaxVect(size(ivect,1))
        
        do i=1,size(ivect,1)
            SocSecTaxVect(i) = tauhatS * minval((/ivect(i),emax*AveEarningsAll/))
        end do
    end function SocSecTaxVect   
              
    
    function socsecScalar(escalar)
        use globals
        implicit none
        real(dbl), intent(in):: escalar
        real(dbl) socsecScalar
         
        if (ProportionalSocSec == 1) then 
            socsecScalar = shat * escalar
        else         
            if (escalar <= tau1*AveEarningsAll) then
                socsecScalar = s1*escalar
            else if (escalar <= tau2*AveEarningsAll) then
                socsecScalar = s1*tau1*AveEarningsAll + s2 * (escalar - tau1*AveEarningsAll)
            else if (escalar <= tau3*AveEarningsAll) then
                socsecScalar = s1*tau1*AveEarningsAll + &
                s2 * (tau2*AveEarningsAll - tau1*AveEarningsAll) + &
                s3 * (escalar - tau2*AveEarningsAll)
            else
                socsecScalar = s1*tau1*AveEarningsAll + &
                s2 * (tau2*AveEarningsAll - tau1*AveEarningsAll) + &
                s3 * (tau3*AveEarningsAll - tau2*AveEarningsAll)  
            end if  
        end if       
        if (ScaleDownSocSecBenefit == 1 .or. MoveSocSecToMedicaid==1) then
            socsecScalar = socsecScalar*SocSecReduction
        end if
                                 
    end function socsecScalar  
        
     function socsecVect(evect)
        use globals
        implicit none
        real(dbl), intent(in):: evect(:)
        real(dbl) socsecVect(size(evect,1))
        integer i
        
        if (ProportionalSocSec == 1) then 
            socsecVect = shat * evect
        else
            do i = 1, size(evect,1)
                if (evect(i) <= tau1*AveEarningsAll) then
                    socsecVect(i) = s1*evect(i)
                else if (evect(i) <= tau2*AveEarningsAll) then
                    socsecVect(i) = s1*tau1*AveEarningsAll + s2 * (evect(i) - tau1*AveEarningsAll)
                else if (evect(i) <= tau3*AveEarningsAll) then
                    socsecVect(i) = s1*tau1*AveEarningsAll + &
                    s2 * (tau2*AveEarningsAll - tau1*AveEarningsAll) + &
                    s3 * (evect(i) - tau2*AveEarningsAll)
                else
                    socsecVect(i) = s1*tau1*AveEarningsAll + &
                    s2 * (tau2*AveEarningsAll - tau1*AveEarningsAll) + &
                    s3 * (tau3*AveEarningsAll - tau2*AveEarningsAll)  
                end if    
            end do        
        end if
         
    end function socsecVect

    function socsec_joint_s(efscalar,emscalar,status)
        use globals
        implicit none
        real(dbl), intent(in):: efscalar,emscalar
        integer, intent(in):: status
        real(dbl) socsec_joint_s, socsecf, socsecm        
        socsecf = socsecScalar(efscalar)
        socsecm = socsecScalar(emscalar)        
        if (status == 1) then
            socsec_joint_s = maxval( (/socsecf+socsecm, 1.5d0*socsecf, 1.5d0*socsecm /) )                                   
        else
            socsec_joint_s = maxval( (/socsecf, socsecm/) )
        end if
    end function socsec_joint_s              
    
    function socsec_joint_v(efvect,emvect,statvect)
        use globals
        implicit none
        real(dbl), intent(in):: efvect(:),emvect(:)
        integer, intent(in):: statvect(:)
        real(dbl) socsec_joint_v(size(efvect,1))       
        integer i, n
        n = size(efvect,1)
        do i=1,n
            if (statvect(i) == 1) then
                socsec_joint_v(i) = maxval( (/socsecScalar(efvect(i))+socsecScalar(emvect(i)), &
                    1.5d0*socsecScalar(efvect(i)), 1.5d0*socsecScalar(emvect(i)) /) )                                   
            else
                socsec_joint_v(i) = maxval( (/socsecScalar(efvect(i)), socsecScalar(emvect(i))/) )
            end if
        end do                                            
    end function socsec_joint_v 
    
    function govcons(efscalar,emscalar,status)
        use globals
        implicit none
        real(dbl), intent(in):: efscalar,emscalar
        integer, intent(in):: status
        real(dbl) govcons, govconsf, govconsm        
        
            govconsf = (1-SocSecReduction)/SocSecReduction *socsecScalar(efscalar)
            govconsm = (1-SocSecReduction)/SocSecReduction *socsecScalar(emscalar)        
            if (status == 1) then
                govcons = maxval( (/govconsf+govconsm, 1.5d0*govconsf, 1.5d0*govconsm /) )                                   
            else
                govcons = maxval( (/govconsf, govconsm/) )
            end if              
        
    end function govcons               

    function PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, a1, age,ConsUtil,FoundMax)
        use globals
        use interpolate1
        use mathutils
        implicit none
        real(dbl), intent(in):: a1        
        integer, intent(in):: la, lzf, lzm, lebarf, lebarm, leduch, age, FoundMax
        real(dbl) PhiHatW 
        real(dbl), intent(out):: ConsUtil
        real(dbl) lab, lvect(nl), Vvect(nl), llower, lupper, cons, preTaxIncomeF, preTaxIncomeM, postTaxWealth, mxa1
        integer i, maxl, loca1, leducm, leducf
        !call GoldenSectionSearchl(0.0d0, 1.0d0, lab, PhiHatW, la, lz, lebar, a1, age)            

        !if (age == 4 .and. leduch == 1) then
        !    print *, "here"
        !end if
        if (FoundMax == 0) then
            ConsUtil = -1.0d10
            leducm = mod(leduch+1,2)+1
            leducf = int((leduch+1)/2)        
            llower = 0.0d0
            lupper = 0.7d0
            lvect = linspace(llower, lupper, nl)
            preTaxIncomeM = w*emmat(lzf,lzm,leducm,age)*hbar - SocSecTax(w*emmat(lzf,lzm,leducm,age)*hbar) + & 
0.5d0*r* avect(la) - EarnsTaxFn(w*emmat(lzf,lzm,leducm,age)*hbar)
            call get_interps(avect,a1, loca1, mxa1)
            do i=1,nl
                preTaxIncomeF = w*lvect(i)*efmat(lzf,lzm,leducf,age) - SocSecTax(w*lvect(i)*& 
efmat(lzf,lzm,leducf,age)) + 0.5d0*r* avect(la) - EarnsTaxFn(w*lvect(i)*efmat(lzf,lzm,leducf,age))
                postTaxWealth = preTaxIncomeF + preTaxIncomeM - max(0.0d0,capTax*r*avect(la))  + & 
avect(la) - IncomeTax(preTaxIncomeF+preTaxIncomeM- max(0.0d0,capTax*r*avect(la)))          
                !if (age == 1) postTaxWealth = postTaxWealth + btran(leduch)
                cons = postTaxWealth + transfers(postTaxWealth) - a1   
                if (cons .le. 0.0d0) then
                    Vvect(i) = -1.d10
                else            
                    Vvect(i) = PhiHatl(lvect(i), la, lzf, lzm, lebarf, lebarm, & 
leduch, leducm, leducf, mxa1, loca1, age,cons,ConsUtil)  
                end if
            end do
                    
            maxl = maxloc(Vvect,1)
            lab = lvect(maxl)
            phiHatW = Vvect(maxl)
            if (abs(phiHatW - Vvect(1)) < 1.d-6) phiHatW = Vvect(1) 
        else
            ConsUtil = 0.0d0
            leducm = mod(leduch+1,2)+1
            leducf = int((leduch+1)/2)        
            lvect = linspace(llower, lupper, nl)
            preTaxIncomeM = w*emmat(lzf,lzm,leducm,age)*hbar - SocSecTax(w*emmat(lzf,lzm,leducm,age)*hbar) + 0.5d0*r* avect(la) &
                - EarnsTaxFn(w*emmat(lzf,lzm,leducm,age)*hbar)
            call get_interps(avect,a1, loca1, mxa1)
           
            preTaxIncomeF = w*lab*efmat(lzf,lzm,leducf,age) - SocSecTax(w*lab*efmat(lzf,lzm,leducf,age)) + 0.5d0*r* avect(la) &
                 - EarnsTaxFn(w*lab*efmat(lzf,lzm,leducf,age))
            postTaxWealth = preTaxIncomeF + preTaxIncomeM + avect(la) - IncomeTax(preTaxIncomeF+preTaxIncomeM- & 
max(0.0d0,capTax*r*avect(la))) - max(0.0d0,capTax*r*avect(la))          
            !if (age == 1) postTaxWealth = postTaxWealth + btran(leduch)
            cons = postTaxWealth + transfers(postTaxWealth) - a1   
            if (cons .le. 0.0d0) then
                phiHatW = -1.d10
                ConsUtil = -1.d10
            else            
                phiHatW = PhiHatl(lab, la, lzf, lzm, lebarf, lebarm, leduch, leducm, leducf, mxa1, loca1, age,cons,ConsUtil)  
            end if         
        end if           
    end function PhiHatW 
        
    function PhiHatR(cons, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a1)
        use globals
        use interpolate1
        implicit none
        real(dbl), intent(in):: cons, a1
        integer, intent(in):: CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus
        real(dbl) PhiHatR, V1vectMarried(nm,nhht,4), V1vectWidow(nm,nsht,2), V1vectWidower(nm,nsht,2), Vprime       
        integer i,s, hm, hf, hm1, hf1, hh,pm, tm, pm0
                                
        Vprime = 0.0d0                    
        if (CurStatus==1) then  
            hm = mod(CurHealth+1,2)+1
            hf = int((CurHealth+1)/2)    
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm)              
 
            if (FutStatus == 1) then
                do s=1,4
                do hh =1,nhht
                do i=1,nm !over h1vect           
                    call Interpolate(avect, VnewRCubeMarried(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectMarried(i,hh,s))                  
                end do  
                end do
                end do
            
                do hh=1,nhht
                    hm1 = mod(hh+1,2)+1
                    hf1 = int((hh+1)/2)  
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1             
                    if (CurAge == 17) then
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* &
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*V1vectMarried(i,hh,4)                    
                    else
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*survivalprobVectF(hf1,FutStatus,CurAge+1)* &
survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectMarried(i,hh,1)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*survivalprobVectF(hf1,FutStatus,CurAge+1)* & 
(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectMarried(i,hh,2)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus,CurAge+1))* &
survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectMarried(i,hh,3)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus,CurAge+1))* & 
(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectMarried(i,hh,4)
                    end if
                end do
                end do            
            else if (FutStatus == 2) then
                do s=1,2
                do hh =1,nsht
                do i=1,nm !over h1vect             
                    call Interpolate(avect, VnewRCubeWidow(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectWidow(i,hh,s))                       
                end do  
                end do
                end do 
                
                do hf1=1,nsht                   
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1
                    if (CurAge == 17) then                        
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*V1vectWidow(i,hf1,2)                    
                    else            
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*survivalprobVectF(hf1,FutStatus,CurAge+1)*V1vectWidow(i,hf1,1)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus,CurAge+1))*V1vectWidow(i,hf1,2)
                    end if
                end do
                end do                   
            
            else !FutStatus ==3 
                do s=1,2
                do hh =1,nsht
                do i=1,nm !over h1vect           
                    call Interpolate(avect, VnewRCubeWidower(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), &
 a1, V1vectWidower(i,hh,s))               
                end do  
                end do
                end do 
                            
                do hm1=1,nsht                   
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1 
                    if (CurAge == 17) then                        
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*V1vectWidower(i,hm1,2)                    
                    else            
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectWidower(i,hm1,1)
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectWidower(i,hm1,2)
                    end if    
                end do
                end do                   
          
            end if
            
            if (cons - a1 < 0) then
                PhiHatR = U(0.00001d0,0.0d0,CurStatus,0) + beta * Vprime
                print *, "negative"                
            else
                PhiHatR = U(cons-a1,0.0d0,CurStatus,0) + beta * Vprime
            end if         
        else if (CurStatus==2) then
        
            hf = CurHealth
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm) 
      
            do s=1,2
            do hh =1,nsht
            do i=1,nm !over h1vect             
                call Interpolate(avect, VnewRCubeWidow(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), a1, V1vectWidow(i,hh,s))                       
            end do  
            end do
            end do                                    
         
            do hf1=1,nsht           
            do i=1,nm    
                pm = i-npm*((i-1)/npm)
                tm = (i-1)/npm + 1 
                if (CurAge == 17) then                        
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*V1vectWidow(i,hf1,2)                                                          
                else                        
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*survivalprobVectF(hf1,FutStatus+1,CurAge+1)*V1vectWidow(i,hf1,1)                                                          
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus+1,CurAge+1))*V1vectWidow(i,hf1,2)                                                          
                end if
            end do
            end do
                   
            if (cons - a1 < 0) then
                PhiHatR = U(0.00001d0,0.0d0,CurStatus,0) + beta * Vprime
                print *, "negative"
            else
                PhiHatR = U(cons-a1,0.0d0,CurStatus,0) + beta * Vprime
            end if
        else
        
            hm = CurHealth            
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm)                  
            
            do s=1,2
            do hh =1,nsht
            do i=1,nm !over h1vect           
                call Interpolate(avect, VnewRCubeWidower(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), a1, V1vectWidower(i,hh,s))               
            end do  
            end do
            end do              

            do hm1=1,nsht        
            do i=1,nm     
                pm = i-npm*((i-1)/npm)
                tm = (i-1)/npm + 1    
                if (CurAge == 17) then                        
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*V1vectWidower(i,hm1,2)                                                          
                else                  
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*survivalprobVectM(hm1,FutStatus+2,CurAge+1)*V1vectWidower(i,hm1,1)  
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*(1-survivalprobVectM(hm1,FutStatus+2,CurAge+1))*V1vectWidower(i,hm1,2)                                                          
                end if    
            end do
            end do         
            if (cons - a1 < 0) then
                PhiHatR = U(0.00001d0,0.0d0,CurStatus,0) + beta * Vprime
                print *, "negative"
            else
                PhiHatR = U(cons-a1,0.0d0,CurStatus,0) + beta * Vprime
            end if            
        end if
      
     end function PhiHatR 
     
    function ConsUtilr(cons, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a1)
        use globals
        use interpolate1
        implicit none
        real(dbl), intent(in):: cons, a1
        integer, intent(in):: CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus
        real(dbl) ConsUtilr, V1vectMarried(nm,nhht,4), V1vectWidow(nm,nsht,2), V1vectWidower(nm,nsht,2), Vprime       
        integer i,s, hm, hf, hm1, hf1, hh,pm, tm, pm0

        Vprime = 0.0d0                    
        if (CurStatus==1) then  
            hm = mod(CurHealth+1,2)+1
            hf = int((CurHealth+1)/2)    
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm)              
 
            if (FutStatus == 1) then
                do s=1,4
                do hh =1,nhht
                do i=1,nm !over h1vect           
                    call Interpolate(avect, UtilConsRCubeMarried(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectMarried(i,hh,s))                  
                end do  
                end do
                end do
            
                do hh=1,nhht
                    hm1 = mod(hh+1,2)+1
                    hf1 = int((hh+1)/2)  
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1   
                    if (CurAge == 17) then                        
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*V1vectMarried(i,hh,4)
                    else           
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*survivalprobVectF(hf1,FutStatus,CurAge+1)* & 
survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectMarried(i,hh,1)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*survivalprobVectF(hf1,FutStatus,CurAge+1)* & 
(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectMarried(i,hh,2)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus,CurAge+1))* & 
survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectMarried(i,hh,3)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*PhmatM(hm1,hm,CurStatus,CurAge)* & 
Pummat(pm,pm0,CurAge)*Ptmvect(tm)*(1-survivalprobVectF(hf1,FutStatus,CurAge+1))* &
(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectMarried(i,hh,4)
                    end if
                end do
                end do            
            else if (FutStatus == 2) then
                do s=1,2
                do hh =1,nsht
                do i=1,nm !over h1vect             
                    call Interpolate(avect, UtilConsRCubeWidow(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectWidow(i,hh,s))                       
                end do  
                end do
                end do 
                
                do hf1=1,nsht                   
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1    
                    if (CurAge == 17) then                        
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
V1vectWidow(i,hf1,2)
                    else          
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
survivalprobVectF(hf1,FutStatus,CurAge+1)*V1vectWidow(i,hf1,1)
                        Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
(1-survivalprobVectF(hf1,FutStatus,CurAge+1))*V1vectWidow(i,hf1,2)
                    end if
                end do
                end do                   
            
            else !FutStatus ==3 
                do s=1,2
                do hh =1,nsht
                do i=1,nm !over h1vect           
                    call Interpolate(avect, UtilConsRCubeWidower(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectWidower(i,hh,s))               
                end do  
                end do
                end do 
                            
                do hm1=1,nsht                   
                do i=1,nm
                    pm = i-npm*((i-1)/npm)
                    tm = (i-1)/npm + 1 
                    if (CurAge == 17) then                        
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
V1vectWidower(i,hm1,2)
                    else            
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
survivalprobVectM(hm1,FutStatus,CurAge+1)*V1vectWidower(i,hm1,1)
                        Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
(1-survivalprobVectM(hm1,FutStatus,CurAge+1))*V1vectWidower(i,hm1,2)
                    end if
                end do
                end do                   
          
            end if
            
            if (cons - a1 < 0) then
                ConsUtilr = Ucons(0.00001d0,CurStatus) + beta * Vprime
                print *, "negative"                
            else
                ConsUtilr = Ucons(cons-a1,CurStatus) + beta * Vprime
            end if         
        else if (CurStatus==2) then
        
            hf = CurHealth
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm) 
      
            do s=1,2
            do hh =1,nsht
            do i=1,nm !over h1vect             
                call Interpolate(avect, UtilConsRCubeWidow(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), a1, V1vectWidow(i,hh,s))                       
            end do  
            end do
            end do                                    
         
            do hf1=1,nsht           
            do i=1,nm    
                pm = i-npm*((i-1)/npm)
                tm = (i-1)/npm + 1 
                if (CurAge == 17) then                        
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
V1vectWidow(i,hf1,2)                                                          
                else                                        
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
survivalprobVectF(hf1,FutStatus+1,CurAge+1)*V1vectWidow(i,hf1,1)                                                          
                    Vprime = Vprime + PhmatF(hf1,hf,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)*Ptmvect(tm)* & 
(1-survivalprobVectF(hf1,FutStatus+1,CurAge+1))*V1vectWidow(i,hf1,2)                                                          
                end if
            end do
            end do
                   
            if (cons - a1 < 0) then
                ConsUtilr = Ucons(0.00001d0,CurStatus) + beta * Vprime
                print *, "negative"
            else
                ConsUtilr = Ucons(cons-a1,CurStatus) + beta * Vprime
            end if
        else
        
            hm = CurHealth            
            pm0 = CurMedLoc-npm*((CurMedLoc-1)/npm)                  
            
            do s=1,2
            do hh =1,nsht
            do i=1,nm !over h1vect           
                call Interpolate(avect, UtilConsRCubeWidower(:,i,CurEbarFLoc, CurEbarMLoc, hh, s, CurAge+1), & 
a1, V1vectWidower(i,hh,s))               
            end do  
            end do
            end do              

            do hm1=1,nsht        
            do i=1,nm     
                pm = i-npm*((i-1)/npm)
                tm = (i-1)/npm + 1  
                if (CurAge == 17) then                        
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*V1vectWidower(i,hm1,2)                                                          
                else                    
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*survivalprobVectM(hm1,FutStatus+2,CurAge+1)*V1vectWidower(i,hm1,1)  
                    Vprime = Vprime + PhmatM(hm1,hm,CurStatus,CurAge)*Pummat(pm,pm0,CurAge)* & 
Ptmvect(tm)*(1-survivalprobVectM(hm1,FutStatus+2,CurAge+1))*V1vectWidower(i,hm1,2)                                                          
                end if
            end do
            end do         
            if (cons - a1 < 0) then
                ConsUtilr = Ucons(0.00001d0,CurStatus) + beta * Vprime
                print *, "negative"
            else
                ConsUtilr = Ucons(cons-a1,CurStatus) + beta * Vprime
            end if            
        end if
      
     end function ConsUtilr           
   
    function PhiHatl(lab, la, lzf, lzm, lebarf, lebarm, leduch, leducm, leducf, mxa1, loca1, age,cons,ConsUtil)
        use globals
        use interpolate1
        use output
        implicit none
        real(dbl), intent(in):: lab, mxa1, cons
        integer, intent(in):: la, lzf, lzm, lebarf, lebarm, leduch, leducm, leducf, loca1, age
        real(dbl), intent(out):: ConsUtil
        real(dbl) PhiHatl
        real(dbl) ebarf1, ebarm1, V1vect(nm,nhht,4), V1mat(nef,nem), yt(8), mxs(3), Vprime, probs, yt0(8), V1vect0(nm,nhht,4)
        integer i, j, locs(3), s, h, pm, tm , hm, hf
               
        ebarf1 = (w*lab*efmat(lzf,lzm,leducf,age) + (age-1)*aveEarnFvect(lebarf))/age 
        ebarm1 = (w*hbar*emmat(lzf,lzm,leducm,age) + (age-1)*aveEarnMvect(lebarm))/age   
        
        locs(1) = loca1
        mxs(1) = mxa1
                
        call get_interps(aveEarnFvect,ebarf1, locs(2), mxs(2))
        call get_interps(aveEarnMvect,ebarm1, locs(3), mxs(3))
        !call get_interp3d(avect, aveEarnFvect, aveEarnMvect, a1, ebarf1, ebarm1, locs, mxs)              
        if (age == nw) then    
            !do i=1,nm !over h1vect                       
                !call interpol2d(avect, aveEarnFvect, VnewRCube(:,i,:,1), a1, ebarf1,  V1vect(i))  
                !call interpol3d(avect, aveEarnFvect, aveEarnMvect, VnewRCube(:,i,:,:,1), a1, ebarf1, ebarm1, V1vect(i))  
            !end do   
            Vprime = 0.0d0 
            do s=1,4               
            do h=1,nhht  
                hm = mod(h+1,2)+1
                hf = int((h+1)/2)                       
            do i=1,nm
                pm = i-npm*((i-1)/npm)            
                tm = (i-1)/npm + 1                         
	            yt(1) = VnewRCubeMarried(locs(1),i,locs(2),locs(3),h,s,1)
	            yt(2) = VnewRCubeMarried(locs(1)+1,i,locs(2),locs(3),h,s,1)
	            yt(3) = VnewRCubeMarried(locs(1),i,locs(2)+1,locs(3),h,s,1)
	            yt(4) = VnewRCubeMarried(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	            yt(5) = VnewRCubeMarried(locs(1),i,locs(2),locs(3)+1,h,s,1)
	            yt(6) = VnewRCubeMarried(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	            yt(7) = VnewRCubeMarried(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	            yt(8) = VnewRCubeMarried(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	
            	
	            V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                        mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                        (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                        mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                        (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                        mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                        (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                        mxs(1)*mxs(2)*mxs(3)*yt(8)                         
                 if (s==1) then
                    probs = survivalprobVectF(hf,1,1)*survivalprobVectM(hm,1,1)
                 else if (s==2) then
                    probs = survivalprobVectF(hf,1,1)*(1-survivalprobVectM(hm,1,1))
                 else if (s==3) then
                    probs = (1-survivalprobVectF(hf,1,1))*survivalprobVectM(hm,1,1)
                 else
                    probs = (1-survivalprobVectF(hf,1,1))*(1-survivalprobVectM(hm,1,1))
                 end if
                                             
                Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitH(h,leduch)*sinitH(1,lebarm)*probs*V1vect(i,h,s)                        
            end do
            end do 
            end do 

            do s=1,2
            do h=1,nsht                     
            do i=1,nm
                pm = i-npm*((i-1)/npm)            
                tm = (i-1)/npm + 1         
                
                !if average earnings of male is not 0 starting at age 65                        
	            yt(1) = VnewRCubeWidow(locs(1),i,locs(2),locs(3),h,s,1)
	            yt(2) = VnewRCubeWidow(locs(1)+1,i,locs(2),locs(3),h,s,1)
	            yt(3) = VnewRCubeWidow(locs(1),i,locs(2)+1,locs(3),h,s,1)
	            yt(4) = VnewRCubeWidow(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	            yt(5) = VnewRCubeWidow(locs(1),i,locs(2),locs(3)+1,h,s,1)
	            yt(6) = VnewRCubeWidow(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	            yt(7) = VnewRCubeWidow(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	            yt(8) = VnewRCubeWidow(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	
            	
	            V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                        mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                        (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                        mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                        (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                        mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                        (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                        mxs(1)*mxs(2)*mxs(3)*yt(8)  
                
                !if average earnings of male is 0 starting at age 65        
                yt0(1) = VnewRCubeWidow(locs(1),i,locs(2),1,h,s,1)
	            yt0(2) = VnewRCubeWidow(locs(1)+1,i,locs(2),1,h,s,1)
	            yt0(3) = VnewRCubeWidow(locs(1),i,locs(2)+1,1,h,s,1)
	            yt0(4) = VnewRCubeWidow(locs(1)+1,i,locs(2)+1,1,h,s,1)	                	
            	
	            V1vect0(i,h,s) =    (1-mxs(1))*(1-mxs(2))*yt0(1) + &
                        mxs(1)*(1-mxs(2))*yt0(2) + &
                        (1-mxs(1))*mxs(2)*yt0(3) + &
                        mxs(1)*mxs(2)*yt0(4)                         
                        
                 if (s==1) then
                    probs = survivalprobVectF(h,2,1)
                 else if (s==2) then
                    probs = 1-survivalprobVectF(h,2,1)
                 end if
                                         
                Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitF(h,leducf)* & 
sinitH(2,lebarm)*probs*(1-probHusband0)*V1vect(i,h,s) 
                Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitF(h,leducf)* & 
sinitH(2,lebarm)*probs*probHusband0*V1vect0(i,h,s)                         
            end do
            end do 
            end do   

            do s=1,2
            do h=1,nsht                     
            do i=1,nm
                pm = i-npm*((i-1)/npm)            
                tm = (i-1)/npm + 1                         
	            yt(1) = VnewRCubeWidower(locs(1),i,locs(2),locs(3),h,s,1)
	            yt(2) = VnewRCubeWidower(locs(1)+1,i,locs(2),locs(3),h,s,1)
	            yt(3) = VnewRCubeWidower(locs(1),i,locs(2)+1,locs(3),h,s,1)
	            yt(4) = VnewRCubeWidower(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	            yt(5) = VnewRCubeWidower(locs(1),i,locs(2),locs(3)+1,h,s,1)
	            yt(6) = VnewRCubeWidower(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	            yt(7) = VnewRCubeWidower(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	            yt(8) = VnewRCubeWidower(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	
            	
	            V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                        mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                        (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                        mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                        (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                        mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                        (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                        mxs(1)*mxs(2)*mxs(3)*yt(8)  
                 if (s==1) then
                    probs = survivalprobVectM(h,3,1)
                 else if (s==2) then
                    probs = 1-survivalprobVectM(h,3,1)
                 end if
                                         
                Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitM(h,leducm)*sinitH(3,lebarm)*probs*V1vect(i,h,s)                        
            end do
            end do 
            end do                               
            
            PhiHatl = U(cons,lab,4,leduch)  + beta * Vprime    
            if (ConsUtil >-1.0d0) then
                Vprime = 0.0d0 
                do s=1,4
                do h=1,nhht  
                    hm = mod(h+1,2)+1
                    hf = int((h+1)/2)                                   
                do i=1,nm
                    pm = i-npm*((i-1)/npm)            
                    tm = (i-1)/npm + 1                         
                    
	                yt(1) = UtilConsRCubeMarried(locs(1),i,locs(2),locs(3),h,s,1)
	                yt(2) = UtilConsRCubeMarried(locs(1)+1,i,locs(2),locs(3),h,s,1)
	                yt(3) = UtilConsRCubeMarried(locs(1),i,locs(2)+1,locs(3),h,s,1)
	                yt(4) = UtilConsRCubeMarried(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	                yt(5) = UtilConsRCubeMarried(locs(1),i,locs(2),locs(3)+1,h,s,1)
	                yt(6) = UtilConsRCubeMarried(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	                yt(7) = UtilConsRCubeMarried(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	                yt(8) = UtilConsRCubeMarried(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	
                	
	                V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                            mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                            (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                            mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                            (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                            mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                            (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                            mxs(1)*mxs(2)*mxs(3)*yt(8)
                     if (s==1) then
                        probs = survivalprobVectF(hf,1,1)*survivalprobVectM(hm,1,1)
                     else if (s==2) then
                        probs = survivalprobVectF(hf,1,1)*(1-survivalprobVectM(hm,1,1))
                     else if (s==3) then
                        probs = (1-survivalprobVectF(hf,1,1))*survivalprobVectM(hm,1,1)
                     else
                        probs = (1-survivalprobVectF(hf,1,1))*(1-survivalprobVectM(hm,1,1))
                     end if                              
                    Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitH(h,leduch)*sinitH(1,lebarm)*probs*V1vect(i,h,s)                        
                end do
                end do 
                end do    

               do s=1,2
                do h=1,nsht                     
                do i=1,nm
                    pm = i-npm*((i-1)/npm)            
                    tm = (i-1)/npm + 1                         
                    
                    !if average earnings of male is not 0 starting at age 65     
	                yt(1) = UtilConsRCubeWidow(locs(1),i,locs(2),locs(3),h,s,1)
	                yt(2) = UtilConsRCubeWidow(locs(1)+1,i,locs(2),locs(3),h,s,1)
	                yt(3) = UtilConsRCubeWidow(locs(1),i,locs(2)+1,locs(3),h,s,1)
	                yt(4) = UtilConsRCubeWidow(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	                yt(5) = UtilConsRCubeWidow(locs(1),i,locs(2),locs(3)+1,h,s,1)
	                yt(6) = UtilConsRCubeWidow(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	                yt(7) = UtilConsRCubeWidow(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	                yt(8) = UtilConsRCubeWidow(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	                                	
                	
	                V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                            mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                            (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                            mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                            (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                            mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                            (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                            mxs(1)*mxs(2)*mxs(3)*yt(8)  
                            
                    !if average earnings of male is 0 starting at age 65        
                    yt0(1) = UtilConsRCubeWidow(locs(1),i,locs(2),1,h,s,1)
	                yt0(2) = UtilConsRCubeWidow(locs(1)+1,i,locs(2),1,h,s,1)
	                yt0(3) = UtilConsRCubeWidow(locs(1),i,locs(2)+1,1,h,s,1)
	                yt0(4) = UtilConsRCubeWidow(locs(1)+1,i,locs(2)+1,1,h,s,1)	                	
                	
	                V1vect0(i,h,s) =    (1-mxs(1))*(1-mxs(2))*yt0(1) + &
                            mxs(1)*(1-mxs(2))*yt0(2) + &
                            (1-mxs(1))*mxs(2)*yt0(3) + &
                            mxs(1)*mxs(2)*yt0(4)                                              
                            
                     if (s==1) then
                        probs = survivalprobVectF(h,2,1)
                     else if (s==2) then
                        probs = 1-survivalprobVectF(h,2,1)
                     end if                            
                            
                    Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitF(h,leducf)* & 
sinitH(2,lebarm)*probs*(1-probHusband0)*V1vect(i,h,s)   
                    Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitF(h,leducf)* & 
sinitH(2,lebarm)*probs*probHusband0*V1vect0(i,h,s)                           
                end do
                end do 
                end do    
                
               do s=1,2
                do h=1,nsht                     
                do i=1,nm
                    pm = i-npm*((i-1)/npm)            
                    tm = (i-1)/npm + 1                         
                    
	                yt(1) = UtilConsRCubeWidower(locs(1),i,locs(2),locs(3),h,s,1)
	                yt(2) = UtilConsRCubeWidower(locs(1)+1,i,locs(2),locs(3),h,s,1)
	                yt(3) = UtilConsRCubeWidower(locs(1),i,locs(2)+1,locs(3),h,s,1)
	                yt(4) = UtilConsRCubeWidower(locs(1)+1,i,locs(2)+1,locs(3),h,s,1)
	                yt(5) = UtilConsRCubeWidower(locs(1),i,locs(2),locs(3)+1,h,s,1)
	                yt(6) = UtilConsRCubeWidower(locs(1)+1,i,locs(2),locs(3)+1,h,s,1)
	                yt(7) = UtilConsRCubeWidower(locs(1),i,locs(2)+1,locs(3)+1,h,s,1)
	                yt(8) = UtilConsRCubeWidower(locs(1)+1,i,locs(2)+1,locs(3)+1,h,s,1)       	
                	
	                V1vect(i,h,s) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                            mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                            (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                            mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                            (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                            mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                            (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                            mxs(1)*mxs(2)*mxs(3)*yt(8) 
                     if (s==1) then
                        probs = survivalprobVectM(h,3,1)
                     else if (s==2) then
                        probs = 1-survivalprobVectM(h,3,1)                     
                     end if                            
                             
                    Vprime = Vprime + minit(pm,leduch)*Ptmvect(tm)*hinitM(h,leducm)*sinitH(3,lebarm)*probs*V1vect(i,h,s)                        
                end do
                end do 
                end do                                                     
                
                ConsUtil = Ucons(cons,4)  + beta * Vprime   
            end if
!!!!!!!!!!!!!!!!!!!!!!!                                                                                         
            
        else            
            !do i=1,nef !over h1vect                       
            !do j=1,nem
                !call interpol2d(avect, aveEarnFvect, VnewWCube(:,i,j,:,v,age+1), a1, ebarf1,  V1vect(i)) 
            !    call interpol3d(avect, aveEarnFvect, aveEarnMvect, VnewWCube(:,i,j,:,:,age+1), a1, ebarf1, ebarm1, V1mat(i,j))  
            !end do    
            !end do                          
            
            do i=1,nef !over h1vect                       
            do j=1,nem
	            yt(1) = VnewWCube(locs(1),i,j,locs(2),locs(3),leduch,age+1)
	            yt(2) = VnewWCube(locs(1)+1,i,j,locs(2),locs(3),leduch,age+1)
	            yt(3) = VnewWCube(locs(1),i,j,locs(2)+1,locs(3),leduch,age+1)
	            yt(4) = VnewWCube(locs(1)+1,i,j,locs(2)+1,locs(3),leduch,age+1)
	            yt(5) = VnewWCube(locs(1),i,j,locs(2),locs(3)+1,leduch,age+1)
	            yt(6) = VnewWCube(locs(1)+1,i,j,locs(2),locs(3)+1,leduch,age+1)
	            yt(7) = VnewWCube(locs(1),i,j,locs(2)+1,locs(3)+1,leduch,age+1)
	            yt(8) = VnewWCube(locs(1)+1,i,j,locs(2)+1,locs(3)+1,leduch,age+1)        	
            	
	            V1mat(i,j) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                        mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                        (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                        mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                        (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                        mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                        (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                        mxs(1)*mxs(2)*mxs(3)*yt(8)  
            end do 
            end do           
            PhiHatl = U(cons,lab,4,leduch) + &
                beta *  sum(spread(Puefmat(:,lzf),2,nem)*spread(Puemmat(:,lzm),1,nef) * V1mat)      
            if (ConsUtil >-1.0d0) then    
                do i=1,nef !over h1vect                       
                do j=1,nem
	                yt(1) = UtilConsWCube(locs(1),i,j,locs(2),locs(3),leduch,age+1)
	                yt(2) = UtilConsWCube(locs(1)+1,i,j,locs(2),locs(3),leduch,age+1)
	                yt(3) = UtilConsWCube(locs(1),i,j,locs(2)+1,locs(3),leduch,age+1)
	                yt(4) = UtilConsWCube(locs(1)+1,i,j,locs(2)+1,locs(3),leduch,age+1)
	                yt(5) = UtilConsWCube(locs(1),i,j,locs(2),locs(3)+1,leduch,age+1)
	                yt(6) = UtilConsWCube(locs(1)+1,i,j,locs(2),locs(3)+1,leduch,age+1)
	                yt(7) = UtilConsWCube(locs(1),i,j,locs(2)+1,locs(3)+1,leduch,age+1)
	                yt(8) = UtilConsWCube(locs(1)+1,i,j,locs(2)+1,locs(3)+1,leduch,age+1)        	
                	
	                V1mat(i,j) =    (1-mxs(1))*(1-mxs(2))*(1-mxs(3))*yt(1) + &
                            mxs(1)*(1-mxs(2))*(1-mxs(3))*yt(2) + &
                            (1-mxs(1))*mxs(2)*(1-mxs(3))*yt(3) + &
                            mxs(1)*mxs(2)*(1-mxs(3))*yt(4) + &
                            (1-mxs(1))*(1-mxs(2))*mxs(3)*yt(5) + &
                            mxs(1)*(1-mxs(2))*mxs(3)*yt(6) + &
                            (1-mxs(1))*mxs(2)*mxs(3)*yt(7) + &
                            mxs(1)*mxs(2)*mxs(3)*yt(8)  
                end do 
                end do           
                ConsUtil = Ucons(cons,4) + &
                    beta *  sum(spread(Puefmat(:,lzf),2,nem)*spread(Puemmat(:,lzm),1,nef) * V1mat) 
            end if                        
        end if         
     end function PhiHatl  	                      
     
     
     subroutine GoldenSectionSearchW(lower, upper, MaxValx, MaxValfx, lab, la, lzf, lzm, lebarf, lebarm, leduch, age,ConsUtil)
        implicit none

        real(dbl), intent(in):: upper, lower
        integer, intent(in):: la, lzf, lzm, lebarf, lebarm, leduch, age
        real(dbl), intent(out):: MaxValx, MaxValfx, lab, ConsUtil
        real(dbl) a1, a2, a3, a4, fa2, fa3, p
        
        p = (sqrt(5.0)-1)/2
        a1 = lower
        a4 = upper
        a2 = p*a1 + (1-p)*a4
        a3 = (1-p)*a1 + p*a4                   
        fa2 = PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, a2, age, ConsUtil,0)
        fa3 = PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, a3, age, ConsUtil,0)
    
        do
            if (fa2 > fa3) then
                a4 = a3 
                a3 = a2
                fa3 = fa2
                a2 = p*a3 + (1-p)*a1
                fa2 = PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, a2, age, ConsUtil,0)
            else 
                a1 = a2
                a2 = a3
                fa2 = fa3
                a3 = p*a2 + (1-p)*a4
                fa3 = PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, a3, age, ConsUtil,0)
            end if            
            !print *, "diff=", abs(a4-a1)
            if (abs(a4-a1) < eps*max(1.0,abs(a2)+abs(a3))) then
                exit
            end if
        end do        
        MaxValx = a2
        if (MaxValx > upper) then
            MaxValx = upper
            print *, "Greater"
            pause
        else if (MaxValx < lower) then
            MaxValx = lower
            print *, "Less"
        end if                    
        MaxValfx = PhiHatW(lab, la, lzf, lzm, lebarf, lebarm, leduch, MaxValx, age, ConsUtil,1)                
	 end subroutine GoldenSectionSearchW    

     subroutine GoldenSectionSearchR(lower, upper, c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, &
 CurStatus, FutStatus, MaxValx, MaxValfx,ConsUtil)
        implicit none

        real(dbl), intent(in):: upper, lower
        real(dbl), intent(in):: c1
        integer, intent(in):: CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurStatus
        integer, intent(in):: CurHealth, FutStatus
        real(dbl), intent(out):: MaxValx, MaxValfx, ConsUtil
        real(dbl) a1, a2, a3, a4, fa2, fa3, p 
        
        p = (sqrt(5.0)-1)/2
        a1 = lower
        a4 = upper
        a2 = p*a1 + (1-p)*a4
        a3 = (1-p)*a1 + p*a4  
        fa2 = PhiHatR(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a2)
        fa3 = PhiHatR(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a3)

        do
            if (fa2 > fa3) then
                a4 = a3 
                a3 = a2
                fa3 = fa2
                a2 = p*a3 + (1-p)*a1
                fa2 = PhiHatR(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a2)
            else 
                a1 = a2
                a2 = a3
                fa2 = fa3
                a3 = p*a2 + (1-p)*a4                
                fa3 = PhiHatR(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, a3)               
            end if
            
            if (abs(a4-a1) < eps*max(1.0,abs(a2)+abs(a3))) then
                exit
            end if
        end do        
        MaxValx = a2
        
        !print *, "upper =", upper, "lower=", lower, "MaxValx=", MaxValx, "avect(MaxL) =", avect(MaxL)
        !pause
        if (MaxValx > upper) then
            MaxValx = upper
            print *, "Greater"
        else if (MaxValx < lower) then
            MaxValx = lower
            print *, "Less"
        end if
        MaxValfx = PhiHatR(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, MaxValx)
        ConsUtil = ConsUtilr(c1, CurAge, CurMedLoc, CurEbarFLoc, CurEbarMLoc, CurHealth, CurStatus, FutStatus, MaxValx)
	end subroutine GoldenSectionSearchR     
	
end module functions
