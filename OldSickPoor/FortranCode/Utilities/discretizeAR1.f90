  

module discretizeAR1
    use constants
    implicit none
    contains
    

    !Discretizes an AR(1) of the following form:
    !z' = rho*z + eps
    !eps ~ N(mu,sigma^2)
    !with an evenly-spaced grid over
    ![mu_z-m*sigma_z,mu_z+m*sigma_z] and
    !a PTM that is based on the areas
    !under the conditional densities.
    !
    !zvect (1 x N) is a vector of the grid points.
    !Pmat (N x N) is the transition probability matrix.
    !Pmat(i,j) is the probability of z'=zvect(j) given z=zvect(i).
    !    
    subroutine Tauchen(rho, mu_eps, sigma_eps, N, m, zvect, Pmat)
        use mathutils
        use distributions
        implicit none
        
        real(dbl), intent(in):: rho, mu_eps, sigma_eps, m
        integer, intent(in):: N
        real(dbl), intent(out):: zvect(N)
        real(dbl), intent(out):: Pmat(N,N)
        
        real(dbl) mu_z, sigma_z, sigma_zcond
        real(dbl) w
        integer i,j
        
        mu_z = mu_eps/(1-rho)
        sigma_z = sigma_eps/sqrt(1-rho**2)
        sigma_zcond = sigma_eps

        zvect = linspace(mu_z-m*sigma_z,mu_z+m*sigma_z,N)
        w = (zvect(2)-zvect(1))/2
        
        
        do i=1,N
            Pmat(i,1) = enordf((zvect(1) + w - rho*zvect(i) - mu_eps)/sigma_eps)
            do j=2,N-1
               Pmat(i,j) = enordf((zvect(j) + w- rho*zvect(i) - mu_eps)/sigma_eps) - &
                  enordf((zvect(j) - w- rho*zvect(i) - mu_eps)/sigma_eps) 
            end do
            Pmat(i,N) = 1 - enordf((zvect(N) - w - rho*zvect(i) - mu_eps)/sigma_eps)
        end do
      
    
    end subroutine Tauchen
    
 
        
    !Discretizes an AR(1) of the following form:
    !z' = rho*z + eps
    !eps ~ N(mu,sigma^2)
    !using Gauss-Hermite quadrature where the weighting function
    !is the density of z' with variance given by Fixedsigma
    !If Fixedsigma = sigma_zcond then the weighting function is the conditional
    !density of z as Tauchen and Hussey suggest.
    !If Fixedsigma = sigma_z then the weighting function is the unconditional
    !density.
    !If Fixedsigma is a  w x sigma_zcond + (1-w) x sigma_z where w = 0.5 +
    !rho/4 then the weighting function is as suggested by Floden.
    !
    !zvect (1 x N) is a vector of the grid points.
    !Pmat (N x N) is the transition probability matrix.
    !Pmat(i,j) is the probability of z'=zvect(j) given z=zvect(i).
    subroutine TauHuss(rho, mu_eps, sigma_eps, N, Fixedsigma, zvect, Pmat)
        use hermite
        implicit none
        
        real(dbl), intent(in):: rho, mu_eps, sigma_eps, Fixedsigma
        integer, intent(in):: N
        real(dbl), intent(out):: zvect(N)
        real(dbl), intent(out):: Pmat(N,N)
        
        real(dbl) mu_z, sigma_z, sigma_zcond
        real(dbl) xvect(N), wvect(N), colSum(N)
        real(dbl) zMat(N,N), z1Mat(N,N), wMat(N,N)
        real(dbl) f_zjcondzi(N,N), f_zj(N,N)
            
        mu_z = mu_eps/(1-rho)
        sigma_z = sigma_eps/sqrt(1-rho**2)
        sigma_zcond = sigma_eps
    
        call herzo(N, xvect, wvect)
        xvect = -xvect/100
        wvect = wvect/100
        zvect = sqrt(2.0) * Fixedsigma * xvect*100 + mu_z

        zMat = spread(zvect,2,N)
        z1Mat = transpose(zMat)
        wMat = spread(wvect,1,N)

        f_zjcondzi = -((z1Mat-rho*zMat-mu_eps)/sqrt(2.0)/sigma_zcond)**2
        f_zjcondzi = exp(f_zjcondzi)/sqrt(2*pi)/sigma_zcond
        f_zj = -((z1Mat-rho*mu_z-mu_eps)/sqrt(2.0)/Fixedsigma)**2
        f_zj = exp(f_zj)/sqrt(2*pi)/Fixedsigma

        Pmat = wMat*f_zjcondzi/f_zj/sqrt(pi)
        colSum = sum(Pmat,2)
        Pmat = Pmat/(spread(colSum,2,N))

    end subroutine TauHuss


    subroutine Rouwenhurst(rho, mu_eps, sigma_eps, N, zvect, Pmat)
        use mathutils
        implicit none
        
        real(dbl), intent(in):: rho, mu_eps, sigma_eps
        integer, intent(in):: N
        real(dbl), intent(out):: zvect(N)
        real(dbl), intent(out):: Pmat(N,N)
        
        real(dbl) mu_z, sigma_z, q, eps
        real(dbl), allocatable, dimension(:,:):: P1, P2
        integer status, i, j
        
        
        mu_z = mu_eps/(1-rho)
        sigma_z = sigma_eps/sqrt(1-rho**2)
        
        q = (rho+1)/2
        eps = sqrt(dble(N-1)) * sigma_z
        
        if (N == 1) then
            Pmat = 1.0d0
            zvect = mu_z
            return
        else if (N == 2) then
            Pmat = reshape((/q, 1-q, 1-q, q/),(/2,2/))  
            zvect = (/mu_z-eps,mu_z+eps/)
            return
        end if
        
        allocate(P1(2,2),stat=status)        
        P1 = reshape((/q, 1-q, 1-q, q/),(/2,2/))        
                        
        do i=2,N-1            
            allocate(P2(i+1,i+1),stat=status)
            P2 = q * reshape( (/  (/(P1(:,j),0.0d0 ,j=1,i)/) ,  (/(0.0d0,j=1,i+1)/)    /), (/i+1,i+1/) ) + &
                 (1-q) * reshape( (/  (/(0.0d0,j=1,i+1)/), (/ (P1(:,j),0.0d0 ,j=1,i)/)   /) ,   (/i+1,i+1/) ) + &
                 (1-q) * reshape( (/  (/ (0.0d0,P1(:,j) ,j=1,i) /) ,  (/(0.0d0,j=1,i+1)/)  /), (/i+1,i+1/) ) + &
                 q * reshape( (/ (/(0.0d0,j=1,i+1)/), (/(0.0d0,P1(:,j) ,j=1,i)/)   /) ,   (/i+1,i+1/) )
            
            P2(2:i,:) = P2(2:i,:)/2
            
            deallocate(P1,stat=status)            
            
            if (i==N-1) then
                Pmat = P2
            else
                allocate(P1(i+1,i+1), stat=status)
                P1 = P2
            end if
            
            deallocate(P2,stat=status)
        end do
        
        zvect = linspace(mu_z-eps,mu_z+eps,N)

    end subroutine Rouwenhurst


end module discretizeAR1

