
INCLUDE 'link_fnl_static.h'
!DEC$ OBJCOMMENT LIB:"libguide.lib"

module temps
    use constants
    implicit none
    save
    
    integer N2
    real(dbl) e1, e2, sigma_z, mu_z, rho2, sigma_eps2
end module temps  

module addacooper1
    use constants
    implicit none
    contains
    
    subroutine AddaCooper(rho, mu_eps, sigma_eps, N, zvect, Pmat)
        use anordf_int
        use anorin_int
        use qdag_int
        use qdagi_int
        use temps
        implicit none
        
        real(dbl), intent(in):: rho, mu_eps, sigma_eps
        integer, intent(in):: N
        real(dbl), intent(out):: zvect(N)
        real(dbl), intent(out):: Pmat(N,N)
        
        real(dbl) evect(N+1), errabs, errest
        integer i, j, irule
        external faddacooper
        
        errabs = 1.0e-12
        irule = 6
        
        N2 = N
        rho2 = rho
        sigma_eps2 = sigma_eps
        mu_z = mu_eps/(1-rho)
        sigma_z = sigma_eps/sqrt(1-rho**2)        

        evect(1)   = -1.0e6
        evect(N+1) = 1.0e6
        do i = 2,N
            evect(i) = sigma_z*anorin(real((i-1))/N) + mu_z
        end do

        do i = 1,N       
            zvect(i) = N*sigma_z*(norm_pdf((evect(i)-mu_z)/sigma_z) - norm_pdf((evect(i+1)-mu_z)/sigma_z)) + mu_z      
        end do
       
        do i = 2,N-1
            do j = 1,N
                e1 = evect(j)
                e2 = evect(j+1)                
                
                call qdag(faddacooper, evect(i),evect(i+1), Pmat(i,j), errabs=errabs, errrel=errabs, irule = irule, errest=errest)
                
            end do
        end do
        
        do j=1,N
            e1 = evect(j)
            e2 = evect(j+1) 
            call qdagi(faddacooper, evect(2), -1, Pmat(1,j), errabs=errabs,errrel=errabs,errest=errest)
            call qdagi(faddacooper, evect(N), 1, Pmat(N,j), errabs=errabs,errest=errest)
            
        end do

        do i = 1,N
            Pmat(i,:) = Pmat(i,:) / sum(Pmat(i,:));
        end do 
    
    end subroutine
    
    function norm_pdf(x)
        use constants
        implicit none
        real(dbl) x, norm_pdf
        norm_pdf = 1.0d0/sqrt(2.0d0*pi) * exp(-(x)**2/2.0d0)
    end function
    
end module addacooper1
    
    function faddacooper (x)
        use anordf_int
        use temps
        implicit none
        
        real(dbl) faddacooper, x

        faddacooper  = N2/sqrt(2.0d0*pi*sigma_z**2) * (exp(-(x-mu_z)**2 / (2.0d0*sigma_z**2)) * &
                      (anordf((e2-mu_z*(1-rho2)-rho2*x)/sigma_eps2) - anordf((e1-mu_z*(1-rho2)-rho2*x)/sigma_eps2)))
       
    end function faddacooper