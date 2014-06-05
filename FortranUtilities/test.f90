program main
        use hermite
        implicit none
        integer :: N = 6;
        real(kind(1d0)):: xvect(6), wvect(6)
        call herzo(6, xvect, wvect);          
end
