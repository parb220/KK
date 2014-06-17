module Policies
    use functions
    use interpolate1
    use output   
    implicit none 
    contains

subroutine ComputePolicies()
    use functions
    implicit none
    include 'mpif.h'
        
    real(dbl) postTaxWealth, preTaxIncome, ssIncome              
    integer ti, i, j, n, vf, vm, s, h  
    integer num_sent_em, num_sent_ef, num_sent_aem, num_sent_aef, num_sent_et, num_recv, sender
    integer num_sent_m, num_sent_h, num_sent_s
    integer status(mpi_status_size), wind(10)
            
    !character(8) date
    !character(10) time
    !character(30) fileName        
    !call date_and_time(date,time)
    !fileName = 'Policies_'//date(3:)//'_'//time(:6)//'.xls'                  
    !open(2,file = fileName)
    
    !Begin backsolving...         
    !!!Solve during retired periods
    print '(a)', "     Solving for retirement periods..."
    !!!!Solve last period of life
    !Married  
    do h=1,nhht    
    do vm=1,naem    
    do vf=1,naef    
    do j=1,nm    
    do i=1,na  
        ssIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),1)         
        preTaxIncome = ssIncome + avect(i) * r 
        postTaxWealth = preTaxIncome + avect(i) -  IncomeTax(avect(i)*r- & 
max(0.0d0,avect(i)*r*capTax),ssIncome,mmat(j,h,4,1,nr),1) - max(0.0d0,avect(i)*r*capTax)
        consumptionRCubeMarried(i,j,vf,vm,h,4,nr) = postTaxWealth + & 
transfersRetired(postTaxWealth, nr, j ,h, 4, 1, i) - mmat(j,h,4,1,nr)        
        VnewRCubeMarried(i,j,vf,vm,h,4,nr) = U(consumptionRCubeMarried(i,j,vf,vm,h,4,nr),0.0d0,1,0)
        UtilConsRCubeMarried(i,j,vf,vm,h,4,nr) = Ucons(consumptionRCubeMarried(i,j,vf,vm,h,4,nr),1)
    end do
    end do
    end do
    end do
    end do
    aPolicyRCubeMarried(:,:,:,:,:,4,nr) = 0.0d0
    
    !Widow
    s=2
    do h=1,nsht    
    do vm=1,naem    
    do vf=1,naef    
    do j=1,nm    
    do i=1,na  
        ssIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),s)         
        preTaxIncome = ssIncome + avect(i) * r 
        postTaxWealth = preTaxIncome + avect(i) -  IncomeTax(avect(i)*r- & 
max(0.0d0,avect(i)*r*capTax),ssIncome,mmat(j,h,2,s,nr),s) - max(0.0d0,avect(i)*r*capTax)
        consumptionRCubeWidow(i,j,vf,vm,h,2,nr) = postTaxWealth + transfersRetired(postTaxWealth, nr, j ,h, 2, s, i) - & 
mmat(j,h,2,s,nr)        
        VnewRCubeWidow(i,j,vf,vm,h,2,nr) = U(consumptionRCubeWidow(i,j,vf,vm,h,2,nr),0.0d0,s,0)
        UtilConsRCubeWidow(i,j,vf,vm,h,2,nr) = Ucons(consumptionRCubeWidow(i,j,vf,vm,h,2,nr),s)
    end do
    end do
    end do
    end do
    end do
    aPolicyRCubeWidow(:,:,:,:,:,2,nr) = 0.0d0
    
    !Widower
    s=3
    do h=1,nsht    
    do vm=1,naem    
    do vf=1,naef    
    do j=1,nm    
    do i=1,na  
        ssIncome = socsec(aveEarnFvect(vf),aveEarnMvect(vm),s)         
        preTaxIncome = ssIncome + avect(i) * r 
        postTaxWealth = preTaxIncome + avect(i) -  IncomeTax(avect(i)*r- &
 max(0.0d0,avect(i)*r*capTax),ssIncome,mmat(j,h,2,s,nr),s) - max(0.0d0,avect(i)*r*capTax)
        consumptionRCubeWidower(i,j,vf,vm,h,2,nr) = postTaxWealth + transfersRetired(postTaxWealth, nr, j ,h, 2, s, i) - &
 mmat(j,h,2,s,nr)        
        VnewRCubeWidower(i,j,vf,vm,h,2,nr) = U(consumptionRCubeWidower(i,j,vf,vm,h,2,nr),0.0d0,s,0)
        UtilConsRCubeWidower(i,j,vf,vm,h,2,nr) = Ucons(consumptionRCubeWidower(i,j,vf,vm,h,2,nr),s)
    end do
    end do
    end do
    end do
    end do
    aPolicyRCubeWidower(:,:,:,:,:,2,nr) = 0.0d0        
       
    !each period starting at T-1 to nw+1
    !married
    do ti=nr-1,1,-1
        print *, "ti=", ti    
        !send initial set of indices out to workers with tag = 1
        num_sent_s = 0        
        ind(1) = 1
        ind(2) = 0        
        ind(3) = 1
        ind(4) = count_rh
        ind(5) = 1
        ind(6) = count_raem
        ind(7) = 1  
        ind(8) = count_raef
        ind(9) = 1
        ind(10) = count_rm                 
        
        num_sent_h = count_rh               
        num_sent_aem = count_raem   
        num_sent_aef = count_raef
        num_sent_m = count_rm 
        
        !actprocs = numprocs
                
        do n=1,numprocs-1
            if (num_sent_s < 4) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 4) ind(2) = 4
                call mpi_send(ind,10,mpi_integer,n,1, mpi_comm_world,ier) 
                num_sent_s = num_sent_s + count_rs
            else if (num_sent_h < nhht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
        
                if (ind(4) > nhht) ind(4) = nhht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                               
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
        
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh                 
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh 
                ind(5) = 1
                ind(6) = count_raem                  
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem               
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                   
                ind(5) = 1
                ind(6) = count_raem
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef      
            end if 
        end do       

        !send ti+1 value functions to workers
        call mpi_bcast(ti,1,mpi_integer,0,mpi_comm_world,ier)        
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
        num_recv = 0
                       
        !print *, "num_sent_m=", num_sent_m  
        !print *, "num_sent_s=", num_sent_s   
        !print *, "num_sent_aef=", num_sent_aef   
        !print *, "num_sent_aem=", num_sent_aem                    
        do while (num_recv < nm*naef*naem*nhht*4)           
            !receive data from a worker
            call mpi_recv(wind,10,mpi_integer,mpi_any_source,mpi_any_tag, mpi_comm_world,status, ier) 
            sender =status(mpi_source) 
            !print *, "sender=", sender
            call mpi_recv(part_aPolicyRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                           
            call mpi_recv(part_consRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)            
            call mpi_recv(part_VRcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)    
            call mpi_recv(part_CURcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                                                             
                        
            aPolicyRCubeMarried(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_aPolicyRCube
            consumptionRCubeMarried(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_consRCube
            VnewRCubeMarried(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_VRcube         
            UtilConsRCubeMarried(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_CURcube          
            
            num_recv = num_recv + (wind(10)-wind(9)+1)*(wind(8)-wind(7)+1)*(wind(6)-wind(5)+1)* & 
(wind(4)-wind(3)+1)*(wind(2)-wind(1)+1)                     
            !print *, "num_recv =", num_recv
            !send him more work with tag = 2
            !print *, "num_sent_s =", num_sent_s
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_aef =", num_sent_aef
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_m =", num_sent_m
            !print *, "num_sent_h=", num_sent_h
            
            !print *, "wind=", wind
            
            if (num_sent_s < 4) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 4) ind(2) = 4
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_s = num_sent_s + ind(2) -ind(1) + 1 
            else if (num_sent_h < nhht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
                if (ind(4) > nhht) ind(4) = nhht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                                 
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_h = count_rh   
                num_sent_s = count_rs                                                   
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem                
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef                                                                   
            end if                                           
        end do
        
        !print *, "married done"  
        !pause           
        
        num_sent_s = 0        
        ind(1) = 1
        ind(2) = 0        
        ind(3) = 1
        ind(4) = count_rh
        ind(5) = 1
        ind(6) = count_raem
        ind(7) = 1  
        ind(8) = count_raef
        ind(9) = 1
        ind(10) = count_rm                 
        
        num_sent_h = count_rh               
        num_sent_aem = count_raem   
        num_sent_aef = count_raef
        num_sent_m = count_rm 
        
       do n=1,numprocs-1
            if (num_sent_s < 2) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 2) ind(2) = 2
                call mpi_send(ind,10,mpi_integer,n,3, mpi_comm_world,ier) 
                num_sent_s = num_sent_s + count_rs
            else if (num_sent_h < nsht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
        
                if (ind(4) > nsht) ind(4) = nsht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, n, 3, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                               
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
        
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                call mpi_send(ind, 10, mpi_integer, n, 3, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh                 
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh 
                ind(5) = 1
                ind(6) = count_raem                  
                call mpi_send(ind, 10, mpi_integer, n, 3, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem               
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                   
                ind(5) = 1
                ind(6) = count_raem
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, n, 3, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef      
            end if
        end do       
                    
        num_recv = 0
         
          
        !print *, "num_sent_m=", num_sent_m  
        !print *, "num_sent_s=", num_sent_s   
        !print *, "num_sent_aef=", num_sent_aef   
        !print *, "num_sent_aem=", num_sent_aem                    
        do while (num_recv < nm*naef*naem*nsht*2)           
            !receive data from a worker
            call mpi_recv(wind,10,mpi_integer,mpi_any_source,mpi_any_tag, mpi_comm_world,status, ier) 
            sender =status(mpi_source) 
            !if (ti == 16) then
            !    print *, "*************************"
            !    print *, "sender=", sender                
            !    print *, "wind policies", wind
            !    print *, "*************************"
            !    pause
            !end if
            !print *, "sender=", sender
            call mpi_recv(part_aPolicyRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                           
            call mpi_recv(part_consRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)            
            call mpi_recv(part_VRcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)    
            call mpi_recv(part_CURcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                                                             
                        
            aPolicyRCubeWidow(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_aPolicyRCube
            consumptionRCubeWidow(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_consRCube
            VnewRCubeWidow(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_VRcube         
            UtilConsRCubeWidow(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_CURcube          
            
            num_recv = num_recv + (wind(10)-wind(9)+1)*(wind(8)-wind(7)+1)*(wind(6)-wind(5)+1)* & 
(wind(4)-wind(3)+1)*(wind(2)-wind(1)+1)                     
            !print *, "num_recv =", num_recv
            !send him more work with tag = 2
            !print *, "num_sent_s =", num_sent_s
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_aef =", num_sent_aef
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_m =", num_sent_m
            
            if (num_sent_s < 2) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 2) ind(2) = 2
                call mpi_send(ind, 10, mpi_integer, sender, 3, mpi_comm_world, ier)
                num_sent_s = num_sent_s + ind(2) -ind(1) + 1 
            else if (num_sent_h < nsht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
                if (ind(4) > nsht) ind(4) = nsht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, sender, 3, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                                 
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                
                call mpi_send(ind, 10, mpi_integer, sender, 3, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_h = count_rh   
                num_sent_s = count_rs                                                   
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                call mpi_send(ind, 10, mpi_integer, sender, 3, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem                
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, sender, 3, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef                                                                   
            end if                                  
        end do        
        
        num_sent_s = 0        
        ind(1) = 1
        ind(2) = 0        
        ind(3) = 1
        ind(4) = count_rh
        ind(5) = 1
        ind(6) = count_raem
        ind(7) = 1  
        ind(8) = count_raef
        ind(9) = 1
        ind(10) = count_rm                 
        
        num_sent_h = count_rh               
        num_sent_aem = count_raem   
        num_sent_aef = count_raef
        num_sent_m = count_rm 
        
        !actprocs = numprocs
                
        do n=1,numprocs-1
            if (num_sent_s < 2) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 2) ind(2) = 2
                call mpi_send(ind,10,mpi_integer,n,4, mpi_comm_world,ier) 
                num_sent_s = num_sent_s + count_rs
            else if (num_sent_h < nsht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
        
                if (ind(4) > nsht) ind(4) = nsht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, n, 4, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                               
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
        
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                call mpi_send(ind, 10, mpi_integer, n, 4, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh                 
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh 
                ind(5) = 1
                ind(6) = count_raem                  
                call mpi_send(ind, 10, mpi_integer, n, 4, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem               
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                   
                ind(5) = 1
                ind(6) = count_raem
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, n, 4, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef      
            else                
                call mpi_send(ind, 10, mpi_integer, n, 4, mpi_comm_world, ier)                                                                       
                !actprocs = actprocs - 1                
                print *, "n=", n
                pause
            end if 
        end do
        
        num_recv = 0       
                  
        do while (num_recv < nm*naef*naem*nsht*2)           
            !receive data from a worker
            call mpi_recv(wind,10,mpi_integer,mpi_any_source,mpi_any_tag, mpi_comm_world,status, ier) 
            sender =status(mpi_source) 
            !print *, "sender=", sender
            call mpi_recv(part_aPolicyRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                           
            call mpi_recv(part_consRCube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)            
            call mpi_recv(part_VRcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)    
            call mpi_recv(part_CURcube,na*count_rm*count_raef*count_raem*count_rh*count_rs,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                                                             
                        
            aPolicyRCubeWidower(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_aPolicyRCube
            consumptionRCubeWidower(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_consRCube
            VnewRCubeWidower(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_VRcube         
            UtilConsRCubeWidower(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6), & 
wind(3):wind(4),wind(1):wind(2),ti) = part_CURcube          
            
            num_recv = num_recv + (wind(10)-wind(9)+1)*(wind(8)-wind(7)+1)*(wind(6)-wind(5)+1)* & 
(wind(4)-wind(3)+1)*(wind(2)-wind(1)+1)                     
            !print *, "num_recv =", num_recv
            !send him more work with tag = 2
            !print *, "num_sent_s =", num_sent_s
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_aef =", num_sent_aef
            !print *, "num_sent_aem =", num_sent_aem
            !print *, "num_sent_m =", num_sent_m
            
            if (num_sent_s < 2) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_rs - 1
                if (ind(2) > 2) ind(2) = 2
                call mpi_send(ind, 10, mpi_integer, sender, 4, mpi_comm_world, ier)
                num_sent_s = num_sent_s + ind(2) -ind(1) + 1 
            else if (num_sent_h < nsht) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_rh - 1
                if (ind(4) > nsht) ind(4) = nsht
                ind(1) = 1
                ind(2) = count_rs
                call mpi_send(ind, 10, mpi_integer, sender, 4, mpi_comm_world, ier)
                num_sent_h = num_sent_h + ind(4) -ind(3) + 1
                num_sent_s = count_rs                                                 
            else if (num_sent_aem < naem) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_raem - 1
                if (ind(6) > naem) ind(6) = naem
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh                
                call mpi_send(ind, 10, mpi_integer, sender, 4, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(6) -ind(5) + 1
                num_sent_h = count_rh   
                num_sent_s = count_rs                                                   
            else if (num_sent_aef < naef) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_raef - 1
                if (ind(8) > naef) ind(8) = naef
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                call mpi_send(ind, 10, mpi_integer, sender, 4, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(8) - ind(7) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem                
            else if (num_sent_m < nm) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_rm - 1
                if (ind(10) > nm) ind(10) = nm
                ind(1) = 1
                ind(2) = count_rs
                ind(3) = 1
                ind(4) = count_rh
                ind(5) = 1
                ind(6) = count_raem                   
                ind(7) = 1
                ind(8) = count_raef
                call mpi_send(ind, 10, mpi_integer, sender, 4, mpi_comm_world, ier)
                num_sent_m = num_sent_m + ind(10) - ind(9) + 1
                num_sent_s = count_rs
                num_sent_h = count_rh
                num_sent_aem = count_raem     
                num_sent_aef = count_raef                                                                   
            end if                                 
        end do       
    end do 
    
    !send workers message that this GE loop is done (tag = 0)  
    do n=1,numprocs-1
        call mpi_send(ind,10,mpi_integer,n,0,mpi_comm_world,ier)
    end do
       
                                
    !each period starting at nw to 1
    print '(a)', "     Solving for working periods..."
    !pause
    do ti=nw,1,-1
        print *, "ti=", ti    
        !send initial set of indices out to workers with tag = 1
        num_sent_et = 0        
        ind(1) = 1
        ind(2) = 0
        ind(3) = 1
        ind(4) = count_waem
        ind(5) = 1
        ind(6) = count_waef
        ind(7) = 1  
        ind(8) = count_wem
        ind(9) = 1  
        ind(10) = count_wef
               
        num_sent_aem = count_waem     
        num_sent_aef = count_waef   
        num_sent_em = count_wem
        num_sent_ef = count_wef 
        
        !print *, "1pol"   
                   
        do n=1,numprocs-1
            if (num_sent_et < nhet) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_wet - 1
                if (ind(2) > nhet) ind(2) = nhet
                call mpi_send(ind,10,mpi_integer,n,1, mpi_comm_world,ier) 
                num_sent_et = num_sent_et + count_wet 
            else if (num_sent_aem < naem) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_waem - 1
                if (ind(4) > naem) ind(4) = naem
                ind(1) = 1
                ind(2) = count_wet
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(4) -ind(3) + 1
                num_sent_et = count_wet                           
            else if (num_sent_aef < naef) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_waef - 1
                if (ind(6) > naef) ind(6) = naef
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(6) - ind(5) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem                
            else if (num_sent_em < nem) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_wem - 1
                if (ind(8) > nem) ind(8) = nem
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                ind(5) = 1
                ind(6) = count_waef
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_em = num_sent_em + ind(8) - ind(7) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem     
                num_sent_aef = count_waef    
            else if (num_sent_ef < nef) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_wef - 1
                if (ind(10) > nef) ind(10) = nef
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                ind(5) = 1
                ind(6) = count_waef
                ind(7) = 1
                ind(8) = count_wem                
                call mpi_send(ind, 10, mpi_integer, n, 1, mpi_comm_world, ier)
                num_sent_ef = num_sent_ef + ind(10) - ind(9) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem
                num_sent_aef = count_waef     
                num_sent_em = count_wem                                                                                   
            end if 
        end do       
        
        !print *, "2pol"  
        
        !send ti+1 value functions to workers
        call mpi_bcast(ti,1,mpi_integer,0,mpi_comm_world,ier)
        
        !print *, "2pola" 
        
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
        num_recv = 0
                 
        !print *, "3pol"           
                       
        !print *, "num_sent_ef=", num_sent_ef   
        !print *, "num_sent_aef=", num_sent_aef   
        !print *, "num_sent_em=", num_sent_em   
        !print *, "num_sent_aem=", num_sent_aem                    
        do while (num_recv < nef*nem*naef*naem*nhet)
            !receive data from a worker
            call mpi_recv(wind,10,mpi_integer,mpi_any_source,mpi_any_tag, mpi_comm_world,status, ier) 
            sender =status(mpi_source) 
            !print *, "sender=", sender
            call mpi_recv(part_aPolicyCube,na*count_wef*count_wem*count_waem*count_waef*count_wet,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                           
            call mpi_recv(part_consCube,na*count_wef*count_wem*count_waem*count_waef*count_wet,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)
            call mpi_recv(part_labCube,na*count_wef*count_wem*count_waem*count_waef*count_wet,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier) 
            call mpi_recv(part_Vcube,na*count_wef*count_wem*count_waem*count_waef*count_wet,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier)                                             
            call mpi_recv(part_CUcube,na*count_wef*count_wem*count_waem*count_waef*count_wet,mpi_double_precision, &
                sender, mpi_any_tag, mpi_comm_world, status, ier) 
                        
            aPolicyWCube(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6),wind(3):wind(4),wind(1):wind(2),ti) = part_aPolicyCube
            consumptionWCube(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6),wind(3):wind(4),wind(1):wind(2),ti) = part_consCube
            labWcube(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6),wind(3):wind(4),wind(1):wind(2),ti) = part_labCube
            VnewWCube(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6),wind(3):wind(4),wind(1):wind(2),ti) = part_Vcube    
            UtilConsWCube(:,wind(9):wind(10),wind(7):wind(8),wind(5):wind(6),wind(3):wind(4),wind(1):wind(2),ti) = part_CUcube          
            
            num_recv = num_recv + (wind(10)-wind(9)+1)*(wind(8)-wind(7)+1)*(wind(6)-wind(5)+1)* & 
(wind(4)-wind(3)+1)*(wind(2)-wind(1)+1)                     
            !print *, "num_recv =", num_recv
            !send him more work with tag = 2
            if (num_sent_et < nhet) then
                ind(1) = ind(2) + 1
                ind(2) = ind(1) + count_wet - 1
                if (ind(2) > nhet) ind(2) = nhet
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_et = num_sent_et + ind(2) -ind(1) + 1                           
            else if (num_sent_aem < naem) then
                ind(3) = ind(4) + 1
                ind(4) = ind(3) + count_waem - 1
                if (ind(4) > naem) ind(4) = naem
                ind(1) = 1
                ind(2) = count_wet
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_aem = num_sent_aem + ind(4) -ind(3) + 1
                num_sent_et = count_wet                                                                                                                           
            else if (num_sent_aef < naef) then
                ind(5) = ind(6) + 1
                ind(6) = ind(5) + count_waef - 1
                if (ind(6) > naef) ind(6) = naef
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_aef = num_sent_aef + ind(6) - ind(5) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem                
            else if (num_sent_em < nem) then
                ind(7) = ind(8) + 1
                ind(8) = ind(7) + count_wem - 1
                if (ind(8) > nem) ind(8) = nem
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                ind(5) = 1
                ind(6) = count_waef
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_em = num_sent_em + ind(8) - ind(7) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem     
                num_sent_aef = count_waef   
            else if (num_sent_ef < nef) then
                ind(9) = ind(10) + 1
                ind(10) = ind(9) + count_wef - 1
                if (ind(10) > nef) ind(10) = nef
                ind(1) = 1
                ind(2) = count_wet
                ind(3) = 1
                ind(4) = count_waem                   
                ind(5) = 1
                ind(6) = count_waef
                ind(7) = 1
                ind(8) = count_wem
                call mpi_send(ind, 10, mpi_integer, sender, 2, mpi_comm_world, ier)
                num_sent_ef = num_sent_ef + ind(10) - ind(9) + 1
                num_sent_et = count_wet
                num_sent_aem = count_waem     
                num_sent_aef = count_waef 
                num_sent_em = count_wem                                                                                  
            end if 
            !print *, "num_sent_ef=", num_sent_ef   
            !print *, "num_sent_aef=", num_sent_aef   
            !print *, "num_sent_em=", num_sent_em   
            !print *, "num_sent_aem=", num_sent_aem                                  
        end do
        
        !print *, "4pol"  
    end do 
    
    !send workers message that this GE loop is done (tag = 0)  
    do n=1,numprocs-1
        call mpi_send(ind,10,mpi_integer,n,0,mpi_comm_world,ier)
    end do
          
 end subroutine ComputePolicies    
   
   

end module Policies
