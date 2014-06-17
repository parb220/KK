#include <mpi.h>
#include "parameters.hpp"

using namespace std; 

extern "C"{
	void master_(int *, int *); 
	void worker_(int *, int *, int *); 
}

int main(int argc, char **argv)
{
	MPI_Init(&argc, &argv);
        int my_rank, nNode;
        MPI_Comm_size(MPI_COMM_WORLD, &nNode);
        MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	
	int getype, gbtype; 
	MPI_Type_contiguous( (nef*nem*nset*nw)+(nef*nem*nset*nw)+3+naef+naem+1+1+3*naem, MPI_DOUBLE_PRECISION, &getype); 
	MPI_Type_commit(&getype); 
	MPI_Type_contiguous( (nm*nhht*4*3*nr)+na+(nsht*3*nr)-1+(nsht*3*nr)-1+(npm*nhet)+(nef*nef)+(nem*nem)+(npm*npm*(nr-1))+(nhht*nhet)+(nsht*nsht*3*nr)+(nsht*nsht*3*nr)+ntm, MPI_DOUBLE_PRECISION, &gbtype);
	MPI_Type_commit(&gbtype); 

	if (my_rank == 0)
		master_(&gbtype, &getype); 
	else 
		worker_(&my_rank, &gbtype, &getype); 
	
	MPI_Type_free(&gbtype); 
	MPI_Type_free(&getype); 
	MPI_Finalize(); 
	return 0; 	
}

/*program MAIN !WorkingRetiredTogether
#    implicit none
#    use subroutines  

    include 'mpif.h'
    real(dbl) init
    call MPI_INIT(ier)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ier)
    call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ier)
    call MPI_TYPE_CONTIGUOUS(nef*nem*nset*nw + nef*nem*nset*nw + 3+naef+naem+1+1+3*naem,MPI_DOUBLE_PRECISION,getype,ier)
    call MPI_TYPE_COMMIT(getype, ier)
    call MPI_TYPE_CONTIGUOUS(nm*nhht*4*3*nr + na + nsht*3*nr-1 + &
        nsht*3*nr-1 + npm*nhet + nef*nef+ nem*nem  + npm*npm*(nr-1) + nhht*nhet + nsht*nsht*3*nr + nsht*nsht*3*nr &
        +ntm,MPI_DOUBLE_PRECISION,gbtype,ier)        
    call MPI_TYPE_COMMIT(gbtype, ier)
    
    !set current calibration parameters
    !phi1  = (/0.29d0, 0.16d0, 0.11d0, 0.10d0/)
    !psi = (/3.2d0, 1.6d0, 2.4d0, 1.7d0/) 

    !clowerbarMarried = clowerbarMarried*0.70640d0 !1.29360d0   !0.273d0  
    !clowerbarWidow =  clowerbarWidow*0.7064d0 !1.29360d0      !0.273d0
    !clowerbarWidower =clowerbarWidower*0.7064d0 !1.29360d0    !0.273d0
    
    if (my_rank .eq. 0) then
        init = MPI_WTIME()
        call master
        final = MPI_WTIME()
        final = final - init
        print *, "elapsed time  =", final        
    else    
        call worker(my_rank)
    endif

    call MPI_TYPE_FREE(getype, ier)

    call MPI_TYPE_FREE(gbtype, ier)

    call MPI_FINALIZE(ier)
    
    stop   

end program MAIN !WorkingRetiredTogether
*/

