#!/bin/bash

SRCS=constants.f90 distributions.f90 interpolate.f90 mathutils.f90 mherzo.f90 mtmod.f90 output.f90 discretizeAR1.f90   \
       toms462.f90 \
       parameters.f90 globals.f90 functions.f90 ComputeDistributions.f90 \
       Policies.f90 simulations.f90 results.f90  results2.f90 OutputResults.f90 subroutines.f90 main.f90
OBJS=$(SRCS:.f90=.o)
TARGET=socsec
#
MPIF90=mpif90
#MPIF90 = compile -intel -mpi

FFLAGS=-O2 -heap-arrays

all: $(TARGET)

$(TARGET): $(OBJS)
	$(MPIF90) -o $@ $(OBJS)

$(OBJS): $(SRCS)
	$(MPIF90) $(FFLAGS) -c $(SRCS)

clean:
	rm -f $(OBJS) $(TARGET)

