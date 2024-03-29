#
# This Makefile was generated by Code::Blocks IDE.
#

SRCS_f95d1 = \
searching_procedures.f95 \
main.f95 \
append.f95 \
decompose.f95 

OBJS_f95d1 = \
searching_procedures.o \
main.o \
append.o \
decompose.o 

SRC_DIR_f95d1 = 
OBJS_DIR = obj/Debug/
EXE_DIR = bin/Debug/

EXE = GCD
FC = gfortran
LD = gfortran
IDIR = 
CFLAGS = -Wall -g -fdefault-integer-8 -fdefault-double-8 -fdefault-real-8 -fdefault-integer-8  -J$(OBJS_DIR) $(IDIR)
LFLAGS = 
LIBS = 

VPATH = $(SRC_DIR_f95d1):$(OBJS_DIR)
OBJS = $(addprefix $(OBJS_DIR), $(OBJS_f95d1))

all : $(EXE)

$(EXE) : $(OBJS_f95d1)
	@mkdir -p $(EXE_DIR)
	$(LD) -o $(EXE_DIR)$(EXE) $(OBJS) $(LFLAGS) $(LIBS)

$(OBJS_f95d1):
	@mkdir -p $(OBJS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_f95d1)$(@:.o=.f95) -o $(OBJS_DIR)$@

clean :
	rm -f $(OBJS_DIR)*.*
	rm -f $(EXE_DIR)$(EXE)

# Dependencies of files
searching_procedures.o: \
    searching_procedures.f95
main.o: \
    main.f95 \
    decompose.o \
    searching_procedures.o
append.o: \
    append.f95
decompose.o: \
    decompose.f95 \
    append.o

