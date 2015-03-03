# -*- mode: makefile-gmake -*-

FC = gfortran

.SUFFIXES: .f90 .mod

%.o: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

%.mod: %.f90 %.o
	@:


$(TARGET): $(OBJS)
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@
