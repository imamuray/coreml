SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../readString/ReadString.smi Top.smi Main.smi ../readString/ReadString.o \
 Top.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
../readString/ReadString.o: ../readString/ReadString.sml \
 ../readString/ReadString.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../readString/ReadString.o -c \
 ../readString/ReadString.sml
Top.o: Top.sml ../readString/ReadString.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../readString/ReadString.smi Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
