SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../parser/Syntax.smi ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi \
 ../parser/CoreML.lex.smi ../parser/Parser.smi ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi ../typeinf/Typeinf.smi \
 ../SECD/Instruction.smi ../SECD/Comp.smi ../SECD/Value.smi ../SECD/Exec.smi \
 Top.smi Main.smi ../parser/Syntax.o ../parser/CoreML.grm.o \
 ../parser/CoreML.lex.o ../parser/Parser.o ../typeinf/Type.o \
 ../typeinf/TypeUtils.o ../typeinf/UnifyTy.o ../typeinf/Typeinf.o \
 ../SECD/Instruction.o ../SECD/Comp.o ../SECD/Value.o ../SECD/Exec.o Top.o \
 Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
../parser/Syntax.o: ../parser/Syntax.sml ../parser/Syntax.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/Syntax.o -c ../parser/Syntax.sml
../parser/CoreML.grm.o: ../parser/CoreML.grm.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/CoreML.grm.o -c \
 ../parser/CoreML.grm.sml
../parser/CoreML.lex.o: ../parser/CoreML.lex.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/CoreML.lex.o -c \
 ../parser/CoreML.lex.sml
../parser/Parser.o: ../parser/Parser.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi \
 ../parser/Parser.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/Parser.o -c ../parser/Parser.sml
../typeinf/Type.o: ../typeinf/Type.sml ../typeinf/Type.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/Type.o -c ../typeinf/Type.sml
../typeinf/TypeUtils.o: ../typeinf/TypeUtils.sml ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/TypeUtils.o -c \
 ../typeinf/TypeUtils.sml
../typeinf/UnifyTy.o: ../typeinf/UnifyTy.sml ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/UnifyTy.o -c \
 ../typeinf/UnifyTy.sml
../typeinf/Typeinf.o: ../typeinf/Typeinf.sml ../parser/Syntax.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/Typeinf.o -c \
 ../typeinf/Typeinf.sml
../SECD/Instruction.o: ../SECD/Instruction.sml ../SECD/Instruction.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../SECD/Instruction.o -c \
 ../SECD/Instruction.sml
../SECD/Comp.o: ../SECD/Comp.sml ../parser/Syntax.smi ../SECD/Instruction.smi \
 ../SECD/Comp.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../SECD/Comp.o -c ../SECD/Comp.sml
../SECD/Value.o: ../SECD/Value.sml ../SECD/Instruction.smi ../SECD/Value.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../SECD/Value.o -c ../SECD/Value.sml
../SECD/Exec.o: ../SECD/Exec.sml ../SECD/Instruction.smi ../SECD/Value.smi \
 ../SECD/Exec.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../SECD/Exec.o -c ../SECD/Exec.sml
Top.o: Top.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi ../SECD/Instruction.smi ../SECD/Comp.smi \
 ../SECD/Value.smi ../SECD/Exec.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi ../SECD/Instruction.smi ../SECD/Comp.smi \
 ../SECD/Value.smi ../SECD/Exec.smi Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
