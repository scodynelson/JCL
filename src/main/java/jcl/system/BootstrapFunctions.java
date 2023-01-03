package jcl.system;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import jcl.compiler.icg.emitter.AddInnerClassReference;
import jcl.compiler.icg.emitter.AddOuterClassReference;
import jcl.compiler.icg.emitter.EmitAnnotationField;
import jcl.compiler.icg.emitter.EmitField;
import jcl.compiler.icg.emitter.EmitLabel;
import jcl.compiler.icg.emitter.EmitLine;
import jcl.compiler.icg.emitter.EmitSourceFile;
import jcl.compiler.icg.emitter.EmitTryCatchBlock;
import jcl.compiler.icg.emitter.EndAnnotation;
import jcl.compiler.icg.emitter.EndClass;
import jcl.compiler.icg.emitter.EndField;
import jcl.compiler.icg.emitter.EndMethod;
import jcl.compiler.icg.emitter.NewClass;
import jcl.compiler.icg.emitter.NewClassAnnotation;
import jcl.compiler.icg.emitter.NewField;
import jcl.compiler.icg.emitter.NewFieldAnnotation;
import jcl.compiler.icg.emitter.NewMethod;
import jcl.compiler.icg.emitter.NewMethodAnnotation;
import jcl.compiler.icg.emitter.lisp.EmitNIL;
import jcl.compiler.icg.emitter.lisp.EmitPackage;
import jcl.compiler.icg.emitter.lisp.EmitSymbolPackage;
import jcl.compiler.icg.emitter.lisp.EmitT;
import jcl.compiler.icg.emitter.opcodes.Aaload;
import jcl.compiler.icg.emitter.opcodes.Aastore;
import jcl.compiler.icg.emitter.opcodes.Aconst_null;
import jcl.compiler.icg.emitter.opcodes.Aload;
import jcl.compiler.icg.emitter.opcodes.Anewarray;
import jcl.compiler.icg.emitter.opcodes.Areturn;
import jcl.compiler.icg.emitter.opcodes.Arraylength;
import jcl.compiler.icg.emitter.opcodes.Astore;
import jcl.compiler.icg.emitter.opcodes.Athrow;
import jcl.compiler.icg.emitter.opcodes.Baload;
import jcl.compiler.icg.emitter.opcodes.Bastore;
import jcl.compiler.icg.emitter.opcodes.Bipush;
import jcl.compiler.icg.emitter.opcodes.Caload;
import jcl.compiler.icg.emitter.opcodes.Castore;
import jcl.compiler.icg.emitter.opcodes.Checkcast;
import jcl.compiler.icg.emitter.opcodes.D2f;
import jcl.compiler.icg.emitter.opcodes.D2i;
import jcl.compiler.icg.emitter.opcodes.D2l;
import jcl.compiler.icg.emitter.opcodes.Dadd;
import jcl.compiler.icg.emitter.opcodes.Daload;
import jcl.compiler.icg.emitter.opcodes.Dastore;
import jcl.compiler.icg.emitter.opcodes.Dcmpg;
import jcl.compiler.icg.emitter.opcodes.Dcmpl;
import jcl.compiler.icg.emitter.opcodes.Dconst;
import jcl.compiler.icg.emitter.opcodes.Ddiv;
import jcl.compiler.icg.emitter.opcodes.Dload;
import jcl.compiler.icg.emitter.opcodes.Dmul;
import jcl.compiler.icg.emitter.opcodes.Dneg;
import jcl.compiler.icg.emitter.opcodes.Drem;
import jcl.compiler.icg.emitter.opcodes.Dreturn;
import jcl.compiler.icg.emitter.opcodes.Dstore;
import jcl.compiler.icg.emitter.opcodes.Dsub;
import jcl.compiler.icg.emitter.opcodes.Dup;
import jcl.compiler.icg.emitter.opcodes.Dup2;
import jcl.compiler.icg.emitter.opcodes.Dup2_x1;
import jcl.compiler.icg.emitter.opcodes.Dup2_x2;
import jcl.compiler.icg.emitter.opcodes.Dup_x1;
import jcl.compiler.icg.emitter.opcodes.Dup_x2;
import jcl.compiler.icg.emitter.opcodes.F2d;
import jcl.compiler.icg.emitter.opcodes.F2i;
import jcl.compiler.icg.emitter.opcodes.F2l;
import jcl.compiler.icg.emitter.opcodes.Fadd;
import jcl.compiler.icg.emitter.opcodes.Faload;
import jcl.compiler.icg.emitter.opcodes.Fastore;
import jcl.compiler.icg.emitter.opcodes.Fcmpg;
import jcl.compiler.icg.emitter.opcodes.Fcmpl;
import jcl.compiler.icg.emitter.opcodes.Fconst;
import jcl.compiler.icg.emitter.opcodes.Fdiv;
import jcl.compiler.icg.emitter.opcodes.Fload;
import jcl.compiler.icg.emitter.opcodes.Fmul;
import jcl.compiler.icg.emitter.opcodes.Fneg;
import jcl.compiler.icg.emitter.opcodes.Frem;
import jcl.compiler.icg.emitter.opcodes.Freturn;
import jcl.compiler.icg.emitter.opcodes.Fstore;
import jcl.compiler.icg.emitter.opcodes.Fsub;
import jcl.compiler.icg.emitter.opcodes.Getfield;
import jcl.compiler.icg.emitter.opcodes.Getstatic;
import jcl.compiler.icg.emitter.opcodes.Goto;
import jcl.compiler.icg.emitter.opcodes.I2b;
import jcl.compiler.icg.emitter.opcodes.I2c;
import jcl.compiler.icg.emitter.opcodes.I2d;
import jcl.compiler.icg.emitter.opcodes.I2f;
import jcl.compiler.icg.emitter.opcodes.I2l;
import jcl.compiler.icg.emitter.opcodes.I2s;
import jcl.compiler.icg.emitter.opcodes.Iadd;
import jcl.compiler.icg.emitter.opcodes.Iaload;
import jcl.compiler.icg.emitter.opcodes.Iand;
import jcl.compiler.icg.emitter.opcodes.Iastore;
import jcl.compiler.icg.emitter.opcodes.Iconst;
import jcl.compiler.icg.emitter.opcodes.Idiv;
import jcl.compiler.icg.emitter.opcodes.If_acmpeq;
import jcl.compiler.icg.emitter.opcodes.If_acmpne;
import jcl.compiler.icg.emitter.opcodes.If_icmpeq;
import jcl.compiler.icg.emitter.opcodes.If_icmpge;
import jcl.compiler.icg.emitter.opcodes.If_icmpgt;
import jcl.compiler.icg.emitter.opcodes.If_icmple;
import jcl.compiler.icg.emitter.opcodes.If_icmplt;
import jcl.compiler.icg.emitter.opcodes.If_icmpne;
import jcl.compiler.icg.emitter.opcodes.Ifeq;
import jcl.compiler.icg.emitter.opcodes.Ifge;
import jcl.compiler.icg.emitter.opcodes.Ifgt;
import jcl.compiler.icg.emitter.opcodes.Ifle;
import jcl.compiler.icg.emitter.opcodes.Iflt;
import jcl.compiler.icg.emitter.opcodes.Ifne;
import jcl.compiler.icg.emitter.opcodes.Ifnonnull;
import jcl.compiler.icg.emitter.opcodes.Ifnull;
import jcl.compiler.icg.emitter.opcodes.Iinc;
import jcl.compiler.icg.emitter.opcodes.Iload;
import jcl.compiler.icg.emitter.opcodes.Imul;
import jcl.compiler.icg.emitter.opcodes.Ineg;
import jcl.compiler.icg.emitter.opcodes.Instanceof;
import jcl.compiler.icg.emitter.opcodes.Invokedynamic;
import jcl.compiler.icg.emitter.opcodes.Invokeinterface;
import jcl.compiler.icg.emitter.opcodes.Invokespecial;
import jcl.compiler.icg.emitter.opcodes.Invokestatic;
import jcl.compiler.icg.emitter.opcodes.Invokevirtual;
import jcl.compiler.icg.emitter.opcodes.Ior;
import jcl.compiler.icg.emitter.opcodes.Irem;
import jcl.compiler.icg.emitter.opcodes.Ireturn;
import jcl.compiler.icg.emitter.opcodes.Ishl;
import jcl.compiler.icg.emitter.opcodes.Ishr;
import jcl.compiler.icg.emitter.opcodes.Istore;
import jcl.compiler.icg.emitter.opcodes.Isub;
import jcl.compiler.icg.emitter.opcodes.Iushr;
import jcl.compiler.icg.emitter.opcodes.Ixor;
import jcl.compiler.icg.emitter.opcodes.Jsr;
import jcl.compiler.icg.emitter.opcodes.L2d;
import jcl.compiler.icg.emitter.opcodes.L2f;
import jcl.compiler.icg.emitter.opcodes.L2i;
import jcl.compiler.icg.emitter.opcodes.Ladd;
import jcl.compiler.icg.emitter.opcodes.Laload;
import jcl.compiler.icg.emitter.opcodes.Land;
import jcl.compiler.icg.emitter.opcodes.Lastore;
import jcl.compiler.icg.emitter.opcodes.Lcmp;
import jcl.compiler.icg.emitter.opcodes.Lconst;
import jcl.compiler.icg.emitter.opcodes.Ldc;
import jcl.compiler.icg.emitter.opcodes.Ldiv;
import jcl.compiler.icg.emitter.opcodes.Lload;
import jcl.compiler.icg.emitter.opcodes.Lmul;
import jcl.compiler.icg.emitter.opcodes.Lneg;
import jcl.compiler.icg.emitter.opcodes.Lookupswitch;
import jcl.compiler.icg.emitter.opcodes.Lor;
import jcl.compiler.icg.emitter.opcodes.Lrem;
import jcl.compiler.icg.emitter.opcodes.Lreturn;
import jcl.compiler.icg.emitter.opcodes.Lshl;
import jcl.compiler.icg.emitter.opcodes.Lshr;
import jcl.compiler.icg.emitter.opcodes.Lstore;
import jcl.compiler.icg.emitter.opcodes.Lsub;
import jcl.compiler.icg.emitter.opcodes.Lushr;
import jcl.compiler.icg.emitter.opcodes.Lxor;
import jcl.compiler.icg.emitter.opcodes.Monitorenter;
import jcl.compiler.icg.emitter.opcodes.Monitorexit;
import jcl.compiler.icg.emitter.opcodes.Multianewarray;
import jcl.compiler.icg.emitter.opcodes.New;
import jcl.compiler.icg.emitter.opcodes.Newarray;
import jcl.compiler.icg.emitter.opcodes.Nop;
import jcl.compiler.icg.emitter.opcodes.Pop;
import jcl.compiler.icg.emitter.opcodes.Pop2;
import jcl.compiler.icg.emitter.opcodes.Putfield;
import jcl.compiler.icg.emitter.opcodes.Putstatic;
import jcl.compiler.icg.emitter.opcodes.Ret;
import jcl.compiler.icg.emitter.opcodes.Return;
import jcl.compiler.icg.emitter.opcodes.Saload;
import jcl.compiler.icg.emitter.opcodes.Sastore;
import jcl.compiler.icg.emitter.opcodes.Sipush;
import jcl.compiler.icg.emitter.opcodes.Swap;
import jcl.compiler.icg.emitter.opcodes.Tableswitch;
import jcl.functions.ApplyFunction;
import jcl.functions.CompileFileFunction;
import jcl.functions.CompileFilePathnameFunction;
import jcl.functions.CompileFunction;
import jcl.functions.EqFunction;
import jcl.functions.EqlFunction;
import jcl.functions.EqualFunction;
import jcl.functions.EqualpFunction;
import jcl.functions.EvalFunction;
import jcl.functions.FuncallFunction;
import jcl.functions.LoadFunction;
import jcl.functions.MacroExpand1Function;
import jcl.functions.MacroExpandFunction;
import jcl.functions.PredicateFunctions;
import jcl.functions.ProvideFunction;
import jcl.functions.RequireFunction;
import jcl.functions.ValuesFunction;
import jcl.functions.array.ListToVectorFunction;
import jcl.functions.condition.ErrorFunction;
import jcl.functions.java.JClass;
import jcl.functions.java.JMethod;
import jcl.functions.lisppackage.ExportFunction;
import jcl.functions.lisppackage.InPackageFunction;
import jcl.functions.list.AppendFunction;
import jcl.functions.list.CarFunction;
import jcl.functions.list.CdrFunction;
import jcl.functions.list.ConsFunction;
import jcl.functions.list.ListFunction;
import jcl.functions.list.ListStarFunction;
import jcl.functions.list.MapCFunction;
import jcl.functions.list.MapCanFunction;
import jcl.functions.list.MapCarFunction;
import jcl.functions.list.MapConFunction;
import jcl.functions.list.MapLFunction;
import jcl.functions.list.MapListFunction;
import jcl.functions.list.NconcFunction;
import jcl.functions.readtable.ReadDispatchCharacterFunction;
import jcl.functions.readtable.ReadFunction;
import jcl.functions.readtable.ReadPreservingWhitespaceFunction;
import jcl.functions.symbol.GensymFunction;
import jcl.functions.symbol.GentempFunction;
import jcl.functions.symbol.SetSymbolFunctionFunction;
import jcl.functions.symbol.SetSymbolMacroFunction;
import jcl.functions.symbol.SetSymbolSetfFunctionFunction;
import jcl.functions.system.FreeMemory;
import jcl.functions.system.GC;
import jcl.functions.system.MaxMemory;
import jcl.functions.system.TotalMemory;
import jcl.lang.FunctionStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.printer.functions.PrintObjectFunction;
import jcl.system.function.QuitFunction;
import jcl.util.CodePointConstants;

class BootstrapFunctions {

	static void bootstrap() {
		bootstrapSystemFunctions();

		bootstrapPredicateFunctions();
		bootstrapArrayFunctions();
		bootstrapConditionFunctions();
		bootstrapJavaFunctions();
		bootstrapPackageFunctions();
		bootstrapListFunctions();
		bootstrapReaderFunctions();
		bootstrapSymbolFunctions();
		bootstrapExtensionFunctions();
		bootstrapPrinterFunctions();
		bootstrapEmitterFunctions();
	}

	private static void bootstrapSystemFunctions() {
		final FuncallFunction funcallFunction = new FuncallFunction();

		CommonLispSymbols.MACROEXPAND_HOOK_VAR.setfSymbolValue(funcallFunction);

		final List<FunctionStruct> functions = Arrays.asList(
				new ApplyFunction(),
				new CompileFileFunction(),
				new CompileFilePathnameFunction(),
				new CompileFunction(),
				new EqFunction(),
				new EqlFunction(),
				new EqualFunction(),
				new EqualpFunction(),
				new EvalFunction(),
				funcallFunction,
				new LoadFunction(),
				new MacroExpand1Function(),
				new MacroExpandFunction(),
				new ProvideFunction(),
				new RequireFunction(),
				new ValuesFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapPredicateFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new PredicateFunctions.ArrayPFunction(),
				new PredicateFunctions.AtomFunction(),
				new PredicateFunctions.BitVectorPFunction(),
				new PredicateFunctions.CharacterPFunction(),
				new PredicateFunctions.CompiledFunctionPFunction(),
				new PredicateFunctions.ComplexPFunction(),
				new PredicateFunctions.ConsPFunction(),
				new PredicateFunctions.FloatPFunction(),
				new PredicateFunctions.FunctionPFunction(),
				new PredicateFunctions.HashTablePFunction(),
				new PredicateFunctions.IntegerPFunction(),
				new PredicateFunctions.KeywordPFunction(),
				new PredicateFunctions.ListPFunction(),
				new PredicateFunctions.NullFunction(),
				new PredicateFunctions.NumberPFunction(),
				new PredicateFunctions.PackagePFunction(),
				new PredicateFunctions.PathnamePFunction(),
				new PredicateFunctions.RationalPFunction(),
				new PredicateFunctions.ReadtablePFunction(),
				new PredicateFunctions.RealPFunction(),
				new PredicateFunctions.RandomStatePFunction(),
				new PredicateFunctions.SimpleBitVectorPFunction(),
				new PredicateFunctions.SimpleStringPFunction(),
				new PredicateFunctions.SimpleVectorPFunction(),
				new PredicateFunctions.StringPFunction(),
				new PredicateFunctions.StreamPFunction(),
				new PredicateFunctions.SymbolPFunction(),
				new PredicateFunctions.VectorPFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapArrayFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new ListToVectorFunction()
		);
		bootstrapSystemPackageFunctions(functions);
	}

	private static void bootstrapConditionFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new ErrorFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapJavaFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new JClass(),
				new JMethod()
		);
		bootstrapExtensionPackageFunctions(functions);
	}

	private static void bootstrapPackageFunctions() {
		List<FunctionStruct> functions = Collections.singletonList(
				new ExportFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);

		functions = Collections.singletonList(
				new InPackageFunction() // TODO: in-package is a Macro
		);
		bootstrapSystemPackageFunctions(functions);
	}

	private static void bootstrapListFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new AppendFunction(),
				new CarFunction(),
				new CdrFunction(),
				new ConsFunction(),
				new ListFunction(),
				new ListStarFunction(),
				new MapCFunction(),
				new MapCarFunction(),
				new MapCanFunction(),
				new MapLFunction(),
				new MapListFunction(),
				new MapConFunction(),
				new NconcFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapReaderFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new ReadFunction(),
				new ReadPreservingWhitespaceFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);

		final ReadDispatchCharacterFunction readDispatchCharacterFunction = new ReadDispatchCharacterFunction();
		bootstrapSystemPackageFunctions(Collections.singletonList(readDispatchCharacterFunction));

		CommonLispSymbols.READTABLE_VAR.getVariableValue().makeDispatchMacroCharacter(
				readDispatchCharacterFunction, CodePointConstants.NUMBER_SIGN, false
		);

		BootstrapReaderMacroFunctions.bootstrap();
	}

	private static void bootstrapSymbolFunctions() {
		List<FunctionStruct> functions = Arrays.asList(
				new GensymFunction(),
				new GentempFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);

		functions = Arrays.asList(
				new SetSymbolFunctionFunction(),
				new SetSymbolMacroFunction(),
				new SetSymbolSetfFunctionFunction()
		);
		bootstrapSystemPackageFunctions(functions);
	}

	private static void bootstrapExtensionFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new PredicateFunctions.LogicalPathnamePFunction(),
				new FreeMemory(),
				new GC(),
				new MaxMemory(),
				new QuitFunction(),
				new TotalMemory()
		);
		bootstrapExtensionPackageFunctions(functions);
	}

	private static void bootstrapPrinterFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new PrintObjectFunction()
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapEmitterFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new NewClass(),
				new NewField(),
				new NewMethod(),
				new NewClassAnnotation(),
				new NewFieldAnnotation(),
				new NewMethodAnnotation(),
				new EndClass(),
				new EndField(),
				new EndMethod(),
				new EndAnnotation(),
				new AddInnerClassReference(),
				new AddOuterClassReference(),
				new EmitAnnotationField(),
				new EmitField(),
				new EmitLabel(),
				new EmitLine(),
				new EmitSourceFile(),
				new EmitTryCatchBlock(),
				new Aaload(),
				new Aastore(),
				new Aconst_null(),
				new Aload(),
				new Anewarray(),
				new Areturn(),
				new Arraylength(),
				new Astore(),
				new Athrow(),
				new Baload(),
				new Bastore(),
				new Bipush(),
				new Caload(),
				new Castore(),
				new Checkcast(),
				new D2f(),
				new D2i(),
				new D2l(),
				new Dadd(),
				new Daload(),
				new Dastore(),
				new Dcmpg(),
				new Dcmpl(),
				new Dconst(),
				new Ddiv(),
				new Dload(),
				new Dmul(),
				new Dneg(),
				new Drem(),
				new Dreturn(),
				new Dstore(),
				new Dsub(),
				new Dup(),
				new Dup2(),
				new Dup2_x1(),
				new Dup2_x2(),
				new Dup_x1(),
				new Dup_x2(),
				new F2d(),
				new F2i(),
				new F2l(),
				new Fadd(),
				new Faload(),
				new Fastore(),
				new Fcmpg(),
				new Fcmpl(),
				new Fconst(),
				new Fdiv(),
				new Fload(),
				new Fmul(),
				new Fneg(),
				new Frem(),
				new Freturn(),
				new Fstore(),
				new Fsub(),
				new Getfield(),
				new Getstatic(),
				new Goto(),
				new I2b(),
				new I2c(),
				new I2d(),
				new I2f(),
				new I2l(),
				new I2s(),
				new Iadd(),
				new Iaload(),
				new Iand(),
				new Iastore(),
				new Iconst(),
				new Idiv(),
				new If_acmpeq(),
				new If_acmpne(),
				new If_icmpeq(),
				new If_icmpge(),
				new If_icmpgt(),
				new If_icmple(),
				new If_icmplt(),
				new If_icmpne(),
				new Ifeq(),
				new Ifge(),
				new Ifgt(),
				new Ifle(),
				new Iflt(),
				new Ifne(),
				new Ifnonnull(),
				new Ifnull(),
				new Iinc(),
				new Iload(),
				new Imul(),
				new Ineg(),
				new Instanceof(),
				new Invokedynamic(),
				new Invokeinterface(),
				new Invokespecial(),
				new Invokestatic(),
				new Invokevirtual(),
				new Ior(),
				new Irem(),
				new Ireturn(),
				new Ishl(),
				new Ishr(),
				new Istore(),
				new Isub(),
				new Iushr(),
				new Ixor(),
				new Jsr(),
				new L2d(),
				new L2f(),
				new L2i(),
				new Ladd(),
				new Laload(),
				new Land(),
				new Lastore(),
				new Lcmp(),
				new Lconst(),
				new Ldc(),
				new Ldiv(),
				new Lload(),
				new Lmul(),
				new Lneg(),
				new Lookupswitch(),
				new Lor(),
				new Lrem(),
				new Lreturn(),
				new Lshl(),
				new Lshr(),
				new Lstore(),
				new Lsub(),
				new Lushr(),
				new Lxor(),
				new Monitorenter(),
				new Monitorexit(),
				new Multianewarray(),
				new New(),
				new Newarray(),
				new Nop(),
				new Pop(),
				new Pop2(),
				new Putfield(),
				new Putstatic(),
				new Ret(),
				new Return(),
				new Saload(),
				new Sastore(),
				new Sipush(),
				new Swap(),
				new Tableswitch(),
				new EmitNIL(),
				new EmitT(),
				new EmitPackage(),
				new EmitSymbolPackage()
		);
		bootstrapEmitPackageFunctions(functions);
	}

	private static void bootstrapCommonLispPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.COMMON_LISP);
	}

	private static void bootstrapExtensionPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.EXTENSIONS);
	}

	private static void bootstrapSystemPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.SYSTEM);
	}

	private static void bootstrapEmitPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.EMIT);
	}

	private static void bootstrapFunctions(final List<FunctionStruct> functions, final PackageStruct aPackage) {
		for (final FunctionStruct function : functions) {
			final SymbolStruct functionSymbol = function.getFunctionSymbol();
			functionSymbol.setfSymbolFunction(function);
			aPackage.export(functionSymbol);
		}
	}
}
