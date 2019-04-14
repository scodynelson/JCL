package jcl.system;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
import jcl.functions.ValuesFunction;
import jcl.functions.array.ListToVectorFunction;
import jcl.functions.condition.ErrorFunction;
import jcl.functions.java.JClass;
import jcl.functions.java.JInvoke;
import jcl.functions.java.JInvokeStatic;
import jcl.functions.java.JMethod;
import jcl.functions.java.JNew;
import jcl.functions.lisppackage.ExportFunction;
import jcl.functions.lisppackage.InPackageFunction;
import jcl.functions.list.AppendFunction;
import jcl.functions.list.CarFunction;
import jcl.functions.list.CdrFunction;
import jcl.functions.list.ConsFunction;
import jcl.functions.list.ListFunction;
import jcl.functions.list.ListStarFunction;
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
import jcl.functions.system.Help;
import jcl.functions.system.MaxMemory;
import jcl.functions.system.TotalMemory;
import jcl.lang.FunctionStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.printer.functions.PrintObjectFunction;
import jcl.system.function.QuitFunction;
import jcl.util.CodePointConstants;
import org.springframework.context.ApplicationContext;

class BootstrapFunctions {

	static void bootstrap(final ApplicationContext context) {
		bootstrapSystemFunctions();

		bootstrapPredicateFunctions();
		bootstrapArrayFunctions();
		bootstrapConditionFunctions();
		bootstrapJavaFunctions();
		bootstrapPackageFunctions();
		bootstrapListFunctions();
		bootstrapReaderFunctions();
		bootstrapSymbolFunctions();
		bootstrapExtensionFunctions(context);
		bootstrapPrinterFunctions();
	}

	private static void bootstrapSystemFunctions() {
		final FuncallFunction funcallFunction = new FuncallFunction();

		CompilerVariables.MACROEXPAND_HOOK.setValue(funcallFunction);

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
				new JInvoke(),
				new JInvokeStatic(),
				new JMethod(),
				new JNew()
		);
		bootstrapExtensionPackageFunctions(functions);
	}

	private static void bootstrapPackageFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new ExportFunction(),
				new InPackageFunction() // TODO: in-package is a Macro
		);
		bootstrapCommonLispPackageFunctions(functions);
	}

	private static void bootstrapListFunctions() {
		final List<FunctionStruct> functions = Arrays.asList(
				new AppendFunction(),
				new CarFunction(),
				new CdrFunction(),
				new ConsFunction(),
				new ListFunction(),
				new ListStarFunction(),
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

		ReaderVariables.READTABLE.getVariableValue()
		                         .makeDispatchMacroCharacter(readDispatchCharacterFunction, CodePointConstants.NUMBER_SIGN, false);

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

	private static void bootstrapExtensionFunctions(final ApplicationContext context) {
		final List<FunctionStruct> functions = Arrays.asList(
				new FreeMemory(),
				new GC(),
				new Help(),
				new MaxMemory(),
				new QuitFunction(context),
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

	private static void bootstrapCommonLispPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.COMMON_LISP);
	}

	private static void bootstrapExtensionPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.EXTENSIONS);
	}

	private static void bootstrapSystemPackageFunctions(final List<FunctionStruct> functions) {
		bootstrapFunctions(functions, GlobalPackageStruct.SYSTEM);
	}

	private static void bootstrapFunctions(final List<FunctionStruct> functions, final PackageStruct aPackage) {
		for (final FunctionStruct function : functions) {
			final SymbolStruct functionSymbol = function.getFunctionSymbol();
			functionSymbol.setFunction(function);

			final SymbolStruct symbol = aPackage.intern(functionSymbol.getName()).getSymbol();
			aPackage.export(symbol);
		}
	}
}
