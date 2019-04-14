package jcl.system;

import java.util.Arrays;
import java.util.List;

import jcl.functions.CompileFunction;
import jcl.functions.EqFunction;
import jcl.functions.EqlFunction;
import jcl.functions.EqualFunction;
import jcl.functions.EqualpFunction;
import jcl.functions.EvalFunction;
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
import jcl.functions.system.QuitFunction;
import jcl.functions.system.TotalMemory;
import jcl.lang.FunctionStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.context.ApplicationContext;

class BootstrapFunctions {

	static void bootstrap(final ApplicationContext context) throws Exception {
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
	}

	private static void bootstrapSystemFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
//				new ApplyFunction(),
//				new CompileFileFunction(),
//				new CompileFilePathnameFunction(),
				new CompileFunction(),
				new EqFunction(),
				new EqlFunction(),
				new EqualFunction(),
				new EqualpFunction(),
				new EvalFunction(),
//				new FuncallFunction(),
//				new LoadFunction(),
				new MacroExpand1Function(),
				new MacroExpandFunction(),
				new ValuesFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapPredicateFunctions() throws Exception {
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
				new PredicateFunctions.VectorPFunction(),

				new PredicateFunctions.KeywordPFunction()
		);		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapArrayFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new ListToVectorFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapConditionFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new ErrorFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapJavaFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new JClass(),
				new JInvoke(),
				new JInvokeStatic(),
				new JMethod(),
				new JNew()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapPackageFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new ExportFunction(),
				new InPackageFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapListFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new AppendFunction(),
				new CarFunction(),
				new CdrFunction(),
				new ConsFunction(),
				new ListFunction(),
				new ListStarFunction(),
				new NconcFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapReaderFunctions() throws Exception {
		final ReadDispatchCharacterFunction readDispatchCharacterFunction = new ReadDispatchCharacterFunction();

		ReaderVariables.READTABLE.getVariableValue()
		                         .makeDispatchMacroCharacter(readDispatchCharacterFunction, CodePointConstants.NUMBER_SIGN, false);

		BootstrapReaderMacroFunctions.bootstrap();

		final List<FunctionStruct> functions = Arrays.asList(
				readDispatchCharacterFunction,
				new ReadFunction(),
				new ReadPreservingWhitespaceFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapSymbolFunctions() throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new GensymFunction(),
				new GentempFunction(),
				new SetSymbolFunctionFunction(),
				new SetSymbolMacroFunction(),
				new SetSymbolSetfFunctionFunction()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}

	private static void bootstrapExtensionFunctions(final ApplicationContext context) throws Exception {
		final List<FunctionStruct> functions = Arrays.asList(
				new FreeMemory(),
				new GC(),
				new Help(),
				new MaxMemory(),
				new QuitFunction(context),
				new TotalMemory()
		);

		for (final FunctionStruct function : functions) {
			function.afterPropertiesSet();
		}
	}
}
