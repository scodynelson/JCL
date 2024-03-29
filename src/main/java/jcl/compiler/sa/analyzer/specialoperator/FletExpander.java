package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import jcl.compiler.StackUtils;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;

public final class FletExpander extends InnerLambdaExpander {

	public static final FletExpander INSTANCE = new FletExpander();

	private FletExpander() {
		super("FLET");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.FLET;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final ListStruct innerLambdas,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStruct> functionNameStack,
	                                             final List<SymbolStruct> functionNames) {

		final List<InnerLambdaStruct.InnerLambdaVar> vars
				= getVars(innerLambdas, innerLambdaEnvironment, declare, functionNames);

		try {
			// Add function names AFTER analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult);
		} finally {
			StackUtils.popX(functionNameStack, functionNames.size());
		}
	}

	@Override
	protected ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStruct functionNameSymbol,
	                                        final List<SymbolStruct> functionNames) {

		final List<LispStruct> letFunctionBindVars = new ArrayList<>();
		final List<LispStruct> rebindFunctions = new ArrayList<>();
		functionNames.stream()
		             .filter(name -> !name.eq(functionNameSymbol))
		             .forEach(name -> {
			             final String tempFunctionBindName = "temp_" + name.getName() + "_bind_" + System.nanoTime();
			             final SymbolStruct tempFunctionBindVar = GlobalPackageStruct.COMMON_LISP_USER.intern(tempFunctionBindName).getSymbol();

			             final ListStruct quoteName = ListStruct.toLispList(CommonLispSymbols.QUOTE, name);

			             // Unbinding of the function
			             final ListStruct unbindFunction = ListStruct.toLispList(CommonLispSymbols.UNBIND_SYMBOL_FUNCTION, quoteName);
			             final ListStruct letFunctionBindVar = ListStruct.toLispList(tempFunctionBindVar, unbindFunction);
			             letFunctionBindVars.add(letFunctionBindVar);

			             // Rebinding of the function
			             final ListStruct rebindFunction = ListStruct.toLispList(CommonLispSymbols.BIND_SYMBOL_FUNCTION, quoteName, tempFunctionBindVar);
			             rebindFunctions.add(rebindFunction);
		             });
		final ListStruct letFunctionBindVarList = ListStruct.toLispList(letFunctionBindVars);
		final ListStruct rebindFunctionList = ListStruct.toLispList(rebindFunctions);

		// NOTE: Make Dotted list here so the 'rebind functions' are added each as a separate cleanup-form
		final ListStruct unwindProtect = (ListStruct)
				ListStruct.toLispDottedList(CommonLispSymbols.UNWIND_PROTECT, innerBlockListStruct, rebindFunctionList);
		return ListStruct.toLispList(CommonLispSymbols.LET, letFunctionBindVarList, unwindProtect);
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'outer' environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return FunctionExpander.INSTANCE.expand(innerFunctionListStruct, parentEnvironment);
	}
}
