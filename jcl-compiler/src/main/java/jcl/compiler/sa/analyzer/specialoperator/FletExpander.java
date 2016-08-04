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
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.SymbolStructImpl;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FletExpander extends InnerLambdaExpander {

	@Autowired
	private FunctionExpander functionExpander;

	protected FletExpander() {
		super("FLET");
	}

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		return SpecialOperatorStructImpl.FLET;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final ListStruct innerLambdas,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStructImpl> functionNameStack,
	                                             final List<SymbolStructImpl> functionNames) {

		final List<InnerLambdaStruct.InnerLambdaVar> vars
				= getVars(innerLambdas, innerLambdaEnvironment, declare, functionNames);

		try {
			// Add function names AFTER analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult, declare);
		} finally {
			StackUtils.popX(functionNameStack, functionNames.size());
		}
	}

	@Override
	protected ListStruct getInnerLambdaBody(final ListStruct innerBlockListStruct, final SymbolStructImpl functionNameSymbol,
	                                        final List<SymbolStructImpl> functionNames) {

		final List<LispStruct> letFunctionBindVars = new ArrayList<>();
		final List<LispStruct> rebindFunctions = new ArrayList<>();
		functionNames.stream()
		             .filter(name -> !name.equals(functionNameSymbol))
		             .forEach(name -> {
			             final String tempFunctionBindName = "temp_" + name.getName() + "_bind_" + System.nanoTime();
			             final SymbolStructImpl tempFunctionBindVar = GlobalPackageStruct.COMMON_LISP_USER.intern(tempFunctionBindName).getSymbol();

			             final ListStruct quoteName = LispStructFactory.toProperList(SpecialOperatorStructImpl.QUOTE, name);

			             // Unbinding of the function
			             final ListStruct unbindFunction = LispStructFactory.toProperList(CommonLispSymbols.UNBIND_SYMBOL_FUNCTION, quoteName);
			             final ListStruct letFunctionBindVar = LispStructFactory.toProperList(tempFunctionBindVar, unbindFunction);
			             letFunctionBindVars.add(letFunctionBindVar);

			             // Rebinding of the function
			             final ListStruct rebindFunction = LispStructFactory.toProperList(CommonLispSymbols.BIND_SYMBOL_FUNCTION, quoteName, tempFunctionBindVar);
			             rebindFunctions.add(rebindFunction);
		             });
		final ListStruct letFunctionBindVarList = LispStructFactory.toProperList(letFunctionBindVars);
		final ListStruct rebindFunctionList = LispStructFactory.toProperList(rebindFunctions);

		// NOTE: Make Dotted list here so the 'rebind functions' are added each as a separate cleanup-form
		final ListStruct unwindProtect = LispStructFactory.toDottedList(SpecialOperatorStructImpl.UNWIND_PROTECT, innerBlockListStruct, rebindFunctionList);
		return LispStructFactory.toProperList(SpecialOperatorStructImpl.LET, letFunctionBindVarList, unwindProtect);
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'outer' environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return functionExpander.expand(innerFunctionListStruct, parentEnvironment);
	}
}
