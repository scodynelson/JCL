package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.body.BodyProcessingResult;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.declare.DeclareStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.functions.BindSymbolFunctionFunction;
import jcl.symbols.functions.UnbindSymbolFunctionFunction;
import jcl.system.StackUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FletExpander extends InnerLambdaExpander {

	private static final long serialVersionUID = -3183832254183452606L;

	@Autowired
	private FunctionExpander functionExpander;

	protected FletExpander() {
		super("FLET");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.FLET;
	}

	@Override
	protected InnerLambdaStruct buildInnerLambda(final List<LispStruct> innerLambdasAsJavaList,
	                                             final Environment innerLambdaEnvironment,
	                                             final BodyProcessingResult bodyProcessingResult,
	                                             final DeclareStruct declare,
	                                             final Stack<SymbolStruct> functionNameStack,
	                                             final List<SymbolStruct> functionNames) {

		final List<InnerLambdaStruct.InnerLambdaVar> vars
				= getVars(innerLambdasAsJavaList, innerLambdaEnvironment, declare, functionNames);

		try {
			// Add function names BEFORE analyzing the functions.
			StackUtils.pushAll(functionNameStack, functionNames);
			return getInnerLambda(vars, innerLambdaEnvironment, bodyProcessingResult, declare);
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
		             .filter(name -> !name.equals(functionNameSymbol))
		             .forEach(name -> {
			             final String tempFunctionBindName = "temp_" + name.getName() + "_bind_" + System.nanoTime();
			             final SymbolStruct tempFunctionBindVar = GlobalPackageStruct.COMMON_LISP_USER.intern(tempFunctionBindName).getSymbol();

			             final ListStruct quoteName = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, name);

			             // Unbinding of the function
			             final ListStruct unbindFunction = ListStruct.buildProperList(UnbindSymbolFunctionFunction.UNBIND_SYMBOL_FUNCTION, quoteName);
			             final ListStruct letFunctionBindVar = ListStruct.buildProperList(tempFunctionBindVar, unbindFunction);
			             letFunctionBindVars.add(letFunctionBindVar);

			             // Rebinding of the function
			             final ListStruct rebindFunction = ListStruct.buildProperList(BindSymbolFunctionFunction.BIND_SYMBOL_FUNCTION, quoteName, tempFunctionBindVar);
			             rebindFunctions.add(rebindFunction);
		             });
		final ListStruct letFunctionBindVarList = ListStruct.buildProperList(letFunctionBindVars);
		final ListStruct rebindFunctionList = ListStruct.buildProperList(rebindFunctions);

		// NOTE: Make Dotted list here so the 'rebind functions' are added each as a separate cleanup-form
		final ListStruct unwindProtect = ListStruct.buildDottedList(SpecialOperatorStruct.UNWIND_PROTECT, innerBlockListStruct, rebindFunctionList);
		return ListStruct.buildProperList(SpecialOperatorStruct.LET, letFunctionBindVarList, unwindProtect);
	}

	@Override
	protected CompilerFunctionStruct expandBuiltInnerFunction(final ListStruct innerFunctionListStruct, final Environment environment) {
		// Evaluate in the 'outer' environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return functionExpander.expand(innerFunctionListStruct, parentEnvironment);
	}
}
