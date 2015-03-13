/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.functioncall.FunctionCallStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionCallAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = -2348426068345427015L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public FunctionCallStruct analyze(final ListStruct input, final Environment environment) {

		final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) input.getFirst();
		final List<LispStruct> functionArguments = input.getRest().getAsJavaList();

		final Set<SymbolStruct<?>> undefinedFunctions = environment.getUndefinedFunctions();

		final FunctionStruct function = functionSymbol.getFunction();
		if (function == null) {
			final Stack<SymbolStruct<?>> functionNameStack = environment.getFunctionNameStack();

			if (functionNameStack.contains(functionSymbol)) {
				// Function is undefined, but name exists on the stack to be created
				undefinedFunctions.remove(functionSymbol);
			} else {
				// Add this as a possible undefined function
				undefinedFunctions.add(functionSymbol);
			}
		} else {
			// Function is defined
			undefinedFunctions.remove(functionSymbol);

			final String functionName = functionSymbol.getName();
			final OrdinaryLambdaListBindings lambdaListBindings = function.getLambdaListBindings();
			validateFunctionArguments(functionName, lambdaListBindings, functionArguments);
		}

		final List<LispStruct> analyzedFunctionArguments = new ArrayList<>(functionArguments.size());

		for (final LispStruct functionArgument : functionArguments) {
			final LispStruct analyzedFunctionArgument = formAnalyzer.analyze(functionArgument, environment);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final boolean hasFunctionBinding = Environments.hasFunctionBinding(environment, functionSymbol);

		return new FunctionCallStruct(hasFunctionBinding, functionSymbol, analyzedFunctionArguments);
	}
}
