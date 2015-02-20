/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.FunctionCallElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

@Component
public class SymbolFunctionCallAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = -2348426068345427015L;

	@Override
	public FunctionCallElement analyze(final SemanticAnalyzer analyzer, final ConsStruct input, final AnalysisBuilder analysisBuilder) {

		final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) input.getFirst();
		final ListStruct functionArguments = input.getRest();

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final Set<SymbolStruct<?>> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		final FunctionStruct function = functionSymbol.getFunction();
		if (function == null) {
			final Stack<SymbolStruct<?>> functionNameStack = analysisBuilder.getFunctionNameStack();

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
			validateFunctionArguments(functionName, lambdaListBindings, functionArgumentsList);
		}

		final List<Element> analyzedFunctionArguments = new ArrayList<>(functionArgumentsList.size());

		for (final LispStruct functionArgument : functionArgumentsList) {
			final Element analyzedFunctionArgument = analyzer.analyzeForm(functionArgument, analysisBuilder);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final boolean hasFunctionBinding = Environments.hasFunctionBinding(currentEnvironment, functionSymbol);

		final SymbolElement<?> functionSymbolSE = new SymbolElement<>(functionSymbol);
		return new FunctionCallElement(hasFunctionBinding, functionSymbolSE, analyzedFunctionArguments);
	}
}
