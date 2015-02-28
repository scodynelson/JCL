/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.functioncall.FunctionCallElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.functions.FunctionStruct;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

@Component
public class SymbolFunctionCallAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = -2348426068345427015L;

	@Override
	public FunctionCallElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final SymbolElement functionSymbol = (SymbolElement) elements.getFirst();
		final EnhancedLinkedList<SimpleElement> functionArguments = elements.getAllButFirst();

		final Set<SymbolElement> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		final FunctionStruct function = null; //functionSymbol.getFunction(); TODO: ???
		if (function == null) {
			final Stack<SymbolElement> functionNameStack = analysisBuilder.getFunctionNameStack();

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

			final String functionName = functionSymbol.getSymbolName();
			final OrdinaryLambdaListBindings lambdaListBindings = function.getLambdaListBindings();
			validateFunctionArguments(functionName, lambdaListBindings, functionArguments);
		}

		final List<Element> analyzedFunctionArguments = new ArrayList<>(functionArguments.size());

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		for (final SimpleElement functionArgument : functionArguments) {
			final Element analyzedFunctionArgument = analyzer.analyzeForm(functionArgument, analysisBuilder);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final boolean hasFunctionBinding = Environments.hasFunctionBinding(currentEnvironment, functionSymbol);

		return new FunctionCallElement(hasFunctionBinding, functionSymbol, analyzedFunctionArguments);
	}
}
