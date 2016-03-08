/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.Analyzer;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.JavaMethodCallStruct;
import jcl.compiler.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.java.JavaNameStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsAnalyzer implements Analyzer<LispStruct, ConsStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LambdaExpander lambdaExpander;

	@Autowired
	private Printer printer;

	@Override
	public LispStruct analyze(final ConsStruct input, final Environment environment) {

		final LispStruct first = input.getCar();

		if (first instanceof SymbolStruct) {
			return analyzeSymbolFunctionCall(input, environment);
		} else if (first instanceof JavaNameStruct) {
			return analyzeJavaMethodCall(input, environment);
		} else if (first instanceof ConsStruct) {
			return analyzeLambdaFunctionCall(input, environment);
		} else {
			final String printedObject = printer.print(first);
			throw new ProgramErrorException("SA LIST: First element must be a symbol, a Java method name, or a lambda list. Got: " + printedObject);
		}
	}

	private SymbolFunctionCallStruct analyzeSymbolFunctionCall(final ListStruct input, final Environment environment) {

		final SymbolStruct functionSymbol = (SymbolStruct) input.getCar();
		final List<LispStruct> functionArguments = input.getRest().getAsJavaList();

		final Set<SymbolStruct> undefinedFunctions = environment.getUndefinedFunctions();

		if (functionSymbol.hasFunction()) {
			// Function is defined
			undefinedFunctions.remove(functionSymbol);
		} else {
			final Stack<SymbolStruct> functionNameStack = environment.getFunctionNameStack();

			if (functionNameStack.contains(functionSymbol)) {
				// Function is undefined, but name exists on the stack to be created
				undefinedFunctions.remove(functionSymbol);
			} else {
				// Add this as a possible undefined function
				undefinedFunctions.add(functionSymbol);
			}
		}

		final List<LispStruct> analyzedFunctionArguments = new ArrayList<>(functionArguments.size());

		for (final LispStruct functionArgument : functionArguments) {
			final LispStruct analyzedFunctionArgument = formAnalyzer.analyze(functionArgument, environment);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		// TODO: work on how to determine tail recursive optimization here!!!
		final SymbolCompilerFunctionStruct symbolCompilerFunction = new SymbolCompilerFunctionStruct(functionSymbol);
		return new SymbolFunctionCallStruct(symbolCompilerFunction, analyzedFunctionArguments, false);
	}

	private JavaMethodCallStruct analyzeJavaMethodCall(final ListStruct input, final Environment environment) {

		final JavaNameStruct methodName = (JavaNameStruct) input.getCar();

		final ListStruct inputRest = input.getRest();
		final LispStruct second = inputRest.getCar();
		final LispStruct javaObject = formAnalyzer.analyze(second, environment);

		final List<LispStruct> methodArguments = inputRest.getRest().getAsJavaList();

		final List<LispStruct> analyzedMethodArguments = new ArrayList<>(methodArguments.size());
		for (final LispStruct methodArgument : methodArguments) {
			final LispStruct analyzedMethodArgument = formAnalyzer.analyze(methodArgument, environment);
			analyzedMethodArguments.add(analyzedMethodArgument);
		}

		return new JavaMethodCallStruct(methodName, javaObject, analyzedMethodArguments);
	}

	private LambdaFunctionCallStruct analyzeLambdaFunctionCall(final ListStruct input, final Environment environment) {

		// ex ((lambda (x) (+ x 1)) 3)
		final ListStruct functionList = (ListStruct) input.getCar();

		final LispStruct functionListFirst = functionList.getCar();

		if (!functionListFirst.equals(SpecialOperatorStruct.LAMBDA)) {
			throw new ProgramErrorException("LIST ANALYZER: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + functionListFirst);
		}

		final LambdaStruct lambdaAnalyzed = lambdaExpander.expand(functionList, environment);

		final List<LispStruct> functionArguments = input.getRest().getAsJavaList();
		final List<LispStruct> analyzedFunctionArguments = new ArrayList<>(functionArguments.size());

		for (final LispStruct functionArgument : functionArguments) {
			final LispStruct analyzedFunctionArgument = formAnalyzer.analyze(functionArgument, environment);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final LambdaCompilerFunctionStruct lambdaCompilerFunction = new LambdaCompilerFunctionStruct(lambdaAnalyzed);
		return new LambdaFunctionCallStruct(lambdaCompilerFunction, analyzedFunctionArguments);
	}
}
