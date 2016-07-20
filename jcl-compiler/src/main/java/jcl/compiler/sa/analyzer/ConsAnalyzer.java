/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.Analyzer;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.JavaMethodCallStruct;
import jcl.compiler.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.list.ConsStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsAnalyzer implements Analyzer<LispStruct, ConsStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LambdaExpander lambdaExpander;

	@Override
	public LispStruct analyze(final ConsStruct input, final Environment environment) {
		final LispStruct first = input.getCar();

		if (NILStruct.INSTANCE.equals(first)) {
			throw new ProgramErrorException("CONS ANALYZER: First element must be a symbol, a Java method name, or a lambda. Got: " + input);
		} else if (first instanceof SymbolStruct) {
			return analyzeSymbolFunctionCall(input, environment);
		} else if (first instanceof JavaNameStruct) {
			return analyzeJavaMethodCall(input, environment);
		} else if (first instanceof ListStruct) {
			return analyzeLambdaFunctionCall(input, environment);
		} else {
			throw new ProgramErrorException("CONS ANALYZER: First element must be a symbol, a Java method name, or a lambda. Got: " + first);
		}
	}

	private SymbolFunctionCallStruct analyzeSymbolFunctionCall(final ListStruct input, final Environment environment) {
		final Iterator<LispStruct> iterator = input.iterator();

		final SymbolStruct functionSymbol = (SymbolStruct) iterator.next();

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

		final List<LispStruct> functionArguments = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			functionArguments.add(analyzedElement);
		});

		// TODO: work on how to determine tail recursive optimization here!!!
		final SymbolCompilerFunctionStruct symbolCompilerFunction = new SymbolCompilerFunctionStruct(functionSymbol);
		return new SymbolFunctionCallStruct(symbolCompilerFunction, functionArguments, false);
	}

	private JavaMethodCallStruct analyzeJavaMethodCall(final ListStruct input, final Environment environment) {
		final Iterator<LispStruct> iterator = input.iterator();

		final JavaNameStruct methodName = (JavaNameStruct) iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("CONS ANALYZER: Incorrect number of arguments to Java method call: 0. Expected at least 1 argument.");
		}
		final LispStruct second = iterator.next();
		final LispStruct javaObject = formAnalyzer.analyze(second, environment);

		final List<LispStruct> methodArguments = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			methodArguments.add(analyzedElement);
		});
		return new JavaMethodCallStruct(methodName, javaObject, methodArguments);
	}

	private LambdaFunctionCallStruct analyzeLambdaFunctionCall(final ListStruct input, final Environment environment) {
		// ex ((lambda (x) (+ x 1)) 3)

		final Iterator<LispStruct> iterator = input.iterator();

		final ListStruct functionList = (ListStruct) iterator.next();

		final LispStruct functionListFirst = functionList.getCar();
		if (!functionListFirst.equals(SpecialOperatorStruct.LAMBDA)) {
			throw new ProgramErrorException("CONS ANALYZER: First element of a lambda list must be the SpecialOperator 'LAMBDA'. Got: " + functionListFirst);
		}

		final LambdaStruct lambdaAnalyzed = lambdaExpander.expand(functionList, environment);

		final List<LispStruct> functionArguments = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			functionArguments.add(analyzedElement);
		});

		final LambdaCompilerFunctionStruct lambdaCompilerFunction = new LambdaCompilerFunctionStruct(lambdaAnalyzed);
		return new LambdaFunctionCallStruct(lambdaCompilerFunction, functionArguments);
	}
}
