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
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.FunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsAnalyzerImpl implements ConsAnalyzer {

	private static final long serialVersionUID = -503813311252711494L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LambdaExpander lambdaExpander;

	@Override
	public LispStruct analyze(final ConsStruct input, final Environment environment) {

		final LispStruct first = input.getFirst();

		if (first instanceof SymbolStruct) {
			return analyzeSymbolFunctionCall(input, environment);
		} else if (first instanceof ConsStruct) {
			return analyzeLambdaFunctionCall(input, environment);
		} else {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}
	}

	private FunctionCallStruct analyzeSymbolFunctionCall(final ListStruct input, final Environment environment) {

		final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) input.getFirst();
		final List<LispStruct> functionArguments = input.getRest().getAsJavaList();

		final Set<SymbolStruct<?>> undefinedFunctions = environment.getUndefinedFunctions();

		FunctionStruct function;
		try {
			function = functionSymbol.getFunction();
		} catch (final ErrorException ignore) {
			function = null;
		}
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
		}

		final List<LispStruct> analyzedFunctionArguments = new ArrayList<>(functionArguments.size());

		for (final LispStruct functionArgument : functionArguments) {
			final LispStruct analyzedFunctionArgument = formAnalyzer.analyze(functionArgument, environment);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		final boolean hasFunctionBinding = Environments.hasFunctionBinding(environment, functionSymbol);

		return new FunctionCallStruct(hasFunctionBinding, functionSymbol, analyzedFunctionArguments);
	}

	private LambdaFunctionCallStruct analyzeLambdaFunctionCall(final ListStruct input, final Environment environment) {

		// ex ((lambda (x) (+ x 1)) 3)
		final ListStruct functionList = (ListStruct) input.getFirst();

		final LispStruct functionListFirst = functionList.getFirst();

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

		return new LambdaFunctionCallStruct(lambdaAnalyzed, analyzedFunctionArguments);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(formAnalyzer)
		                            .append(lambdaExpander)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ConsAnalyzerImpl rhs = (ConsAnalyzerImpl) obj;
		return new EqualsBuilder().append(formAnalyzer, rhs.formAnalyzer)
		                          .append(lambdaExpander, rhs.lambdaExpander)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(lambdaExpander)
		                                                                .toString();
	}
}
