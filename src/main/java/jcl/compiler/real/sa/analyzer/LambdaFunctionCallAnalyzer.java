/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.functioncall.LambdaFunctionCallElement;
import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.lambda.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class LambdaFunctionCallAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = -2147198941590214441L;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public LambdaFunctionCallElement analyze(final SemanticAnalyzer analyzer, final ConsStruct input, final AnalysisBuilder analysisBuilder) {
		// ex ((lambda (x) (+ x 1)) 3)
		final ListStruct functionList = (ListStruct) input.getFirst();

		final LispStruct functionListFirst = functionList.getFirst();

		if (!functionListFirst.equals(SpecialOperator.LAMBDA)) {
			throw new ProgramErrorException("LIST ANALYZER: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + functionListFirst);
		}

		final LambdaElement lambdaAnalyzed = lambdaAnalyzer.analyze(analyzer, functionList, analysisBuilder);
		final OrdinaryLambdaListBindings lambdaListBindings = lambdaAnalyzed.getLambdaListBindings();

		final ListStruct functionArguments = input.getRest();
		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		validateFunctionArguments("Anonymous Lambda", lambdaListBindings, functionArgumentsList);

		final List<Element> analyzedFunctionArguments = new ArrayList<>(functionArgumentsList.size());

		for (final LispStruct functionArgument : functionArgumentsList) {
			final Element analyzedFunctionArgument = analyzer.analyzeForm(functionArgument, analysisBuilder);
			analyzedFunctionArguments.add(analyzedFunctionArgument);
		}

		return new LambdaFunctionCallElement(lambdaAnalyzed, analyzedFunctionArguments);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
