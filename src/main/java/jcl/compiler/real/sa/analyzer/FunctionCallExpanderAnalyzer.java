/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroExpandFunction;
import jcl.compiler.real.sa.analyzer.expander.MacroExpandReturn;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;

@Component
public class FunctionCallExpanderAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = 322096040503229739L;

	@Resource
	private Map<Class<? extends LispStruct>, FunctionCallAnalyzer> expandedFunctionCallAnalyzerStrategies;

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ConsStruct input, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, currentEnvironment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		if (expandedForm instanceof ConsStruct) {
			final ConsStruct expandedFormList = (ConsStruct) expandedForm;
			final LispStruct expandedFormListFirst = expandedFormList.getFirst();

			final FunctionCallAnalyzer expandedFunctionCallAnalyzer = expandedFunctionCallAnalyzerStrategies.get(expandedFormListFirst.getClass());
			if (expandedFunctionCallAnalyzer == null) {
				throw new ProgramErrorException("LIST ANALYZER: First element of expanded form must be a SpecialOperator or of type SymbolStruct. Got: " + expandedFormListFirst);
			}

			return expandedFunctionCallAnalyzer.analyze(analyzer, expandedFormList, analysisBuilder);
		} else {
			return analyzer.analyzeForm(expandedForm, analysisBuilder);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
