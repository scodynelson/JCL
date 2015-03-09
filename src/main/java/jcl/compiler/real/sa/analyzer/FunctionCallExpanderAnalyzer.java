/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class FunctionCallExpanderAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = 322096040503229739L;

	@Resource
	private Map<Class<? extends LispStruct>, FunctionCallAnalyzer> expandedFunctionCallAnalyzerStrategies;

	@Override
	public LispStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final LispStruct formFirst = input.getFirst();

		final FunctionCallAnalyzer expandedFunctionCallAnalyzer = expandedFunctionCallAnalyzerStrategies.get(formFirst.getClass());
		if (expandedFunctionCallAnalyzer == null) {
			throw new ProgramErrorException("LIST ANALYZER: First element of expanded form must be a SpecialOperator or of type SymbolStruct. Got: " + formFirst);
		}

		return expandedFunctionCallAnalyzer.analyze(input, analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
