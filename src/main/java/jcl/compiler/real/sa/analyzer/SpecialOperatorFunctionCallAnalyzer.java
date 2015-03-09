/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class SpecialOperatorFunctionCallAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = -3414591309442849313L;

	@Resource
	private Map<SpecialOperator, SpecialOperatorAnalyzer> specialOperatorAnalyzerStrategies;

	@Override
	public LispStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final SpecialOperator specialOperator = (SpecialOperator) input.getFirst();
		final SpecialOperatorAnalyzer specialOperatorAnalyzer = specialOperatorAnalyzerStrategies.get(specialOperator);
		if (specialOperatorAnalyzer == null) {
			throw new ProgramErrorException("LIST ANALYZER: SpecialOperator symbol supplied is not supported: " + specialOperator);
		}

		return specialOperatorAnalyzer.analyze(input, analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
