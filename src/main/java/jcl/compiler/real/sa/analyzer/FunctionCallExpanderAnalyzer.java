/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;

@Component
public class FunctionCallExpanderAnalyzer extends FunctionCallAnalyzer {

	private static final long serialVersionUID = 322096040503229739L;

	@Resource
	private Map<Class<? extends SimpleElement>, FunctionCallAnalyzer> expandedFunctionCallAnalyzerStrategies;

	@Override
	public Element analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final SimpleElement formFirst = elements.getFirst();

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
