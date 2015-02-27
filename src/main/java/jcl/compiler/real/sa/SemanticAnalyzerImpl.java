/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ProgramErrorException;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.Set;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Resource
	private Map<Class<? extends SimpleElement>, Analyzer<? extends Element, SimpleElement>> elementAnalyzerStrategies;

	@Override
	public Element analyzeForm(final SimpleElement form) {

		final AnalysisBuilder analysisBuilder = new AnalysisBuilder();

		final Element analyzedForm = analyzeForm(form, analysisBuilder);

		// now see if we have any functions still undefined
		final Set<SymbolElement> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		undefinedFunctions.stream()
		                  .forEach(undefinedFunction -> {
			                  LOGGER.warn("; Warning: no function or macro function defined for ");

			                  final String functionName = undefinedFunction.getSymbolName();
			                  final String symbolPackage = undefinedFunction.getPackageName();
			                  if (symbolPackage != null) {
				                  LOGGER.warn("{}::{}\n", symbolPackage, functionName);
			                  } else {
				                  LOGGER.warn("#:{}\n", functionName);
			                  }
		                  });

		return analyzedForm;
	}

	@Override
	public Element analyzeForm(final SimpleElement form, final AnalysisBuilder analysisBuilder) {

		final Analyzer<? extends Element, SimpleElement> functionCallAnalyzer = elementAnalyzerStrategies.get(form.getClass());
		if (functionCallAnalyzer == null) {
			throw new ProgramErrorException("Semantic Analyzer: Unsupported object type cannot be analyzed: " + form);
		}

		return functionCallAnalyzer.analyze(this, form, analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
