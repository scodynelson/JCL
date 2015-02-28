/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpand;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpandReturn;
import jcl.conditions.exceptions.ProgramErrorException;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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

	@Autowired
	private NewMacroExpand newMacroExpand;

	@Resource
	private Map<Class<? extends Element>, Analyzer<? extends Element, Element>> elementAnalyzerStrategies;

	@Override
	public Element analyzeForm(final SimpleElement form) {

		final AnalysisBuilder analysisBuilder = new AnalysisBuilder(this);

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

		final NewMacroExpandReturn macroExpandReturn = newMacroExpand.macroExpand(form, analysisBuilder);
		final Element expandedForm = macroExpandReturn.getExpandedForm();

		final Analyzer<? extends Element, Element> functionCallAnalyzer = elementAnalyzerStrategies.get(expandedForm.getClass());
		if (functionCallAnalyzer == null) {
			return expandedForm; // TODO: we need to rework the logic a bit, so for now we just return...
//			throw new ProgramErrorException("Semantic Analyzer: Unsupported object type cannot be analyzed: " + expandedForm);
		}

		return functionCallAnalyzer.analyze(expandedForm, analysisBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
