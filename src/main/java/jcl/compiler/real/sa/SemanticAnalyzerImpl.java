/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import java.util.Map;
import java.util.Set;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpand;
import jcl.compiler.real.sa.analyzer.expander.real.NewMacroExpandReturn;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Autowired
	private NewMacroExpand newMacroExpand;

	@Resource
	private Map<Class<? extends LispStruct>, Analyzer<? extends LispStruct, LispStruct>> analyzerStrategies;

	@Override
	public LispStruct analyzeForm(final LispStruct form) {

		final AnalysisBuilder analysisBuilder = new AnalysisBuilder(this);

		final LispStruct analyzedForm = analyzeForm(form, analysisBuilder);

		// now see if we have any functions still undefined
		final Set<SymbolStruct<?>> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		undefinedFunctions.stream()
		                  .forEach(undefinedFunction -> {
			                  LOGGER.warn("; Warning: no function or macro function defined for ");

			                  final String functionName = undefinedFunction.getName();
			                  final String symbolPackage = undefinedFunction.getSymbolPackage().getName();
			                  if (symbolPackage != null) {
				                  LOGGER.warn("{}::{}", symbolPackage, functionName);
			                  } else {
				                  LOGGER.warn("#:{}", functionName);
			                  }
		                  });

		return analyzedForm;
	}

	@Override
	public LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {

		final NewMacroExpandReturn macroExpandReturn = newMacroExpand.macroExpand(form, analysisBuilder);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		final Analyzer<? extends LispStruct, LispStruct> functionCallAnalyzer = analyzerStrategies.get(expandedForm.getClass());
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
