/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.sa.analyzer.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.analyzer.ListStructAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.Set;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Autowired
	private ListStructAnalyzer listStructAnalyzer;

	@Autowired
	private LexicalSymbolStructAnalyzer lexicalSymbolStructAnalyzer;

	@Override
	public LispStruct analyzeForm(final LispStruct form) {

		final AnalysisBuilder analysisBuilder = new AnalysisBuilder();

		final LispStruct analyzedForm = analyzeForm(form, analysisBuilder);

		// now see if we have any functions still undefined
		final Set<SymbolStruct<?>> undefinedFunctions = analysisBuilder.getUndefinedFunctions();

		undefinedFunctions.stream()
		                  .forEach(undefinedFunction -> {
			                  LOGGER.warn("; Warning: no function or macro function defined for ");
			                  if (undefinedFunction.getSymbolPackage() != null) {
				                  LOGGER.warn("{}::{}\n", undefinedFunction.getSymbolPackage().getName(), undefinedFunction.getName());
			                  } else {
				                  LOGGER.warn("#:{}\n", undefinedFunction.getName());
			                  }
		                  });

		return analyzedForm;
	}

	@Override
	public LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {

		LispStruct analyzedForm = form;
		if (form instanceof ListStruct) {
			analyzedForm = listStructAnalyzer.analyze(this, (ListStruct) form, analysisBuilder);
		} else if (form instanceof SymbolStruct) {
			analyzedForm = lexicalSymbolStructAnalyzer.analyze(this, (SymbolStruct<?>) form, analysisBuilder);
		} else {
			LOGGER.warn("Unsupported Object Type sent through Analyzer: {}", form);
		}
		return analyzedForm;
	}
}
