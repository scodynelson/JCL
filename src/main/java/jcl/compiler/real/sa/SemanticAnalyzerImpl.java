/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.sa.analyzer.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.analyzer.ListStructAnalyzer;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.util.InstanceOf;
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

			                  final String functionName = undefinedFunction.getName();
			                  final PackageStruct symbolPackage = undefinedFunction.getSymbolPackage();
			                  if (symbolPackage != null) {
				                  LOGGER.warn("{}::{}\n", symbolPackage.getName(), functionName);
			                  } else {
				                  LOGGER.warn("#:{}\n", functionName);
			                  }
		                  });

		return analyzedForm;
	}

	@Override
	public LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {
		return InstanceOf.when(form)
		                 .isInstanceOf(ListStruct.class).thenReturn(e -> listStructAnalyzer.analyze(this, e, analysisBuilder))
		                 .isInstanceOf(SymbolStruct.class).thenReturn(e -> lexicalSymbolStructAnalyzer.analyze(this, e, analysisBuilder))
		                 .otherwise(e -> {
			                 LOGGER.warn("Unsupported Object Type sent through Analyzer: {}", e);
			                 return e;
		                 });
	}
}
