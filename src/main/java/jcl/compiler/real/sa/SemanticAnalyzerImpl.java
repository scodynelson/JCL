/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.element.CharacterElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.FloatElement;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.RatioElement;
import jcl.compiler.real.sa.analyzer.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.analyzer.ListStructAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
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
	public Element analyzeForm(final LispStruct form) {

		final AnalysisBuilder analysisBuilder = new AnalysisBuilder();

		final Element analyzedForm = analyzeForm(form, analysisBuilder);

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
	public Element analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {

		if (NullStruct.INSTANCE.equals(form) || NILStruct.INSTANCE.equals(form)) {
			return NullElement.INSTANCE;
		} else if (form instanceof CharacterStruct) {
			return new CharacterElement((CharacterStruct) form);
		} else if (form instanceof IntegerStruct) {
			return new IntegerElement((IntegerStruct) form);
		} else if (form instanceof FloatStruct) {
			return new FloatElement((FloatStruct) form);
		} else if (form instanceof RatioStruct) {
			return new RatioElement((RatioStruct) form);
		} else if (form instanceof SymbolStruct) {
			return lexicalSymbolStructAnalyzer.analyze(this, (SymbolStruct<?>) form, analysisBuilder);
		} else if (form instanceof ListStruct) {
			return listStructAnalyzer.analyze(this, (ListStruct) form, analysisBuilder);
		} else {
			throw new ProgramErrorException("Semantic Analyzer: Unsupported object type cannot be analyzed: " + form);
		}
	}
}
