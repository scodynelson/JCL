/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa;

import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.LambdaExpander;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.printer.Printer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Autowired
	private LambdaExpander lambdaExpander;

	@Autowired
	private Printer printer;

	@Override
	public LambdaStruct analyze(final ListStruct form) {
		final Environment nullEnvironment = Environment.NULL;

		final Set<SymbolStruct> undefinedFunctions = nullEnvironment.getUndefinedFunctions();
		undefinedFunctions.clear();

		final LambdaStruct analyzedForm = lambdaExpander.expand(form, nullEnvironment);

		// now see if we have any functions still undefined
		undefinedFunctions.stream()
		                  .forEach(this::unknownFunctionWarning);
		undefinedFunctions.clear();

		return analyzedForm;
	}

	private void unknownFunctionWarning(final SymbolStruct undefinedFunction) {
		final String printedUndefinedFunction = printer.print(undefinedFunction);
		LOGGER.warn("Warning: no function or macro function defined for: {}", printedUndefinedFunction);
	}
}
