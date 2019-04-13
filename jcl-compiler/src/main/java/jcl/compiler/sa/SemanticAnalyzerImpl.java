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
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private final LambdaExpander lambdaExpander;

	public SemanticAnalyzerImpl(final LambdaExpander lambdaExpander) {
		this.lambdaExpander = lambdaExpander;
	}

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
		log.warn("Warning: no function or macro function defined for: {}", undefinedFunction);
	}
}
