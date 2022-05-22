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
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;

@Log4j2
@UtilityClass
public final class SemanticAnalyzer {

	public static LambdaStruct analyze(final ListStruct form) {
		final Environment nullEnvironment = Environment.NULL;

		final Set<SymbolStruct> undefinedFunctions = nullEnvironment.getUndefinedFunctions();
		undefinedFunctions.clear();

		final LambdaStruct analyzedForm = LambdaExpander.INSTANCE.expand(form, nullEnvironment);

		// now see if we have any functions still undefined
		undefinedFunctions.forEach(SemanticAnalyzer::unknownFunctionWarning);
		undefinedFunctions.clear();

		return analyzedForm;
	}

	private static void unknownFunctionWarning(final SymbolStruct undefinedFunction) {
		log.warn("Warning: no function or macro function defined for: {}", undefinedFunction);
	}
}
