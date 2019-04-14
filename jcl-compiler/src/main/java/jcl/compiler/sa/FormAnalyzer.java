/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.InternalMacroExpand;
import jcl.compiler.function.MacroExpandResult;
import jcl.compiler.sa.analyzer.ConsAnalyzer;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class FormAnalyzer {

	public static LispStruct analyze(final LispStruct input, final Environment environment) {

		final MacroExpandResult macroExpandReturn = InternalMacroExpand.macroExpand(input, environment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		if (expandedForm instanceof SymbolStruct) {
			return expandedForm;
		} else if (expandedForm instanceof ConsStruct) {
			return ConsAnalyzer.analyze((ConsStruct) expandedForm, environment);
		} else {
			return expandedForm;
		}
	}
}
