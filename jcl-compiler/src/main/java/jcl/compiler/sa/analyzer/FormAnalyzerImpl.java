/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa.analyzer;

import jcl.compiler.environment.Environment;
import jcl.compiler.functions.MacroExpandFunction;
import jcl.compiler.functions.MacroExpandResult;
import jcl.compiler.sa.FormAnalyzer;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.list.ConsStructImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FormAnalyzerImpl implements FormAnalyzer {

	@Autowired
	private MacroExpandFunction macroExpandFunction;

	@Autowired
	private ConsAnalyzer consAnalyzer;

	@Override
	public LispStruct analyze(final LispStruct input, final Environment environment) {

		final MacroExpandResult macroExpandReturn = macroExpandFunction.macroExpand(input, environment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		if (expandedForm instanceof SymbolStruct) {
			return expandedForm;
		} else if (expandedForm instanceof ConsStructImpl) {
			return consAnalyzer.analyze((ConsStructImpl) expandedForm, environment);
		} else {
			return expandedForm;
		}
	}
}
