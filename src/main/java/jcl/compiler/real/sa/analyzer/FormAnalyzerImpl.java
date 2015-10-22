/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.functions.MacroExpandFunction;
import jcl.compiler.real.functions.MacroExpandResult;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.lists.ConsStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FormAnalyzerImpl implements FormAnalyzer {

	private static final long serialVersionUID = 7315325926130864447L;

	@Autowired
	private MacroExpandFunction macroExpandFunction;

	@Autowired
	private SymbolAnalyzer symbolAnalyzer;

	@Autowired
	private ConsAnalyzer consAnalyzer;

	@Override
	public LispStruct analyze(final LispStruct input, final Environment environment) {

		final MacroExpandResult macroExpandReturn = macroExpandFunction.macroExpand(input, environment);
		final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

		if (expandedForm instanceof SymbolStruct) {
			return symbolAnalyzer.analyze((SymbolStruct<?>) expandedForm, environment);
		} else if (expandedForm instanceof ConsStruct) {
			return consAnalyzer.analyze((ConsStruct) expandedForm, environment);
		} else {
			return expandedForm;
		}
	}
}
