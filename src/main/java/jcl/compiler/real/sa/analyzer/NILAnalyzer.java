/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.symbols.NILStruct;
import org.springframework.stereotype.Component;

@Component
public class NILAnalyzer implements Analyzer<NullElement, NILStruct> {

	private static final long serialVersionUID = -2298246185357779150L;

	@Override
	public NullElement analyze(final SemanticAnalyzer analyzer, final NILStruct input, final AnalysisBuilder analysisBuilder) {
		return NullElement.INSTANCE;
	}
}
