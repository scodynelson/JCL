/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.NullStruct;
import org.springframework.stereotype.Component;

@Component
public class NullAnalyzer implements Analyzer<NullElement, NullStruct> {

	private static final long serialVersionUID = 7173011071787739613L;

	@Override
	public NullElement analyze(final SemanticAnalyzer analyzer, final NullStruct input, final AnalysisBuilder analysisBuilder) {
		return NullElement.INSTANCE;
	}
}
