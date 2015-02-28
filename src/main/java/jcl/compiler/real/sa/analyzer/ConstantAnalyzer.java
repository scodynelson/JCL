/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import org.springframework.stereotype.Component;

@Component
public class ConstantAnalyzer implements Analyzer<SimpleElement, SimpleElement> {

	private static final long serialVersionUID = 7792522745314462699L;

	@Override
	public SimpleElement analyze(final SimpleElement input, final AnalysisBuilder analysisBuilder) {
		return input;
	}
}
