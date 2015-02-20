/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.FloatElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.numbers.FloatStruct;
import org.springframework.stereotype.Component;

@Component
public class FloatAnalyzer implements Analyzer<FloatElement, FloatStruct> {

	private static final long serialVersionUID = -4966578405275740432L;

	@Override
	public FloatElement analyze(final SemanticAnalyzer analyzer, final FloatStruct input, final AnalysisBuilder analysisBuilder) {
		return new FloatElement(input);
	}
}
