/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.RatioElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.numbers.RatioStruct;
import org.springframework.stereotype.Component;

@Component
public class RatioAnalyzer implements Analyzer<RatioElement, RatioStruct> {

	private static final long serialVersionUID = -5754068040594110512L;

	@Override
	public RatioElement analyze(final SemanticAnalyzer analyzer, final RatioStruct input, final AnalysisBuilder analysisBuilder) {
		return new RatioElement(input);
	}
}
