/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public class IntegerAnalyzer implements Analyzer<IntegerElement, IntegerStruct> {

	private static final long serialVersionUID = -1524072073515869260L;

	@Override
	public IntegerElement analyze(final SemanticAnalyzer analyzer, final IntegerStruct input, final AnalysisBuilder analysisBuilder) {
		return new IntegerElement(input.getBigInteger());
	}
}
