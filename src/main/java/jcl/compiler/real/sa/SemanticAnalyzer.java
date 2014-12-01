/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;

@FunctionalInterface
public interface SemanticAnalyzer {

	LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder);
}
