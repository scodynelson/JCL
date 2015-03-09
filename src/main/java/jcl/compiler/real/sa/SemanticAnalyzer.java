/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import java.io.Serializable;

import jcl.LispStruct;

public interface SemanticAnalyzer extends Serializable {

	LispStruct analyzeForm(final LispStruct form);

	LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder);
}
