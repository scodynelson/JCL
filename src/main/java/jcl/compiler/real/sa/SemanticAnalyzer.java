/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;

import java.io.Serializable;

public interface SemanticAnalyzer extends Serializable {

	LispStruct analyzeForm(final LispStruct form);

	LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder);
}
