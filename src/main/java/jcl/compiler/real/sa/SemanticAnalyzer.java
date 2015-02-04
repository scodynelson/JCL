/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.Element;

import java.io.Serializable;

public interface SemanticAnalyzer extends Serializable {

	Element analyzeForm(final LispStruct form);

	Element analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder);
}
