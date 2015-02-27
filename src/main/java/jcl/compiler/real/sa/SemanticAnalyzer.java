/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;

import java.io.Serializable;

public interface SemanticAnalyzer extends Serializable {

	Element analyzeForm(final SimpleElement form);

	Element analyzeForm(final SimpleElement form, final AnalysisBuilder analysisBuilder);
}
