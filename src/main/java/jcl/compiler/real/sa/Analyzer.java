package jcl.compiler.real.sa;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;

import java.io.Serializable;

public interface Analyzer<O extends Element, I extends SimpleElement> extends Serializable {

	O analyze(SemanticAnalyzer analyzer, I input, AnalysisBuilder analysisBuilder);
}
