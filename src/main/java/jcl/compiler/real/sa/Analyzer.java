package jcl.compiler.real.sa;

import jcl.compiler.real.element.Element;

import java.io.Serializable;

@FunctionalInterface
public interface Analyzer<O extends Element, I extends Element> extends Serializable {

	O analyze(I input, AnalysisBuilder analysisBuilder);
}
