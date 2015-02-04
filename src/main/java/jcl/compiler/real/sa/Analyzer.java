package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.Element;

import java.io.Serializable;

public interface Analyzer<O extends Element, I extends LispStruct> extends Serializable {

	O analyze(SemanticAnalyzer analyzer, I input, AnalysisBuilder analysisBuilder);
}
