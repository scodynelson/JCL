package jcl.compiler.real.sa;

import jcl.LispStruct;

import java.io.Serializable;

public interface Analyzer<O extends LispStruct, I extends LispStruct> extends Serializable {

	O analyze(SemanticAnalyzer analyzer, I input, AnalysisBuilder analysisBuilder);
}
