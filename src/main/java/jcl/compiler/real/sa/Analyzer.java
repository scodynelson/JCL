package jcl.compiler.real.sa;

import java.io.Serializable;

import jcl.LispStruct;

@FunctionalInterface
public interface Analyzer<O extends LispStruct, I extends LispStruct> extends Serializable {

	O analyze(I input, AnalysisBuilder analysisBuilder);
}
