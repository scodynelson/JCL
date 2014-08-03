package jcl.compiler.real.sa;

import jcl.LispStruct;

public interface Analyzer<O extends LispStruct, I extends LispStruct> {

	O analyze(I input);
}