package jcl.compiler.sa;

import jcl.compiler.environment.Environment;
import jcl.lang.LispStruct;

@FunctionalInterface
public interface Analyzer<O extends LispStruct, I extends LispStruct> {

	O analyze(I input, Environment environment);
}
