package jcl.compiler.sa;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;

@FunctionalInterface
public interface Analyzer<O extends LispStruct, I extends LispStruct> {

	O analyze(I input, Environment environment);
}
