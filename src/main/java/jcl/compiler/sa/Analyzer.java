package jcl.compiler.sa;

import java.io.Serializable;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;

@FunctionalInterface
public interface Analyzer<O extends LispStruct, I extends LispStruct> extends Serializable {

	O analyze(I input, Environment environment);
}
