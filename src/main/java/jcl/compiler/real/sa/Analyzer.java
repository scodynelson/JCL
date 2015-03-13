package jcl.compiler.real.sa;

import java.io.Serializable;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;

@FunctionalInterface
public interface Analyzer<O extends LispStruct, I extends LispStruct> extends Serializable {

	O analyze(I input, Environment environment);
}
