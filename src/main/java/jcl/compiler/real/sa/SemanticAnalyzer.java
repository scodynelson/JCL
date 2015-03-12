/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import java.io.Serializable;

import jcl.LispStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;

@FunctionalInterface
public interface SemanticAnalyzer extends Serializable {

	LambdaStruct analyze(final LispStruct form);
}
