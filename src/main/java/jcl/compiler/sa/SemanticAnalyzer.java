/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.sa;

import java.io.Serializable;

import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ListStruct;

@FunctionalInterface
public interface SemanticAnalyzer extends Serializable {

	LambdaStruct analyze(final ListStruct form);
}
