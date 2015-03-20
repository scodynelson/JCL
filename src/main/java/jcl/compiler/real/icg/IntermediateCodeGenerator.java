/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import java.util.List;

import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;

public interface IntermediateCodeGenerator {

	List<ClassDef> funcall(LambdaStruct lambdaStruct);
}
