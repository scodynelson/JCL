/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

abstract class SpecialOperatorCodeGenerator<T extends CompilerSpecialOperatorStruct> implements CodeGenerator<T> {

	protected static final String SPECIAL_OPERATOR_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	protected final String methodNamePrefix;

	protected SpecialOperatorCodeGenerator(final String methodNamePrefix) {
		this.methodNamePrefix = methodNamePrefix;
	}
}
