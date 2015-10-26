/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.java.JavaNameStruct;

public class JavaMethodCallStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 1475342953851995228L;

	private final JavaNameStruct methodName;

	private final LispStruct javaObject;

	private final List<LispStruct> arguments;

	public JavaMethodCallStruct(final JavaNameStruct methodName, final LispStruct javaObject,
	                            final List<LispStruct> arguments) {
		this.methodName = methodName;
		this.javaObject = javaObject;
		this.arguments = arguments;
	}

	public JavaNameStruct getMethodName() {
		return methodName;
	}

	public LispStruct getJavaObject() {
		return javaObject;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}
}
