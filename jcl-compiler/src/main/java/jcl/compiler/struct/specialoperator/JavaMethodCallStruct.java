/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.java.JavaNameStruct;

public class JavaMethodCallStruct extends CompilerSpecialOperatorStruct {

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

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(JAVA-METHOD-CALL ");

		builder.append(methodName);
		builder.append(' ');
		builder.append(javaObject);

		final String argumentsString
				= arguments.stream()
				           .map(Object::toString)
				           .collect(Collectors.joining(" "));
		builder.append(argumentsString);
		builder.append(')');

		return builder.toString();
	}
}
