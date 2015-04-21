/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.java.JavaNameStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(methodName)
		                            .append(javaObject)
		                            .append(arguments)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final JavaMethodCallStruct rhs = (JavaMethodCallStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(methodName, rhs.methodName)
		                          .append(javaObject, rhs.javaObject)
		                          .append(arguments, rhs.arguments)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(methodName)
		                                                                .append(javaObject)
		                                                                .append(arguments)
		                                                                .toString();
	}
}
