/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

public class JavaClassNameDeclarationStruct implements DeclarationStruct {

	private final String className;

	public JavaClassNameDeclarationStruct(final String className) {
		this.className = className;
	}

	public String getClassName() {
		return className;
	}
}
