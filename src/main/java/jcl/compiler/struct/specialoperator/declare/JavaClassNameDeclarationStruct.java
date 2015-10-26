/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

public class JavaClassNameDeclarationStruct implements DeclarationStruct {

	private static final long serialVersionUID = 1282436684482407122L;

	private final String className;

	public JavaClassNameDeclarationStruct(final String className) {
		this.className = className;
	}

	public String getClassName() {
		return className;
	}
}
