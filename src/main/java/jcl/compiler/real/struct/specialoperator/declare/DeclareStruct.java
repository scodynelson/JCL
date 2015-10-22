/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class DeclareStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -7730761501615283012L;

	private final List<SpecialDeclarationStruct> specialDeclarations = new ArrayList<>();

	private JavaClassNameDeclarationStruct javaClassNameDeclaration;

	public List<SpecialDeclarationStruct> getSpecialDeclarations() {
		return specialDeclarations;
	}

	public JavaClassNameDeclarationStruct getJavaClassNameDeclaration() {
		return javaClassNameDeclaration;
	}

	public void setJavaClassNameDeclaration(final JavaClassNameDeclarationStruct javaClassNameDeclaration) {
		this.javaClassNameDeclaration = javaClassNameDeclaration;
	}
}
