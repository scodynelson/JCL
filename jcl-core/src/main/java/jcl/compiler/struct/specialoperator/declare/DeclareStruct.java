/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class DeclareStruct extends CompilerSpecialOperatorStruct {

	private final List<SpecialDeclarationStruct> specialDeclarations = new ArrayList<>();

	private JavaClassNameDeclarationStruct javaClassNameDeclaration;

	private LispNameDeclarationStruct lispNameDeclarationStruct;

	public DeclareStruct() {
		super("declare");
	}

	public List<SpecialDeclarationStruct> getSpecialDeclarations() {
		return specialDeclarations;
	}

	public JavaClassNameDeclarationStruct getJavaClassNameDeclaration() {
		return javaClassNameDeclaration;
	}

	public void setJavaClassNameDeclaration(final JavaClassNameDeclarationStruct javaClassNameDeclaration) {
		this.javaClassNameDeclaration = javaClassNameDeclaration;
	}

	public LispNameDeclarationStruct getLispNameDeclarationStruct() {
		return lispNameDeclarationStruct;
	}

	public void setLispNameDeclarationStruct(final LispNameDeclarationStruct lispNameDeclarationStruct) {
		this.lispNameDeclarationStruct = lispNameDeclarationStruct;
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int environmentArgStore) {
		// Do Nothing.
	}
}
