/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DeclareStruct extends CompilerSpecialOperatorStruct {

	private final List<SpecialDeclarationStruct> specialDeclarations = new ArrayList<>();

	private JavaClassNameDeclarationStruct javaClassNameDeclaration;

	private LispNameDeclarationStruct lispNameDeclarationStruct;

	public DeclareStruct() {
		super("declare");
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing.
	}
}
