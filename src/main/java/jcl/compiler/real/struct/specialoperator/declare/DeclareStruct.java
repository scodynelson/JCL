/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.declare;

import java.util.ArrayList;
import java.util.List;

import jcl.compiler.real.struct.SpecialOperatorStruct;

public class DeclareStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -7730761501615283012L;

	private final List<SpecialDeclarationStruct> specialDeclarationElements = new ArrayList<>();

	public List<SpecialDeclarationStruct> getSpecialDeclarationElements() {
		return specialDeclarationElements;
	}
}
