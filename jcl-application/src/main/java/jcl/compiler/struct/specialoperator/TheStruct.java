/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.type.typespecifier.TypeSpecifier;

public class TheStruct extends CompilerSpecialOperatorStruct {

	private final TypeSpecifier valueType;

	private final LispStruct form;

	public TheStruct(final TypeSpecifier valueType, final LispStruct form) {
		this.valueType = valueType;
		this.form = form;
	}

	public TypeSpecifier getValueType() {
		return valueType;
	}

	public LispStruct getForm() {
		return form;
	}
}
