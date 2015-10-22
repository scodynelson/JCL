/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.types.typespecifiers.TypeSpecifier;

public class TheStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -8054543185157500625L;

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
