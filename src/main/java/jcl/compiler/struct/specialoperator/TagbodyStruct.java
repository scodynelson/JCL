/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Map;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.go.GoStruct;

public class TagbodyStruct extends CompilerSpecialOperatorStruct {

	private final Map<GoStruct<?>, PrognStruct> tagbodyForms;

	public TagbodyStruct(final Map<GoStruct<?>, PrognStruct> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<GoStruct<?>, PrognStruct> getTagbodyForms() {
		return tagbodyForms;
	}
}
