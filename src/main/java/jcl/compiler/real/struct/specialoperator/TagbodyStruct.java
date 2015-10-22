/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.Map;

import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;

public class TagbodyStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<GoStruct<?>, PrognStruct> tagbodyForms;

	public TagbodyStruct(final Map<GoStruct<?>, PrognStruct> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<GoStruct<?>, PrognStruct> getTagbodyForms() {
		return tagbodyForms;
	}
}
