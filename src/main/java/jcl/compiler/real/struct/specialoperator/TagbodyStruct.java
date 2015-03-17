/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;

public class TagbodyStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<GoStruct<?>, List<LispStruct>> tagbodyForms;

	public TagbodyStruct(final Map<GoStruct<?>, List<LispStruct>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<GoStruct<?>, List<LispStruct>> getTagbodyForms() {
		return tagbodyForms;
	}
}
