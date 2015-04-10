/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.classes.StructureClassStruct;

public class FooStructure extends StructureClassStruct {

	private static final long serialVersionUID = 3160462871541685321L;

	protected FooStructure(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(FooStructureType.INSTANCE, directSuperClasses, subClasses);
	}
}
