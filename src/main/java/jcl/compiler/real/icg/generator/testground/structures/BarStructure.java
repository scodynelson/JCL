/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.classes.StructureClassStruct;

public class BarStructure extends StructureClassStruct {

	private static final long serialVersionUID = 1900114614023213039L;

	protected BarStructure(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(BarStructureType.INSTANCE, directSuperClasses, subClasses);
	}
}
