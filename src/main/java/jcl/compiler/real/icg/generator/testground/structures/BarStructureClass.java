/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.StructureObjectStruct;
import jcl.compiler.real.icg.generator.testground.TestGroundLambdaFunction;
import jcl.functions.FunctionStruct;

public class BarStructureClass extends FooStructureClass {

	public static final BarStructureClass INSTANCE
			= new BarStructureClass(new TestGroundLambdaFunction(null), null, null);

	private static final long serialVersionUID = -3178191979068838368L;

	protected BarStructureClass(final FunctionStruct defaultConstructor,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(BarStructureType.INSTANCE, defaultConstructor, directSuperClasses, subClasses);
	}

	protected BarStructureClass(final LispType type, final FunctionStruct defaultConstructor,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructor, directSuperClasses, subClasses);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new BarStructureObject();
	}
}
