/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.StructureClassStruct;
import jcl.classes.StructureObjectStruct;
import jcl.compiler.real.icg.generator.testground.TestGroundLambdaFunction;
import jcl.functions.FunctionStruct;

public class FooStructureClass extends StructureClassStruct {

	public static final FooStructureClass INSTANCE
			= new FooStructureClass(new TestGroundLambdaFunction(null), null, null);

	private static final long serialVersionUID = -7548921709400992640L;

	protected FooStructureClass(final FunctionStruct defaultConstructor,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(FooStructureType.INSTANCE, defaultConstructor, directSuperClasses, subClasses);
	}

	protected FooStructureClass(final LispType type, final FunctionStruct defaultConstructor,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructor, directSuperClasses, subClasses);
	}

	@Override
	public StructureObjectStruct newInstance() {
		return new FooStructureObject();
	}
}
