/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.StructureClassStruct;
import jcl.classes.StructureObjectStruct;
import jcl.functions.FunctionStruct;
import jcl.symbols.SymbolStruct;

public class FooStructureClass extends StructureClassStruct {

	public static final FooStructureClass INSTANCE
			= new FooStructureClass(new DefaultStructureConstructor(null));

	private static final long serialVersionUID = -7548921709400992640L;

	protected FooStructureClass(final FunctionStruct defaultConstructor) {
		this(defaultConstructor, null);
	}

	protected FooStructureClass(final FunctionStruct defaultConstructor, final FunctionStruct printer) {
		this(defaultConstructor, printer, null, null);
	}

	protected FooStructureClass(final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(FooStructureType.INSTANCE, defaultConstructor, printer, directSuperClasses, subClasses);
	}

	protected FooStructureClass(final LispType type, final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructor, printer, directSuperClasses, subClasses);
	}

	@Override
	public StructureObjectStruct newInstance(final Map<SymbolStruct<?>, LispStruct> slots) {
		return new StructureObjectStruct(INSTANCE, slots, printer);
	}
}
