/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.StructureObjectStruct;
import jcl.functions.FunctionStruct;
import jcl.symbols.SymbolStruct;

public class BarStructureClass extends FooStructureClass {

	public static final BarStructureClass INSTANCE
			= new BarStructureClass(new DefaultStructureConstructor(null));

	private static final long serialVersionUID = -3178191979068838368L;

	protected BarStructureClass(final FunctionStruct defaultConstructor) {
		this(defaultConstructor, null);
	}

	protected BarStructureClass(final FunctionStruct defaultConstructor, final FunctionStruct printer) {
		this(defaultConstructor, printer, null, null);
	}

	protected BarStructureClass(final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(BarStructureType.INSTANCE, defaultConstructor, printer, directSuperClasses, subClasses);
	}

	protected BarStructureClass(final LispType type, final FunctionStruct defaultConstructor, final FunctionStruct printer,
	                            final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, defaultConstructor, printer, directSuperClasses, subClasses);
	}

	@Override
	public StructureObjectStruct newInstance(final Map<SymbolStruct<?>, LispStruct> slots) {
		return new StructureObjectStruct(INSTANCE, slots, printer);
	}
}
