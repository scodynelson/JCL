/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.type.TypeBaseClass;
import jcl.type.TypeFactory;
import jcl.type.typespecifier.AtomicTypeSpecifier;

@SuppressWarnings("all")
public interface BarStructureType extends FooStructureType {

	BarStructureType INSTANCE = new Factory.BarStructureTypeImpl();

	class Factory implements TypeFactory<BarStructureType> {

		@Override
		public BarStructureType getInstance() {
			return INSTANCE;
		}

		private static final class BarStructureTypeImpl extends TypeBaseClass implements BarStructureType, AtomicTypeSpecifier {

			private BarStructureTypeImpl() {
				super("BAR");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof BarStructureType);
			}
		}
	}
}
