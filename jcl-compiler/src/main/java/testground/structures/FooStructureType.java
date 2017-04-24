/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.type.StructureObjectType;
import jcl.type.TypeBaseClass;
import jcl.type.TypeFactory;
import jcl.type.typespecifier.AtomicTypeSpecifier;

@SuppressWarnings("all")
public interface FooStructureType extends StructureObjectType {

	FooStructureType INSTANCE = new Factory.FooStructureTypeImpl();

	class Factory implements TypeFactory<FooStructureType> {

		@Override
		public FooStructureType getInstance() {
			return INSTANCE;
		}

		private static final class FooStructureTypeImpl extends TypeBaseClass implements FooStructureType, AtomicTypeSpecifier {

			private FooStructureTypeImpl() {
				super("FOO");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof FooStructureType);
			}
		}
	}
}
