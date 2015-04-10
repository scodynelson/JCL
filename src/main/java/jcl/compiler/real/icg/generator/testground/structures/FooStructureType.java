/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground.structures;

import jcl.types.StructureObjectType;
import jcl.types.TypeBaseClass;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public interface FooStructureType extends StructureObjectType {

	FooStructureType INSTANCE = new Factory.FooStructureTypeImpl();

	class Factory implements TypeFactory<FooStructureType> {

		@Override
		public FooStructureType getInstance() {
			return INSTANCE;
		}

		private static final class FooStructureTypeImpl extends TypeBaseClass implements FooStructureType, AtomicTypeSpecifier {

			private static final long serialVersionUID = -938155620899376896L;

			private FooStructureTypeImpl() {
				super("FOO");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof FooStructureType);
			}
		}
	}
}
