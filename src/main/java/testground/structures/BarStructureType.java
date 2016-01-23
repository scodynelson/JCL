/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground.structures;

import jcl.types.TypeBaseClass;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

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
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BarStructureType);
			}
		}
	}
}
