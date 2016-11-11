/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link StructureObjectType} is an instance of {@link StructureClassType} and is a superclass of every {@link
 * ClassType} that is an instance of {@link StructureClassType} except itself.
 * <p>
 * {@link StructureObjectType} -&gt; {@link TType}
 */
public interface StructureObjectType extends TType {

	/**
	 * Singleton instance of the {@link StructureObjectType} type.
	 */
	StructureObjectType INSTANCE = new Factory.StructureObjectTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureObjectType> {

		@Override
		public StructureObjectType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StructureObjectType} type implementation.
		 */
		private static final class StructureObjectTypeImpl extends TypeBaseClass implements StructureObjectType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StructureObjectTypeImpl() {
				super("STRUCTURE-OBJECT");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StructureObjectType);
			}
		}
	}
}
