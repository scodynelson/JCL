/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link StructureClassType} is the default structure type {@link ClassType}.
 * <p>
 * {@link StructureClassType} -&gt; {@link ClassType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface StructureClassType extends ClassType {

	/**
	 * Singleton instance of the {@link StructureClassType} type.
	 */
	StructureClassType INSTANCE = new Factory.StructureClassTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureClassType> {

		@Override
		public StructureClassType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StructureClassType} type implementation.
		 */
		private static final class StructureClassTypeImpl extends TypeBaseClass implements StructureClassType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StructureClassTypeImpl() {
				super("STRUCTURE-CLASS");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof StructureClassType);
			}
		}
	}
}
