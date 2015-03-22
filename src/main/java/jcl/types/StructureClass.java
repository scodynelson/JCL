/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link StructureClass} is the default structure type {@link Class}.
 * <p>
 * {@link StructureClass} -&gt; {@link Class} -&gt; {@link StandardObject} -&gt; {@link T}
 */
public interface StructureClass extends Class {

	/**
	 * Singleton instance of the {@link StructureClass} type.
	 */
	StructureClass INSTANCE = new Factory.StructureClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureClass> {

		@Override
		public StructureClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StructureClass} type implementation.
		 */
		private static final class StructureClassImpl extends TypeBaseClass implements StructureClass, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -199682619819533157L;

			/**
			 * Private constructor.
			 */
			private StructureClassImpl() {
				super("STRUCTURE-CLASS");
			}
		}
	}
}
