/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.OrTypeSpecifier;

/**
 * The type {@link BooleanType} contains the {@link SymbolType}s {@link TType} and {@link NILType}, which represent
 * true and false, respectively.
 * <p>
 * {@link BooleanType} -&gt; {@link SymbolType} -&gt; {@link TType}
 */
public interface BooleanType extends SymbolType {

	/**
	 * Singleton instance of the {@link BooleanType} type.
	 */
	BooleanType INSTANCE = new Factory.BooleanTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BooleanType> {

		@Override
		public BooleanType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BooleanType} type implementation.
		 */
		private static final class BooleanTypeImpl extends OrTypeSpecifier implements BooleanType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BooleanTypeImpl() {
				super("BOOLEAN", TType.INSTANCE, NILType.INSTANCE);
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof BooleanType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}
