/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link KeywordType} includes all {@link SymbolType}s interned the KEYWORD {@link PackageType}.
 * <p>
 * Interning a {@link SymbolType} in the KEYWORD {@link PackageType} has three automatic effects:
 * 1. It causes the {@link SymbolType} to become bound to itself.
 * 2. It causes the {@link SymbolType} to become an external symbol of the KEYWORD package.
 * 3. It causes the {@link SymbolType} to become a constant variable.
 * <p>
 * {@link KeywordType} -&gt; {@link SymbolType} -&gt; {@link TType}
 */
public interface KeywordType extends SymbolType {

	/**
	 * Singleton instance of the {@link KeywordType} type.
	 */
	KeywordType INSTANCE = new Factory.KeywordTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<KeywordType> {

		@Override
		public KeywordType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link KeywordType} type implementation.
		 */
		private static final class KeywordTypeImpl extends TypeBaseClass implements KeywordType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5121311310780522485L;

			/**
			 * Private constructor.
			 */
			private KeywordTypeImpl() {
				super("KEYWORD");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof KeywordType);
			}
		}
	}
}
