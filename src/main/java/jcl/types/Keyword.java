/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The type {@link Keyword} includes all {@link Symbol}s interned the KEYWORD {@link Package}.
 * <p>
 * Interning a {@link Symbol} in the KEYWORD {@link Package} has three automatic effects:
 * 1. It causes the {@link Symbol} to become bound to itself.
 * 2. It causes the {@link Symbol} to become an external symbol of the KEYWORD package.
 * 3. It causes the {@link Symbol} to become a constant variable.
 * <p>
 * {@link Keyword} -&gt; {@link Symbol} -&gt; {@link T}
 */
public interface Keyword extends Symbol {

	/**
	 * Singleton instance of the {@link Keyword} type.
	 */
	Keyword INSTANCE = new Factory.KeywordImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Keyword> {

		@Override
		public Keyword getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Keyword} type implementation.
		 */
		private static final class KeywordImpl extends TypeBaseClass implements Keyword, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5121311310780522485L;

			/**
			 * Private constructor.
			 */
			private KeywordImpl() {
				super("KEYWORD");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Keyword);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
