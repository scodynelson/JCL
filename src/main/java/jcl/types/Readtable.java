/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link Readtable} maps characters into syntax types for the Lisp reader. A {@link Readtable} also contains
 * associations between macro characters and their reader macro functions, and records information about the case
 * conversion rules to be used by the Lisp reader when parsing {@link Symbol}s.
 * <p>
 * Each simple {@link Character} must be representable in the {@link Readtable}.
 * <p>
 * {@link Readtable} -&gt; {@link T}
 */
public interface Readtable extends T {

	/**
	 * Singleton instance of the {@link Readtable} type.
	 */
	Readtable INSTANCE = new Factory.ReadtableImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Readtable> {

		@Override
		public Readtable getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Readtable} type implementation.
		 */
		private static final class ReadtableImpl extends TypeBaseClass implements Readtable, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -9157541006854981003L;

			/**
			 * Private constructor.
			 */
			private ReadtableImpl() {
				super("READTABLE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Readtable);
			}
		}
	}
}
