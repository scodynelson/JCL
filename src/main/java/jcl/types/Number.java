/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Number} contains objects which represent mathematical numbers. The types {@link Real} and {@link Complex}
 * are disjoint subtypes of {@link Number}.
 * <p>
 * Notes:
 * Common Lisp differs from mathematics on some naming issues. In mathematics, the set of real numbers is traditionally
 * described as a subset of the complex numbers, but in Common Lisp, the type {@link Real} and the type {@link Complex}
 * are disjoint. The Common Lisp type which includes all mathematical complex numbers is called number. The reasons for
 * these differences include historical precedent, compatibility with most other popular computer languages, and
 * various issues of time and space efficiency.
 * <p>
 * {@link Number} -&gt; {@link T}
 */
public interface Number extends T {

	/**
	 * Singleton instance of the {@link Number} type.
	 */
	Number INSTANCE = new Factory.NumberImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Number> {

		@Override
		public Number getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Number} type implementation.
		 */
		private static final class NumberImpl extends TypeBaseClass implements Number, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5294062331876830385L;

			/**
			 * Private constructor.
			 */
			private NumberImpl() {
				super("NUMBER");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Number);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
