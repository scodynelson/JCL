/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link NumberType} contains objects which represent mathematical numbers. The types {@link RealType} and {@link
 * ComplexType} are disjoint subtypes of {@link NumberType}.
 * <p>
 * Notes:
 * Common Lisp differs from mathematics on some naming issues. In mathematics, the set of real numbers is traditionally
 * described as a subset of the complex numbers, but in Common Lisp, the type {@link RealType} and the type {@link
 * ComplexType} are disjoint. The Common Lisp type which includes all mathematical complex numbers is called number.
 * The reasons for these differences include historical precedent, compatibility with most other popular computer
 * languages, and various issues of time and space efficiency.
 * <p>
 * {@link NumberType} -&gt; {@link TType}
 */
public interface NumberType extends TType {

	/**
	 * Singleton instance of the {@link NumberType} type.
	 */
	NumberType INSTANCE = new Factory.NumberTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<NumberType> {

		@Override
		public NumberType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link NumberType} type implementation.
		 */
		private static final class NumberTypeImpl extends TypeBaseClass implements NumberType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private NumberTypeImpl() {
				super("NUMBER");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NumberType);
			}
		}
	}
}
