/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Ratio} is a {@link Number} representing the mathematical ratio of two non-zero integers, the numerator and
 * denominator, whose greatest common divisor is one, and of which the denominator is positive and greater than one.
 * <p>
 * {@link Ratio} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Ratio extends Rational {

	/**
	 * Singleton instance of the {@link Ratio} type.
	 */
	Ratio INSTANCE = new Factory.RatioImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Ratio> {

		@Override
		public Ratio getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Ratio} type implementation.
		 */
		private static final class RatioImpl extends TypeBaseClass implements Ratio, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -7412019342239495711L;

			/**
			 * Private constructor.
			 */
			private RatioImpl() {
				super("RATIO");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Ratio);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
