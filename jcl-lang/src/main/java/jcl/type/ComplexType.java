/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.Objects;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link ComplexType} includes all mathematical complex numbers other than those included in the type {@link
 * RationalType}. Complexes are expressed in Cartesian form with a real part and an imaginary part, each of which is a
 * {@link RealType}. The real part and imaginary part are either both {@link RationalType} or both of the same {@link
 * FloatType} type. The imaginary part can be a {@link FloatType} zero, but can never be a {@link RationalType} zero,
 * for such a number is always represented by Common Lisp as a {@link RationalType} rather than a {@link ComplexType}.
 * <p>
 * {@link ComplexType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface ComplexType extends NumberType {

	/**
	 * Singleton instance of the {@link ComplexType} type.
	 */
	ComplexType INSTANCE = new Factory.ComplexTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ComplexType> {

		/**
		 * Gets instance of compound {@link ComplexType} type.
		 *
		 * @param realType
		 * 		the type of {@link RealType} that comprises the {@link ComplexType}
		 *
		 * @return the newly created compound {@link ComplexType} type
		 */
		public static ComplexType getInstance(final RealType realType) {
			return ComplexTypeImpl.getInstance(realType);
		}

		@Override
		public ComplexType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ComplexType} type implementation.
		 */
		private static final class ComplexTypeImpl extends TypeBaseClass implements ComplexType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The type of {@link RealType} that comprises the {@link ComplexType}.
			 */
			private final RealType realType;

			/**
			 * Private constructor.
			 */
			private ComplexTypeImpl() {
				this(RealType.INSTANCE);
			}

			/**
			 * Private constructor for compound {@link ComplexType} type.
			 *
			 * @param realType
			 * 		the type of {@link RealType} that comprises the {@link ComplexType}
			 */
			private ComplexTypeImpl(final RealType realType) {
				super("COMPLEX");
				this.realType = realType;
			}

			/**
			 * Gets instance of compound {@link ComplexType} type.
			 *
			 * @param realType
			 * 		the type of {@link RealType} that comprises the {@link ComplexType}
			 *
			 * @return the newly created compound {@link ComplexType} type
			 */
			public static ComplexType getInstance(final RealType realType) {
				return new ComplexTypeImpl(realType);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(realType)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof ComplexType)) {
					return false;
				}

				final ComplexType complexType = (ComplexType) obj;
				if (complexType == INSTANCE) {
					return true;
				}

				if (complexType instanceof ComplexTypeImpl) {
					final ComplexTypeImpl complexTypeImpl = (ComplexTypeImpl) complexType;

					return Objects.equals(realType, complexTypeImpl.realType);
				}

				return false;
			}

			@Override
			public String toString() {
				return '(' + getName() + ' ' + ((realType == null) ? '*' : realType) + ')';
			}
		}
	}
}
