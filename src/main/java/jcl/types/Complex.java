package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@code Complex} includes all mathematical complex numbers other than those included in the type {@code Rational}.
 * Complexes are expressed in Cartesian form with a real part and an imaginary part, each of which is a {@code Real}.
 * The real part and imaginary part are either both {@code Rational} or both of the same {@code Float} type. The imaginary
 * part can be a {@code Float} zero, but can never be a {@code Rational} zero, for such a number is always represented
 * by Common Lisp as a {@code Rational} rather than a {@code Complex}.
 * <p/>
 * {@code Complex} -> {@code Number} -> {@code T}
 */
public interface Complex extends Number {

	Complex INSTANCE = new Factory.ComplexImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Complex> {

		@Override
		public Complex getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Complex} type.
		 *
		 * @param realType the type of {@code Real} that comprises the {@code Complex}
		 * @return the newly created compound {@code Complex} type
		 */
		public static Complex getInstance(final Real realType) {
			return ComplexImpl.getInstance(realType);
		}

		/**
		 * Inner {@code Complex} type implementation.
		 */
		private static class ComplexImpl implements Complex, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final Real realType;

			/**
			 * Private constructor.
			 */
			private ComplexImpl() {
				realType = Real.INSTANCE;
			}

			/**
			 * Private constructor for compound {@code Complex} type.
			 *
			 * @param realType the type of {@code Real} that comprises the {@code Complex}
			 */
			private ComplexImpl(final Real realType) {
				this.realType = realType;
			}

			/**
			 * Gets instance of compound {@code Complex} type.
			 *
			 * @param realType the type of {@code Real} that comprises the {@code Complex}
			 * @return the newly created compound {@code Complex} type
			 */
			public static Complex getInstance(final Real realType) {
				return new ComplexImpl(realType);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Complex)) {
					return false;
				}

				final Complex complex = (Complex) obj;
				if (complex == INSTANCE) {
					return true;
				}

				if (complex instanceof ComplexImpl) {
					final ComplexImpl complexImpl = (ComplexImpl) complex;

					return ObjectUtils.equals(realType, complexImpl.realType);
				}

				return false;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(realType)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "ComplexImpl{"
						+ "realType=" + realType
						+ '}';
			}
		}
	}
}
