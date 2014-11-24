package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.util.Objects;

/**
 * A {@link Complex} includes all mathematical complex numbers other than those included in the type {@link Rational}.
 * Complexes are expressed in Cartesian form with a real part and an imaginary part, each of which is a {@link Real}.
 * The real part and imaginary part are either both {@link Rational} or both of the same {@link Float} type. The
 * imaginary part can be a {@link Float} zero, but can never be a {@link Rational} zero, for such a number is always
 * represented by Common Lisp as a {@link Rational} rather than a {@link Complex}.
 * <p>
 * {@link Complex} -> {@link Number} -> {@link T}
 */
public interface Complex extends Number {

	Complex INSTANCE = new Factory.ComplexImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Complex> {

		/**
		 * Gets instance of compound {@link Complex} type.
		 *
		 * @param realType
		 * 		the type of {@link Real} that comprises the {@link Complex}
		 *
		 * @return the newly created compound {@link Complex} type
		 */
		public static Complex getInstance(final Real realType) {
			return ComplexImpl.getInstance(realType);
		}

		@Override
		public Complex getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Complex} type implementation.
		 */
		private static final class ComplexImpl extends TypeBaseClass implements Complex, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final Real realType;

			/**
			 * Private constructor.
			 */
			private ComplexImpl() {
				this(Real.INSTANCE);
			}

			/**
			 * Private constructor for compound {@link Complex} type.
			 *
			 * @param realType
			 * 		the type of {@link Real} that comprises the {@link Complex}
			 */
			private ComplexImpl(final Real realType) {
				super("COMPLEX");
				this.realType = realType;
			}

			/**
			 * Gets instance of compound {@link Complex} type.
			 *
			 * @param realType
			 * 		the type of {@link Real} that comprises the {@link Complex}
			 *
			 * @return the newly created compound {@link Complex} type
			 */
			public static Complex getInstance(final Real realType) {
				return new ComplexImpl(realType);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
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

					return Objects.equals(realType, complexImpl.realType);
				}

				return false;
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
