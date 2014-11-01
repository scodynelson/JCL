package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Ratio} is a {@link Number} representing the mathematical ratio of two non-zero integers, the numerator and
 * denominator, whose greatest common divisor is one, and of which the denominator is positive and greater than one.
 * <p>
 * {@link Ratio} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Ratio extends Rational {

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
		private static class RatioImpl extends TypeBaseClass implements Ratio, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private RatioImpl() {
				super("RATIO", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Ratio);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
