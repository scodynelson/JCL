package jcl.types;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Cons} is a compound object having two components, called the car and cdr. These form a dotted pair. Each
 * component can be any object.
 * <p>
 * {@link Cons} -> {@link List} -> {@link Sequence} -> {@link T}
 */
public interface Cons extends List {

	Cons INSTANCE = new Factory.ConsImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Cons> {

		@Override
		public Cons getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@link Cons} type.
		 *
		 * @param carSpec
		 * 		the type of the car element
		 * @param cdrSpec
		 * 		the type of the cdr element
		 *
		 * @return the newly created compound {@link Cons} type
		 */
		public static Cons getInstance(final LispType carSpec, final LispType cdrSpec) {
			return ConsImpl.getInstance(carSpec, cdrSpec);
		}

		/**
		 * Inner {@link Cons} type implementation.
		 */
		private static class ConsImpl extends TypeBaseClass implements Cons, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final LispType carSpec;
			private final LispType cdrSpec;

			/**
			 * Private constructor.
			 */
			private ConsImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link Cons} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 */
			private ConsImpl(final LispType carSpec, final LispType cdrSpec) {
				super("CONS", GlobalPackageStruct.COMMON_LISP);
				this.carSpec = carSpec;
				this.cdrSpec = cdrSpec;
			}

			/**
			 * Gets instance of compound {@link Cons} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 *
			 * @return the newly created compound {@link Cons} type
			 */
			public static Cons getInstance(final LispType carSpec, final LispType cdrSpec) {
				return new ConsImpl(carSpec, cdrSpec);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Cons)) {
					return false;
				}

				final Cons cons = (Cons) obj;
				return (cons == INSTANCE) || ((cons instanceof ConsImpl) && checkConsImplEquality((ConsImpl) cons));
			}

			/**
			 * This method checks the equality of the provide consImpl object to this instance.
			 *
			 * @param consImpl
			 * 		the consImpl object to test for equality
			 *
			 * @return true if the consImpl object is equivalent to this instance; false otherwise
			 */
			private boolean checkConsImplEquality(final ConsImpl consImpl) {
				if (carSpec != null) {
					return carSpec.equals(consImpl.carSpec);
				}

				return (cdrSpec == null) || cdrSpec.equals(consImpl.cdrSpec);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(carSpec)
						.append(cdrSpec)
						.toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
