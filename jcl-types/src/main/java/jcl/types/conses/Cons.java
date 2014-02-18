package jcl.types.conses;

import jcl.types.LispType;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Cons} is a compound object having two components, called the car and cdr. These form a dotted pair. Each
 * component can be any object.
 * <p/>
 * {@code Cons} -> {@code List} -> {@code Sequence} -> {@code T}
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
		 * Gets instance of compound {@code Cons} type.
		 *
		 * @param carSpec the type of the car element
		 * @param cdrSpec the type of the cdr element
		 * @return the newly created compound {@code Cons} type
		 */
		public static Cons getInstance(final LispType carSpec, final LispType cdrSpec) {
			return new ConsImpl(carSpec, cdrSpec);
		}

		/**
		 * Inner {@code Cons} type implementation.
		 */
		private static class ConsImpl implements Cons, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final LispType carSpec;
			private final LispType cdrSpec;

			/**
			 * Private constructor.
			 */
			private ConsImpl() {
				carSpec = null;
				cdrSpec = null;
			}

			/**
			 * Private constructor for compound {@code Cons} type.
			 *
			 * @param carSpec the type of the car element
			 * @param cdrSpec the type of the cdr element
			 */
			private ConsImpl(final LispType carSpec, final LispType cdrSpec) {
				this.carSpec = carSpec;
				this.cdrSpec = cdrSpec;
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
				if (cons == INSTANCE) {
					return true;
				}

				if (cons instanceof ConsImpl) {
					final ConsImpl consImpl = (ConsImpl) cons;

					if (carSpec != null) {
						return carSpec.equals(consImpl.carSpec);
					}

					return (cdrSpec == null) || cdrSpec.equals(consImpl.cdrSpec);
				}

				return false;
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
				return "ConsImpl{" +
						"carSpec=" + carSpec +
						", cdrSpec=" + cdrSpec +
						'}';
			}
		}
	}
}
