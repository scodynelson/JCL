package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.OrTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code Boolean} contains the {@code Symbol}s {@code T} and {@code NIL}, which represent true and false,
 * respectively.
 * <p/>
 * {@code Boolean} -> {@code Symbol} -> {@code T}
 */
public interface Boolean extends Symbol {

	Boolean INSTANCE = new Factory.BooleanImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Boolean> {

		@Override
		public Boolean getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Boolean} type implementation.
		 */
		private static class BooleanImpl extends OrTypeSpecifier implements Boolean, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BooleanImpl() {
				super(T.INSTANCE, NIL.INSTANCE);
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
