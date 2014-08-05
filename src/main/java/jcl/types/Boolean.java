package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.OrTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The type {@link Boolean} contains the {@link Symbol}s {@link T} and {@link NIL}, which represent true and false,
 * respectively.
 * <p>
 * {@link Boolean} -> {@link Symbol} -> {@link T}
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
		 * Inner {@link Boolean} type implementation.
		 */
		private static class BooleanImpl extends OrTypeSpecifier implements Boolean, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BooleanImpl() {
				super("BOOLEAN", T.INSTANCE, NIL.INSTANCE);
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "BooleanImpl{}";
			}
		}
	}
}
