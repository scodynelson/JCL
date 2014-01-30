package jcl.types.conses;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.compound.NotTypeSpecifier;

/**
 * An {@code Atom} is a type equivalent to (not cons).
 */
public interface Atom extends T {

	Atom INSTANCE = new Factory.AtomImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Atom> {

		@Override
		public Atom getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Atom} type implementation.
		 */
		private static class AtomImpl extends NotTypeSpecifier implements Atom, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private AtomImpl() {
				super(Cons.INSTANCE);
			}
		}
	}
}
