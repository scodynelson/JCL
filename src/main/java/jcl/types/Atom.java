package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.NotTypeSpecifier;

import java.lang.String;

/**
 * An {@link Atom} is a type equivalent to (not cons).
 * <p>
 * {@link Atom} -> {@link T}
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
		 * Inner {@link Atom} type implementation.
		 */
		private static class AtomImpl extends NotTypeSpecifier implements Atom, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private AtomImpl() {
				super("ATOM", Cons.INSTANCE);
			}

			@Override
			public String toString() {
				return "AtomImpl{}";
			}
		}
	}
}
