package jcl.types.symbols;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * {@code Symbol}s are used for their object identity to name various entities in Common Lisp, including (but not limited to)
 * linguistic entities such as variables and {@code Function}s.
 * <p/>
 * {@code Symbol}s can be collected together into {@code Package}s. A {@code Symbol} is said to be interned in a
 * {@code Package} if it is accessible in that {@code Package}; the same {@code Symbol} can be interned in more than
 * one {@code Package}. If a {@code Symbol} is not interned in any {@code Package}, it is called uninterned.
 * <p/>
 * An interned {@code Symbol} is uniquely identifiable by its name from any {@code Package} in which it is accessible.
 * <p/>
 * {@code Symbol}s have the following attributes:
 * <p/>
 * <b>Name</b>
 * <tab>The name of a {@code Symbol} is a {@code String} used to identify the {@code Symbol}.The name is used as part of
 * <tab>the external, printed representation of the {@code Symbol}. A {@code Symbol} may have any {@code Character} in its name.
 * <p/>
 * <b>Package</b>
 * <tab>The object in this cell is called the home {@code Package} of the {@code Symbol}. If the home {@code Package} is
 * <tab>{@code NIL}, the {@code Symbol} is said to have no home {@code Package}.
 * <p/>
 * <tab>When a {@code Symbol} is first created, it has no home {@code Package}. When it is first interned, the {@code Package}
 * <tab>in which it is initially interned becomes its home {@code Package}.
 * <p/>
 * <tab>If a {@code Symbol} is uninterned from the {@code Package} which is its home {@code Package}, its home {@code Package}
 * <tab>is set to {@code NIL}. Depending on whether there is another {@code Package} in which the {@code Symbol} is interned,
 * <tab>the {@code Symbol} might or might not really be an uninterned {@code Symbol}. A {@code Symbol} with no home {@code Package}
 * <tab>is therefore called apparently uninterned.
 * <p/>
 * <b>Property List</b>
 * <tab>The property {@code List} of a {@code Symbol} provides a mechanism for associating named attributes with that
 * <tab>{@code Symbol}. The operations for adding and removing entries are destructive to the property {@code List}. The
 * <tab>property {@code List} associated with a fresh {@code Symbol} is initially {@code Null}.
 * <p/>
 * <b>Value</b>
 * <tab>If a {@code Symbol} has a value attribute, it is said to be bound. The object contained in the value cell of a
 * <tab>bound {@code Symbol} is the value of the global variable named by that {@code Symbol}.
 * <p/>
 * <b>Function</b>
 * <tab>If a {@code Symbol} has a {@code Function} attribute, it is said to be fbound. If the {@code Symbol} is the name
 * <tab>of a {@code Function} in the global environment, the function cell contains the {@code Function}. If the {@code Symbol}
 * <tab>is the name of either a macro in the global environment or a special operator, the {@code Symbol} is fbound.
 * <p/>
 * Operations on a {@code Symbol}'s value cell and function cell are sometimes described in terms of their effect on the
 * {@code Symbol} itself.
 * <p/>
 * {@code Symbol}s are used as identifiers for lexical variables and lexical {@code Function} definitions, but in that role,
 * only their object identity is significant.
 * <p/>
 * {@code Symbol} -> {@code T}
 */
public interface Symbol extends T {

	Symbol INSTANCE = new Factory.SymbolImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Symbol> {

		@Override
		public Symbol getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Symbol} type implementation.
		 */
		private static class SymbolImpl implements Symbol, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Symbol);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
