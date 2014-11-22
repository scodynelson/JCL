package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * {@link Symbol}s are used for their object identity to name various entities in Common Lisp, including (but not
 * limited to) linguistic entities such as variables and {@link Function}s.
 * <p>
 * {@link Symbol}s can be collected together into {@link Package}s. A {@link Symbol} is said to be interned in a
 * {@link Package} if it is accessible in that {@link Package}; the same {@link Symbol} can be interned in more than
 * one {@link Package}. If a {@link Symbol} is not interned in any {@link Package}, it is called uninterned.
 * <p>
 * An interned {@link Symbol} is uniquely identifiable by its name from any {@link Package} in which it is accessible.
 * <p>
 * {@link Symbol}s have the following attributes:
 * <p>
 * <b>Name</b>
 * <tab>
 * The name of a {@link Symbol} is a {@link String} used to identify the {@link Symbol}.The name is used as part
 * of the external, printed representation of the {@link Symbol}. A {@link Symbol} may have any {@link Character} in
 * its name.
 * </tab>
 * <p>
 * <b>Package</b>
 * <tab>
 * The object in this cell is called the home {@link Package} of the {@link Symbol}. If the home {@link Package} is
 * {@link NIL}, the {@link Symbol} is said to have no home {@link Package}.
 * </tab>
 * <p>
 * <tab>
 * When a {@link Symbol} is first created, it has no home {@link Package}. When it is first interned, the
 * {@link Package} in which it is initially interned becomes its home {@link Package}.
 * </tab>
 * <p>
 * <tab>
 * If a {@link Symbol} is uninterned from the {@link Package} which is its home {@link Package}, its home
 * {@link Package} is set to {@link NIL}. Depending on whether there is another {@link Package} in which the
 * {@link Symbol} is interned, the {@link Symbol} might or might not really be an uninterned {@link Symbol}. A
 * {@link Symbol} with no home {@link Package} is therefore called apparently uninterned.
 * </tab>
 * <p>
 * <b>Property List</b>
 * <tab>
 * The property {@link List} of a {@link Symbol} provides a mechanism for associating named attributes with that
 * {@link Symbol}. The operations for adding and removing entries are destructive to the property {@link List}. The
 * property {@link List} associated with a fresh {@link Symbol} is initially {@link Null}.
 * </tab>
 * <p>
 * <b>Value</b>
 * <tab>
 * If a {@link Symbol} has a value attribute, it is said to be bound. The object contained in the value cell of a bound
 * {@link Symbol} is the value of the global variable named by that {@link Symbol}.
 * </tab>
 * <p>
 * <b>Function</b>
 * <tab>
 * If a {@link Symbol} has a {@link Function} attribute, it is said to be fbound. If the {@link Symbol} is the name of
 * a {@link Function} in the global environment, the function cell contains the {@link Function}. If the {@link Symbol}
 * is the name of either a macro in the global environment or a special operator, the {@link Symbol} is fbound.
 * </tab>
 * <p>
 * Operations on a {@link Symbol}'s value cell and function cell are sometimes described in terms of their effect on
 * the {@link Symbol} itself.
 * <p>
 * {@link Symbol}s are used as identifiers for lexical variables and lexical {@link Function} definitions, but in that
 * role, only their object identity is significant.
 * <p>
 * {@link Symbol} -> {@link T}
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
		 * Inner {@link Symbol} type implementation.
		 */
		private static final class SymbolImpl extends TypeBaseClass implements Symbol, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SymbolImpl() {
				super("SYMBOL");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Symbol);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
