/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * {@link SymbolType}s are used for their object identity to name various entities in Common Lisp, including (but not
 * limited to) linguistic entities such as variables and {@link FunctionType}s.
 * <p>
 * {@link SymbolType}s can be collected together into {@link PackageType}s. A {@link SymbolType} is said to be interned
 * in a {@link PackageType} if it is accessible in that {@link PackageType}; the same {@link SymbolType} can be
 * interned in more than one {@link PackageType}. If a {@link SymbolType} is not interned in any {@link PackageType},
 * it is called uninterned.
 * <p>
 * An interned {@link SymbolType} is uniquely identifiable by its name from any {@link PackageType} in which it is
 * accessible.
 * <p>
 * {@link SymbolType}s have the following attributes:
 * <p>
 * <b>Name</b>
 * The name of a {@link SymbolType} is a {@link String} used to identify the {@link SymbolType}.The name is used as
 * part of the external, printed representation of the {@link SymbolType}. A {@link SymbolType} may have any {@link
 * CharacterType} in its name.
 * <p>
 * <b>Package</b>
 * The object in this cell is called the home {@link PackageType} of the {@link SymbolType}. If the home {@link
 * PackageType} is {@link NILType}, the {@link SymbolType} is said to have no home {@link PackageType}.
 * <p>
 * When a {@link SymbolType} is first created, it has no home {@link PackageType}. When it is first interned, the
 * {@link PackageType} in which it is initially interned becomes its home {@link PackageType}.
 * <p>
 * If a {@link SymbolType} is uninterned from the {@link PackageType} which is its home {@link PackageType}, its home
 * {@link PackageType} is set to {@link NILType}. Depending on whether there is another {@link PackageType} in which
 * the {@link SymbolType} is interned, the {@link SymbolType} might or might not really be an uninterned {@link
 * SymbolType}.
 * A
 * {@link SymbolType} with no home {@link PackageType} is therefore called apparently uninterned.
 * <p>
 * <b>Property List</b>
 * The property {@link ListType} of a {@link SymbolType} provides a mechanism for associating named attributes with
 * that {@link SymbolType}. The operations for adding and removing entries are destructive to the property {@link
 * ListType}. The property {@link ListType} associated with a fresh {@link SymbolType} is initially {@link NullType}.
 * <p>
 * <b>Value</b>
 * If a {@link SymbolType} has a value attribute, it is said to be bound. The object contained in the value cell of a
 * bound {@link SymbolType} is the value of the global variable named by that {@link SymbolType}.
 * <p>
 * <b>Function</b>
 * If a {@link SymbolType} has a {@link FunctionType} attribute, it is said to be fbound. If the {@link SymbolType} is
 * the name of a {@link FunctionType} in the global environment, the function cell contains the {@link FunctionType}.
 * If the {@link SymbolType} is the name of either a macro in the global environment or a special operator, the {@link
 * SymbolType} is fbound.
 * <p>
 * Operations on a {@link SymbolType}'s value cell and function cell are sometimes described in terms of their effect
 * on the {@link SymbolType} itself.
 * <p>
 * {@link SymbolType}s are used as identifiers for lexical variables and lexical {@link FunctionType} definitions, but
 * in that role, only their object identity is significant.
 * <p>
 * {@link SymbolType} -&gt; {@link TType}
 */
public interface SymbolType extends TType {

	/**
	 * Singleton instance of the {@link SymbolType} type.
	 */
	SymbolType INSTANCE = new Factory.SymbolTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SymbolType> {

		@Override
		public SymbolType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SymbolType} type implementation.
		 */
		private static final class SymbolTypeImpl extends TypeBaseClass implements SymbolType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SymbolTypeImpl() {
				super("SYMBOL");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SymbolType);
			}
		}
	}
}
