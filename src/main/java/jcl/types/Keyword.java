package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link Keyword} includes all {@link Symbol}s interned the KEYWORD {@link Package}.
 * <p>
 * Interning a {@link Symbol} in the KEYWORD {@link Package} has three automatic effects:
 * 1. It causes the {@link Symbol} to become bound to itself.
 * 2. It causes the {@link Symbol} to become an external symbol of the KEYWORD package.
 * 3. It causes the {@link Symbol} to become a constant variable.
 * <p>
 * {@link Keyword} -> {@link Symbol} -> {@link T}
 */
public interface Keyword extends Symbol {

	Keyword INSTANCE = new Factory.KeywordImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Keyword> {

		@Override
		public Keyword getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Keyword} type implementation.
		 */
		private static final class KeywordImpl extends TypeBaseClass implements Keyword, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private KeywordImpl() {
				super("KEYWORD");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Keyword);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
