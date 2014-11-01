package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link ExtendedChar} is equivalent to the type (and character (not base-char)).
 * <p>
 * {@link ExtendedChar} -> {@link Character} -> {@link T}
 */
public interface ExtendedChar extends Character {

	ExtendedChar INSTANCE = new Factory.ExtendedCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ExtendedChar> {

		@Override
		public ExtendedChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ExtendedChar} type implementation.
		 */
		private static class ExtendedCharImpl extends TypeBaseClass implements ExtendedChar, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ExtendedCharImpl() {
				super("EXTENDED-CHAR", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ExtendedChar);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
