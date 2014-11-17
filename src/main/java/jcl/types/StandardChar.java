package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link StandardChar} is fixed set of 96 characters.
 * <p>
 * Any character that is not simple is not a standard character.
 * <p>
 * {@link StandardChar} -> {@link BaseChar} -> {@link Character} -> {@link T}
 */
public interface StandardChar extends BaseChar {

	StandardChar INSTANCE = new Factory.StandardCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardChar> {

		@Override
		public StandardChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardChar} type implementation.
		 */
		private static final class StandardCharImpl extends TypeBaseClass implements StandardChar, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardCharImpl() {
				super("STANDARD-CHAR", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardChar);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
