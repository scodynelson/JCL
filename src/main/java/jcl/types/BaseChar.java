package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link BaseChar} is defined as the upgraded array element type of {@link StandardChar}.
 * <p>
 * Base characters are distinguished in the following respects:
 * 1. The type {@link StandardChar} is a subrepertoire of the type {@link BaseChar}.
 * 2. The selection of base characters that are not standard characters is implementation defined.
 * 3. Only objects of the type {@link BaseChar} can be elements of a base string.
 * 4. No upper bound is specified for the number of characters in the {@link BaseChar} repertoire. The lower bound
 * is 96, the number of standard characters.
 * <p>
 * The type {@link StandardChar} is a subtype of type {@link BaseChar}.
 * <p>
 * {@link BaseChar} -> {@link Character} -> {@link T}
 */
public interface BaseChar extends Character {

	BaseChar INSTANCE = new Factory.BaseCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseChar> {

		@Override
		public BaseChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BaseChar} type implementation.
		 */
		private static final class BaseCharImpl extends TypeBaseClass implements BaseChar, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BaseCharImpl() {
				super("BASE-CHAR", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BaseChar);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
