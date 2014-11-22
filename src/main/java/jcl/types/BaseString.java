package jcl.types;

import jcl.LispType;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.Integer;

/**
 * The type {@link BaseString} is equivalent to (vector base-char). The {@link BaseString} representation is the most
 * efficient {@link String} representation that can hold an arbitrary sequence of {@link StandardChar}s.
 * <p>
 * {@link BaseString} -> {@link String} -> {@link Vector} -> {@link Array} -> {@link Sequence} -> {@link T}
 */
public interface BaseString extends String {

	BaseString INSTANCE = new Factory.BaseStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseString> {

		/**
		 * Gets instance of compound {@link BaseString} type.
		 *
		 * @param size
		 * 		the size of the {@link BaseString}
		 *
		 * @return the newly created compound {@link BaseString} type
		 */
		public static BaseString getInstance(final Integer size) {
			return BaseStringImpl.getInstance(size);
		}

		@Override
		public BaseString getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BaseString} type implementation.
		 */
		private static final class BaseStringImpl extends TypeBaseClass implements BaseString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private static final LispType ELEMENT_TYPE = BaseChar.INSTANCE;
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private BaseStringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link BaseString} type.
			 *
			 * @param size
			 * 		the size of the {@link BaseString}
			 */
			private BaseStringImpl(final Integer size) {
				super("BASE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link BaseString} type.
			 *
			 * @param size
			 * 		the size of the {@link BaseString}
			 *
			 * @return the newly created compound {@link BaseString} type
			 */
			public static BaseString getInstance(final Integer size) {
				return new BaseStringImpl(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BaseString)) {
					return false;
				}

				final BaseString baseString = (BaseString) obj;
				if (baseString == INSTANCE) {
					return true;
				}

				if (baseString instanceof BaseStringImpl) {
					final BaseStringImpl baseStringImpl = (BaseStringImpl) baseString;

					return (size == null) || size.equals(baseStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, baseString);
			}

			@Override
			public java.lang.String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
