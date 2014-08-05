package jcl.types;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;
import java.lang.String;

/**
 * The type {@link SimpleBaseString} is equivalent to (simple-array base-char (*)).
 * <p>
 * {@link SimpleBaseString} -> {@link BaseString} -> {@link SimpleString} -> {@link String} -> {@link Vector} -> {@link SimpleArray} -> {@link Array} -> {@link Sequence} -> {@link T}
 */
public interface SimpleBaseString extends BaseString, SimpleString {

	SimpleBaseString INSTANCE = new Factory.SimpleBaseStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBaseString> {

		@Override
		public SimpleBaseString getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@link SimpleBaseString} type.
		 *
		 * @param size the size of the {@link SimpleBaseString}
		 * @return the newly created compound {@link SimpleBaseString} type
		 */
		public static SimpleBaseString getInstance(final Integer size) {
			return SimpleBaseStringImpl.getInstance(size);
		}

		/**
		 * Inner {@link SimpleBaseString} type implementation.
		 */
		private static class SimpleBaseStringImpl extends TypeBaseClass implements SimpleBaseString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private static final LispType ELEMENT_TYPE = BaseChar.INSTANCE;

			/**
			 * Private constructor.
			 */
			private SimpleBaseStringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleBaseString} type.
			 *
			 * @param size the size of the {@link SimpleBaseString}
			 */
			private SimpleBaseStringImpl(final Integer size) {
				super("SIMPLE-BASE-STRING", GlobalPackageStruct.COMMON_LISP);
				this.size = new DimensionsDesignator(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			/**
			 * Gets instance of compound {@link SimpleBaseString} type.
			 *
			 * @param size the size of the {@link SimpleBaseString}
			 * @return the newly created compound {@link SimpleBaseString} type
			 */
			public static SimpleBaseString getInstance(final Integer size) {
				return new SimpleBaseStringImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleBaseString)) {
					return false;
				}

				final SimpleBaseString simpleBaseString = (SimpleBaseString) obj;
				if (simpleBaseString == INSTANCE) {
					return true;
				}

				if (simpleBaseString instanceof SimpleBaseStringImpl) {
					final SimpleBaseStringImpl simpleBaseStringImpl = (SimpleBaseStringImpl) simpleBaseString;

					return (size == null) || size.equals(simpleBaseStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleBaseString);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(ELEMENT_TYPE)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "SimpleBaseStringImpl{"
						+ "size=" + size
						+ ", elementType=" + ELEMENT_TYPE
						+ '}';
			}
		}
	}
}
