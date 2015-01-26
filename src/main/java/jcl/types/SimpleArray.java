package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.Integer;
import java.lang.String;
import java.util.List;

/**
 * A {@link SimpleArray} is the type of an {@link Array} that is not displaced to another {@link Array}, has no fill
 * pointer, and is not expressly adjustable.
 * <p>
 * The types {@link SimpleVector}, {@link SimpleString}, and {@link SimpleBitVector} are disjoint subtypes of type
 * {@link SimpleArray}, for they respectively mean (simple-array t (*)), the union of all (simple-array c (*)) for any
 * c being a subtype of type {@link Character}, and (simple-array bit (*)).
 * <p>
 * {@link SimpleArray} -> {@link Array} -> {@link T}
 */
public interface SimpleArray extends Array {

	SimpleArray INSTANCE = new Factory.SimpleArrayImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleArray> {

		/**
		 * Gets instance of compound {@link SimpleArray} type.
		 *
		 * @param dimensions
		 * 		the dimensions of the {@link SimpleArray}
		 * @param elementType
		 * 		the types of elements within the {@link SimpleArray}
		 *
		 * @return the newly created compound {@link SimpleArray} type
		 */
		public static SimpleArray getInstance(final List<Integer> dimensions, final LispType elementType) {
			return SimpleArrayImpl.getInstance(dimensions, elementType);
		}

		@Override
		public SimpleArray getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleArray} type implementation.
		 */
		private static final class SimpleArrayImpl extends TypeBaseClass implements SimpleArray, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private static final long serialVersionUID = 7655313490354936454L;

			private final DimensionsDesignator dimensions;
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private SimpleArrayImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link SimpleArray} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link SimpleArray}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleArray}
			 */
			private SimpleArrayImpl(final List<Integer> dimensions, final LispType elementType) {
				super("SIMPLE-ARRAY");
				this.dimensions = new DimensionsDesignator(dimensions);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link SimpleArray} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link SimpleArray}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleArray}
			 *
			 * @return the newly created compound {@link SimpleArray} type
			 */
			public static SimpleArray getInstance(final List<Integer> dimensions, final LispType elementType) {
				return new SimpleArrayImpl(dimensions, elementType);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return dimensions;
			}

			@Override
			public LispType getElementType() {
				return elementType;
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

				if (!(obj instanceof SimpleArray)) {
					return false;
				}

				final SimpleArray simpleArray = (SimpleArray) obj;
				if (simpleArray == INSTANCE) {
					return true;
				}

				if (simpleArray instanceof SimpleArrayImpl) {
					return checkSimpleArrayImplEquality((SimpleArrayImpl) simpleArray);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleArray);
			}

			/**
			 * This method checks the equality of the provide {@code simpleArrayImpl} object to this instance.
			 *
			 * @param simpleArrayImpl
			 * 		the SimpleArrayImpl object to test for equality
			 *
			 * @return true if the {@code simpleArrayImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkSimpleArrayImplEquality(final SimpleArrayImpl simpleArrayImpl) {
				if (dimensions == null) {
					return elementType.equals(simpleArrayImpl.elementType);
				}

				return dimensions.equals(simpleArrayImpl.dimensions) && elementType.equals(simpleArrayImpl.elementType);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
