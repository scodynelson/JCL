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

/**
 * A {@link SimpleVector} is a type of a {@link Vector} that is not displaced to another {@link Array}, has no fill
 * pointer, is not expressly adjustable, and is able to hold elements of any type is a subtype of type {@link
 * SimpleVector}.
 * <p>
 * The type {@link SimpleVector} is a subtype of type {@link Vector}, and is a subtype of type (vector t).
 * <p>
 * {@link SimpleVector} -> {@link Vector} -> {@link SimpleArray} -> {@link Array} -> {@link Sequence} -> {@link T}
 */
public interface SimpleVector extends Vector, SimpleArray {

	SimpleVector INSTANCE = new Factory.SimpleVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleVector> {

		/**
		 * Gets instance of compound {@link SimpleVector} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleVector}
		 *
		 * @return the newly created compound {@link SimpleVector} type
		 */
		public static SimpleVector getInstance(final Integer size) {
			return SimpleVectorImpl.getInstance(size, T.INSTANCE);
		}

		/**
		 * Gets instance of compound {@link SimpleVector} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleVector}
		 * @param elementType
		 * 		the types of elements within the {@link SimpleVector}
		 *
		 * @return the newly created compound {@link SimpleVector} type
		 */
		public static SimpleVector getInstance(final Integer size, final LispType elementType) {
			return SimpleVectorImpl.getInstance(size, elementType);
		}

		@Override
		public SimpleVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleVector} type implementation.
		 */
		private static final class SimpleVectorImpl extends TypeBaseClass implements SimpleVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private SimpleVectorImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link SimpleVector} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleVector}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleVector}
			 */
			private SimpleVectorImpl(final Integer size, final LispType elementType) {
				super("SIMPLE-VECTOR");
				this.size = new DimensionsDesignator(size);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link SimpleVector} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleVector}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleVector}
			 *
			 * @return the newly created compound {@link SimpleVector} type
			 */
			public static SimpleVector getInstance(final Integer size, final LispType elementType) {
				return new SimpleVectorImpl(size, elementType);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
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

				if (!(obj instanceof SimpleVector)) {
					return false;
				}

				final SimpleVector simpleVector = (SimpleVector) obj;
				if (simpleVector == INSTANCE) {
					return true;
				}

				if (simpleVector instanceof SimpleVectorImpl) {
					return checkSimpleVectorImplEquality((SimpleVectorImpl) simpleVector);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleVector);
			}

			/**
			 * This method checks the equality of the provide {@code simpleVectorImpl} object to this instance.
			 *
			 * @param simpleVectorImpl
			 * 		the SimpleVectorImpl object to test for equality
			 *
			 * @return true if the {@code simpleVectorImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkSimpleVectorImplEquality(final SimpleVectorImpl simpleVectorImpl) {
				if (size == null) {
					return elementType.equals(simpleVectorImpl.elementType);
				}

				return size.equals(simpleVectorImpl.size) && elementType.equals(simpleVectorImpl.elementType);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
