package jcl.types;

import jcl.LispType;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.Integer;
import java.lang.String;

/**
 * A {@link SimpleBitVector} is a type of a {@link BitVector} that is not displaced to another {@link Array}, has no
 * fill pointer, and is not expressly adjustable is a subtype of type {@link SimpleBitVector}.
 * <p>
 * {@link SimpleBitVector} -> {@link BitVector} -> {@link Vector} -> {@link SimpleArray} -> {@link Array} -> {@link
 * Sequence} -> {@link T}
 */
public interface SimpleBitVector extends BitVector, SimpleArray {

	SimpleBitVector INSTANCE = new Factory.SimpleBitVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBitVector> {

		/**
		 * Gets instance of compound {@link SimpleBitVector} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleBitVector}
		 *
		 * @return the newly created compound {@link SimpleBitVector} type
		 */
		public static SimpleBitVector getInstance(final Integer size) {
			return SimpleBitVectorImpl.getInstance(size);
		}

		@Override
		public SimpleBitVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleBitVector} type implementation.
		 */
		private static final class SimpleBitVectorImpl extends TypeBaseClass implements SimpleBitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private static final LispType ELEMENT_TYPE = Bit.INSTANCE;
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleBitVectorImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleBitVector} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBitVector}
			 */
			private SimpleBitVectorImpl(final Integer size) {
				super("SIMPLE-BIT-VECTOR");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleBitVector} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBitVector}
			 *
			 * @return the newly created compound {@link SimpleBitVector} type
			 */
			public static SimpleBitVector getInstance(final Integer size) {
				return new SimpleBitVectorImpl(size);
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

				if (!(obj instanceof SimpleBitVector)) {
					return false;
				}

				final SimpleBitVector simpleBitVector = (SimpleBitVector) obj;
				if (simpleBitVector == INSTANCE) {
					return true;
				}

				if (simpleBitVector instanceof SimpleBitVectorImpl) {
					final SimpleBitVectorImpl simpleBitVectorImpl = (SimpleBitVectorImpl) simpleBitVector;

					return (size == null) || size.equals(simpleBitVectorImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleBitVector);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
