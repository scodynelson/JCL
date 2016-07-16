/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link ConsType} is a compound object having two components, called the car and cdr. These form a dotted pair.
 * Each component can be any object.
 * <p>
 * {@link ConsType} -&gt; {@link ListType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface ConsType extends ListType {

	/**
	 * Singleton instance of the {@link ConsType} type.
	 */
	ConsType INSTANCE = new Factory.ConsTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ConsType> {

		/**
		 * Gets instance of compound {@link ConsType} type.
		 *
		 * @param carSpec
		 * 		the type of the car element
		 * @param cdrSpec
		 * 		the type of the cdr element
		 *
		 * @return the newly created compound {@link ConsType} type
		 */
		public static ConsType getInstance(final LispType carSpec, final LispType cdrSpec) {
			return ConsTypeImpl.getInstance(carSpec, cdrSpec);
		}

		@Override
		public ConsType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ConsType} type implementation.
		 */
		private static final class ConsTypeImpl extends TypeBaseClass implements ConsType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The type of the car element.
			 */
			private final LispType carSpec;

			/**
			 * The type of the cdr element.
			 */
			private final LispType cdrSpec;

			/**
			 * Private constructor.
			 */
			private ConsTypeImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link ConsType} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 */
			private ConsTypeImpl(final LispType carSpec, final LispType cdrSpec) {
				super("CONS");
				this.carSpec = carSpec;
				this.cdrSpec = cdrSpec;
			}

			/**
			 * Gets instance of compound {@link ConsType} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 *
			 * @return the newly created compound {@link ConsType} type
			 */
			public static ConsType getInstance(final LispType carSpec, final LispType cdrSpec) {
				return new ConsTypeImpl(carSpec, cdrSpec);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(carSpec)
				                            .append(cdrSpec)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof ConsType)) {
					return false;
				}

				final ConsType consType = (ConsType) obj;
				return (consType == INSTANCE) || ((consType instanceof ConsTypeImpl) && checkConsImplEquality((ConsTypeImpl) consType));
			}

			/**
			 * This method checks the equality of the provide consImpl object to this instance.
			 *
			 * @param consImpl
			 * 		the consImpl object to test for equality
			 *
			 * @return true if the consImpl object is equivalent to this instance; false otherwise
			 */
			private boolean checkConsImplEquality(final ConsTypeImpl consImpl) {
				if (carSpec != null) {
					return carSpec.equals(consImpl.carSpec);
				}

				return (cdrSpec == null) || cdrSpec.equals(consImpl.cdrSpec);
			}

			@Override
			public String toString() {
				return '(' + getName() + ' ' + ((carSpec == null) ? '*' : carSpec) + ' ' + ((cdrSpec == null) ? '*' : cdrSpec) + ')';
			}
		}
	}
}
