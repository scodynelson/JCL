/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Cons} is a compound object having two components, called the car and cdr. These form a dotted pair. Each
 * component can be any object.
 * <p>
 * {@link Cons} -> {@link List} -> {@link Sequence} -> {@link T}
 */
public interface Cons extends List {

	/**
	 * Singleton instance of the {@link Cons} type.
	 */
	Cons INSTANCE = new Factory.ConsImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Cons> {

		/**
		 * Gets instance of compound {@link Cons} type.
		 *
		 * @param carSpec
		 * 		the type of the car element
		 * @param cdrSpec
		 * 		the type of the cdr element
		 *
		 * @return the newly created compound {@link Cons} type
		 */
		public static Cons getInstance(final LispType carSpec, final LispType cdrSpec) {
			return ConsImpl.getInstance(carSpec, cdrSpec);
		}

		@Override
		public Cons getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Cons} type implementation.
		 */
		private static final class ConsImpl extends TypeBaseClass implements Cons, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -991060349966644883L;

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
			private ConsImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link Cons} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 */
			private ConsImpl(final LispType carSpec, final LispType cdrSpec) {
				super("CONS");
				this.carSpec = carSpec;
				this.cdrSpec = cdrSpec;
			}

			/**
			 * Gets instance of compound {@link Cons} type.
			 *
			 * @param carSpec
			 * 		the type of the car element
			 * @param cdrSpec
			 * 		the type of the cdr element
			 *
			 * @return the newly created compound {@link Cons} type
			 */
			public static Cons getInstance(final LispType carSpec, final LispType cdrSpec) {
				return new ConsImpl(carSpec, cdrSpec);
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

				if (!(obj instanceof Cons)) {
					return false;
				}

				final Cons cons = (Cons) obj;
				return (cons == INSTANCE) || ((cons instanceof ConsImpl) && checkConsImplEquality((ConsImpl) cons));
			}

			/**
			 * This method checks the equality of the provide consImpl object to this instance.
			 *
			 * @param consImpl
			 * 		the consImpl object to test for equality
			 *
			 * @return true if the consImpl object is equivalent to this instance; false otherwise
			 */
			private boolean checkConsImplEquality(final ConsImpl consImpl) {
				if (carSpec != null) {
					return carSpec.equals(consImpl.carSpec);
				}

				return (cdrSpec == null) || cdrSpec.equals(consImpl.cdrSpec);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
