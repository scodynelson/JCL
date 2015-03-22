/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link Class} represents objects that determine the structure and behavior of their instances. Associated
 * with an object of type {@link Class} is information describing its place in the directed acyclic graph of classes,
 * its slots, and its options.
 * <p>
 * {@link Class} -&gt; {@link StandardObject} -&gt; {@link T}
 */
public interface Class extends StandardObject {

	/**
	 * Singleton instance of the {@link Class} type.
	 */
	Class INSTANCE = new Factory.ClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Class> {

		@Override
		public Class getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Class} type implementation.
		 */
		private static final class ClassImpl extends TypeBaseClass implements Class, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 6240185539703787326L;

			/**
			 * Private constructor.
			 */
			private ClassImpl() {
				super("CLASS");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Class);
			}
		}
	}
}
