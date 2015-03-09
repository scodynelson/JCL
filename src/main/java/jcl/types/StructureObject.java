/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The {@link StructureObject} is an instance of {@link StructureClass} and is a superclass of every {@link Class} that
 * is an instance of {@link StructureClass} except itself.
 * <p>
 * {@link StructureObject} -> {@link T}
 */
public interface StructureObject extends T {

	/**
	 * Singleton instance of the {@link StructureObject} type.
	 */
	StructureObject INSTANCE = new Factory.StructureObjectImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureObject> {

		@Override
		public StructureObject getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StructureObject} type implementation.
		 */
		private static final class StructureObjectImpl extends TypeBaseClass implements StructureObject, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 1530448854463167701L;

			/**
			 * Private constructor.
			 */
			private StructureObjectImpl() {
				super("STRUCTURE-OBJECT");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StructureObject);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
