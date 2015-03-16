/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Package} is a namespace that maps symbol names to {@link Symbol}s.
 * <p>
 * {@link Package} -&gt; {@link T}
 */
public interface Package extends T {

	/**
	 * Singleton instance of the {@link Package} type.
	 */
	Package INSTANCE = new Factory.PackageImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Package> {

		@Override
		public Package getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Package} type implementation.
		 */
		private static final class PackageImpl extends TypeBaseClass implements Package, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3289071503998628075L;

			/**
			 * Private constructor.
			 */
			private PackageImpl() {
				super("PACKAGE");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Package);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
