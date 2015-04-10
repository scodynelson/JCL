/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link PackageType} is a namespace that maps symbol names to {@link SymbolType}s.
 * <p>
 * {@link PackageType} -&gt; {@link TType}
 */
public interface PackageType extends TType {

	/**
	 * Singleton instance of the {@link PackageType} type.
	 */
	PackageType INSTANCE = new Factory.PackageTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<PackageType> {

		@Override
		public PackageType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link PackageType} type implementation.
		 */
		private static final class PackageTypeImpl extends TypeBaseClass implements PackageType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3289071503998628075L;

			/**
			 * Private constructor.
			 */
			private PackageTypeImpl() {
				super("PACKAGE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof PackageType);
			}
		}
	}
}
