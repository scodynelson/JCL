/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link FileStreamType} is a {@link StreamType} from which the direct source or sink is a file.
 * <p>
 * {@link FileStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface FileStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link FileStreamType} type.
	 */
	FileStreamType INSTANCE = new Factory.FileStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<FileStreamType> {

		@Override
		public FileStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link FileStreamType} type implementation.
		 */
		private static final class FileStreamTypeImpl extends TypeBaseClass implements FileStreamType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 772081181395170511L;

			/**
			 * Private constructor.
			 */
			private FileStreamTypeImpl() {
				super("FILE-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof FileStreamType);
			}
		}
	}
}
