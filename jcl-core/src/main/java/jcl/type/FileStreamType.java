/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			 * Private constructor.
			 */
			private FileStreamTypeImpl() {
				super("FILE-STREAM");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof FileStreamType);
			}
		}
	}
}
