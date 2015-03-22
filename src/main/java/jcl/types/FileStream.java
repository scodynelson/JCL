/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link FileStream} is a {@link Stream} from which the direct source or sink is a file.
 * <p>
 * {@link FileStream} -&gt; {@link Stream} -&gt; {@link T}
 */
public interface FileStream extends Stream {

	/**
	 * Singleton instance of the {@link FileStream} type.
	 */
	FileStream INSTANCE = new Factory.FileStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<FileStream> {

		@Override
		public FileStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link FileStream} type implementation.
		 */
		private static final class FileStreamImpl extends TypeBaseClass implements FileStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 772081181395170511L;

			/**
			 * Private constructor.
			 */
			private FileStreamImpl() {
				super("FILE-STREAM");
			}
		}
	}
}
