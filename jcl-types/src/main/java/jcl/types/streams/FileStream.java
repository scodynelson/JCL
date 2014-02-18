package jcl.types.streams;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code FileStream} is a stream from which the direct source or sink is a file.
 * <p/>
 * {@code FileStream} -> {@code Stream} -> {@code T}
 */
public interface FileStream extends Stream {

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
		 * Inner {@code FileStream} type implementation.
		 */
		private static class FileStreamImpl implements FileStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof FileStream)) {
					return false;
				}

				final FileStream fileStream = (FileStream) obj;
				return fileStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
