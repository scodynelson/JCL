package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link FileStream} is a {@link Stream} from which the direct source or sink is a file.
 * <p>
 * {@link FileStream} -> {@link Stream} -> {@link T}
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
		 * Inner {@link FileStream} type implementation.
		 */
		private static final class FileStreamImpl extends TypeBaseClass implements FileStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private FileStreamImpl() {
				super("FILE-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof FileStream);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
