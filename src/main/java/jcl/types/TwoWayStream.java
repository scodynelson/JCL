package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link TwoWayStream} is a bidirectional composite stream that receives its input from an associated input stream and
 * sends its output to an associated output stream.
 * <p>
 * {@link TwoWayStream} -> {@link Stream} -> {@link T}
 */
public interface TwoWayStream extends Stream {

	TwoWayStream INSTANCE = new Factory.TwoWayStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TwoWayStream> {

		@Override
		public TwoWayStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link TwoWayStream} type implementation.
		 */
		private static class TwoWayStreamImpl extends TypeBaseClass implements TwoWayStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private TwoWayStreamImpl() {
				super("TWO-WAY-STREAM", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof TwoWayStream);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "TwoWayStream{}";
			}
		}
	}
}
