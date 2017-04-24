/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link ConcatenatedStreamType} is an input stream which is a composite stream of zero or more other input streams,
 * such that the sequence of data which can be read from the {@link ConcatenatedStreamType} is the same as the
 * concatenation of the sequences of data which could be read from each of the constituent streams.
 * <p>
 * Input from a {@link ConcatenatedStreamType} is taken from the first of the associated input streams until it reaches
 * end of file; then that stream is discarded, and subsequent input is taken from the next input stream, and so on. An
 * end of file on the associated input streams is always managed invisibly by the {@link ConcatenatedStreamType}---the
 * only time a client of a {@link ConcatenatedStreamType} sees an end of file is when an attempt is made to obtain data
 * from the {@link ConcatenatedStreamType} but it has no remaining input streams from which to obtain such data.
 * <p>
 * {@link ConcatenatedStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface ConcatenatedStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link ConcatenatedStreamType} type.
	 */
	ConcatenatedStreamType INSTANCE = new Factory.ConcatenatedStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ConcatenatedStreamType> {

		@Override
		public ConcatenatedStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ConcatenatedStreamType} type implementation.
		 */
		private static final class ConcatenatedStreamTypeImpl extends TypeBaseClass implements ConcatenatedStreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ConcatenatedStreamTypeImpl() {
				super("CONCATENATED-STREAM");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof ConcatenatedStreamType);
			}
		}
	}
}
