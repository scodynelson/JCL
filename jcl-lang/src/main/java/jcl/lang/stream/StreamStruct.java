/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.LispType;
import jcl.lang.BuiltInClassStruct;
import jcl.type.StreamType;

/**
 * The {@link StreamStruct} is the object representation of a Lisp 'stream' type.
 */
public abstract class StreamStruct extends BuiltInClassStruct implements LispStream {

	/**
	 * Whether or not the StreamStruct is interactive.
	 */
	private final boolean interactive;

	/**
	 * The {@link LispType} of the elements in the StreamStruct.
	 */
	private final LispType elementType;

	/**
	 * Whether or not the StreamStruct is closed.
	 */
	private boolean closed;

	protected long lineNumber;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the stream elementType
	 */
	protected StreamStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses,
	                       final boolean interactive, final LispType elementType) {
		super(StreamType.INSTANCE, directSuperClasses, subClasses);
		this.interactive = interactive;
		this.elementType = elementType;
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the stream object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the stream elementType
	 */
	StreamStruct(final StreamType type,
	             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses,
	             final boolean interactive, final LispType elementType) {
		super(type, directSuperClasses, subClasses);
		this.interactive = interactive;
		this.elementType = elementType;
	}

	@Override
	public boolean close() {
		final boolean wasClosed = closed;
		closed = true;
		return wasClosed;
	}

	@Override
	public LispType getElementType() {
		return elementType;
	}

	@Override
	public boolean isInteractive() {
		return !closed && interactive;
	}

	@Override
	public boolean isOpen() {
		return !closed;
	}

	@Override
	public boolean isClosed() {
		return closed;
	}

	@Override
	public Long lineNumber() {
		return lineNumber;
	}
}
