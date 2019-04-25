/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StreamStructImpl} is the object representation of a Lisp 'stream' type.
 */
public abstract class StreamStructImpl extends LispStructImpl implements StreamStruct {

	/**
	 * Whether or not the StreamStruct is interactive.
	 */
	private boolean interactive;

	/**
	 * The {@link LispStruct} of the elements in the StreamStruct.
	 */
	private final LispStruct elementType;

	/**
	 * Whether or not the StreamStruct is closed.
	 */
	private boolean closed;

	protected long lineNumber;

	/**
	 * Protected constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the stream elementType
	 */
	StreamStructImpl(final boolean interactive, final LispStruct elementType) {
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
	public LispStruct getElementType() {
		return elementType;
	}

	@Override
	public boolean isInteractive() {
		return !closed && interactive;
	}

	@Override
	public void setInteractive(final boolean interactive) {
		if (closed) {
			throw new IllegalStateException("Cannot modify the state of a closed stream.");
		}
		this.interactive = interactive;
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

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
