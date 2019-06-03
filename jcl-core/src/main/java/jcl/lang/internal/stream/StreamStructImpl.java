/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StreamStructImpl} is the object representation of a Lisp 'stream' type.
 */
class StreamStructImpl extends LispStructImpl implements StreamStruct {

	/**
	 * Whether or not the stream is interactive.
	 */
	private boolean interactive;

	/**
	 * The type of elements in the stream.
	 */
	private final LispStruct elementType;

	/**
	 * Whether or not the stream is closed.
	 */
	private boolean closed;

	/**
	 * The current line number (based on the number of newline characters read or written).
	 */
	protected long lineNumber;

	/**
	 * Protected constructor, initializing the {@link #elementType}.
	 *
	 * @param elementType
	 * 		the stream element-type to initialize
	 */
	protected StreamStructImpl(final LispStruct elementType) {
		this.elementType = elementType;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		final boolean wasClosed = closed;
		closed = true;
		return BooleanStruct.toLispBoolean(wasClosed);
	}

	@Override
	public LispStruct streamElementType() {
		return elementType;
	}

	@Override
	public BooleanStruct interactiveStreamP() {
		return BooleanStruct.toLispBoolean(!closed && interactive);
	}

	@Override
	public void setInteractive(final boolean interactive) {
		if (closed) {
			throw new StreamErrorException("Cannot modify the state of a closed stream.", this);
		}
		this.interactive = interactive;
	}

	@Override
	public BooleanStruct openStreamP() {
		return BooleanStruct.toLispBoolean(!closed);
	}

	@Override
	public BooleanStruct closedStreamP() {
		return BooleanStruct.toLispBoolean(closed);
	}

	@Override
	public LispStruct fileLength() {
		throw new StreamErrorException("Operation only supported on a FileStream.", this);
	}

	@Override
	public LispStruct filePosition() {
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct filePosition(final IntegerStruct position) {
		return NILStruct.INSTANCE;
	}

	@Override
	public IntegerStruct lineNumber() {
		return IntegerStruct.toLispInteger(lineNumber);
	}

	/*
	LISP-STRUCT
	 */

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
