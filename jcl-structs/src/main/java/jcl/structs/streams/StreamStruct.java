package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.streams.Stream;

import java.util.List;

public abstract class StreamStruct extends BuiltInClassStruct implements LispStream {

	private final boolean isInteractive;
	private final LispType elementType;
	private boolean isClosed;

	protected StreamStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses,
						   final boolean isInteractive, final LispType elementType) {
		super(Stream.INSTANCE, directSuperClasses, subClasses);
		this.isInteractive = isInteractive;
		this.elementType = elementType;
	}

	protected StreamStruct(final Stream type,
						   final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses,
						   final boolean isInteractive, final LispType elementType) {
		super(type, directSuperClasses, subClasses);
		this.isInteractive = isInteractive;
		this.elementType = elementType;
	}

	@Override
	public void close() throws StreamErrorException {
		isClosed = true;
	}

	@Override
	public LispType elementType() {
		return elementType;
	}

	public abstract Long fileLength() throws StreamErrorException;

	public abstract Long filePosition(Long filePosition) throws StreamErrorException;

	@Override
	public boolean isInteractive() {
		return !isClosed && isInteractive;
	}

	@Override
	public boolean isClosed() {
		return isClosed;
	}

}
