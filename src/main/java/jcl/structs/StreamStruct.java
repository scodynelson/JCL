package jcl.structs;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.Stream;

import java.util.List;

/**
 * The {@code StreamStruct} is the object representation of a Lisp 'stream' type.
 */
public abstract class StreamStruct extends BuiltInClassStruct implements LispStream {

	private final boolean isInteractive;
	private final LispType elementType;
	private boolean isClosed;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 * @param isInteractive      whether or not the struct created is 'interactive'
	 * @param elementType        the stream elementType
	 */
	protected StreamStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses,
						   final boolean isInteractive, final LispType elementType) {
		super(Stream.INSTANCE, directSuperClasses, subClasses);
		this.isInteractive = isInteractive;
		this.elementType = elementType;
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the stream object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 * @param isInteractive      whether or not the struct created is 'interactive'
	 * @param elementType        the stream elementType
	 */
	protected StreamStruct(final Stream type,
						   final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses,
						   final boolean isInteractive, final LispType elementType) {
		super(type, directSuperClasses, subClasses);
		this.isInteractive = isInteractive;
		this.elementType = elementType;
	}

	@Override
	public void close() {
		isClosed = true;
	}

	@Override
	public LispType getElementType() {
		return elementType;
	}

	@Override
	public boolean isInteractive() {
		return !isClosed && isInteractive;
	}

	@Override
	public boolean isClosed() {
		return isClosed;
	}

	@Override
	public String toString() {
		return "StreamStruct{"
				+ "isInteractive=" + isInteractive
				+ ", elementType=" + elementType
				+ ", isClosed=" + isClosed
				+ '}';
	}
}
