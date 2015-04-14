/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import jcl.types.StreamType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StreamStruct} is the object representation of a Lisp 'stream' type.
 */
public abstract class StreamStruct extends BuiltInClassStruct implements LispStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5129615077425690834L;

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
	public void close() {
		closed = true;
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
	public boolean isClosed() {
		return closed;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(interactive)
		                            .append(elementType)
		                            .append(closed)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final StreamStruct rhs = (StreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(interactive, rhs.interactive)
		                          .append(elementType, rhs.elementType)
		                          .append(closed, rhs.closed)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(interactive)
		                                                                .append(elementType)
		                                                                .append(closed)
		                                                                .toString();
	}
}
