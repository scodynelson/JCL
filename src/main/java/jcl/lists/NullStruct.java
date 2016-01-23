package jcl.lists;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.types.NullType;

/**
 * The {@link NullStruct} is the object representation of a Lisp 'null' type.
 */
public final class NullStruct extends ListStruct {

	public static final NullStruct INSTANCE = new NullStruct();

	/**
	 * Private constructor.
	 */
	private NullStruct() {
		super(NullType.INSTANCE, null, null);
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public LispStruct getFirst() {
		return INSTANCE;
	}

	@Override
	public ListStruct getRest() {
		return INSTANCE;
	}

	@Override
	public ListStruct getLast() {
		return INSTANCE;
	}

	@Override
	public ListStruct getAllButLast() {
		return INSTANCE;
	}

	@Override
	public LispStruct getElement(final int index) {
		return INSTANCE;
	}

	@Override
	public void setElement(final int index, final LispStruct newValue) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	public boolean isDotted() {
		return false;
	}

	@Override
	public boolean isCircular() {
		return false;
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		return Collections.emptyList();
	}
}
