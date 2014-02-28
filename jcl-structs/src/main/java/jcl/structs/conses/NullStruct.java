package jcl.structs.conses;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.types.conses.Null;

import java.util.Collections;
import java.util.List;

/**
 * The {@code NullStruct} is the object representation of a Lisp 'null' type.
 */
public final class NullStruct extends ListStruct {

	public static final NullStruct INSTANCE = new NullStruct();

	/**
	 * Private constructor.
	 */
	private NullStruct() {
		super(Null.INSTANCE, null, null);
	}

	@Override
	public int size() {
		return 0;
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
	public LispStruct getFirst() {
		return INSTANCE;
	}

	@Override
	public ListStruct getRest() {
		return INSTANCE;
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		return Collections.emptyList();
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
	public String toString() {
		return "NullStruct{}";
	}
}
