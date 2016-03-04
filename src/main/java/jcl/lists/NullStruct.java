package jcl.lists;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.types.NullType;
import org.apache.commons.lang3.ArrayUtils;

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

	@Override
	public Iterator<LispStruct> iterator() {
		return Collections.emptyIterator();
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.emptySpliterator();
	}

	@Override
	public Stream<LispStruct> stream() {
		return Stream.empty();
	}

	@Override
	public Stream<LispStruct> parallelStream() {
		return Stream.empty();
	}

	@Override
	public LispStruct[] toArray() {
		return ArrayUtils.toArray();
	}

	@Override
	public ListStruct copyTree() {
		return INSTANCE;
	}

	@Override
	public ListStruct copyList() {
		return INSTANCE;
	}

	@Override
	public Long listLength() {
		return 0L;
	}
}
