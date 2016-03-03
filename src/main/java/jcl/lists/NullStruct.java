package jcl.lists;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
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
	public int size() {
		return 0;
	}

	@Override
	public boolean isEmpty() {
		return true;
	}

	@Override
	public boolean contains(final Object o) {
		return false;
	}

	@Override
	public Object[] toArray() {
		return ArrayUtils.EMPTY_OBJECT_ARRAY;
	}

	@Override
	public <T> T[] toArray(final T[] a) {
		return a;
	}

	@Override
	public boolean add(final LispStruct e) {
		throw new UnsupportedOperationException("Cannot add elements to NIL.");
	}

	@Override
	public boolean remove(final Object o) {
		throw new UnsupportedOperationException("Cannot remove elements from NIL.");
	}

	@Override
	public boolean containsAll(final Collection<?> c) {
		return false;
	}

	@Override
	public boolean addAll(final Collection<? extends LispStruct> c) {
		throw new UnsupportedOperationException("Cannot add elements to NIL.");
	}

	@Override
	public boolean removeAll(final Collection<?> c) {
		throw new UnsupportedOperationException("Cannot remove elements from NIL.");
	}

	@Override
	public boolean retainAll(final Collection<?> c) {
		throw new UnsupportedOperationException("Cannot remove elements from NIL.");
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException("Cannot remove elements from NIL.");
	}

	@Override
	public Stream<LispStruct> stream() {
		return Stream.empty();
	}

	@Override
	public Stream<LispStruct> parallelStream() {
		return Stream.empty();
	}
}
