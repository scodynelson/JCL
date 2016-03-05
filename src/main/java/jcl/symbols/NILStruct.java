package jcl.symbols;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.types.NILType;
import org.apache.commons.lang3.ArrayUtils;

/**
 * The {@link NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStruct implements ListStruct {

	public static final NILStruct INSTANCE = new NILStruct();

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super(NILType.INSTANCE, "NIL", false);
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public LispStruct getCar() {
		return INSTANCE;
	}

	@Override
	public LispStruct getCdr() {
		return INSTANCE;
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
	public ListStruct copyAlist() {
		return INSTANCE;
	}

	@Override
	public Long listLength() {
		return 0L;
	}

	@Override
	public boolean tailp(final LispStruct object) {
		return INSTANCE.equals(object);
	}

	@Override
	public ListStruct ldiff(final LispStruct object) {
		return INSTANCE;
	}

	@Override
	public ListStruct nthCdr(final long n) {
		return INSTANCE;
	}

	@Override
	public LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue) {
		return INSTANCE;
	}

	@Override
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		return new ConsStruct(indicator, newValue);
	}

	@Override
	public boolean removeProperty(final LispStruct indicator) {
		return false;
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		return new ValuesStruct(INSTANCE, INSTANCE, INSTANCE);
	}
}
