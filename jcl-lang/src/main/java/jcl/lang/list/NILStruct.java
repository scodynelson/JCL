package jcl.lang.list;

import java.util.Collections;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.type.NILType;
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

	/*
	LIST
	 */

	@Override
	public LispStruct getCar() {
		return INSTANCE;
	}

	@Override
	public void setCar(final LispStruct car) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	public LispStruct getCdr() {
		return INSTANCE;
	}

	@Override
	public void setCdr(final LispStruct cdr) {
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
	public LispStruct nth(final long index) {
		return INSTANCE;
	}

	@Override
	public void setNth(final long index, final LispStruct newValue) {
		throw new SimpleErrorException("Cannot set element within NIL.");
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
		return ConsStruct.valueOf(indicator, newValue);
	}

	@Override
	public boolean removeProperty(final LispStruct indicator) {
		return false;
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		return new ValuesStruct(INSTANCE, INSTANCE, INSTANCE);
	}

	@Override
	public LispStruct last(final long n) {
		return INSTANCE;
	}

	@Override
	public ListStruct butLast(final long n) {
		return INSTANCE;
	}

	@Override
	public ListStruct nButLast(final long n) {
		return INSTANCE;
	}

	/*
	SEQUENCE
	 */

	@Override
	public Long length() {
		return 0L;
	}

	@Override
	public LispStruct elt(final long index) {
		return INSTANCE;
	}

	@Override
	public void setElt(final long index, final LispStruct newValue) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	public ListStruct reverse() {
		return INSTANCE;
	}

	@Override
	public ListStruct nReverse() {
		return INSTANCE;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return Collections.emptyIterator();
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.emptySpliterator();
	}
}
