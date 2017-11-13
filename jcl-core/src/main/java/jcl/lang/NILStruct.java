package jcl.lang;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.internal.BooleanStructImpl;
import jcl.type.NILType;
import org.apache.commons.lang3.ArrayUtils;

/**
 * The {@link NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStructImpl implements ListStruct {

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
	public LispStruct car() {
		return INSTANCE;
	}

	@Override
	public LispStruct cdr() {
		return INSTANCE;
	}

	@Override
	public ListStruct copyList() {
		return INSTANCE;
	}

	@Override
	public ListStruct copyTree() {
		return INSTANCE;
	}

	@Override
	public LispStruct listLength() {
		return IntegerStruct.ZERO;
	}

	@Override
	public LispStruct nth(final FixnumStruct index) {
		return INSTANCE;
	}

	@Override
	public LispStruct setNth(final FixnumStruct index, final LispStruct newValue) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	public ListStruct nthCdr(final FixnumStruct index) {
		return INSTANCE;
	}

	@Override
	public BooleanStruct endP() {
		return BooleanStruct.T;
	}

	@Override
	public boolean tailp(final LispStruct object) {
		return INSTANCE == object;
	}

	@Override
	public ListStruct ldiff(final LispStruct object) {
		return INSTANCE;
	}

	@Override
	public LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue) {
		return INSTANCE;
	}

	@Override
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		return ConsStruct.toLispCons(indicator, newValue);
	}

	@Override
	public boolean removeProperty(final LispStruct indicator) {
		return false;
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		return ValuesStruct.valueOf(INSTANCE, INSTANCE, INSTANCE);
	}

	@Override
	public ListStruct copyAlist() {
		return INSTANCE;
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

	@Override
	public List<LispStruct> toJavaList() {
		return Collections.emptyList();
	}

	/*
	SEQUENCE
	 */

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
	public IntegerStruct length() {
		return IntegerStruct.ZERO;
	}

	@Override
	public LispStruct elt(final IntegerStruct index) {
		return INSTANCE;
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
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

	/*
	OLD
	 */

	@Override
	@Deprecated
	public LispStruct getCar() {
		return INSTANCE;
	}

	@Override
	@Deprecated
	public void setCar(final LispStruct car) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	@Deprecated
	public LispStruct getCdr() {
		return INSTANCE;
	}

	@Override
	@Deprecated
	public void setCdr(final LispStruct cdr) {
		throw new SimpleErrorException("Cannot set element within NIL.");
	}

	@Override
	@Deprecated
	public boolean isDotted() {
		return false;
	}

	@Override
	@Deprecated
	public boolean isCircular() {
		return false;
	}

}
