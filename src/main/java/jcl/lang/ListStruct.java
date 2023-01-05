package jcl.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import org.apache.commons.collections4.CollectionUtils;

/**
 * The {@link ListStruct} is the object representation of a Lisp 'list' type.
 */
public interface ListStruct extends SequenceStruct {

	/**
	 * Returns the 'car' (or head) of the list. If the list is a {@link ConsStruct}, the car {@link LispStruct} is
	 * returned. If the list is a {@link NILStruct}, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @return the 'car' (or head) of the list
	 */
	LispStruct car();

	/**
	 * Returns the 'cdr' (or tail) of the list. If the list is a {@link ConsStruct}, the car {@link LispStruct} is
	 * returned. If the list is a {@link NILStruct}, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @return the 'cdr' (or tail) of the list
	 */
	LispStruct cdr();

	/**
	 * Returns a shallow copy of the list. Only the structure of the original list is copied, preseving the elements of
	 * the original list within the copied list result.
	 *
	 * @return a shallow copy of list
	 */
	ListStruct copyList();

	/**
	 * Creates a deep copy of the list. This operation does not preserve circularities and the sharing of substructure.
	 * In other words, for each cons within the car and cdr of the list, a new cons is the result of the copy.
	 *
	 * @return a deep copy of the list
	 */
	ListStruct copyTree();

	/**
	 * Returns the length of the list if the list is a proper-list. Returns {@link NILStruct#INSTANCE} if the list is a
	 * circular-list. Throws a type-error if the list is not a proper-list.
	 *
	 * @return the length of the list if the list is a proper-list; otherwise {@link NILStruct#INSTANCE} if the list is
	 * a circular-list
	 */
	LispStruct listLength();

	/**
	 * Returns a list of length given by size, each of the elements of which is initial-element.
	 * <p>
	 * NOTE: We're doing this in Java instead of Lisp for the performance.
	 *
	 * @param size
	 * 		the size of the resulting list
	 * @param initialElement
	 * 		the elements of the resulting list
	 *
	 * @return a list of length given by size, each of the elements of which is initial-element
	 */
	static ListStruct makeList(final FixnumStruct size, final LispStruct initialElement) {
		if (size.minusp().toJavaPBoolean()) {
			throw new TypeErrorException("Size must be a non-negative integer.");
		}

		ListStruct result = NILStruct.INSTANCE;
		for (int i = 0; i < size.toJavaInt(); i++) {
			result = ConsStruct.toLispCons(initialElement, result);
		}
		return result;
	}

	/**
	 * Returns the cons within the list where the head of the returned list is at the provided index.
	 *
	 * @param index
	 * 		the index of the cons within the list to be the head of the returned list
	 *
	 * @return the cons within the list where the head of the returned list is at the provided index
	 */
	ListStruct nthCdr(final FixnumStruct index);

	/**
	 * Returns true if the list is the empty list. Returns false if the list is a cons.
	 *
	 * @return true if the list is the empty list; false if the list is a cons
	 */
	BooleanStruct endp();

	default ListStruct butLast() {
		return butLast(IntegerStruct.ONE);
	}

	ListStruct butLast(final FixnumStruct n);

	default ListStruct nButLast() {
		return nButLast(IntegerStruct.ONE);
	}

	ListStruct nButLast(final FixnumStruct n);

	default LispStruct last() {
		return last(IntegerStruct.ONE);
	}

	LispStruct last(final FixnumStruct n);

	ListStruct ldiff(final LispStruct object);

	static ListStruct pairlis(final ListStruct keys, final ListStruct datums, final ListStruct alist) {

		final long keysLength = keys.length().toJavaPLong();
		final long datumsLength = datums.length().toJavaPLong();
		if (keysLength != datumsLength) {
			throw new SimpleErrorException("The lists of keys and datums are not the same length.");
		}

		final LispStruct[] keysArray = keys.toArray();
		final LispStruct[] datumsArray = datums.toArray();

		ListStruct theAlist = alist;

		for (int i = 0; i < keysLength; i++) {
			final LispStruct key = keysArray[i];
			final LispStruct datum = datumsArray[i];
			final ConsStruct pair = ConsStruct.toLispCons(key, datum);
			theAlist = ConsStruct.toLispCons(pair, theAlist);
		}

		return theAlist;
	}

	BooleanStruct tailp(final LispStruct object);

	/**
	 * Returns a copy of alist. The list structure of alist is copied, and the elements of alist which are conses are
	 * also copied (as conses only). Any other objects which are referred to, whether directly or indirectly, by the
	 * alist continue to be shared.
	 *
	 * @return a copy of alist
	 */
	ListStruct copyAlist();

	GetPropertiesResult getProperties(final ListStruct indicators);

	default LispStruct getf(final LispStruct indicator) {
		return getf(indicator, NILStruct.INSTANCE);
	}

	LispStruct getf(final LispStruct indicator, final LispStruct defaultValue);

	ListStruct putf(final LispStruct indicator, final LispStruct newValue);

	boolean remf(final LispStruct indicator);

	List<LispStruct> toJavaList();

	/**
	 * Returns a new ListStruct containing the provided {@link LispStruct} arguments as the elements. If there are no
	 * arguments, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param args
	 * 		the elements of the resulting ListStruct
	 *
	 * @return a new ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	static ListStruct toLispList(final LispStruct... args) {
		return (args.length == 0) ? NILStruct.INSTANCE : buildLispList(List.of(args));
	}

	/**
	 * Returns a new ListStruct containing the provided {@link LispStruct} arguments as the elements. If there are no
	 * arguments, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param args
	 * 		the elements of the resulting ListStruct
	 *
	 * @return a new ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	static ListStruct toLispList(final List<? extends LispStruct> args) {
		return CollectionUtils.isEmpty(args) ? NILStruct.INSTANCE : buildLispList(args);
	}

	/**
	 * Returns a new ListStruct containing the provided {@link LispStruct} arguments as the elements. This is a helper
	 * method used by {@link #toLispList(LispStruct...)} and {@link #toLispList(List)} methods for building a new
	 * ListStruct.
	 *
	 * @param args
	 * 		the elements of the resulting ListStruct
	 *
	 * @return a new ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	private static ListStruct buildLispList(final List<? extends LispStruct> args) {
		if (args.size() == 1) {
			return ConsStruct.toLispCons(args.get(0), NILStruct.INSTANCE);
		}

		final LispStruct car = args.get(0);
		final List<? extends LispStruct> rest = args.subList(1, args.size());

		final ListStruct cdr = buildLispList(rest);
		return ConsStruct.toLispCons(car, cdr);
	}

	/**
	 * Returns a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements. If there
	 * are no elements in the provided {@code others}, the {@code arg} will be returned.
	 *
	 * @param arg
	 * 		the initial element in the resulting structure
	 * @param others
	 * 		the remaining elements in the resulting structure
	 *
	 * @return a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	static LispStruct toLispDottedList(final LispStruct arg, final LispStruct... others) {
		if (others.length == 0) {
			return arg;
		}
		if (others.length == 1) {
			return ConsStruct.toLispCons(arg, others[0]);
		}

		final List<LispStruct> otherList = new ArrayList<>(others.length + 1);
		otherList.add(arg);
		otherList.addAll(Arrays.asList(others));
		return buildLispDottedList(otherList);
	}

	/**
	 * Returns a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements. If there
	 * are no elements in the provided {@code others}, the {@code arg} will be returned.
	 *
	 * @param arg
	 * 		the initial element in the resulting structure
	 * @param others
	 * 		the remaining elements in the resulting structure
	 *
	 * @return a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	static LispStruct toLispDottedList(final LispStruct arg, final List<? extends LispStruct> others) {
		if (CollectionUtils.isEmpty(others)) {
			return arg;
		}
		if (others.size() == 1) {
			return ConsStruct.toLispCons(arg, others.get(0));
		}

		final List<LispStruct> otherList = new ArrayList<>(others.size() + 1);
		otherList.add(arg);
		otherList.addAll(others);
		return buildLispDottedList(otherList);
	}

	/**
	 * Returns a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements. This is a
	 * helper method used by {@link #toLispDottedList(LispStruct, LispStruct...)} and
	 * {@link #toLispDottedList(LispStruct, List)} methods for building a new ListStruct.
	 *
	 * @param args
	 * 		the elements of the resulting ListStruct
	 *
	 * @return a new dotted ListStruct containing the provided {@link LispStruct} arguments as the elements
	 */
	private static LispStruct buildLispDottedList(final List<? extends LispStruct> args) {
		if (args.size() == 2) {
			return ConsStruct.toLispCons(args.get(0), args.get(1));
		}

		final LispStruct car = args.get(0);
		final List<? extends LispStruct> rest = args.subList(1, args.size());

		final LispStruct cdr = buildLispDottedList(rest);
		return ConsStruct.toLispCons(car, cdr);
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	ListStruct reverse();

	@Override
	ListStruct nReverse();
}
