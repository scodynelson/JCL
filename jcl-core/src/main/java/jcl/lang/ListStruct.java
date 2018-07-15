package jcl.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.util.ClassUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.iterators.ReverseListIterator;

/**
 * The {@link ListStruct} is the object representation of a Lisp 'list' type.
 */
public interface ListStruct extends SequenceStruct {

	/*
	LIST
	 */

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
	FixnumStruct listLength();

	/**
	 * Returns a list of length given by size, each of the elements of which is initial-element.
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
	 * Returns the nth element of list at the provided index.
	 *
	 * @param index
	 * 		the index of the element to return
	 *
	 * @return the nth element of list at the provided index
	 */
	LispStruct nth(final FixnumStruct index);

	/**
	 * Sets the nth element of list at the provided index to the provided new value.
	 *
	 * @param index
	 * 		the index of the element to set
	 * @param newValue
	 * 		the new value to be set
	 *
	 * @return the new value
	 */
	LispStruct setNth(final FixnumStruct index, final LispStruct newValue);

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
	boolean endP();

	static LispStruct append(final LispStruct... args) {
		final int size = args.length;
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return args[0];
		}

		return append(Arrays.asList(args));
	}

	static LispStruct append(final List<LispStruct> listArg) {
		final int size = listArg.size();
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return listArg.get(0);
		}

		final ReverseListIterator<LispStruct> reverseListIterator = new ReverseListIterator<>(listArg);
		final LispStruct object = reverseListIterator.next();

		final List<ListStruct> lists = new ArrayList<>();
		reverseListIterator.forEachRemaining(argument -> {
			final ListStruct list = ClassUtils.convert(argument, ListStruct.class);
			lists.add(list);
		});
		return append(lists, object);
	}

	private static LispStruct append(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.eq(list)) {
				continue;
			}

			final ListStruct copyList = list.copyList();
			if (NILStruct.INSTANCE.eq(result)) {
				result = copyList;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ConsStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ConsStruct lastOfResult = (ConsStruct) last;
			lastOfResult.rplacd(copyList);
		}

		if (NILStruct.INSTANCE.eq(result)) {
			return object;
		}

		final LispStruct last = result.last();
		if (!(last instanceof ConsStruct)) {
			throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
		}
		final ConsStruct lastOfResult = (ConsStruct) last;
		lastOfResult.rplacd(object);

		return result;
	}

	static LispStruct nConc(final LispStruct... args) {
		final int size = args.length;
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return args[0];
		}

		return nConc(Arrays.asList(args));
	}

	static LispStruct nConc(final List<LispStruct> listArg) {
		final int size = listArg.size();
		if (size == 0) {
			return NILStruct.INSTANCE;
		}
		if (size == 1) {
			return listArg.get(0);
		}

		final ReverseListIterator<LispStruct> reverseListIterator = new ReverseListIterator<>(listArg);
		final LispStruct object = reverseListIterator.next();

		final List<ListStruct> lists = new ArrayList<>();
		reverseListIterator.forEachRemaining(argument -> {
			final ListStruct list = ClassUtils.convert(argument, ListStruct.class);
			lists.add(list);
		});
		return nConc(lists, object);
	}

	static LispStruct nConc(final List<ListStruct> lists, final LispStruct object) {
		final Iterator<ListStruct> iterator = lists.iterator();

		ListStruct result = NILStruct.INSTANCE;

		while (iterator.hasNext()) {
			final ListStruct list = iterator.next();

			if (NILStruct.INSTANCE.eq(list)) {
				continue;
			}

			if (NILStruct.INSTANCE.eq(result)) {
				result = list;
				continue;
			}

			final LispStruct last = result.last();
			if (!(last instanceof ConsStruct)) {
				throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
			}
			final ConsStruct lastOfResult = (ConsStruct) last;
			lastOfResult.rplacd(list);
		}

		if (NILStruct.INSTANCE.eq(result)) {
			return object;
		}

		final LispStruct last = result.last();
		if (!(last instanceof ConsStruct)) {
			throw new TypeErrorException("Arguments contain a non-proper list -- " + result);
		}
		final ConsStruct lastOfResult = (ConsStruct) last;
		lastOfResult.rplacd(object);

		return result;
	}

	default LispStruct revAppend(final LispStruct tail) {
		final ListStruct reverse = reverse();
		return nConc(Collections.singletonList(reverse), tail);
	}

	default LispStruct nReconc(final LispStruct tail) {
		final ListStruct nReverse = nReverse();
		return nConc(Collections.singletonList(nReverse), tail);
	}

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

	boolean tailp(final LispStruct object);

	// TODO: Fast-Member???

	enum Accumulate {
		NONE,
		LIST,
		NCONC
	}

	static LispStruct mapC(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.NONE, true);
	}

	static LispStruct mapCar(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.LIST, true);
	}

	static LispStruct mapCan(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.NCONC, true);
	}

	static LispStruct mapL(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.NONE, false);
	}

	static LispStruct mapList(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.LIST, false);
	}

	static LispStruct mapCon(final FunctionStruct function, final ListStruct lists) {
		return listMapper(function, lists, Accumulate.NCONC, false);
	}

	private static LispStruct listMapper(final FunctionStruct function, final ListStruct originalArglists,
	                                     final Accumulate accumulate, final boolean takeCar) {
		final ListStruct arglists = originalArglists.copyList();
		final ConsStruct retList = ConsStruct.toLispCons(NILStruct.INSTANCE, NILStruct.INSTANCE);
		ConsStruct temp = retList;

		// Outer 'DO' vars
		ListStruct args = NILStruct.INSTANCE;

		while (true) {
			for (final LispStruct arglist : arglists) {
				if (NILStruct.INSTANCE.eq(arglist)) {
					if (accumulate != null) {
						return retList.cdr();
					} else {
						return originalArglists.car();
					}
				}
			}

			// Inner 'DO' vars
			ListStruct remainingLists = arglists;

			while (!NILStruct.INSTANCE.eq(remainingLists)) {

				// PUSH
				final LispStruct carToPush;
				if (takeCar) {
					carToPush = ((ListStruct) remainingLists.car()).car();
				} else {
					carToPush = remainingLists.car();
				}
				args = ConsStruct.toLispCons(carToPush, args);

				// SETF
				final LispStruct cdarL = ((ListStruct) remainingLists.car()).cdr();
				((ConsStruct) remainingLists).rplaca(cdarL);

				// Update remainingLists Inner 'DO' var
				remainingLists = (ListStruct) remainingLists.cdr();
			}

			// SETQ
			final LispStruct res = function.apply(args.nReverse().toArray());

			// CASE
			switch (accumulate) {
				case NCONC:
					final List<ListStruct> lists = new ArrayList<>();
					for (final LispStruct curr : temp) {
						lists.add((ListStruct) curr);
					}
					final ListStruct nconc = (ListStruct) nConc(lists, res);
					temp = (ConsStruct) nconc.last();
					break;
				case LIST:
					temp.rplacd(toLispList(res));
					temp = (ConsStruct) temp.cdr();
					break;
				case NONE:
					break;
			}

			// Update Args Outer 'DO' var
			args = NILStruct.INSTANCE;
		}
	}


	/**
	 * Returns a copy of alist. The list structure of alist is copied, and the elements of alist which are conses are
	 * also copied (as conses only). Any other objects which are referred to, whether directly or indirectly, by the
	 * alist continue to be shared.
	 *
	 * @return a copy of alist
	 */
	ListStruct copyAlist();

	// TODO: Fast-Assoc???

	// TODO: Pairlis

	// TODO: Fast Rassoc???

	ValuesStruct getProperties(final ListStruct indicators);

	default LispStruct getf(final LispStruct indicator) {
		return getf(indicator, NILStruct.INSTANCE);
	}

	LispStruct getf(final LispStruct indicator, final LispStruct defaultValue);

	ListStruct putf(final LispStruct indicator, final LispStruct newValue);

	boolean remf(final LispStruct indicator);

	// TODO: Fast-Adjoin???

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
		return (args.length == 0) ? NILStruct.INSTANCE : buildLispList(Arrays.asList(args));
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
	 * helper method used by {@link #toLispDottedList(LispStruct, LispStruct...)} and {@link
	 * #toLispDottedList(LispStruct, List)} methods for building a new ListStruct.
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
	SEQUENCE
	 */

	@Override
	ListStruct reverse();

	@Override
	ListStruct nReverse();

	/*
	OLD
	 */

	/**
	 * Determines if the list is a dotted list.
	 *
	 * @return if the list is a dotted list
	 */
	@Deprecated
	boolean isDotted();

	/**
	 * Determines if the list is a proper list.
	 *
	 * @return if the list is a proper list
	 */
	@Deprecated
	default boolean isProper() {
		return !isDotted() && !IntegerStruct.MINUS_ONE.eql(listLength());
	}
}
