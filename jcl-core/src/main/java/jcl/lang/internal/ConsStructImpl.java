package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.ConsType;

/**
 * The {@link ConsStructImpl} is the object representation of a Lisp 'cons' type.
 */
//@EqualsAndHashCode(callSuper = false)
public final class ConsStructImpl extends BuiltInClassStruct implements ConsStruct {

	/**
	 * The 'car' (or head) of the cons linking structure.
	 */
	private LispStruct car;

	/**
	 * The 'cdr' (or tail) of the cons linking structure.
	 */
	private LispStruct cdr;

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 * @param cdr
	 * 		the cdr of the binary cons structure
	 */
	public ConsStructImpl(final LispStruct car, final LispStruct cdr) {
		super(ConsType.INSTANCE, null, null);
		this.car = car;
		this.cdr = cdr;
	}

	/*
	CONS
	 */

	@Override
	public ConsStruct rplaca(final LispStruct car) {
		this.car = car;
		return this;
	}

	@Override
	public ConsStruct rplacd(final LispStruct cdr) {
		this.cdr = cdr;
		return this;
	}

	/*
	LIST
	 */

	@Override
	public LispStruct car() {
		return car;
	}

	@Override
	public LispStruct setCar(final LispStruct car) {
		this.car = car;
		return car;
	}

	@Override
	public LispStruct cdr() {
		return cdr;
	}

	@Override
	public LispStruct setCdr(final LispStruct cdr) {
		this.cdr = cdr;
		return cdr;
	}

	@Override
	public ListStruct copyList() {
		final LispStruct copyCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyList() : cdr;
		return ConsStruct.toLispCons(car, copyCdr);
	}

	@Override
	public ListStruct copyTree() {
		final LispStruct deepCopyCar = (car instanceof ConsStruct) ? ((ConsStruct) car).copyTree() : car;
		final LispStruct deepCopyCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyTree() : cdr;
		return ConsStruct.toLispCons(deepCopyCar, deepCopyCdr);
	}

	@Override
	public IntegerStruct listLength() {
		int n = 0; // Counter.
		LispStruct fast = this; // Fast pointer: leaps by 2.
		ListStruct slow = this; // Slow pointer: leaps by 1.

		while (true) {
			if (!(fast instanceof ListStruct)) {
				throw new TypeErrorException("Not a proper list.");
			}

			// If fast pointer hits the end, return the count.
			if (NILStruct.INSTANCE.eq(fast)) {
				return IntegerStruct.toLispInteger(n);
			}

			final ListStruct fastList = (ListStruct) fast;
			final LispStruct fastCdr = fastList.cdr();
			if (!(fastCdr instanceof ListStruct)) {
				throw new TypeErrorException("Not a proper list.");
			}
			if (NILStruct.INSTANCE.eq(fastCdr)) {
				return IntegerStruct.toLispInteger(n + 1);
			}

			// If fast pointer eventually equals slow pointer, then we must be stuck in a circular list.
			// (A deeper property is the converse: if we are stuck in a circular list, then eventually the
			//  fast pointer will equal the slow pointer. That fact justifies this implementation.)
			if (fast.eq(slow) && (n > 0)) {
				return IntegerStruct.MINUS_ONE;
			}

			n += 2;
			fast = ((ListStruct) fastCdr).cdr();
			slow = (ListStruct) slow.cdr();
		}
	}

	@Override
	public LispStruct nth(final FixnumStruct index) {
		if (index.minusp()) {
			throw new TypeErrorException("N value must be non-negative.");
		}
		final int indexInt = index.toJavaInt();

		int currentIndex = 0;
		ListStruct list = this;
		while (true) {
			if (currentIndex == indexInt) {
				return list.car();
			}

			final LispStruct listCdr = list.cdr();
			if (listCdr instanceof ListStruct) {
				list = (ListStruct) listCdr;
				currentIndex++;
			} else {
				throw new ErrorException("Cannot retrieve index " + currentIndex + " from " + listCdr);
			}
		}
	}

	@Override
	public LispStruct setNth(final FixnumStruct index, final LispStruct newValue) {
		if (index.minusp()) {
			throw new TypeErrorException("N value must be non-negative.");
		}
		final int indexInt = index.toJavaInt();

		int currentIndex = 0;
		ListStruct list = this;
		while (true) {
			if (currentIndex == indexInt) {
				if (NILStruct.INSTANCE.eq(list)) {
					throw new SimpleErrorException("Cannot set element within NIL.");
				}
				((ConsStruct) list).rplaca(newValue);
				return newValue;
			}

			final LispStruct listCdr = list.cdr();
			if (listCdr instanceof ListStruct) {
				list = (ListStruct) listCdr;
				currentIndex++;
			} else {
				throw new ErrorException("Cannot retrieve index " + currentIndex + " from " + listCdr);
			}
		}
	}

	@Override
	public ListStruct nthCdr(final FixnumStruct index) {
		if (index.minusp()) {
			throw new TypeErrorException("N value must be non-negative.");
		}
		final int indexInt = index.toJavaInt();

		int currentIndex = 0;
		ListStruct list = this;
		while (true) {
			if (currentIndex == indexInt) {
				return list;
			}

			final LispStruct listCdr = list.cdr();
			if (listCdr instanceof ListStruct) {
				list = (ListStruct) listCdr;
				currentIndex++;
			} else {
				throw new ErrorException("Cannot retrieve index " + currentIndex + " from " + listCdr);
			}
		}
	}

	@Override
	public BooleanStruct endP() {
		return BooleanStruct.NIL;
	}

	@Override
	public boolean tailp(final LispStruct object) {
		if (eql(object)) {
			return true;
		}
		if (cdr instanceof ListStruct) {
			final ListStruct listCdr = (ListStruct) cdr;
			return listCdr.tailp(object);
		}
		return cdr.eql(object);
	}

	@Override
	public ListStruct ldiff(final LispStruct object) {
		if (eql(object)) {
			return NILStruct.INSTANCE;
		}
		if (cdr instanceof ListStruct) {
			final ListStruct listCdr = (ListStruct) cdr;
			return ConsStruct.toLispCons(car, listCdr.ldiff(object));
		}
		return (cdr.eql(object)) ? ConsStruct.toLispCons(car, NILStruct.INSTANCE) : ConsStruct.toLispCons(car, cdr);
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}
		final boolean carIsIndicator = indicators.stream().anyMatch(indicator -> car.eq(indicator));

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (carIsIndicator) {
			return ValuesStruct.valueOf(car, cdrCons.car(), this);
		} else if (!(cdrCons.cdr() instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr();
			return cdrCdrList.getProperties(indicators);
		}
	}

	@Override
	public LispStruct getf(final LispStruct indicator, final LispStruct defaultValue) {
		final Iterator<LispStruct> iterator = iterator();
		while (iterator.hasNext()) {
			final LispStruct key = iterator.next();
			if (!iterator.hasNext()) {
				return defaultValue;
			}
			final LispStruct value = iterator.next();
			if (key.eq(indicator)) {
				return value;
			}
		}
		return defaultValue;
	}

	@Override
	public ListStruct putf(final LispStruct indicator, final LispStruct newValue) {
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (car.eq(indicator)) {
			cdrCons.rplaca(newValue);
		} else if (!(cdrCons.cdr() instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr();
			cdrCdrList.putf(indicator, newValue);
		}
		return this;
	}

	@Override
	public LispStruct last(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		LispStruct currentList = this;
		LispStruct returnList = currentList;

		long index = 0L;
		while (currentList instanceof ConsStruct) {
			if (n <= index) {
				// NOTE: We know this will be safe as the 'cdr' operation will always be performed on the currentList
				//          first; the only time this isn't true is if 'n' is 0, in which case, the returnList and the
				//          currentList refer to the same object, 'this', which we already know to be a ConsStruct.
				final ConsStruct returnCons = (ConsStruct) returnList;
				returnList = returnCons.cdr();
			}

			final ConsStruct currentCons = (ConsStruct) currentList;
			currentList = currentCons.cdr();
			index++;
		}
		return returnList;
	}

	@Override
	public ListStruct butLast(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final LispStruct listLength = listLength();
		if (listLength == NILStruct.INSTANCE) {
			return NILStruct.INSTANCE;
		}
		final long listLengthInt = ((IntegerStruct) listLength).toJavaPLong();
		if (listLengthInt < n) {
			return NILStruct.INSTANCE;
		}

		final long limit = listLengthInt - n;
		return butLastAux(this, limit);
	}

	private static ListStruct butLastAux(final ListStruct list, final long limit) {
		if (limit <= 0) {
			return NILStruct.INSTANCE;
		} else {
			final LispStruct listCar = list.getCar();
			final LispStruct listCdr = list.getCdr();
			// NOTE: We know this will be safe due to the use of 'listLength()'. Therefore, only 'proper' lists will
			//          pass through here, in which case this is always a good cast.
			return ConsStruct.toLispCons(listCar, butLastAux((ListStruct) listCdr, limit - 1));
		}
	}

	@Override
	public ListStruct nButLast(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final LispStruct listLength = listLength();
		if (listLength == NILStruct.INSTANCE) {
			return NILStruct.INSTANCE;
		}
		final long listLengthInt = ((IntegerStruct) listLength).toJavaPLong();
		if (listLengthInt < n) {
			return NILStruct.INSTANCE;
		}
		if (listLengthInt == 1) {
			return NILStruct.INSTANCE;
		}

		final long limit = listLengthInt - n;
		nButLastAux(this, limit);
		return this;
	}

	private static void nButLastAux(final ListStruct list, final long limit) {
		if (limit <= 0) {
			((ConsStruct) list).rplacd(NILStruct.INSTANCE);
		} else {
			final LispStruct listCdr = list.getCdr();
			// NOTE: We know this will be safe due to the use of 'listLength()'. Therefore, only 'proper' lists will
			//          pass through here, in which case this is always a good cast.
			nButLastAux((ListStruct) listCdr, limit - 1);
		}
	}

	@Override
	public ListStruct copyAlist() {
		final LispStruct copyCar;
		if (car instanceof ConsStruct) {
			final ConsStruct assocCar = (ConsStruct) car;
			copyCar = ConsStruct.toLispCons(assocCar.car(), assocCar.cdr());
		} else {
			copyCar = car;
		}
		final LispStruct copyCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyAlist() : cdr;
		return ConsStruct.toLispCons(copyCar, copyCdr);
	}

	@Override
	public List<LispStruct> toJavaList() {
		final List<LispStruct> list = new ArrayList<>();

		LispStruct currentCar = car;
		LispStruct currentCdr = cdr;
		while (true) {
			list.add(currentCar);

			if (NILStruct.INSTANCE.eq(currentCdr)) {
				break;
			} else if (currentCdr instanceof ConsStruct) {
				currentCar = ((ConsStruct) currentCdr).car();
				currentCdr = ((ConsStruct) currentCdr).cdr();
			} else {
				list.add(currentCdr);
				break;
			}
		}
		return list;
	}

	/*
	SEQUENCE
	 */

	@Override
	public Stream<LispStruct> stream() {
		return StreamSupport.stream(spliterator(), false);
	}

	@Override
	public Stream<LispStruct> parallelStream() {
		return StreamSupport.stream(spliterator(), true);
	}

	@Override
	public LispStruct[] toArray() {
		final LispStruct[] result = new LispStruct[length().toJavaInt()];
		int i = 0;

		for (LispStruct x = this; x instanceof ConsStruct; x = ((ConsStruct) x).cdr()) {
			final ConsStruct xCons = (ConsStruct) x;
			result[i++] = xCons.car();
		}
		return result;
	}

	@Override
	public IntegerStruct length() {
		long length = 1L;
		LispStruct obj = cdr;
		while (!NILStruct.INSTANCE.eq(obj)) {
			++length;
			if (obj instanceof ConsStruct) {
				obj = ((ConsStruct) obj).cdr();
			} else {
				throw new TypeErrorException("Not a proper list.");
			}
		}
		return IntegerStruct.toLispInteger(length);
	}

	@Override
	public LispStruct elt(final IntegerStruct index) {
		final long longIndex = index.toJavaPLong();
		if (longIndex < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		long currentIndex = 0L;
		ConsStruct cons = this;
		while (true) {
			if (currentIndex == longIndex) {
				return cons.car();
			}

			final LispStruct consCdr = cons.cdr();
			if (consCdr instanceof ConsStruct) {
				cons = (ConsStruct) consCdr;
				currentIndex++;
			} else if (NILStruct.INSTANCE.eq(consCdr)) {
				throw new TypeErrorException("Index is too large.");
			} else {
				throw new TypeErrorException("Not a proper list.");
			}
		}
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		final long longIndex = index.toJavaPLong();
		if (longIndex < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		long currentIndex = 0L;
		ConsStruct cons = this;
		while (true) {
			if (currentIndex == longIndex) {
				cons.rplaca(newElement);
				return newElement;
			}

			final LispStruct consCdr = cons.cdr();
			if (consCdr instanceof ConsStruct) {
				cons = (ConsStruct) consCdr;
				currentIndex++;
			} else if (NILStruct.INSTANCE.eq(consCdr)) {
				throw new TypeErrorException("Index is too large.");
			} else {
				throw new TypeErrorException("Not a proper list.");
			}
		}
	}

	@Override
	public ListStruct reverse() {
		LispStruct current = this;
		ListStruct result = NILStruct.INSTANCE;
		while (current instanceof ConsStruct) {
			final ConsStruct currentCons = (ConsStruct) current;
			result = ConsStruct.toLispCons(currentCons.car(), result);
			current = currentCons.cdr();
		}
		if (!NILStruct.INSTANCE.eq(current)) {
			throw new TypeErrorException("Not a proper list.");
		}
		return result;
	}

	@Override
	public ListStruct nReverse() {
		ConsStruct cons = this;

		ListStruct prevCons = NILStruct.INSTANCE;
		LispStruct nextCons = cdr;
		while (nextCons instanceof ConsStruct) {
			final ConsStruct nextConsAsCons = (ConsStruct) nextCons;
			cons.rplacd(prevCons);
			prevCons = cons;
			cons = nextConsAsCons;
			nextCons = nextConsAsCons.cdr();
		}
		cons.rplacd(prevCons);

		if (!NILStruct.INSTANCE.eq(nextCons)) {
			throw new TypeErrorException("Not a proper list.");
		}

		return cons;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new ConsIterator(this);
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(),
		                                listLength().toJavaPLong(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.NONNULL |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-PRETTY* and the pretty printer in general right now...

		if (IntegerStruct.MINUS_ONE.eq(listLength())) {
			return "CIRCULAR LIST PRINTING NOT YET SUPPORTED!!!";
		}

		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append('(');

		printElements(this, stringBuilder);

		stringBuilder.append(')');

		return stringBuilder.toString();
	}

	private static String printElements(final ConsStruct object, final StringBuilder stringBuilder) {

		final LispStruct car = object.car();
		final String printedCar = car.toString();

		stringBuilder.append(printedCar);

		if (object.cdr() instanceof ConsStruct) {
			final ConsStruct cdrAsCons = (ConsStruct) object.cdr();
			final String innerConsPrinted = printElements(cdrAsCons, new StringBuilder());

			stringBuilder.append(' ');
			stringBuilder.append(innerConsPrinted);
		} else if (!object.cdr().eq(NILStruct.INSTANCE)) {
			stringBuilder.append(" . ");

			final LispStruct cdr = object.cdr();
			final String printedCdr = cdr.toString();

			stringBuilder.append(printedCdr);
		}

		return stringBuilder.toString();
	}

	private static final class ConsIterator implements Iterator<LispStruct> {

		private ListStruct current;

		private ConsIterator(final ConsStruct cons) {
			current = cons;
		}

		@Override
		public boolean hasNext() {
			return current instanceof ConsStruct;
		}

		@Override
		public LispStruct next() {
			if (!hasNext()) {
				throw new NoSuchElementException("No elements left in the Cons.");
			}
			final ConsStruct currentAsCons = (ConsStruct) current;

			final LispStruct currentCdr = currentAsCons.cdr();
			if (!(currentCdr instanceof ListStruct)) {
				throw new TypeErrorException("Not a proper list.");
			}
			current = (ListStruct) currentCdr;
			return currentAsCons.car();
		}
	}

	/*
	OLD
	 */

	@Override
	@Deprecated
	public LispStruct getCar() {
		return car;
	}

	@Override
	@Deprecated
	public LispStruct getCdr() {
		return cdr;
	}

	@Override
	@Deprecated
	public LispStruct getProperty(final LispStruct indicator, final LispStruct defaultValue) {
		final Iterator<LispStruct> iterator = iterator();
		while (iterator.hasNext()) {
			final LispStruct key = iterator.next();
			if (!iterator.hasNext()) {
				return defaultValue;
			}
			final LispStruct value = iterator.next();
			if (key.eq(indicator)) {
				return value;
			}
		}
		return defaultValue;
	}

	@Override
	@Deprecated
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (car.eq(indicator)) {
			cdrCons.rplaca(newValue);
		} else if (!(cdrCons.cdr() instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr();
			cdrCdrList.setProperty(indicator, newValue);
		}
		return this;
	}

	@Override
	@Deprecated
	public boolean removeProperty(final LispStruct indicator) {
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (car.eq(indicator)) {
			if (NILStruct.INSTANCE.eq(cdrCons.cdr())) {
				throw new ErrorException("Cannot remove last entry from property list.");
			}
			if (!(cdrCons.cdr() instanceof ConsStruct)) {
				throw new ErrorException("List is not a valid property list.");
			}
			final ConsStruct cdrCdrCons = (ConsStruct) cdrCons.cdr();
			car = cdrCdrCons.car();
			cdr = cdrCdrCons.cdr();
		} else if (!(cdrCons.cdr() instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr();
			return cdrCdrList.removeProperty(indicator);
		}
		return false;
	}

	@Override
	@Deprecated
	public boolean isDotted() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.isDotted();
		}
		return true;
	}
}
