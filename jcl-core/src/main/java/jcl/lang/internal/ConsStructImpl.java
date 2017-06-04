package jcl.lang.internal;

import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.ConsType;

/**
 * The {@link ConsStructImpl} is the object representation of a Lisp 'cons' type.
 */
public final class ConsStructImpl extends BuiltInClassStruct implements ConsStruct {

	private LispStruct car;

	private LispStruct cdr;

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 */
	private ConsStructImpl(final LispStruct car) {
		this(car, NILStruct.INSTANCE);
	}

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 * @param cdr
	 * 		the cdr of the binary cons structure
	 */
	private ConsStructImpl(final LispStruct car, final LispStruct cdr) {
		super(ConsType.INSTANCE, null, null);
		this.car = car;
		if (NILStruct.INSTANCE.eq(cdr)) {
			this.cdr = NILStruct.INSTANCE;
		} else {
			this.cdr = cdr;
		}
	}

	public static ConsStruct valueOf(final LispStruct car) {
		return new ConsStructImpl(car);
	}

	public static ConsStruct valueOf(final LispStruct car, final LispStruct cdr) {
		return new ConsStructImpl(car, cdr);
	}

	/*
	LIST
	 */

	/**
	 * Getter for cons {@link #car} property.
	 *
	 * @return cons {@link #car} property
	 */
	@Override
	public LispStruct getCar() {
		return car;
	}

	/**
	 * Setter for cons {@link #car} property.
	 *
	 * @param car
	 * 		new cons {@link #car} property value
	 */
	@Override
	public void setCar(final LispStruct car) {
		this.car = car;
	}

	/**
	 * Getter for cons {@link #cdr} property.
	 *
	 * @return cons {@link #cdr} property
	 */
	@Override
	public LispStruct getCdr() {
		return cdr;
	}

	/**
	 * Setter for cons {@link #cdr} property.
	 *
	 * @param cdr
	 * 		new cons {@link #cdr} property value
	 */
	@Override
	public void setCdr(final LispStruct cdr) {
		this.cdr = cdr;
	}

	@Override
	public boolean isDotted() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.isDotted();
		}
		return true;
	}

	private static final Object PRESENT = new Object();

	@Override
	public boolean isCircular() {
		final Map<ConsStructImpl, Object> conses = new IdentityHashMap<>();
		conses.put(this, PRESENT);

		return innerIsCircular(this, conses);
	}

	/**
	 * Tests the provided {@code consStruct} for circularity. If the consStruct itself, or any of its cons nodes are
	 * located in the provided conses list, the cons is circular. This method is recursive and will constantly populate
	 * the {@code conses} set with node values for testing.
	 *
	 * @param consStruct
	 * 		the cons structure to test for circularity
	 * @param conses
	 * 		the set of cons nodes currently found in the tree
	 *
	 * @return whether or not the provided {@code consStruct} is circular
	 */
	private static boolean innerIsCircular(final ConsStructImpl consStruct, final Map<ConsStructImpl, Object> conses) {
		return isElementCircular(consStruct.car, conses) || isElementCircular(consStruct.cdr, conses);
	}

	/**
	 * Tests the provided {@code element} for circularity. If the element is a consStruct, it tests it for circularity
	 * appropriately.
	 *
	 * @param element
	 * 		the element structure to test for circularity
	 * @param conses
	 * 		the set of cons nodes currently found in the tree
	 *
	 * @return whether or not the provided {@code element} is circular
	 */
	private static boolean isElementCircular(final LispStruct element, final Map<ConsStructImpl, Object> conses) {
		final boolean isElementCircular;
		if (element instanceof ConsStructImpl) {
			final ConsStructImpl elementAsCons = (ConsStructImpl) element;
			if (conses.containsKey(elementAsCons)) {
				return true;
			}

			conses.put(elementAsCons, PRESENT);
			isElementCircular = innerIsCircular(elementAsCons, conses);
			conses.remove(elementAsCons);
		} else {
			isElementCircular = false;
		}
		return isElementCircular;
	}

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

		for (LispStruct x = this; x instanceof ConsStructImpl; x = ((ConsStructImpl) x).cdr) {
			final ConsStructImpl xCons = (ConsStructImpl) x;
			result[i++] = xCons.car;
		}
		return result;
	}

	@Override
	public ListStruct copyTree() {
		final LispStruct deepCar = (car instanceof ConsStructImpl) ? ((ConsStructImpl) car).copyTree() : car;
		final LispStruct deepCdr = (cdr instanceof ConsStructImpl) ? ((ConsStructImpl) cdr).copyTree() : cdr;
		return new ConsStructImpl(deepCar, deepCdr);
	}

	@Override
	public ListStruct copyList() {
		final LispStruct copyCdr = (cdr instanceof ConsStructImpl) ? ((ConsStructImpl) cdr).copyList() : cdr;
		return new ConsStructImpl(car, copyCdr);
	}

	@Override
	public ListStruct copyAlist() {
		final LispStruct copyCar;
		if (car instanceof ConsStructImpl) {
			final ConsStructImpl assocCar = (ConsStructImpl) car;
			copyCar = new ConsStructImpl(assocCar.car, assocCar.cdr);
		} else {
			copyCar = car;
		}
		final LispStruct copyCdr = (cdr instanceof ConsStructImpl) ? ((ConsStructImpl) cdr).copyAlist() : cdr;
		return new ConsStructImpl(copyCar, copyCdr);
	}

	@Override
	public Long listLength() {
		if (isDotted()) {
			throw new TypeErrorException("Not a proper or circular list.");
		}
		if (isCircular()) {
			return null;
		}
		return length().toJavaPLong();
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
			return new ConsStructImpl(car, listCdr.ldiff(object));
		}
		return (cdr.eql(object)) ? new ConsStructImpl(car) : new ConsStructImpl(car, cdr);
	}

	@Override
	public LispStruct nth(final long index) {
		if (index < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		long currentIndex = 0L;
		ConsStructImpl cons = this;
		while (true) {
			if (currentIndex == index) {
				return cons.car;
			}

			final LispStruct consCdr = cons.cdr;
			if (consCdr instanceof ConsStructImpl) {
				cons = (ConsStructImpl) consCdr;
				currentIndex++;
			} else if (NILStruct.INSTANCE.eq(consCdr)) {
				return NILStruct.INSTANCE;
			} else {
				throw new TypeErrorException("Not a proper list.");
			}
		}
	}

	@Override
	public void setNth(final long index, final LispStruct newValue) {
		if (index < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		long currentIndex = 0L;
		ConsStructImpl cons = this;
		while (true) {
			if (currentIndex == index) {
				cons.car = newValue;
				return;
			}

			final LispStruct consCdr = cons.cdr;
			if (consCdr instanceof ConsStructImpl) {
				cons = (ConsStructImpl) consCdr;
				currentIndex++;
			} else if (NILStruct.INSTANCE.eq(consCdr)) {
				return;
			} else {
				throw new TypeErrorException("Not a proper list.");
			}
		}
	}

	@Override
	public ListStruct nthCdr(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}
		if (n == 0) {
			return this;
		}
		if (cdr instanceof ListStruct) {
			final ListStruct listCdr = (ListStruct) cdr;
			return listCdr.nthCdr(n - 1);
		} else {
			throw new ErrorException("Cannot take CDR of " + cdr);
		}
	}

	@Override
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
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		if (!(cdr instanceof ConsStructImpl)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStructImpl cdrCons = (ConsStructImpl) cdr;
		if (car.eq(indicator)) {
			cdrCons.car = newValue;
		} else if (!(cdrCons.cdr instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr;
			cdrCdrList.setProperty(indicator, newValue);
		}
		return this;
	}

	@Override
	public boolean removeProperty(final LispStruct indicator) {
		if (!(cdr instanceof ConsStructImpl)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStructImpl cdrCons = (ConsStructImpl) cdr;
		if (car.eq(indicator)) {
			if (NILStruct.INSTANCE.eq(cdrCons.cdr)) {
				throw new ErrorException("Cannot remove last entry from property list.");
			}
			if (!(cdrCons.cdr instanceof ConsStructImpl)) {
				throw new ErrorException("List is not a valid property list.");
			}
			final ConsStructImpl cdrCdrCons = (ConsStructImpl) cdrCons.cdr;
			car = cdrCdrCons.car;
			cdr = cdrCdrCons.cdr;
		} else if (!(cdrCons.cdr instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr;
			return cdrCdrList.removeProperty(indicator);
		}
		return false;
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		if (!(cdr instanceof ConsStructImpl)) {
			throw new ErrorException("List is not a valid property list.");
		}
		final boolean carIsIndicator = indicators.stream().anyMatch(indicator -> car.eq(indicator));

		final ConsStructImpl cdrCons = (ConsStructImpl) cdr;
		if (carIsIndicator) {
			return ValuesStruct.valueOf(car, cdrCons.car, this);
		} else if (!(cdrCons.cdr instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr;
			return cdrCdrList.getProperties(indicators);
		}
	}

	@Override
	public LispStruct last(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		LispStruct currentList = this;
		LispStruct returnList = currentList;

		long index = 0L;
		while (currentList instanceof ConsStructImpl) {
			if (n <= index) {
				// NOTE: We know this will be safe as the 'cdr' operation will always be performed on the currentList
				//          first; the only time this isn't true is if 'n' is 0, in which case, the returnList and the
				//          currentList refer to the same object, 'this', which we already know to be a ConsStruct.
				final ConsStructImpl returnCons = (ConsStructImpl) returnList;
				returnList = returnCons.cdr;
			}

			final ConsStructImpl currentCons = (ConsStructImpl) currentList;
			currentList = currentCons.cdr;
			index++;
		}
		return returnList;
	}

	@Override
	public ListStruct butLast(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final long listLength = listLength();
		if (listLength < n) {
			return NILStruct.INSTANCE;
		}

		final long limit = listLength - n;
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
			return new ConsStructImpl(listCar, butLastAux((ListStruct) listCdr, limit - 1));
		}
	}

	@Override
	public ListStruct nButLast(final long n) {
		if (n < 0) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final long listLength = listLength();
		if (listLength < n) {
			return NILStruct.INSTANCE;
		}
		if (listLength == 1) {
			return NILStruct.INSTANCE;
		}

		final long limit = listLength - n;
		nButLastAux(this, limit);
		return this;
	}

	private static void nButLastAux(final ListStruct list, final long limit) {
		if (limit <= 0) {
			((ConsStructImpl) list).cdr = NILStruct.INSTANCE;
		} else {
			final LispStruct listCdr = list.getCdr();
			// NOTE: We know this will be safe due to the use of 'listLength()'. Therefore, only 'proper' lists will
			//          pass through here, in which case this is always a good cast.
			nButLastAux((ListStruct) listCdr, limit - 1);
		}
	}

	/*
	SEQUENCE
	 */

	@Override
	public IntegerStruct length() {
		long length = 1L;
		LispStruct obj = cdr;
		while (!NILStruct.INSTANCE.eq(obj)) {
			++length;
			if (obj instanceof ConsStructImpl) {
				obj = ((ConsStructImpl) obj).cdr;
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
		ConsStructImpl cons = this;
		while (true) {
			if (currentIndex == longIndex) {
				return cons.car;
			}

			final LispStruct consCdr = cons.cdr;
			if (consCdr instanceof ConsStructImpl) {
				cons = (ConsStructImpl) consCdr;
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
		ConsStructImpl cons = this;
		while (true) {
			if (currentIndex == longIndex) {
				cons.car = newElement;
				return newElement;
			}

			final LispStruct consCdr = cons.cdr;
			if (consCdr instanceof ConsStructImpl) {
				cons = (ConsStructImpl) consCdr;
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
		while (current instanceof ConsStructImpl) {
			final ConsStructImpl currentCons = (ConsStructImpl) current;
			result = new ConsStructImpl(currentCons.car, result);
			current = currentCons.cdr;
		}
		if (!NILStruct.INSTANCE.eq(current)) {
			throw new TypeErrorException("Not a proper list.");
		}
		return result;
	}

	@Override
	public ListStruct nReverse() {
		ConsStructImpl cons = this;

		ListStruct prevCons = NILStruct.INSTANCE;
		LispStruct nextCons = cdr;
		while (nextCons instanceof ConsStructImpl) {
			final ConsStructImpl nextConsAsCons = (ConsStructImpl) nextCons;
			cons.cdr = prevCons;
			prevCons = cons;
			cons = nextConsAsCons;
			nextCons = nextConsAsCons.cdr;
		}
		cons.cdr = prevCons;

		if (!NILStruct.INSTANCE.eq(nextCons)) {
			throw new TypeErrorException("Not a proper list.");
		}

		return cons;
	}

	/*
	LISP_STRUCT
	 */

	@Override
	public boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ConsStructImpl) {
			final ConsStructImpl objectCons = (ConsStructImpl) object;
			if (car.equal(objectCons.car) && cdr.equal(objectCons.cdr)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ConsStructImpl) {
			final ConsStructImpl objectCons = (ConsStructImpl) object;
			if (car.equalp(objectCons.car) && cdr.equalp(objectCons.cdr)) {
				return true;
			}
		}
		return false;
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
		                                length().toJavaPLong(),
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

		if (isCircular()) {
			return "CIRCULAR LIST PRINTING NOT YET SUPPORTED!!!";
		}

		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append('(');

		printElements(this, stringBuilder);

		stringBuilder.append(')');

		return stringBuilder.toString();
	}

	private static String printElements(final ConsStructImpl object, final StringBuilder stringBuilder) {

		final LispStruct car = object.car;
		final String printedCar = car.toString();

		stringBuilder.append(printedCar);

		if (object.cdr instanceof ConsStructImpl) {
			final ConsStructImpl cdrAsCons = (ConsStructImpl) object.cdr;
			final String innerConsPrinted = printElements(cdrAsCons, new StringBuilder());

			stringBuilder.append(' ');
			stringBuilder.append(innerConsPrinted);
		} else if (!object.cdr.eq(NILStruct.INSTANCE)) {
			stringBuilder.append(" . ");

			final LispStruct cdr = object.cdr;
			final String printedCdr = cdr.toString();

			stringBuilder.append(printedCdr);
		}

		return stringBuilder.toString();
	}

	private static final class ConsIterator implements Iterator<LispStruct> {

		private ListStruct current;

		private ConsIterator(final ConsStructImpl cons) {
			current = cons;
		}

		@Override
		public boolean hasNext() {
			return current instanceof ConsStructImpl;
		}

		@Override
		public LispStruct next() {
			if (!hasNext()) {
				throw new NoSuchElementException("No elements left in the Cons.");
			}
			final ConsStructImpl currentAsCons = (ConsStructImpl) current;

			final LispStruct currentCdr = currentAsCons.getCdr();
			if (!(currentCdr instanceof ListStruct)) {
				throw new TypeErrorException("Not a proper list.");
			}
			current = (ListStruct) currentCdr;
			return currentAsCons.getCar();
		}
	}
}
