package jcl.lists;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.symbols.NILStruct;
import jcl.types.ConsType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link ConsStruct} is the object representation of a Lisp 'cons' type.
 */
public class ConsStruct extends BuiltInClassStruct implements ListStruct {

	private LispStruct car;

	private LispStruct cdr;

	/**
	 * Public constructor.
	 *
	 * @param car
	 * 		the car of the binary cons structure
	 */
	public ConsStruct(final LispStruct car) {
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
	public ConsStruct(final LispStruct car, final LispStruct cdr) {
		super(ConsType.INSTANCE, null, null);
		this.car = car;
		if (NILStruct.INSTANCE.equals(cdr)) {
			this.cdr = NILStruct.INSTANCE;
		} else {
			this.cdr = cdr;
		}
	}

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
	public void setCdr(final LispStruct cdr) {
		this.cdr = cdr;
	}

	@Override
	public int size() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return 1 + cdrAsList.size();
		} else {
			return 2;
		}
	}

	@Override
	public LispStruct getFirst() {
		return car;
	}

	@Override
	public ListStruct getRest() {
		return (cdr instanceof ListStruct) ? (ListStruct) cdr : new ConsStruct(car);
	}

	@Override
	public ListStruct getLast() {
		return (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).getLast() : this;
	}

	@Override
	public ListStruct getAllButLast() {
		if (!(cdr instanceof ConsStruct)) {
			return NILStruct.INSTANCE;
		}

		final ConsStruct cdrAsCons = (ConsStruct) cdr;
		final ListStruct newCdr = cdrAsCons.getAllButLast();

		return new ConsStruct(car, newCdr);
	}

	@Override
	public LispStruct getElement(final int index) {
		if (index == 0) {
			return car;
		} else if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.getElement(index - 1);
		} else {
			return cdr;
		}
	}

	@Override
	public void setElement(final int index, final LispStruct newValue) {
		if (index == 0) {
			car = newValue;
		} else if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			cdrAsList.setElement(index - 1, newValue);
		} else {
			cdr = newValue;
		}
	}

	@Override
	public boolean isDotted() {
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			return cdrAsList.isDotted();
		}
		return true;
	}

	@Override
	public boolean isCircular() {
		final Set<ConsStruct> conses = new HashSet<>();
		conses.add(this);

		return innerIsCircular(this, conses);
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		final List<LispStruct> javaList = new ArrayList<>();
		javaList.add(car);
		if (cdr instanceof ListStruct) {
			final ListStruct cdrAsList = (ListStruct) cdr;
			javaList.addAll(cdrAsList.getAsJavaList());
		} else {
			javaList.add(cdr);
		}
		return javaList;
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
	private static boolean innerIsCircular(final ConsStruct consStruct, final Set<ConsStruct> conses) {
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
	private static boolean isElementCircular(final LispStruct element, final Set<ConsStruct> conses) {
		final boolean isElementCircular;
		if (element instanceof ConsStruct) {
			final ConsStruct elementAsCons = (ConsStruct) element;
			if (conses.contains(elementAsCons)) {
				return true;
			}

			conses.add(elementAsCons);
			isElementCircular = innerIsCircular(elementAsCons, conses);
			conses.remove(elementAsCons);
		} else {
			isElementCircular = false;
		}
		return isElementCircular;
	}

	@Override
	public int hashCode() {
//		if (isCircular()) {
//		    // TODO: we should figure out how to handle circularities here... or should we???
//		    return new HashCodeBuilder().appendSuper(super.hashCode())
//		                                .toHashCode();
//		} else {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(car)
		                            .append(cdr)
		                            .toHashCode();
//		}
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ConsStruct rhs = (ConsStruct) obj;

//		if (isCircular() || rhs.isCircular()) {
//			// TODO: we should figure out how to handle circularities here... or should we???
//			return false;
//		} else {
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(car, rhs.car)
		                          .append(cdr, rhs.cdr)
		                          .isEquals();
//		}
	}

	@Override
	public String toString() {
		if (isCircular()) {
			return "ConsStruct{'circular'}";
		} else {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(car)
			                                                                .append(cdr)
			                                                                .toString();
		}
	}

	@Override
	public Iterator<LispStruct> iterator() {
		return new ConsIterator(this);
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(), size(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.NONNULL |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
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
		final LispStruct[] result = new LispStruct[size()];
		int i = 0;

		for (LispStruct x = this; x instanceof ConsStruct; x = ((ConsStruct) x).cdr) {
			final ConsStruct xCons = (ConsStruct) x;
			result[i++] = xCons.car;
		}
		return result;
	}

	@Override
	public ListStruct copyTree() {
		final LispStruct deepCar = (car instanceof ConsStruct) ? ((ConsStruct) car).copyTree() : car;
		final LispStruct deepCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyTree() : cdr;
		return new ConsStruct(deepCar, deepCdr);
	}

	@Override
	public ListStruct copyList() {
		final LispStruct copyCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyList() : cdr;
		return new ConsStruct(car, copyCdr);
	}

	@Override
	public ListStruct copyAlist() {
		final LispStruct copyCar;
		if (car instanceof ConsStruct) {
			final ConsStruct assocCar = (ConsStruct) car;
			copyCar = new ConsStruct(assocCar.car, assocCar.cdr);
		} else {
			copyCar = car;
		}
		final LispStruct copyCdr = (cdr instanceof ConsStruct) ? ((ConsStruct) cdr).copyAlist() : cdr;
		return new ConsStruct(copyCar, copyCdr);
	}

	@Override
	public Long listLength() {
		if (isDotted()) {
			throw new TypeErrorException("Not a proper or circular list.");
		}
		if (isCircular()) {
			return null;
		}
		return (long) size();
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
			return new ConsStruct(car, listCdr.ldiff(object));
		}
		return (cdr.eql(object)) ? new ConsStruct(car) : new ConsStruct(car, cdr);
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
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStruct cdrCons = (ConsStruct) cdr;
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
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (car.eq(indicator)) {
			if (NILStruct.INSTANCE.equals(cdrCons.cdr)) {
				throw new ErrorException("Cannot remove last entry from property list.");
			}
			if (!(cdrCons.cdr instanceof ConsStruct)) {
				throw new ErrorException("List is not a valid property list.");
			}
			final ConsStruct cdrCdrCons = (ConsStruct) cdrCons.cdr;
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
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}
		final boolean carIsIndicator = indicators.stream().anyMatch(indicator -> car.eq(indicator));

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (carIsIndicator) {
			return new ValuesStruct(car, cdrCons.car, this);
		} else if (!(cdrCons.cdr instanceof ListStruct)) {
			throw new ErrorException("List is not a valid property list.");
		} else {
			final ListStruct cdrCdrList = (ListStruct) cdrCons.cdr;
			return cdrCdrList.getProperties(indicators);
		}
	}

	@Override
	public LispStruct last(final long n) {
		LispStruct currentList = this;
		LispStruct returnList = currentList;

		long index = 0L;
		while (currentList instanceof ConsStruct) {
			if (n <= index) {
				// NOTE: We know this will be safe as the 'cdr' operation will always be performed on the currentList
				//          first; the only time this isn't true is if 'n' is 0, in which case, the returnList and the
				//          currentList refer to the same object, 'this', which we already know to be a ConsStruct.
				final ConsStruct returnCons = (ConsStruct) returnList;
				returnList = returnCons.cdr;
			}

			final ConsStruct currentCons = (ConsStruct) currentList;
			currentList = currentCons.cdr;
			index++;
		}
		return returnList;
	}

	@Override
	public boolean eql(final LispStruct object) {
		// TODO: Fix this when we fix 'eql' for everything
		return eq(object);
	}

	@Override
	public boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ConsStruct) {
			final ConsStruct objectCons = (ConsStruct) object;
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
		if (object instanceof ConsStruct) {
			final ConsStruct objectCons = (ConsStruct) object;
			if (car.equalp(objectCons.car) && cdr.equalp(objectCons.cdr)) {
				return true;
			}
		}
		return false;
	}

	private static final class ConsIterator implements Iterator<LispStruct> {

		private final int totalSize;

		private ConsStruct previous;
		private LispStruct current;
		private int nextIndex;

		private ConsIterator(final ConsStruct cons) {
			totalSize = cons.size();
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

			previous = currentAsCons;
			current = currentAsCons.getCdr();
			nextIndex++;
			return previous.getCar();
		}

		@Override
		public void forEachRemaining(final Consumer<? super LispStruct> action) {
			Objects.requireNonNull(action);
			while (nextIndex < totalSize) {
				final ConsStruct currentAsCons = (ConsStruct) current;
				action.accept(currentAsCons.getCar());
				previous = currentAsCons;
				current = currentAsCons.getCdr();
				nextIndex++;
			}
		}
	}
}
