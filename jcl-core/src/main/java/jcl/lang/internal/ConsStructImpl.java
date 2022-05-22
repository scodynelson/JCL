package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.GetPropertiesResult;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ConsStructImpl} is the object representation of a Lisp 'cons' type.
 */
public final class ConsStructImpl extends LispStructImpl implements ConsStruct {

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
	public LispStruct cdr() {
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
	public LispStruct listLength() {
		final IntegerStruct lispStruct = innerListLength();
		if (IntegerStruct.MINUS_ONE.eql(lispStruct)) {
			return NILStruct.INSTANCE;
		}
		return lispStruct;
	}

	private FixnumStruct innerListLength() {
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
	public ListStruct nthCdr(final FixnumStruct index) {
		if (index.minusp().toJavaPBoolean()) {
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
	public BooleanStruct endp() {
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct tailp(final LispStruct object) {
		if (eql(object)) {
			return TStruct.INSTANCE;
		}
		if (cdr instanceof ListStruct) {
			final ListStruct listCdr = (ListStruct) cdr;
			return listCdr.tailp(object);
		}
		return BooleanStruct.toLispBoolean(cdr.eql(object));
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
		return cdr.eql(object) ? ConsStruct.toLispCons(car, NILStruct.INSTANCE) : ConsStruct.toLispCons(car, cdr);
	}

	@Override
	public GetPropertiesResult getProperties(final ListStruct indicators) {
		if (!(cdr instanceof ConsStruct)) {
			throw new ErrorException("List is not a valid property list.");
		}
		final boolean carIsIndicator = indicators.stream().anyMatch(indicator -> car.eq(indicator));

		final ConsStruct cdrCons = (ConsStruct) cdr;
		if (carIsIndicator) {
			return new GetPropertiesResult(car, cdrCons.car(), this);
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
	public boolean remf(final LispStruct indicator) {
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
			return cdrCdrList.remf(indicator);
		}
		return false;
	}

	@Override
	public LispStruct last(final FixnumStruct n) {
		if (n.minusp().toJavaPBoolean()) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		LispStruct currentList = this;
		LispStruct returnList = currentList;

		int nInt = n.toJavaInt();
		int index = 0;
		while (currentList instanceof ConsStruct) {
			if (nInt <= index) {
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
	public ListStruct butLast(final FixnumStruct n) {
		if (n.minusp().toJavaPBoolean()) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final FixnumStruct listLength = innerListLength();
		if (IntegerStruct.MINUS_ONE.eql(listLength)) {
			return NILStruct.INSTANCE;
		}
		if (listLength.isLessThan(n)) {
			return NILStruct.INSTANCE;
		}

		final int limit = listLength.toJavaInt() - n.toJavaInt();
		return butLastAux(this, limit);
	}

	private static ListStruct butLastAux(final ListStruct list, final int limit) {
		if (limit <= 0) {
			return NILStruct.INSTANCE;
		} else {
			final LispStruct listCar = list.car();
			final LispStruct listCdr = list.cdr();
			// NOTE: We know this will be safe due to the use of 'listLength()'. Therefore, only 'proper' lists will
			//          pass through here, in which case this is always a good cast.
			return ConsStruct.toLispCons(listCar, butLastAux((ListStruct) listCdr, limit - 1));
		}
	}

	@Override
	public ListStruct nButLast(final FixnumStruct n) {
		if (n.minusp().toJavaPBoolean()) {
			throw new TypeErrorException("N value must be non-negative.");
		}

		final FixnumStruct listLength = innerListLength();
		if (IntegerStruct.MINUS_ONE.eql(listLength)) {
			return NILStruct.INSTANCE;
		}
		if (listLength.isLessThan(n)) {
			return NILStruct.INSTANCE;
		}
		if (IntegerStruct.ONE.eql(listLength)) {
			return NILStruct.INSTANCE;
		}

		final int limit = listLength.toJavaInt() - n.toJavaInt();
		nButLastAux(this, limit);
		return this;
	}

	private static void nButLastAux(final ListStruct list, final int limit) {
		if (limit <= 0) {
			((ConsStruct) list).rplacd(NILStruct.INSTANCE);
		} else {
			final LispStruct listCdr = list.cdr();
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
		                                innerListLength().toJavaPLong(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.NONNULL |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ConsStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ConsStruct#car()} value</li>
	 * <li>Building the {@link ConsStruct#cdr()} value</li>
	 * <li>Constructing a new {@link ConsStruct} with the built car and cdr values</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		car.generate(generatorState);
		final int carStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, carStore);

		cdr.generate(generatorState);
		final int cdrStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, cdrStore);

		mv.visitVarInsn(Opcodes.ALOAD, carStore);
		mv.visitVarInsn(Opcodes.ALOAD, cdrStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.CONS_STRUCT_NAME,
		                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_NAME,
		                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.CONS;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.CONS;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier instanceof SymbolStruct) {
			if (typeSpecifier == CommonLispSymbols.LIST) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == CommonLispSymbols.CONS) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == CommonLispSymbols.SEQUENCE) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == CommonLispSymbols.T) {
				return TStruct.INSTANCE;
			}
		} else if (typeSpecifier instanceof ClassStruct) {
			if (typeSpecifier == BuiltInClassStruct.LIST) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == BuiltInClassStruct.CONS) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == BuiltInClassStruct.SEQUENCE) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == BuiltInClassStruct.CLASS_T) {
				return TStruct.INSTANCE;
			}
		}
		return NILStruct.INSTANCE;
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-PRETTY* and the pretty printer in general right now...

		if ((cdr instanceof ConsStruct) && IntegerStruct.MINUS_ONE.eql(listLength())) {
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
}
