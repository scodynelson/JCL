package jcl.lang;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.internal.BooleanStructImpl;
import jcl.type.NILType;
import org.apache.commons.lang3.ArrayUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

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
	public FixnumStruct listLength() {
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
	public boolean endP() {
		return true;
	}

	@Override
	public boolean tailp(final LispStruct object) {
		return INSTANCE.eq(object);
	}

	@Override
	public ListStruct ldiff(final LispStruct object) {
		return INSTANCE;
	}

	@Override
	public ValuesStruct getProperties(final ListStruct indicators) {
		return ValuesStruct.valueOf(INSTANCE, INSTANCE, INSTANCE);
	}

	@Override
	public LispStruct getf(final LispStruct indicator, final LispStruct defaultValue) {
		return INSTANCE;
	}

	@Override
	public ListStruct putf(final LispStruct indicator, final LispStruct newValue) {
		return ConsStruct.toLispCons(indicator, newValue);
	}

	@Override
	public boolean remf(final LispStruct indicator) {
		return false;
	}

	@Override
	public ListStruct copyAlist() {
		return INSTANCE;
	}

	@Override
	public LispStruct last(final FixnumStruct n) {
		return INSTANCE;
	}

	@Override
	public ListStruct butLast(final FixnumStruct n) {
		return INSTANCE;
	}

	@Override
	public ListStruct nButLast(final FixnumStruct n) {
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
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link NILStruct} objects, by retrieving the static singleton {@link NILStruct#INSTANCE}.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC,
		                  GenerationConstants.NIL_STRUCT_NAME,
		                  GenerationConstants.SINGLETON_INSTANCE,
		                  GenerationConstants.NIL_STRUCT_DESC);
	}

	/*
	OLD
	 */

	@Override
	@Deprecated
	public boolean isDotted() {
		return false;
	}
}
