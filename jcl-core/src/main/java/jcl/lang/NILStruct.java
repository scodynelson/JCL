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
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.internal.BooleanStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import org.apache.commons.lang3.ArrayUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStructImpl implements ListStruct {

	/**
	 * Global constant singleton instance of 'nil'.
	 */
	public static final NILStruct INSTANCE = new NILStruct();

	/**
	 * Constant result for the {@link #getProperties(ListStruct)} method.
	 */
	private static final GetPropertiesResult GET_PROPERTIES_RESULT
			= new GetPropertiesResult(INSTANCE, INSTANCE, INSTANCE);

	static {
		INSTANCE.setfSymbolValue(INSTANCE);
		INSTANCE.setConstant();

		GlobalPackageStruct.COMMON_LISP.importSymbol(INSTANCE);
		GlobalPackageStruct.COMMON_LISP.export((SymbolStruct) INSTANCE);
		INSTANCE.setSymbolPackage(GlobalPackageStruct.COMMON_LISP);
	}

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super("NIL", false);
	}

	/*
	LIST-STRUCT
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
	public GetPropertiesResult getProperties(final ListStruct indicators) {
		return GET_PROPERTIES_RESULT;
	}

	@Override
	public LispStruct getf(final LispStruct indicator, final LispStruct defaultValue) {
		return INSTANCE;
	}

	@Override
	public ListStruct putf(final LispStruct indicator, final LispStruct newValue) {
		return ConsStruct.toLispCons(indicator, ConsStruct.toLispCons(newValue, INSTANCE));
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
	SEQUENCE-STRUCT
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

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.NULL;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.NULL;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.NULL) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.LIST) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SEQUENCE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SYMBOL) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.BOOLEAN) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.NULL) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.LIST) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SEQUENCE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SYMBOL) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
