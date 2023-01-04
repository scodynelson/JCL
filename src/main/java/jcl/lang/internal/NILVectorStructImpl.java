package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.stream.Stream;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link NILVectorStructImpl} is the object representation of a Lisp 'nil' 'vector' type.
 */
public class NILVectorStructImpl extends AbstractStringStructImpl {

	/**
	 * Public constructor for initializing size.
	 *
	 * @param size
	 * 		the value used to initialize size property
	 */
	public NILVectorStructImpl(final IntegerStruct size) {
		super(CommonLispSymbols.CHARACTER, size);
	}

	/*
	STRING-STRUCT
	 */

	@Override
	protected StringStruct nCasifyString(final StringIntervalOpContext context,
	                                     final Function<String, String> casifyOp) {
		throw new TypeErrorException("NIL-VECTOR has no character to alter case.");
	}

	@Override
	public CharacterStruct char_(final FixnumStruct index) {
		return aref(index);
	}

	@Override
	public CharacterStruct setfChar(final CharacterStruct newCharacter, final FixnumStruct index) {
		return setfAref(newCharacter, index);
	}

	@Override
	public CharacterStruct schar(final FixnumStruct index) {
		return aref(index);
	}

	@Override
	public CharacterStruct setfSchar(final CharacterStruct newCharacter, final FixnumStruct index) {
		return setfAref(newCharacter, index);
	}

	@Override
	public BooleanStruct isSimpleString() {
		return TStruct.INSTANCE;
	}

	@Override
	public String toJavaString() {
		return StringUtils.EMPTY;
	}

	@Override
	public String toJavaString(final boolean ignoreFillPointer) {
		return StringUtils.EMPTY;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public LispStruct svref(final FixnumStruct index) {
		return aref(index);
	}

	@Override
	public LispStruct setfSvref(final LispStruct newElement, final FixnumStruct index) {
		return setfAref(newElement, index);
	}

	@Override
	public IntegerStruct fillPointer() {
		throw new TypeErrorException("NIL-VECTOR has no fill-pointer to retrieve.");
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		throw new TypeErrorException("Cannot set fill-pointer for NIL-VECTOR.");
	}

	@Override
	public LispStruct vectorPop() {
		throw new TypeErrorException("Cannot pop from a NIL-VECTOR with no fill-pointer.");
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		throw new TypeErrorException("Cannot push into a NIL-VECTOR with no fill-pointer.");
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		throw new TypeErrorException("Cannot push or extend a NIL-VECTOR with no fill-pointer.");
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public CharacterStruct aref(final IntegerStruct... subscripts) {
		throw new TypeErrorException("NIL-VECTOR has no character to retrieve.");
	}

	@Override
	public CharacterStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		throw new TypeErrorException("NIL-VECTOR has no character to set.");
	}

	@Override
	public CharacterStruct rowMajorAref(final IntegerStruct index) {
		throw new TypeErrorException("NIL-VECTOR has no character to retrieve.");
	}

	@Override
	public CharacterStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		throw new TypeErrorException("NIL-VECTOR has no character to set.");
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
	public StringStruct reverse() {
		return new NILVectorStructImpl(totalSize);
	}

	@Override
	public StringStruct nReverse() {
		return this;
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
	 * Generation method for NILVectorStructImpl objects, by performing the following operations:
	 * <ol>
	 * <li>Constructing a new NILVectorStructImpl</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int sizeStore = generateSize(generatorState);

		mv.visitVarInsn(Opcodes.ALOAD, sizeStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.VECTOR_STRUCT_NAME,
		                   GenerationConstants.VECTOR_STRUCT_TO_VECTOR_METHOD_NAME,
		                   GenerationConstants.VECTOR_STRUCT_TO_NIL_VECTOR_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.NIL_VECTOR, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.NIL_VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.NIL_VECTOR) {
			return TStruct.INSTANCE;
		}
		if ((typeSpecifier == CommonLispSymbols.SIMPLE_BASE_STRING) && (CommonLispSymbols.BASE_CHAR == elementType)) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SIMPLE_STRING) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.NIL_VECTOR) {
			return TStruct.INSTANCE;
		}
		if ((typeSpecifier == BuiltInClassStruct.SIMPLE_BASE_STRING) && (CommonLispSymbols.BASE_CHAR == elementType)) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_STRING) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
