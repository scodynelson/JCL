package jcl.lang.internal;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.function.IntConsumer;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SyntaxType;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SimpleStringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class SimpleStringStructImpl extends AbstractStringStructImpl {

	/**
	 * {@link StringBuilder} containing the implementation contents of the {@link StringStruct}.
	 */
	private final StringBuilder contents;

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link LispStruct} type of the elements
	 * @param contents
	 * 		the {@link StringBuilder} contents
	 */
	public SimpleStringStructImpl(final IntegerStruct size, final LispStruct elementType,
	                              final StringBuilder contents) {
		super(elementType, size);
		this.contents = contents;
	}

	/*
	STRING-STRUCT
	 */

	@Override
	protected StringStruct nCasifyString(final StringIntervalOpContext context,
	                                     final Function<String, String> casifyOp) {
		final IntegerStruct start = getStringOpStart(this, context);
		final IntegerStruct end = getStringOpEnd(this, context, start);

		final int startInt = start.toJavaInt();
		final int endInt = end.toJavaInt();

		String str = contents.substring(startInt, endInt);
		str = casifyOp.apply(str);
		contents.replace(startInt, endInt, str);
		return this;
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
		return contents.toString();
	}

	@Override
	public String toJavaString(final boolean ignoreFillPointer) {
		return contents.toString();
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
		throw new TypeErrorException("SIMPLE-STRING has no fill-pointer to retrieve.");
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		throw new TypeErrorException("Cannot set fill-pointer for SIMPLE-STRING.");
	}

	@Override
	public CharacterStruct vectorPop() {
		throw new TypeErrorException("Cannot pop from a SIMPLE-STRING with no fill-pointer.");
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		throw new TypeErrorException("Cannot push into a SIMPLE-STRING with no fill-pointer.");
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		throw new TypeErrorException("Cannot push or extend a SIMPLE-STRING with no fill-pointer.");
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public CharacterStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		return getCharacterFromContents(rowMajorIndex);
	}

	@Override
	public CharacterStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final CharacterStruct newCharacterValue = getCharacter(newElement);
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);

		contents.setCharAt(rowMajorIndex.toJavaInt(), newCharacterValue.toJavaChar());
		return newCharacterValue;
	}

	@Override
	public CharacterStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		return getCharacterFromContents(validIndex);
	}

	@Override
	public CharacterStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final CharacterStruct newCharacterValue = getCharacter(newElement);
		final IntegerStruct validIndex = validateSubscript(index);

		contents.setCharAt(validIndex.toJavaInt(), newCharacterValue.toJavaChar());
		return newCharacterValue;
	}

	/**
	 * Retrieves the {@link CharacterStruct} from the {@link #contents} at the provided index location. If the index is
	 * out of bounds, {@link CharacterConstants#NULL_CHAR} is returned.
	 *
	 * @param index
	 * 		the index of the {@link CharacterStruct} to retrieve
	 *
	 * @return the {@link CharacterStruct} from the {@link #contents} at the provided index location
	 */
	private CharacterStruct getCharacterFromContents(final IntegerStruct index) {
		try {
			final char character = contents.charAt(index.toJavaInt());
			return CharacterStruct.toLispCharacter(character);
		} catch (final StringIndexOutOfBoundsException ignored) {
			// This is here for when the 'totalSize' is more than the contents.
			// Typically will only happen with adjusted strings.
			return CharacterConstants.NULL_CHAR;
		}
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public StringStruct reverse() {
		final String contentsToReverse = contents.toString();
		final StringBuilder reversedContents = new StringBuilder(contentsToReverse).reverse();
		return new SimpleStringStructImpl(totalSize, elementType, reversedContents);
	}

	@Override
	public StringStruct nReverse() {
		contents.reverse();
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new StringIterator(contents);
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(),
		                                totalSize.toJavaInt(),
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
	 * {@inheritDoc} Generation method for {@link StringStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link String} constant produced by performing {@link StringStruct#toJavaString()}</li>
	 * <li>Constructing a new {@link StringStruct} with the loaded {@link String} value</li>
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
		final int elementTypeStore = generateElementType(generatorState);

		final int contentsStore = methodBuilder.getNextAvailableStore();

		final String javaString = contents.toString();
		mv.visitLdcInsn(javaString);
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		mv.visitVarInsn(Opcodes.ALOAD, sizeStore);
		mv.visitVarInsn(Opcodes.ALOAD, elementTypeStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.STRING_STRUCT_NAME,
		                   GenerationConstants.STRING_STRUCT_TO_LISP_STRING_METHOD_NAME,
		                   GenerationConstants.STRING_STRUCT_TO_SIMPLE_STRING_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		if (CommonLispSymbols.BASE_CHAR == elementType) {
			return ListStruct.toLispList(CommonLispSymbols.SIMPLE_BASE_STRING, totalSize);
		} else {
			return ListStruct.toLispList(CommonLispSymbols.SIMPLE_STRING, totalSize);
		}
	}

	@Override
	public ClassStruct classOf() {
		if (CommonLispSymbols.BASE_CHAR == elementType) {
			return BuiltInClassStruct.SIMPLE_BASE_STRING;
		} else {
			return BuiltInClassStruct.SIMPLE_STRING;
		}
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if ((typeSpecifier == CommonLispSymbols.SIMPLE_BASE_STRING) && (CommonLispSymbols.BASE_CHAR == elementType)) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SIMPLE_STRING) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
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

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final boolean printEscape = CommonLispSymbols.PRINT_ESCAPE_VAR.getVariableValue().toJavaPBoolean();

		final ReadtableStruct readtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final IntConsumer appendFn = codePoint -> {
			final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);
			if ((codePoint == '"') || (syntaxType == SyntaxType.SINGLE_ESCAPE)) {
				stringBuilder.append('\\');
			}
			stringBuilder.appendCodePoint(codePoint);
		};

		contents.codePoints()
		        .forEach(appendFn);

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}
}
