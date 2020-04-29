package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.PrinterVariables;

/**
 * The implementation of a zero-ranked {@link ArrayStruct}.
 */
public class NILArrayStructImpl extends AbstractArrayStructImpl {

	/**
	 * The single content value of the structure.
	 */
	private LispStruct content;

	private BooleanStruct adjustable;

	/**
	 * Constructor for building the zero-ranked array structure.
	 *
	 * @param elementType
	 * 		the array elementType
	 * @param content
	 * 		the content value of the structure
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 */
	public NILArrayStructImpl(final SymbolStruct elementType, final LispStruct content, final BooleanStruct adjustable) {
		super(elementType);
		this.content = content;
		this.adjustable = adjustable;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public BooleanStruct adjustableArrayP() {
		return adjustable;
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		validateSubscripts(subscripts);
		return content;
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) { // TODO: type check
		validateSubscripts(subscripts);
		content = newElement;
		return newElement;
	}

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		throw new ErrorException("Cannot determine array dimension for array with rank 0.");
	}

	@Override
	public ListStruct arrayDimensions() {
		return NILStruct.INSTANCE;
	}

	@Override
	public IntegerStruct arrayRank() {
		return IntegerStruct.ZERO;
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		validateSubscripts(subscripts);
		return IntegerStruct.ZERO;
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return IntegerStruct.ONE;
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		if (!IntegerStruct.ZERO.eql(index)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		return content;
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		if (!IntegerStruct.ZERO.eql(index)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		content = newElement;
		return newElement;
	}

	/**
	 * Validates the provided subscripts, ensuring that there are none provided.
	 *
	 * @param subscripts
	 * 		the subscripts to validate
	 */
	private static void validateSubscripts(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		if (numberOfSubscripts != 0) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 0.");
		}
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		if (adjustable.toJavaPBoolean()) {
			return ListStruct.toLispList(CommonLispSymbols.ARRAY, elementType, arrayDimensions());
		} else {
			return ListStruct.toLispList(CommonLispSymbols.SIMPLE_ARRAY, elementType, arrayDimensions());
		}
	}

	@Override
	public ClassStruct classOf() {
		if (adjustable.toJavaPBoolean()) {
			return BuiltInClassStruct.ARRAY;
		} else {
			return BuiltInClassStruct.SIMPLE_ARRAY;
		}
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (adjustable.toJavaPBoolean()) {
			if (typeSpecifier == CommonLispSymbols.ARRAY) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == BuiltInClassStruct.ARRAY) {
				return TStruct.INSTANCE;
			}
		} else {
			if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
				return TStruct.INSTANCE;
			}
			if (typeSpecifier == BuiltInClassStruct.SIMPLE_ARRAY) {
				return TStruct.INSTANCE;
			}
		}
		return super.typep(typeSpecifier);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().toJavaPBoolean();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#0A");
			stringBuilder.append(content);

		} else {
			stringBuilder.append("#<");
			stringBuilder.append(typeOf());
			stringBuilder.append(" NIL type ");

			final String elementTypeClassName = elementType.getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

			if (adjustable.toJavaPBoolean()) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
