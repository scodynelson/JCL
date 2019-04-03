package jcl.lang;

import jcl.lang.internal.readtable.ReadtableStructImpl;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link ReadtableStruct} is the object representation of a Lisp 'readtable' type.
 */
public interface ReadtableStruct extends LispStruct {

	/**
	 * Creates a new dispatching table for the provided {@code codePoint}, designating the {@link SyntaxType} as
	 * terminating if the provided {@code nonTerminatingP} is false.
	 *
	 * @param dispatchTable
	 * 		the dispatching table {@link FunctionStruct} to use
	 * @param codePoint
	 * 		the key for the new dispatching table
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 *
	 * @return the value of {@code nonTerminatingP}
	 */
	boolean makeDispatchMacroCharacter(final FunctionStruct dispatchTable, final int codePoint, final boolean nonTerminatingP);

	default BooleanStruct makeDispatchMacroCharacter(final FunctionStruct dispatchTable,
	                                                 final CharacterStruct character,
	                                                 final BooleanStruct nonTerminatingP) {
		makeDispatchMacroCharacter(dispatchTable, character.toUnicodeCodePoint(), nonTerminatingP.toJavaPBoolean());
		return TStruct.INSTANCE;
	}

	/**
	 * Sets the {@link FunctionStruct} for the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}, designating the {@link SyntaxType} as terminating if the provided {@code nonTerminatingP}
	 * is false.
	 *
	 * @param codePoint
	 * 		the key for the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct}
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 */
	void setMacroCharacter(final int codePoint, final FunctionStruct readerMacroFunction, final boolean nonTerminatingP);

	default LispStruct setMacroCharacter(final CharacterStruct character, final FunctionStruct newFunction,
	                                     final BooleanStruct nonTerminatingP) {
		setMacroCharacter(character.toUnicodeCodePoint(), newFunction, nonTerminatingP.toJavaPBoolean());
		return TStruct.INSTANCE;
	}

	/**
	 * Getter for readtable {@link ReadtableCase} property.
	 *
	 * @return readtable {@link ReadtableCase} property
	 */
	ReadtableCase getReadtableCase();

	default SymbolStruct readtableCase() {
		final ReadtableCase readtableCase = getReadtableCase();
		switch (readtableCase) {
			case UPCASE:
				return CommonLispSymbols.UPCASE_KEYWORD;
			case DOWNCASE:
				return CommonLispSymbols.DOWNCASE_KEYWORD;
			case PRESERVE:
				return CommonLispSymbols.PRESERVE_KEYWORD;
			case INVERT:
				return CommonLispSymbols.INVERT_KEYWORD;
		}
		return NILStruct.INSTANCE;
	}

	/**
	 * Setter for readtable {@link ReadtableCase} property.
	 *
	 * @param readtableCase
	 * 		new readtable {@link ReadtableCase} property value
	 */
	void setReadtableCase(final ReadtableCase readtableCase);

	default SymbolStruct setReadtableCase(final SymbolStruct mode) {
		if (CommonLispSymbols.UPCASE_KEYWORD.eq(mode)) {
			setReadtableCase(ReadtableCase.UPCASE);
		} else if (CommonLispSymbols.DOWNCASE_KEYWORD.eq(mode)) {
			setReadtableCase(ReadtableCase.DOWNCASE);
		} else if (CommonLispSymbols.PRESERVE_KEYWORD.eq(mode)) {
			setReadtableCase(ReadtableCase.PRESERVE);
		} else if (CommonLispSymbols.INVERT_KEYWORD.eq(mode)) {
			setReadtableCase(ReadtableCase.INVERT);
		}
		return mode;
	}

	/**
	 * Retrieves the {@link FunctionStruct} for the provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the key for the {@link FunctionStruct}
	 *
	 * @return the {@link FunctionStruct} for the provided {@code codePoint}
	 */
	FunctionStruct getMacroCharacter(final int codePoint);

	default FunctionStruct getMacroCharacter(final CharacterStruct character) {
		return getMacroCharacter(character.toUnicodeCodePoint());
	}

	/**
	 * Gets the {@link FunctionStruct} for the provided {@code subCodePoint} within the provided {@code
	 * dispatchCodePoint}'s dispatching table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatching table to search for the {@link FunctionStruct}
	 * @param subCodePoint
	 * 		the key for the {@link FunctionStruct}
	 *
	 * @return the {@link FunctionStruct} for the provided {@code subCodePoint}
	 */
	FunctionStruct getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint);

	default FunctionStruct getDispatchMacroCharacter(final CharacterStruct dispatchChar, final CharacterStruct subChar) {
		return getDispatchMacroCharacter(dispatchChar.toUnicodeCodePoint(), subChar.toUnicodeCodePoint());
	}

	/**
	 * Sets the {@link FunctionStruct} for the provided {@code subCodePoint} to the provided {@code
	 * readerMacroFunction} within the provided {@code dispatchCodePoint}'s dispatching table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatching table to set the {@link FunctionStruct}
	 * @param subCodePoint
	 * 		the key for the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct}
	 */
	void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final FunctionStruct readerMacroFunction);

	default LispStruct setDispatchMacroCharacter(final CharacterStruct dispatchChar, final CharacterStruct subChar,
	                                             final FunctionStruct newFunction) {
		setDispatchMacroCharacter(dispatchChar.toUnicodeCodePoint(), subChar.toUnicodeCodePoint(), newFunction);
		return TStruct.INSTANCE;
	}

	/**
	 * Gets the {@link AttributeType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the {@link AttributeType} to retrieve
	 * @param readBase
	 * 		the read-base valued used to retrieve the appropriate {@link AttributeType}
	 *
	 * @return the {@link AttributeType} for the provided {@code codePoint} value
	 */
	AttributeType getAttributeType(final int codePoint, final IntegerStruct readBase);

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the {@link SyntaxType} to retrieve
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} value
	 */
	SyntaxType getSyntaxType(final int codePoint);

	default ReadtableStruct copyReadtable(final LispStruct toReadtable) {
		if (toReadtable instanceof ReadtableStruct) {
			// TODO: This isn't correct. We can't fully copy the Readtable objects right now.
			final ReadtableStruct toReadtableCast = (ReadtableStruct) toReadtable;
			return new ReadtableStructImpl(toReadtableCast.getReadtableCase());
		} else {
			return new ReadtableStructImpl();
		}
	}

	static ReadtableStruct toReadtable() {
		return new ReadtableStructImpl();
	}
}
