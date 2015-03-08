/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.sequences.SequenceStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Implements the '#a' Lisp reader macro.
 */
@Component
public class SharpAReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3446360583440445990L;

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_A, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_A, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct lispToken = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (numArg == null) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}

		if (BigInteger.ZERO.compareTo(numArg) > 0) {
			if (!(lispToken instanceof SequenceStruct)) {
				final String printedToken = printer.print(lispToken);
				throw new ReaderErrorException("The form following a #" + numArg + "A reader macro should have been a sequence, but it was: " + printedToken);
			}
		}

		return createArray(numArg, lispToken);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * ArrayStruct} from the provided {@code contents}.
	 *
	 * @param numArg
	 * 		the number of expected dimensions
	 * @param contents
	 * 		the {@link LispStruct} tokens used to create the {@link ArrayStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link ArrayStruct}
	 */
	private static ListStruct createArray(final BigInteger numArg, final LispStruct contents) {

		final List<LispStruct> dimensionsAsJavaList = getDimensions(numArg, contents);

		final SymbolStruct<?> makeArrayFnSymbol = CommonLispSymbols.MAKE_ARRAY;
		final ListStruct dimensions = ListStruct.buildProperList(dimensionsAsJavaList);
		final SymbolStruct<?> elementTypeKeyword = GlobalPackageStruct.KEYWORD.findSymbol("ELEMENT-TYPE").getSymbolStruct();
		final ListStruct elementType = ListStruct.buildProperList(QUOTE, CommonLispSymbols.T);
		final SymbolStruct<?> initialContentsKeyword = GlobalPackageStruct.KEYWORD.findSymbol("INITIAL-CONTENTS").getSymbolStruct();

		final ListStruct initialContents = ListStruct.buildProperList(QUOTE, contents);

		return ListStruct.buildProperList(makeArrayFnSymbol, dimensions, elementTypeKeyword, elementType, initialContentsKeyword, initialContents);
	}

	/**
	 * Build the dimensions list from the contents of the provided {@link LispStruct}. The dimensions will be based off
	 * of the first element in each axis layer of the supplied {@code dimensions} parameter.
	 *
	 * @param dimensions
	 * 		the number of expected dimensions
	 * @param contents
	 * 		the contents to build and analyze against the {@code dimensions} parameter
	 *
	 * @return a List of {@link IntegerStruct} that make up the expected dimensions of the simple array
	 *
	 * @throws ReaderErrorException
	 * 		if dimensions do not match the provided contents list
	 */
	private static List<LispStruct> getDimensions(final BigInteger dimensions, final LispStruct contents) {

		LispStruct seq = contents;
		BigInteger zeroAxis = null;

		final List<LispStruct> dimensionsAsJavaList = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     dimensions.compareTo(axis) > 0;
		     axis = axis.add(BigInteger.ONE)) {

			final SequenceStruct seqList;
			if (seq instanceof SequenceStruct) {
				seqList = (SequenceStruct) seq;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + seq);
			}

			final List<LispStruct> lispTokens = seqList.getAsJavaList();

			final int seqLength = lispTokens.size();
			final BigInteger seqLengthBI = BigInteger.valueOf(seqLength);
			final IntegerStruct dimension = new IntegerStruct(seqLengthBI);
			dimensionsAsJavaList.add(dimension);

			if (!axis.equals(dimensions.subtract(BigInteger.ONE))) {
				if (lispTokens.isEmpty()) {
					zeroAxis = axis;
				} else if (zeroAxis == null) {
					seq = lispTokens.get(0);
				} else {
					throw new ReaderErrorException("#" + dimensions + "A axis " + zeroAxis + " is empty, but axis " + axis + " is non-empty.");
				}
			}
		}

		return dimensionsAsJavaList;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
