/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.sequences.SequenceStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#a' Lisp reader macro.
 */
@Component
public class SharpAReaderMacroFunction extends ReaderMacroFunction {

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
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}
		final BigInteger numberArgumentValue = numberArgument.get();

		if (BigInteger.ZERO.compareTo(numberArgumentValue) > 0) {
			if (!(token instanceof SequenceStruct)) {
				final String printedToken = printer.print(token);
				throw new ReaderErrorException("The form following a #" + numberArgumentValue + "A reader macro should have been a sequence, but it was: " + printedToken);
			}
		}

		return createArray(token, numberArgumentValue);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * ArrayStruct} from the provided {@code contents}.
	 *
	 * @param contents
	 * 		the {@link LispStruct} tokens used to create the {@link ArrayStruct}
	 * @param numberArgument
	 * 		the number of expected dimensions
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link ArrayStruct}
	 */
	private static ListStruct createArray(final LispStruct contents, final BigInteger numberArgument) {
		final List<LispStruct> dimensionsAsJavaList = getDimensions(numberArgument, contents);

		final ListStruct dimensions = ListStruct.buildProperList(dimensionsAsJavaList);
		final ListStruct elementType = ListStruct.buildProperList(CommonLispSymbols.QUOTE, CommonLispSymbols.T);
		final ListStruct initialContents = ListStruct.buildProperList(CommonLispSymbols.QUOTE, contents);

		return ListStruct.buildProperList(CommonLispSymbols.MAKE_ARRAY,
				dimensions,
				CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
				elementType,
				CommonLispSymbols.INITIAL_CONTENTS_KEYWORD,
				initialContents);
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

		LispStruct sequence = contents;
		BigInteger zeroAxis = null;

		final List<LispStruct> dimensionsAsJavaList = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     dimensions.compareTo(axis) > 0;
		     axis = axis.add(BigInteger.ONE)) {

			final SequenceStruct sequenceToken;
			if (sequence instanceof SequenceStruct) {
				sequenceToken = (SequenceStruct) sequence;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + sequence);
			}

			final List<LispStruct> tokensAsJavaList = sequenceToken.getAsJavaList();

			final BigInteger sequenceLength = BigInteger.valueOf(tokensAsJavaList.size());
			final IntegerStruct dimension = new IntegerStruct(sequenceLength);
			dimensionsAsJavaList.add(dimension);

			if (!axis.equals(dimensions.subtract(BigInteger.ONE))) {
				if (tokensAsJavaList.isEmpty()) {
					zeroAxis = axis;
				} else if (zeroAxis == null) {
					sequence = tokensAsJavaList.get(0);
				} else {
					throw new ReaderErrorException("#" + dimensions + "A axis " + zeroAxis + " is empty, but axis " + axis + " is non-empty.");
				}
			}
		}

		return dimensionsAsJavaList;
	}
}
