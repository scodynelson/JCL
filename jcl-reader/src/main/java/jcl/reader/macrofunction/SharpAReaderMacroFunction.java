/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.ReaderMacroFunctionImpl;
import jcl.lang.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import jcl.lang.SequenceStruct;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '#a' Lisp reader macro.
 */
@Component
public class SharpAReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_A, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_A, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_A) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_A);

		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("#A used without a rank argument.");
		}
		final BigInteger numberArgumentValue = numberArgument.get();

		if (BigInteger.ZERO.compareTo(numberArgumentValue) < 0) {
			if (!(token instanceof SequenceStruct)) {
				throw new ReaderErrorException("The form following a #" + numberArgumentValue + "A reader macro should have been a sequence, but it was: " + token);
			}

			final SequenceStruct contents = (SequenceStruct) token;
			final List<Integer> dimensions = getDimensions(numberArgumentValue, contents);
			final List<LispStruct> initialContents = contents.stream().collect(Collectors.toList());
			return LispStructFactory.toArray(dimensions, initialContents);
		} else {
			return LispStructFactory.toArray(Collections.emptyList(), Collections.singletonList(token));
		}
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
	 * @return a List of {@link Integer} that make up the expected dimensions of the simple array
	 *
	 * @throws ReaderErrorException
	 * 		if dimensions do not match the provided contents list
	 */
	private static List<Integer> getDimensions(final BigInteger dimensions, final SequenceStruct contents) {

		LispStruct sequence = contents;
		BigInteger zeroAxis = null;

		final List<Integer> arrayDimensions = new ArrayList<>();

		for (BigInteger axis = BigInteger.ZERO;
		     dimensions.compareTo(axis) > 0;
		     axis = axis.add(BigInteger.ONE)) {

			final SequenceStruct sequenceToken;
			if (sequence instanceof SequenceStruct) {
				sequenceToken = (SequenceStruct) sequence;
			} else {
				throw new ReaderErrorException("#" + dimensions + "A axis " + axis + " is not a sequence: " + sequence);
			}

			final List<LispStruct> tokensAsJavaList = sequenceToken.stream().collect(Collectors.toList());

			final int dimension = tokensAsJavaList.size();
			arrayDimensions.add(dimension);

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

		return arrayDimensions;
	}
}
