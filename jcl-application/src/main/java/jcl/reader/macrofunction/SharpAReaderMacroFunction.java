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

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.sequences.SequenceStruct;
import jcl.symbols.NILStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#a' Lisp reader macro.
 */
@Component
public class SharpAReaderMacroFunction extends ReaderMacroFunction {

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
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_A, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_A, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_A) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_A);

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
				final String printedToken = printer.print(token);
				throw new ReaderErrorException("The form following a #" + numberArgumentValue + "A reader macro should have been a sequence, but it was: " + printedToken);
			}

			final SequenceStruct contents = (SequenceStruct) token;
			final List<Integer> dimensions = getDimensions(numberArgumentValue, contents);
			final List<LispStruct> initialContents = contents.stream().collect(Collectors.toList());
			return new ArrayStruct<>(dimensions, initialContents);
		} else {
			return new ArrayStruct<>(Collections.emptyList(), Collections.singletonList(token));
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final SharpAReaderMacroFunction rhs = (SharpAReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
