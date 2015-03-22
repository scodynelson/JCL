/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.system.CommonLispSymbols;
import jcl.types.Null;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
@Component
public class SharpLeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1826957244403929085L;

	/**
	 * {@link Autowired} {@link ListReaderMacroFunction} used for reading {@link ListStruct}s.
	 */
	@Autowired
	private ListReaderMacroFunction listReaderMacroFunction;

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
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LEFT_PARENTHESIS, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListStruct listToken = listReaderMacroFunction.readList(reader);

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return Null.INSTANCE;
		}

		if (listToken == null) {
			throw new ReaderErrorException("Ill-formed vector: #");
		}

		if (!listToken.isProper()) {
			final String printedToken = printer.print(listToken);
			throw new ReaderErrorException("Ill-formed vector: #" + printedToken);
		}

		if (!numberArgument.isPresent()) {
			final List<LispStruct> tokensAsJavaList = listToken.getAsJavaList();
			return createVector(tokensAsJavaList);
		}

		final BigInteger numberArgumentValue = numberArgument.get();
		return handleNumberArgument(listToken, numberArgumentValue);
	}

	/**
	 * Handles the processing of the number argument when parsing the provided list of {@link LispStruct}s into a
	 * {@link VectorStruct}.
	 *
	 * @param listToken
	 * 		the vector contents
	 * @param numberArgument
	 * 		the number argument passed to be used as the vector length
	 *
	 * @return the properly created {@link VectorStruct} taking care of the proper vector length
	 */
	private ListStruct handleNumberArgument(final ListStruct listToken, final BigInteger numberArgument) {
		final List<LispStruct> tokensAsJavaList = listToken.getAsJavaList();

		final int numberOfTokens = tokensAsJavaList.size();
		final int numberArgumentIntValue = numberArgument.intValueExact();
		if (numberOfTokens > numberArgumentIntValue) {
			final String printedToken = printer.print(listToken);
			throw new ReaderErrorException("Vector is longer than specified length: #" + numberArgument + printedToken);
		}

		LispStruct lastToken = null;
		if (CollectionUtils.isNotEmpty(tokensAsJavaList)) {
			lastToken = tokensAsJavaList.get(numberOfTokens - 1);
		}

		final int fillAmount = numberArgumentIntValue - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			tokensAsJavaList.add(lastToken);
		}

		return createVector(tokensAsJavaList);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * VectorStruct} from the provided {@code lispTokens}.
	 *
	 * @param tokensAsJavaList
	 * 		the {@link LispStruct} tokens used to create the {@link VectorStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link VectorStruct}
	 */
	private static ListStruct createVector(final List<LispStruct> tokensAsJavaList) {
		final BigInteger numberOfTokens = BigInteger.valueOf(tokensAsJavaList.size());

		final IntegerStruct dimensions = new IntegerStruct(numberOfTokens);
		final ListStruct elementType = ListStruct.buildProperList(CommonLispSymbols.QUOTE, CommonLispSymbols.T);
		final ListStruct initialContents = ListStruct.buildProperList(CommonLispSymbols.QUOTE, ListStruct.buildProperList(tokensAsJavaList));

		return ListStruct.buildProperList(CommonLispSymbols.MAKE_ARRAY,
				dimensions,
				CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
				elementType,
				CommonLispSymbols.INITIAL_CONTENTS_KEYWORD,
				initialContents);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(listReaderMacroFunction)
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
		final SharpLeftParenthesisReaderMacroFunction rhs = (SharpLeftParenthesisReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(listReaderMacroFunction, rhs.listReaderMacroFunction)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(listReaderMacroFunction)
		                                                                .append(printer)
		                                                                .toString();
	}
}
