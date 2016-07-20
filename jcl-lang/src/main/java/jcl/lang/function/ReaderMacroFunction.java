/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.function;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.number.IntegerStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.stream.InputStream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * {@link Autowired} {@link ApplicationContext} used for getting a new {@link Reader} bean instance.
	 */
	@Autowired
	private ApplicationContext applicationContext;

	protected ReaderMacroFunction() {
		// TODO
		super("Some Documentation");
	}

	private static final SymbolStruct DUMMY_SYMBOL = new SymbolStruct("dummySymbol");

	@Override
	public SymbolStruct getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final InputStream stream = (InputStream) lispStructs[0];

		final CharacterStruct macroCharacter = (CharacterStruct) lispStructs[1];
		final int codePoint = macroCharacter.getCodePoint();

		final Optional<BigInteger> numberArgument;
		if (isDispatch()) {
			final IntegerStruct macroNumberArgument = (IntegerStruct) lispStructs[2];
			// TODO: optimize??
			final BigInteger bigInteger = macroNumberArgument.bigIntegerValue();
			numberArgument = Optional.of(bigInteger);
		} else {
			numberArgument = Optional.empty();
		}

		final Reader reader = applicationContext.getBean(Reader.class, stream);

		return readMacro(codePoint, reader, numberArgument);
	}

	/**
	 * Interpret the character stream from the provided {@link Reader} (up to End-of-File or new line) based on the
	 * provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character code point that determines the macro function
	 * @param reader
	 * 		the {@link Reader} used to read tokens
	 * @param numberArgument
	 * 		the optional number argument
	 *
	 * @return the parsed {@link LispStruct} token
	 */
	public abstract LispStruct readMacro(int codePoint, Reader reader, Optional<BigInteger> numberArgument);

	/**
	 * Default method used to determine if the ReaderMacroFunction is a dispatching macro. The default value return is
	 * {@code #false}, however this is overridden in the internal dispatching table in a readtable.
	 *
	 * @return whether or not the ReaderMacroFunction is a dispatching macro
	 */
	public boolean isDispatch() {
		return false;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(applicationContext)
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
		final ReaderMacroFunction rhs = (ReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(applicationContext, rhs.applicationContext)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(applicationContext)
		                                                                .toString();
	}
}
