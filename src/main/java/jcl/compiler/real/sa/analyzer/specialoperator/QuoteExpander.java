package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class QuoteExpander extends MacroFunctionExpander<QuoteStruct> {

	private static final long serialVersionUID = 2741011595927247743L;

	/**
	 * Initializes the quote macro function and adds it to the special operator 'quote'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.QUOTE.setMacroFunctionExpander(this);
	}

	@Override
	public QuoteStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct quotedObject = inputRest.getFirst();
		return new QuoteStruct(quotedObject);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
