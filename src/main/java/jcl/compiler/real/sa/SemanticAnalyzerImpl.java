/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.LambdaExpander;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Autowired
	private LambdaExpander lambdaExpander;

	@Autowired
	private Printer printer;

	@Override
	public LambdaStruct analyze(final LispStruct form) {

		final ListStruct lambdaForm = wrapFormInLambda(form);

		final Environment nullEnvironment = Environment.NULL;
		final LambdaStruct analyzedForm = lambdaExpander.expand(lambdaForm, nullEnvironment);

		// now see if we have any functions still undefined
		final Set<SymbolStruct<?>> undefinedFunctions = nullEnvironment.getUndefinedFunctions();

		undefinedFunctions.stream()
		                  .forEach(this::unknownFunctionWarning);

		return analyzedForm;
	}

	private static ListStruct wrapFormInLambda(final LispStruct form) {

		final ListStruct lambdaForm;
		if (form instanceof ListStruct) {
			final ListStruct formList = (ListStruct) form;
			final LispStruct firstOfFormList = formList.getFirst();
			if (firstOfFormList.equals(SpecialOperator.LAMBDA)) {
				lambdaForm = formList;
			} else {
				lambdaForm = getLambdaFormList(formList);
			}
		} else {
			lambdaForm = getLambdaFormList(form);
		}

		return lambdaForm;
	}

	private static ListStruct getLambdaFormList(final LispStruct form) {
		final List<LispStruct> lambdaFormList = new ArrayList<>();
		lambdaFormList.add(SpecialOperator.LAMBDA);
		lambdaFormList.add(NullStruct.INSTANCE);
		lambdaFormList.add(form);

		return ListStruct.buildProperList(lambdaFormList);
	}

	private void unknownFunctionWarning(final SymbolStruct<?> undefinedFunction) {
		final String printedUndefinedFunction = printer.print(undefinedFunction);
		LOGGER.warn("Warning: no function or macro function defined for: {}", printedUndefinedFunction);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
