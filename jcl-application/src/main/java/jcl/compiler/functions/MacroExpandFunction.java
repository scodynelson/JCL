/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MacroExpandFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MACROEXPAND";
	private static final String FORM_ARGUMENT = "FORM";
	private static final String ENVIRONMENT_ARGUMENT = "ENVIRONMENT";

	@Autowired
	private MacroExpand1Function macroExpand1Function;

	public MacroExpandFunction() {
		super("Repeatedly expands form until it is no longer a macro form.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FORM_ARGUMENT)
		                .optionalParameter(ENVIRONMENT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		Environment environment = Environment.NULL;
		if (arguments.hasOptionalArgument(ENVIRONMENT_ARGUMENT)) {
			environment = arguments.getOptionalArgument(ENVIRONMENT_ARGUMENT, Environment.class);
		}

		final MacroExpandResult macroExpandResult = macroExpand(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return new ValuesStruct(expandedForm, wasExpandedBoolean);
	}

	public MacroExpandResult macroExpand(final LispStruct element, final Environment environment) {
		LispStruct tempElement = element;

		boolean wasExpanded = false;
		while (true) {
			final MacroExpandResult expansion = macroExpand1Function.macroExpand1(tempElement, environment);
			tempElement = expansion.getExpandedForm();

			final boolean innerWasNotExpanded = !expansion.wasExpanded();
			if (innerWasNotExpanded) {
				return new MacroExpandResult(tempElement, wasExpanded);
			}
			wasExpanded = true;
		}
	}
}
