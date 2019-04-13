/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.InternalMacroExpand;
import jcl.compiler.function.MacroExpandResult;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MacroExpandFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MACROEXPAND";
	private static final String FORM_ARGUMENT = "FORM";
	private static final String ENVIRONMENT_ARGUMENT = "ENVIRONMENT";

	public MacroExpandFunction() {
		super("Repeatedly expands form until it is no longer a macro form.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FORM_ARGUMENT)
		                .optionalParameter(ENVIRONMENT_ARGUMENT).withInitialValue(Environment.NULL)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		Environment environment = Environment.NULL;
		if (arguments.hasOptionalArgument(ENVIRONMENT_ARGUMENT)) {
			environment = arguments.getOptionalArgument(ENVIRONMENT_ARGUMENT, Environment.class);
		}

		final MacroExpandResult macroExpandResult = InternalMacroExpand.macroExpand(form, environment);
		final LispStruct expandedForm = macroExpandResult.getExpandedForm();
		final boolean wasExpanded = macroExpandResult.wasExpanded();
		final BooleanStruct wasExpandedBoolean = wasExpanded ? TStruct.INSTANCE : NILStruct.INSTANCE;
		return ValuesStruct.valueOf(expandedForm, wasExpandedBoolean);
	}
}
