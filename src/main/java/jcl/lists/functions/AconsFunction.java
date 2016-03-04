package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import org.springframework.stereotype.Component;

@Component
public final class AconsFunction extends AbstractCommonLispFunctionStruct {

	public AconsFunction() {
		super("Creates a fresh cons, the cdr of which is alist and the car of which is another fresh cons, the car of which is key and the cdr of which is datum.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "KEY").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "DATUM").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "ALIST").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct key = lispStructs[0];
		final LispStruct datum = lispStructs[1];
		final LispStruct alist = lispStructs[2];

		final ConsStruct pair = new ConsStruct(key, datum);
		return new ConsStruct(pair, alist);
	}

	@Override
	protected String functionName() {
		return "ACONS";
	}
}
