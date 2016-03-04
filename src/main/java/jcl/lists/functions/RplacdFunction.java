package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.ConsType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class RplacdFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public RplacdFunction() {
		super("Replaces the cdr of the cons with object.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CONS").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ConsStruct cons = validator.validateType(lispStructs[0], functionName(), "Cons", ConsType.INSTANCE, ConsStruct.class);
		final LispStruct object = lispStructs[1];
		cons.setCdr(object);
		return cons;
	}

	@Override
	protected String functionName() {
		return "RPLACD";
	}
}
