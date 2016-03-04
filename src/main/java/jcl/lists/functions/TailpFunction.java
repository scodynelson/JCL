package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TailpFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public TailpFunction() {
		super("If object is the same as some tail of list, returns true; otherwise, returns false.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct object = lispStructs[0];
		final ListStruct list = validator.validateType(lispStructs[1], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		return BooleanStructs.toLispBoolean(list.tailp(object));
	}

	@Override
	protected String functionName() {
		return "TAILP";
	}
}
